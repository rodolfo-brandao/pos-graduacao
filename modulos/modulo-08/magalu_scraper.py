#!/usr/bin/env python3
import re
import time
import json
import random
import requests as r
from dataclasses import dataclass, asdict
from datetime import datetime, timezone, timedelta
from pathlib import Path
from typing import Any, Collection, Dict, Iterable, List, Optional, Set
from urllib.parse import urljoin, urlparse, urlencode, parse_qs, urlunparse
from bs4 import BeautifulSoup
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry


BASE_URL = "https://www.magazineluiza.com.br"
AMERICA_SP_TIMEZONE = timezone(timedelta(hours=-3))
PRODUCT_URL_RE = re.compile(r"/.+/p/([a-z0-9]+)/", re.IGNORECASE)
PRICE_RE = re.compile(r"R\$\s*([\d\.\,]+)")
RATING_PAREN_RE = re.compile(r"(\d\.\d)\s*\(\s*(\d+)\s*\)")  # e.g. 5.0 (2)


@dataclass
class Product:
    name: Optional[str]
    category: Optional[str]
    brand: Optional[str]
    price: Optional[float]
    product_url: str
    rating: Optional[float]
    in_stock: Optional[bool]
    extracted_at: str  # ISO8601 w/ timezone -> America/SP


def build_session(
        retries: int,
        status_forcelist: Optional[Collection[int]]=None,
        allowed_methods: Optional[Collection[str]]=None
) -> r.Session:
    """
    Configures a session for HTTP requests.

    :param retries: The number of retries for when a request fails.
    :type retries: int

    :param status_forcelist: A collection of HTTP status codes to
    force the request when it fails.
    :type status_forcelist: Optional[Collection[int]]

    :param allowed_methods: A collection of HTTP verbs that will be
    allowed in the current session.
    :type allowed_methods: Optional[Collection[str]]

    :return: A pre-configured HTTP session object.
    :rtype: Session
    """

    s = r.Session()
    retry = Retry(
        total=retries,
        backoff_factor=0.8,
        status_forcelist=status_forcelist or (429, 500, 502, 503, 504),
        allowed_methods=allowed_methods or ("GET"),
        raise_on_status=False,
    )

    s.mount(
        prefix="https://",
        adapter=HTTPAdapter(max_retries=retry)
    )

    s.headers.update({
        "User-Agent": (
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)"
            "AppleWebKit/537.36 (KHTML, like Gecko)"
            "Chrome/124.0.0.0 Safari/537.36"
        ),
        "Accept": (
            "text/html,application/xhtml+xml,application/xml;"
            "q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,"
            "application/signed-exchange;v=b3;q=0.7"
        ),
        "Accept-Language": "en-US,en;q=0.9",
        "Accept-Encoding": "gzip, deflate, br",
        "Connection": "keep-alive",
        "Upgrade-Insecure-Requests": "1"
    })
    return s


def polite_pause(base: float = 1.2, jitter: float = 0.9) -> None:
    """
    Defines a polite way to simulate a "human-like" delay in order
    to prevent multiple requests from being made abruptly.

    :param base: The minimum amount of time to wait, in seconds.
    :type base: float

    :param jitter: The maximum additional random delay, in seconds.
    :type jitter: float
    """

    time.sleep(base + random.random() * jitter)


def append_jsonl(path: Path, rows: Iterable[Dict[str, Any]]) -> None:
    """
    Appends rows in a JSON file in real time, like a streaming.

    :param path: The path of which the JSON file should be written.
    :type path: Path

    :param rows: The data source to be written in the JSON file.
    :type rows: Iterable[Dict[str, Any]]
    """

    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as f:
        for r in rows:
            f.write(json.dumps(r, ensure_ascii=False) + "\n")


def save_json(path: Path, data: Any) -> None:
    """
    Creates and saves a JSON file with the given data.

    :param path: The path of which the JSON file should be created.
    :type path: Path

    :param data: The data to be written in the JSON file.
    :type data: Any
    """

    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, ensure_ascii=False, indent=4), encoding="utf-8")


def normalize_url(url: str) -> str:
    """
    Helper function to handle a given URL in order to make
    it absolute and strip query/fragment for stable dedupe.

    :param href: The URL to be normalized.
    :type href: str

    :return: A normalized absolute URL.
    :rtype: str
    """

    abs_url = urljoin(BASE_URL, url)
    p = urlparse(abs_url)
    return f"{p.scheme}://{p.netloc}{p.path}"


def is_product_url(href: str) -> bool:
    """
    Helper function to check when a given href is a product page.

    :param href: The page to be checked.
    :type href: str

    :return: `True` if the given href is a product page from the
    Magazine Luiza marketplace. Otherwise, `False`.
    :rtype: bool
    """

    return bool(href and PRODUCT_URL_RE.search(href))


def build_search_page_url(search_url: str, page: int) -> str:
    """
    Helper function to add the a pagination query param to a search URL,
    preserving existing query params if any.

    :param search_url: The search URL to have a pagination query param added.
    :type search_url: str

    :param page: The number of the page to be added as query param.
    :type page: int

    :return: The given search URL built with the respective pagination query param.
    :rtype: str
    """

    p = urlparse(search_url)
    qs = parse_qs(p.query)

    if page > 1:
        qs["page"] = [str(page)]
    else:
        qs.pop("page", None)

    new_query = urlencode(qs, doseq=True)
    return urlunparse((p.scheme, p.netloc, p.path, p.params, new_query, ""))


def brl_to_float(blr_value: str) -> Optional[float]:
    """
    Helper function to format a BLR string value to float
    (e.g. 4.499,10 -> 4499.10).

    :param s: The BLR value to be formatted.
    :type s: str

    :return: The given numeric string value formatted to float.
    :rtype: float | None

    :raise ValueError: There was an error when converting a `str` value into `float`.
    """

    if not blr_value:
        return None
    try:
        return float(blr_value.replace(".", "").replace(",", "."))
    except ValueError:
        return None


def parse_jsonld(soup: BeautifulSoup) -> List[dict]:
    """
    Gets all JSON-LD content from a HTML page, if any.

    :param soup: The object representing a HTML page.
    :type soup: BeautifulSoup

    :return: A list of JSON-LD data extracted from the given HTML page.
    :rtype: List[dict[Any, Any]]

    :raise JSONDecodeError: There was a problem when parsing raw string
    data into JSON format.
    """

    output: List[dict] = []
    for tag in soup.select('script[type="application/ld+json"]'):
        raw = (tag.string or "").strip()

        if not raw:
            continue
        try:
            data = json.loads(raw)
            if isinstance(data, list):
                output.extend([d for d in data if isinstance(d, dict)])
            elif isinstance(data, dict):
                output.append(data)
        except json.JSONDecodeError:
            continue

    return output


def find_product_jsonld(jsonlds: List[dict]) -> Optional[dict]:
    """
    Given a list of JSON-LD, performs a search for product data.

    :param jsonlds: A list containg the JSON-LD data.
    :type jsonlds: List[dict]

    :return: The product data extracted from a JSON-LD, if any.
    :rtype: dict[Any, Any] | None
    """

    for jsonld in jsonlds:
        if jsonld.get("@type") == "Product":
            return jsonld
        graph = jsonld.get("@graph")
        if isinstance(graph, list):
            for node in graph:
                if isinstance(node, dict) and node.get("@type") == "Product":
                    return node
    return None


def find_breadcrumb_jsonld(jsonlds: List[dict]) -> Optional[dict]:
    """
    Given a list of JSON-LD, performs a search for breadcrumb content.

    :param jsonlds: A list containing the JSON-LD data.
    :type jsonlds: List[dict]

    :return: The breadcrumb data extracted from a JSON-LD, if any.
    :rtype: dict[Any, Any] | None
    """

    for jsonld in jsonlds:
        if jsonld.get("@type") == "BreadcrumbList":
            return jsonld
        graph = jsonld.get("@graph")
        if isinstance(graph, list):
            for node in graph:
                if isinstance(node, dict) and node.get("@type") == "BreadcrumbList":
                    return node
    return None


def extract_title(soup: BeautifulSoup) -> Optional[str]:
    """
    Extracts the text from 'h1' HTML tags.

    :param soup: The object representing a HTML page.
    :type soup: BeautifulSoup

    :return: The text from 'h1' HTML tag, if any.
    :rtype: str | None
    """

    h1 = soup.find("h1")
    if not h1:
        return None
    return " ".join(h1.get_text(" ", strip=True).split()) or None


def extract_price(soup: BeautifulSoup) -> Optional[float]:
    """
    Extracts the price content of a product by finding the 'PIX' label on the
    marketplace page, if present. If not, fallback to the first label containing 'R$...'.

    :param soup: The object representing a HTML page.
    :type soup: BeautifulSoup

    :return: The price value of a product, if any.
    :rtype: float | None
    """

    text = soup.get_text("\n", strip=True)
    pix_label = text.lower().find("no pix")

    if pix_label != -1:
        window = text[max(0, pix_label - 140): pix_label + 40]
        pix_price = PRICE_RE.findall(window)
        if pix_price:
            return brl_to_float(pix_price[-1])

    all_prices = PRICE_RE.findall(text)
    return brl_to_float(all_prices[0]) if all_prices else None


def extract_rating(soup: BeautifulSoup) -> Optional[float]:
    """
    Extracts the rating of a product in the marketplace.

    :param soup: The object representing a HTML page.
    :type soup: BeautifulSoup

    :return: The rating value of a product (e.g. 4.5), if any.
    :rtype: float | None

    :raise ValueError: There was an error when converting a `str` value into `float`.
    """

    text = soup.get_text("\n", strip=True)
    re_match = RATING_PAREN_RE.search(text)
    if re_match:
        try:
            return float(re_match.group(1))
        except ValueError:
            return None
    return None


def is_in_stock(soup: BeautifulSoup) -> Optional[bool]:
    """
    Determines if a product is in stock by applying the following heuristic:
    - If page has 'Adicionar à sacola' or 'Comprar agora' -> `True`
    - If page has 'indispon' or 'avise-me' -> `False`
    - Else `None`

    :param soup: The object representing a HTML page.
    :type soup: BeautifulSoup

    :return: `True` if the current product is in stock in the marketplace,
    `False` if the product is unavailable. Otherwise, `None`.
    :rtype: bool | None
    """

    text = soup.get_text(separator="\n", strip=True).lower()
    if "indispon" in text or "avise-me" in text:
        return False
    if "adicionar à sacola" in text or "comprar agora" in text:
        return True
    return None


def extract_brand(soup: BeautifulSoup) -> Optional[str]:
    """
    Looks for the label 'Marca' and get the next meaningful text.
    Works on many Magazine Luiza product pages where specs include
    something like 'Marca: Lenovo'.

    :param soup: The object representing a HTML page.
    :type soup: BeautifulSoup

    :return: The brand of a product (e.g. Apple, Samsung).
    :rtype: str
    """

    for node in soup.find_all(string=True):
        if node and node.strip() == "Marca":
            parent = node.parent

            # [common case] Value is a link right after label:
            next_a = parent.find_next("a")  # type: ignore (Pylance may complain)
            if next_a and next_a.get_text(strip=True):
                return next_a.get_text(strip=True)

            # [fallback] Next text node (skip empty/same label):
            nxt_txt = parent.find_next(string=True)  # type: ignore (Pylance may complain)
            if nxt_txt:
                val = nxt_txt.strip()
                if val and val != "Marca":
                    return val
    return None


def extract_category_from_breadcrumb_jsonld(jsonlds: List[dict]) -> Optional[str]:
    """
    Uses a breadcrumb JSON-LD list to pick the leaf category (penultimate item)
    because the last one can be the product name.

    :param jsonlds: The breadcrumb JSON-LD list.
    :type jsonlds: List[dict]

    :return: The category of a product, if present.
    :rtype: str | None
    """

    bc = find_breadcrumb_jsonld(jsonlds)
    if not bc:
        return None

    items = bc.get("itemListElement")
    if not isinstance(items, list) or not items:
        return None

    names: List[str] = []
    for it in items:
        if not isinstance(it, dict):
            continue
        item = it.get("item")
        if isinstance(item, dict) and item.get("name"):
            names.append(str(item["name"]))
        elif it.get("name"):
            names.append(str(it["name"]))

    if len(names) >= 2:
        return names[-2]  # leaf category
    return None


def extract_category_fallback_from_url(product_url: str) -> Optional[str]:
    """
    Fallback function: Use the internal category segment.
    This is not 'human-friendly', but provides a stable categorization key.

    :param product_url: A product URL from Magalu marketplace.
    :type product_url: str

    :return: The product category, if present.
    :rtype: str | None
    """

    parts = urlparse(product_url).path.strip("/").split("/")

    if "in" in parts:
        idx = parts.index("in")
        if idx + 1 < len(parts):
            return parts[idx + 1]

    return None


def fetch_search_product_urls(session: r.Session, search_url: str) -> Set[str]:
    """
    Fetches product URLs from the a given search URL.

    :param session: A pre-configured session object ready to perform HTTP requests.
    :type session: requests.Session

    :param search_url: A search URL from the Magalu marketplace.
    :type search_url: str

    :return: A set of product URLs from a search URL.
    :rtype: Set[str]
    """

    r = session.get(search_url, timeout=30)
    r.raise_for_status()

    soup = BeautifulSoup(r.text, "lxml")
    urls: Set[str] = set()

    for a in soup.select("a[href]"):
        href = a.get("href")
        if href and is_product_url(href):  # type: ignore (Pylance my complain)
            urls.add(normalize_url(href))  # type: ignore (Pylance my complain)

    return urls


def fetch_product_details(session: r.Session, product_url: str) -> Product:
    """
    Fetches all product details from a given product URL.

    :param session: A pre-configured session object ready to perform HTTP requests.
    :type session: requests.Session

    :param product_url: A product URL from the Magalu marketplace.
    :type product_url: str

    :return: A product object filled with available details from the marketplace.
    :rtype: Product
    """

    r = session.get(product_url, timeout=30)
    r.raise_for_status()

    soup = BeautifulSoup(r.text, "lxml")
    extracted_at = datetime.now(AMERICA_SP_TIMEZONE).isoformat(timespec="seconds")

    # Try JSON-LD first (more semantic when present):
    jsonlds = parse_jsonld(soup)
    prod_ld = find_product_jsonld(jsonlds)

    name = extract_title(soup)
    category = extract_category_from_breadcrumb_jsonld(jsonlds)
    brand = None
    price = None
    rating = None
    in_stock = None

    if prod_ld:
        # Name:
        if isinstance(prod_ld.get("name"), str) and prod_ld.get("name").strip():  # type: ignore (Pylance my complain)
            name = prod_ld["name"].strip()

        # Brand:
        b = prod_ld.get("brand")
        if isinstance(b, dict) and isinstance(b.get("name"), str):
            brand = b["name"].strip() or None
        elif isinstance(b, str):
            brand = b.strip() or None

        # Rating:
        agg = prod_ld.get("aggregateRating")
        if isinstance(agg, dict):
            rv = agg.get("ratingValue")
            try:
                rating = float(rv) if rv is not None else None
            except (ValueError, TypeError):
                rating = None

        # Price + stock:
        offers = prod_ld.get("offers")
        offers_list = offers if isinstance(offers, list) else [offers] if isinstance(offers, dict) else []
        if offers_list and isinstance(offers_list[0], dict):
            o0 = offers_list[0]
            pr = o0.get("price")
            try:
                price = float(pr) if pr is not None else None
            except (ValueError, TypeError):
                price = None

            av = o0.get("availability")
            if isinstance(av, str):
                if "InStock" in av:
                    in_stock = True
                elif "OutOfStock" in av:
                    in_stock = False

    # DOM/text fallbacks:
    if brand is None:
        brand = extract_brand(soup)

    if category is None:
        category = extract_category_fallback_from_url(product_url)

    if price is None:
        price = extract_price(soup)

    if rating is None:
        rating = extract_rating(soup)

    if in_stock is None:
        in_stock = is_in_stock(soup)

    return Product(
        name=name,
        category=category,
        brand=brand,
        price=price,
        product_url=product_url,
        rating=rating,
        in_stock=in_stock,
        extracted_at=extracted_at,
    )


def scrape_search(
    search_url: str,
    out_dir: str,
    max_pages: int = 3,
    max_products_total: Optional[int] = None,
) -> None:
    """
    Crawls search pages in order to extract product URLs.\n
    Then, visits each product page to extract details and saves them into JSONL + JSON files.

    :param search_url: A search URL from the Magalu marketplace.
    :type search_url: str

    :param out_dir: The output directory of which the JSON files should be saved.
    :type out_dir: str

    :param max_pages: The number of pages to cap the search.
    :type max_pages: int

    :param max_products_total: The number of products to visit in each search page.
    :type max_products_total: Optional[int]
    """

    session = build_session(retries=6)
    out = Path(out_dir)
    slug = urlparse(search_url).path.strip("/").replace("/", "_") or "search"
    jsonl_path = out / f"{slug}.jsonl"
    json_path = out / f"{slug}.json"

    if jsonl_path.exists():
        jsonl_path.unlink()

    # 1. Collect product URLs:
    all_urls: List[str] = []
    seen: Set[str] = set()

    for page in range(1, max_pages + 1):
        page_url = build_search_page_url(search_url, page)
        try:
            page_urls = fetch_search_product_urls(session, page_url)
        except Exception as e:
            print(f"[search] page={page} FAIL url={page_url} error={e}")
            break

        new_urls = [u for u in page_urls if u not in seen]
        seen.update(new_urls)
        all_urls.extend(new_urls)

        print(f"[search] page={page} +{len(new_urls)} urls (total={len(all_urls)})")
        polite_pause()

        if max_products_total and len(all_urls) >= max_products_total:
            all_urls = all_urls[:max_products_total]
            break

    # 2. Visit each product URL and extract details:
    rows: List[dict] = []
    for i, url in enumerate(all_urls, start=1):
        try:
            row = fetch_product_details(session, url)
            d = asdict(row)
            append_jsonl(jsonl_path, [d])
            rows.append(d)
            print(f"[product] {i}/{len(all_urls)} OK: {row.name}")
            polite_pause(base=1.4)
        except Exception as e:
            print(f"[product] {i}/{len(all_urls)} FAIL: {url} error={e}")
            polite_pause(base=2.2)

    save_json(json_path, rows)
    print(f"\nSaved:\n- {jsonl_path}\n- {json_path}")


if __name__ == "__main__":
    search_term = "notebook"
    scrape_search(
        search_url=f"https://www.magazineluiza.com.br/busca/{search_term}/",
        max_pages=2,
        max_products_total=100,
        out_dir="JSON/2026-02-12",
    )
