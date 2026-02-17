import json
import random
import re
import time
from dataclasses import dataclass, asdict
from datetime import datetime, timezone, timedelta
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Set
from urllib.parse import urljoin, urlparse, urlencode, parse_qs, urlunparse

import requests
from bs4 import BeautifulSoup
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry


# -----------------------------
# Configuration
# -----------------------------
BASE = "https://www.magazineluiza.com.br"
SAO_PAULO_TZ = timezone(timedelta(hours=-3))  # America/Sao_Paulo


WHITESPACE_RE = re.compile(r"\s+")
PRODUCT_URL_RE = re.compile(r"/.+/p/([a-z0-9]+)/", re.IGNORECASE)
PRICE_RE = re.compile(r"R\$\s*([\d\.\,]+)")
RATING_PAREN_RE = re.compile(r"(\d\.\d)\s*\(\s*(\d+)\s*\)")  # e.g. 5.0 (2)


GENERIC_TITLES = {
    "magazine luiza",
    "magalu",
    "tem no magalu",
    "pra você é magalu",
    "pra voce e magalu",
}


# -----------------------------
# Models
# -----------------------------
@dataclass
class ProductRow:
    name: Optional[str]
    category: Optional[str]
    brand: Optional[str]
    price: Optional[float]
    product_url: str
    rating: Optional[float]
    in_stock: Optional[bool]
    extracted_at: str  # ISO8601 with timezone


# -----------------------------
# Infra helpers
# -----------------------------
def build_session() -> requests.Session:
    s = requests.Session()

    retry = Retry(
        total=6,
        backoff_factor=0.8,
        status_forcelist=(429, 500, 502, 503, 504),
        allowed_methods=("GET",),
        raise_on_status=False,
    )
    s.mount("https://", HTTPAdapter(max_retries=retry))

    s.headers.update(
        {
            "User-Agent": (
                "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                "AppleWebKit/537.36 (KHTML, like Gecko) "
                "Chrome/122.0 Safari/537.36"
            ),
            "Accept-Language": "pt-BR,pt;q=0.9,en;q=0.8",
            "Accept": "text/html,*/*;q=0.8",
            "Connection": "keep-alive",
        }
    )
    return s


def polite_sleep(base: float = 1.2, jitter: float = 0.9) -> None:
    time.sleep(base + random.random() * jitter)


def save_json(path: Path, data: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2), encoding="utf-8")


def append_jsonl(path: Path, rows: Iterable[Dict[str, Any]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as f:
        for r in rows:
            f.write(json.dumps(r, ensure_ascii=False) + "\n")


# -----------------------------
# URL helpers
# -----------------------------
def normalize_url(url: str) -> str:
    """Make absolute, strip query/fragment for stable dedupe."""
    abs_url = urljoin(BASE, url)
    p = urlparse(abs_url)
    return f"{p.scheme}://{p.netloc}{p.path}"


def is_product_url(href: str) -> bool:
    return bool(href and PRODUCT_URL_RE.search(href))


def build_search_page_url(search_url: str, page: int) -> str:
    """
    Given a base search URL like:
      https://www.magazineluiza.com.br/busca/notebook/
    return:
      .../busca/notebook/?page=2
    preserving existing query params if any.
    """
    p = urlparse(search_url)
    qs = parse_qs(p.query)
    if page > 1:
        qs["page"] = [str(page)]
    else:
        qs.pop("page", None)

    new_query = urlencode(qs, doseq=True)
    return urlunparse((p.scheme, p.netloc, p.path, p.params, new_query, ""))


# -----------------------------
# JSON-LD helpers (best effort)
# -----------------------------
def parse_jsonld(soup: BeautifulSoup) -> List[dict]:
    out: List[dict] = []
    for tag in soup.select('script[type="application/ld+json"]'):
        raw = (tag.string or "").strip()
        if not raw:
            continue
        try:
            data = json.loads(raw)
            if isinstance(data, list):
                out.extend([d for d in data if isinstance(d, dict)])
            elif isinstance(data, dict):
                out.append(data)
        except json.JSONDecodeError:
            continue
    return out


def find_product_jsonld(jsonlds: List[dict]) -> Optional[dict]:
    for d in jsonlds:
        if d.get("@type") == "Product":
            return d
        graph = d.get("@graph")
        if isinstance(graph, list):
            for g in graph:
                if isinstance(g, dict) and g.get("@type") == "Product":
                    return g
    return None


def find_breadcrumb_jsonld(jsonlds: List[dict]) -> Optional[dict]:
    for d in jsonlds:
        if d.get("@type") == "BreadcrumbList":
            return d
        graph = d.get("@graph")
        if isinstance(graph, list):
            for g in graph:
                if isinstance(g, dict) and g.get("@type") == "BreadcrumbList":
                    return g
    return None


# -----------------------------
# Parsing primitives
# -----------------------------
def brl_to_float(s: str) -> Optional[float]:
    """Convert '4.499,10' -> 4499.10"""
    if not s:
        return None
    try:
        return float(s.replace(".", "").replace(",", "."))
    except ValueError:
        return None


def _clean_text(s: str) -> str:
    return WHITESPACE_RE.sub(" ", (s or "").strip())


def extract_name(soup: BeautifulSoup) -> Optional[str]:
    candidates: List[str] = []

    for tag in soup.select("h1, h2, h3"):
        txt = _clean_text(tag.get_text(" ", strip=True))
        if not txt:
            continue

        low = txt.lower()
        if low in GENERIC_TITLES:
            continue

        # Avoid super short / non-descriptive headings
        if len(txt) < 15:
            continue

        candidates.append(txt)

    if candidates:
        # The product name is usually the most descriptive/longest heading
        candidates.sort(key=len, reverse=True)
        return candidates[0]

    # Fallback: sometimes content is injected and headings are not present.
    # Try using <meta property="og:title"> or <title> if they contain something useful.
    og = soup.select_one('meta[property="og:title"]')
    if og and og.get("content"):
        txt = _clean_text(og["content"])
        if txt and txt.lower() not in GENERIC_TITLES and len(txt) >= 15:
            return txt

    title = soup.title.string if soup.title and soup.title.string else ""
    title = _clean_text(title)
    if title and title.lower() not in GENERIC_TITLES and len(title) >= 15:
        return title

    return None

    # h1 = soup.find("h1")
    # if not h1:
    #     return None
    # return " ".join(h1.get_text(" ", strip=True).split()) or None


def extract_price_prefer_pix(soup: BeautifulSoup) -> Optional[float]:
    """
    Prefer a price that appears near 'no Pix' if present; fallback to first 'R$ ...'.
    """
    text = soup.get_text("\n", strip=True)

    pix_idx = text.lower().find("no pix")
    if pix_idx != -1:
        window = text[max(0, pix_idx - 140): pix_idx + 40]
        pix_prices = PRICE_RE.findall(window)
        if pix_prices:
            return brl_to_float(pix_prices[-1])

    all_prices = PRICE_RE.findall(text)
    return brl_to_float(all_prices[0]) if all_prices else None


def extract_rating(soup: BeautifulSoup) -> Optional[float]:
    text = soup.get_text("\n", strip=True)
    m = RATING_PAREN_RE.search(text)
    if m:
        try:
            return float(m.group(1))
        except ValueError:
            return None
    return None


def extract_in_stock(soup: BeautifulSoup) -> Optional[bool]:
    """
    Best-effort heuristic:
    - If page has 'Adicionar à sacola' or 'Comprar agora' => True
    - If page has 'indispon' or 'avise-me' => False
    - Else None
    """
    text = soup.get_text("\n", strip=True).lower()

    if "indispon" in text or "avise-me" in text:
        return False
    if "adicionar à sacola" in text or "comprar agora" in text:
        return True
    return None


def extract_brand_from_specs_text(soup: BeautifulSoup) -> Optional[str]:
    """
    Look for a label exactly 'Marca' and return the next meaningful text.
    Works on many Magalu product pages where specs include 'Marca: Lenovo'.
    """
    for node in soup.find_all(string=True):
        if node and node.strip() == "Marca":
            parent = node.parent

            # Common case: value is a link right after label
            nxt_a = parent.find_next("a")
            if nxt_a and nxt_a.get_text(strip=True):
                return nxt_a.get_text(strip=True)

            # Fallback: next text node (skip empty / same label)
            nxt_txt = parent.find_next(string=True)
            if nxt_txt:
                val = nxt_txt.strip()
                if val and val != "Marca":
                    return val
    return None


def extract_category_from_breadcrumb_jsonld(jsonlds: List[dict]) -> Optional[str]:
    """
    Use BreadcrumbList JSON-LD if present, pick the leaf category (penultimate item)
    because last can be the product name.
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
    Fallback: use the internal category segment sometimes present: /in/leip/
    This is not 'human-friendly', but provides a stable categorization key.
    """
    parts = urlparse(product_url).path.strip("/").split("/")
    if "in" in parts:
        idx = parts.index("in")
        if idx + 1 < len(parts):
            return parts[idx + 1]
    return None


# -----------------------------
# Scraping steps
# -----------------------------
def fetch_search_product_urls(session: requests.Session, search_url: str) -> Set[str]:
    r = session.get(search_url, timeout=30)
    r.raise_for_status()

    soup = BeautifulSoup(r.text, "lxml")
    urls: Set[str] = set()

    for a in soup.select("a[href]"):
        href = a.get("href")
        if href and is_product_url(href):
            urls.add(normalize_url(href))

    return urls


def fetch_product_details(session: requests.Session, product_url: str) -> ProductRow:
    r = session.get(product_url, timeout=30)
    r.raise_for_status()

    soup = BeautifulSoup(r.text, "lxml")
    extracted_at = datetime.now(SAO_PAULO_TZ).isoformat(timespec="seconds")

    # Try JSON-LD first (more semantic when present)
    jsonlds = parse_jsonld(soup)
    prod_ld = find_product_jsonld(jsonlds)

    name = extract_name(soup)
    category = extract_category_from_breadcrumb_jsonld(jsonlds)
    brand = None
    price = None
    rating = None
    in_stock = None

    if prod_ld:
        # Name
        if isinstance(prod_ld.get("name"), str) and prod_ld.get("name").strip():
            name = prod_ld["name"].strip()

        # Brand
        b = prod_ld.get("brand")
        if isinstance(b, dict) and isinstance(b.get("name"), str):
            brand = b["name"].strip() or None
        elif isinstance(b, str):
            brand = b.strip() or None

        # Rating
        agg = prod_ld.get("aggregateRating")
        if isinstance(agg, dict):
            rv = agg.get("ratingValue")
            try:
                rating = float(rv) if rv is not None else None
            except (ValueError, TypeError):
                rating = None

        # Price + stock
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

    # DOM/text fallbacks
    if brand is None:
        brand = extract_brand_from_specs_text(soup)

    if category is None:
        category = extract_category_fallback_from_url(product_url)

    if price is None:
        price = extract_price_prefer_pix(soup)

    if rating is None:
        rating = extract_rating(soup)

    if in_stock is None:
        in_stock = extract_in_stock(soup)

    return ProductRow(
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
    max_pages: int = 3,
    max_products_total: Optional[int] = None,
    out_dir: str = "out_magalu",
) -> None:
    """
    - Crawl search pages up to max_pages (1..max_pages)
    - Extract product URLs from each page
    - Visit each product page to extract fields
    - Save JSONL + JSON
    """
    session = build_session()
    out = Path(out_dir)

    slug = urlparse(search_url).path.strip("/").replace("/", "_") or "search"
    jsonl_path = out / f"{slug}.jsonl"
    json_path = out / f"{slug}.json"

    if jsonl_path.exists():
        jsonl_path.unlink()

    # 1) Collect product URLs from search pages
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
        polite_sleep()

        if max_products_total and len(all_urls) >= max_products_total:
            all_urls = all_urls[:max_products_total]
            break

    # 2) Visit each product and extract details
    rows: List[dict] = []
    for i, url in enumerate(all_urls, start=1):
        try:
            row = fetch_product_details(session, url)
            d = asdict(row)
            append_jsonl(jsonl_path, [d])
            rows.append(d)
            print(f"[product] {i}/{len(all_urls)} OK: {row.name}")
            polite_sleep(base=1.4)
        except Exception as e:
            print(f"[product] {i}/{len(all_urls)} FAIL: {url} error={e}")
            polite_sleep(base=2.2)

    save_json(json_path, rows)
    print(f"\nSaved:\n- {jsonl_path}\n- {json_path}")


# -----------------------------
# Example usage
# -----------------------------
if __name__ == "__main__":
    scrape_search(
        search_url="https://www.magazineluiza.com.br/busca/tv/",
        max_pages=2,
        max_products_total=100,  # e.g. 60 if you want a cap
        out_dir="JSON/2026-02-12",
    )
