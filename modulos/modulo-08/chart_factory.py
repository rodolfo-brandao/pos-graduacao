import matplotlib.pyplot as plt
from datetime import datetime
from matplotlib.figure import Figure
import matplotlib.dates as mdates
from typing import Dict, List


class ChartFactory:
    """
    Custom factory to create:
    - Bar charts
    - Pie charts
    - Time Series charts
    """

    def __init__(self) -> None:
        pass

    def create_bar_chart(
        self,
        xlabel: str,
        ylabel: str,
        display_xlabel: str,
        display_ylabel: str,
        chart_title: str,
        source: List[Dict],
        horizontal_bars: bool=True) -> Figure:
        """
        Creates a `BAR` chart from the given data source,
        filled with the given labels.
        """

        labels = [row[ylabel] for row in source]
        values = [float(row[xlabel]) for row in source]

        fig, ax = plt.subplots(figsize=(4, 3))

        if horizontal_bars:
            ax.barh(labels, values)
        else:
            ax.bar(labels, values, width=0.3)

        ax.set_xlabel(display_xlabel)
        ax.set_ylabel(display_ylabel)
        ax.set_title(chart_title)

        ax.tick_params(axis="x", labelsize=6)
        ax.tick_params(axis="y", labelsize=6)

        fig.tight_layout()
        return fig


    def create_pie_chart(
        self,
        xlabel: str,
        ylabel: str,
        chart_title: str,
        source: List[Dict]) -> Figure:
        """
        Creates a `PIE` chart from the given data source,
        filled with the given labels.
        """

        labels = [item[ylabel] for item in source]
        values = [float(item[xlabel]) for item in source]
        fig, ax = plt.subplots(figsize=(5, 4))

        ax.pie(
            values,
            labels=labels,
            autopct="%1.1f%%",
            startangle=90
        )

        ax.set_title(chart_title)
        ax.axis("equal")
        return fig


    def create_time_series_chart(
        self,
        xlabel: str,
        ylabel: str,
        display_xlabel: str,
        display_ylabel: str,
        chart_title: str,
        source: List[Dict]) -> Figure:
        """
        Creates a `TIME SERIES` chart from the given data source
        filled withe the given labels.
        """

        dates = [datetime.strptime(item[xlabel], "%Y-%m-%d") for item in source]
        prices = [float(item[ylabel]) for item in source]

        fig, ax = plt.subplots(figsize=(7, 3.5))

        ax.plot(dates, prices, marker="o", linewidth=2)  # type: ignore

        ax.xaxis.set_major_locator(mdates.DayLocator())
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m-%d"))

        ax.set_title(chart_title)
        ax.set_xlabel(display_xlabel)
        ax.set_ylabel(display_ylabel)

        fig.autofmt_xdate()
        fig.tight_layout()

        return fig
