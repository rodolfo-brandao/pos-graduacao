import matplotlib.pyplot as plt
from matplotlib.figure import Figure
from typing import Dict, List


class ChartFactory:
    def __init__(self) -> None:
        pass

    def plot_bar_chart(
        self,
        xlabel: str,
        ylabel: str,
        display_xlabel: str,
        display_ylabel: str,
        chart_title: str,
        source: List[Dict],
        horizontal_bars: bool=True
    ) -> Figure:
        """
        Creates a `BAR` chart from the given data source,
        filled with the given labels.
        """

        labels = [row[ylabel] for row in source]
        values = [row[xlabel] for row in source]

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


    def plot_pie_chart(
        self,
        xlabel: str,
        ylabel: str,
        chart_title: str,
        source: List[Dict]
    ) -> Figure:
        """
        Creates a `PIE` chart from the given data source,
        filled with the given labels.
        """

        labels = [item[ylabel] for item in source]
        values = [item[xlabel] for item in source]
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
