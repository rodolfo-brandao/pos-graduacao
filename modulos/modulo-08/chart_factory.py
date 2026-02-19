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
        source: List[Dict]
    ) -> Figure:
        labels = [row[ylabel] for row in source]
        values = [row[xlabel] for row in source]

        fig, ax = plt.subplots(figsize=(6, 4))

        ax.barh(labels, values)
        ax.set_xlabel(display_xlabel)
        ax.set_ylabel(display_ylabel)
        ax.set_title(chart_title)

        fig.tight_layout()
        return fig
