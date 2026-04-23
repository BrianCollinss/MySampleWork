"""Chart-generation utilities for portfolio reporting outputs."""

from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd


PRIMARY_BLUE = "#1f77d0"
MID_BLUE = "#5b95c8"
LIGHT_BLUE = "#cfd7df"
TEXT_DARK = "#2f2f2f"


def _build_closed_deal_analysis_frame(gold_tables: dict[str, pd.DataFrame]) -> pd.DataFrame:
    """Create a joined closed-deal analysis frame used by dashboard-style visuals."""

    # Pull the Gold facts and dimensions needed for rep, product, and account analysis.
    closed = gold_tables["gold_fct_closed_deals"].copy()
    sales_rep = gold_tables["gold_dim_sales_rep"].copy()
    product = gold_tables["gold_dim_product"].copy()
    account = gold_tables["gold_dim_account"].copy()

    # Join descriptive dimension attributes onto the closed-deal fact grain.
    closed = closed.merge(sales_rep, on="sales_rep_key", how="left")
    closed = closed.merge(product, on="product_key", how="left")
    closed = closed.merge(account, on="account_key", how="left", suffixes=("", "_account"))

    # Rename the measure column after joins so downstream code stays explicit.
    closed = closed.rename(columns={"revenue": "deal_revenue"})

    # Derive a simple product category used in one of the recreated charts.
    closed["product_category"] = closed["product"].map(
        {
            "GTX Basic": "Basic",
            "GTX Plus Basic": "Standard",
            "GTK 500": "Premium",
            "GTX Pro": "Standard",
            "GTX Plus Pro": "Standard",
            "MG Advanced": "Standard",
            "MG Special": "Basic",
        }
    )

    # Add a quarter label for the quarter-product series chart.
    closed["quarter_label"] = "Q" + closed["date"].dt.quarter.astype(str) + " " + closed["date"].dt.year.astype(str)

    return closed


def _style_axis(ax: plt.Axes, title: str, subtitle: str | None = None) -> None:
    """Apply a consistent visual treatment to dashboard-style axes."""

    # Set the main title and optional descriptive subtitle.
    ax.set_title(title, fontsize=15, fontfamily="serif", loc="left", color=TEXT_DARK)
    if subtitle:
        ax.text(0.0, 0.96, subtitle, transform=ax.transAxes, fontsize=10, style="italic", color="#6f6f6f")

    # Remove heavy chart borders for a cleaner dashboard look.
    for spine in ax.spines.values():
        spine.set_visible(False)

    # Keep tick styling muted and readable.
    ax.tick_params(axis="both", length=0, colors="#666666", labelsize=10)


def _truncate_names(series: pd.Series, limit: int = 16) -> pd.Series:
    """Trim long labels so compact dashboard visuals remain readable."""

    # Shorten long labels with an ellipsis while preserving shorter values unchanged.
    return series.apply(lambda value: value if len(str(value)) <= limit else f"{str(value)[: limit - 3]}...")


def _plot_ranked_bar(
    ax: plt.Axes,
    data: pd.DataFrame,
    label_col: str,
    value_col: str,
    title: str,
    subtitle: str | None = None,
    value_fmt: str = "{:,.0f}",
    percentage: bool = False,
    show_in_millions: bool = False,
) -> None:
    """Render a ranked horizontal bar chart matching the requested dashboard style."""

    # Apply a consistent title and layout treatment.
    _style_axis(ax, title, subtitle)

    # Plot highest-ranked values at the top of the chart.
    data = data.iloc[::-1].reset_index(drop=True)
    plot_values = data[value_col] / 1_000_000 if show_in_millions else data[value_col]
    colors = [LIGHT_BLUE] * len(data)
    if len(colors) >= 1:
        colors[-1] = PRIMARY_BLUE
    if len(colors) >= 2:
        colors[-2] = MID_BLUE

    # Draw the horizontal bars with a light-to-dark emphasis.
    ax.barh(data[label_col], plot_values, color=colors, edgecolor="none")

    # Remove x-axis clutter to mimic the reference dashboard cards.
    ax.set_xticks([])
    ax.set_xlabel("")
    ax.set_ylabel("")

    # Add data labels either inside or outside bars depending on available space.
    max_value = plot_values.max() if len(data) else 0
    for idx, (_, row) in enumerate(data.iterrows()):
        value = (row[value_col] / 1_000_000) if show_in_millions else row[value_col]
        label = f"{value:.1f}%" if percentage else value_fmt.format(value)
        inside = value >= max_value * 0.72 if max_value else False
        text_x = value - max_value * 0.06 if inside else value + max_value * 0.03
        ha = "right" if inside else "left"
        color = "white" if inside else "#666666"
        ax.text(text_x, idx, label, va="center", ha=ha, color=color, fontsize=10, fontweight="bold" if inside else None)


def _save_fig(fig: plt.Figure, output_dir: Path, name: str) -> None:
    """Save a figure to the reports folder with tight bounding boxes."""

    # Persist the figure with a white background to match the dashboard references.
    fig.savefig(output_dir / name, dpi=150, bbox_inches="tight", facecolor="white")
    plt.close(fig)


def _apply_million_axis_format(ax: plt.Axes, axis: str = "y", decimals: int = 0) -> None:
    """Format a chart axis in millions and suppress scientific-notation offsets."""

    # Scale raw axis values to millions so charts show human-readable labels with an M suffix.
    formatter = plt.FuncFormatter(lambda value, _: f"{value / 1_000_000:.{decimals}f}M")

    # Apply the formatter to the requested axis and hide matplotlib's separate offset text.
    if axis == "x":
        ax.xaxis.set_major_formatter(formatter)
        ax.xaxis.offsetText.set_visible(False)
    else:
        ax.yaxis.set_major_formatter(formatter)
        ax.yaxis.offsetText.set_visible(False)


def _plot_quarter_series_chart(
    series_data: pd.DataFrame,
    value_col: str,
    title: str,
    figures_dir: Path,
    file_name: str,
    y_as_percent: bool = False,
    y_fmt: str | None = None,
) -> None:
    """Render a quarter-level line chart for one selected KPI."""

    # Create a consistent canvas for the quarterly series charts.
    fig, ax = plt.subplots(figsize=(6.2, 5.0))
    _style_axis(ax, title)

    # Plot a single highlighted series with a lightweight dashboard line style.
    ax.plot(series_data["quarter_label"], series_data[value_col], color="#2484ff", marker="o", linestyle=(0, (2, 3)), linewidth=1.8)
    ax.grid(axis="y", linestyle=(0, (1, 4)), color="#cfcfcf")
    ax.set_xlabel("")
    ax.set_ylabel("")

    # Apply readable y-axis formatting based on the metric being displayed.
    if y_as_percent:
        ax.set_ylim(max(0, series_data[value_col].min() - 0.03), min(1, series_data[value_col].max() + 0.04))
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda value, _: f"{value:.0%}"))
    elif y_fmt == "currency_m":
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda value, _: f"${value / 1_000_000:.1f}M"))
        ax.yaxis.offsetText.set_visible(False)
    elif y_fmt == "integer":
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda value, _: f"{value:,.0f}"))
    elif y_fmt == "days":
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda value, _: f"{value:.0f}"))

    _save_fig(fig, figures_dir, file_name)


def _create_dashboard_recreations(gold_tables: dict[str, pd.DataFrame], figures_dir: Path, tables_dir: Path) -> None:
    """Create dashboard-style figures and a summary table matching the supplied examples."""

    # Build a joined analysis frame from Gold outputs for all recreated visuals.
    closed = _build_closed_deal_analysis_frame(gold_tables)
    won = closed[closed["deal_stage"] == "Won"].copy()
    closed_outcomes = closed[closed["deal_stage"].isin(["Won", "Lost"])].copy()

    # Create the ranked metrics used in the small dashboard card visuals.
    wins_by_agent = won.groupby("sales_agent").size().reset_index(name="wins").sort_values("wins", ascending=False).head(5)
    wins_by_agent["sales_agent"] = _truncate_names(wins_by_agent["sales_agent"])

    revenue_by_agent = (
        won.groupby("sales_agent", as_index=False)["deal_revenue"].sum().sort_values("deal_revenue", ascending=False).head(5)
    )
    revenue_by_agent["sales_agent"] = _truncate_names(revenue_by_agent["sales_agent"])

    top_agents_by_wins = won.groupby("sales_agent").size().reset_index(name="wins").sort_values("wins", ascending=False).head(5)
    avg_days_to_win = (
        won.groupby("sales_agent", as_index=False)["days_to_close"]
        .mean()
        .merge(top_agents_by_wins[["sales_agent"]], on="sales_agent", how="inner")
        .sort_values("days_to_close", ascending=False)
    )
    avg_days_to_win["sales_agent"] = _truncate_names(avg_days_to_win["sales_agent"])

    win_percentage = (
        closed_outcomes.groupby("sales_agent")["deal_stage"]
        .apply(lambda s: s.eq("Won").mean() * 100)
        .reset_index(name="win_pct")
        .merge(top_agents_by_wins[["sales_agent"]], on="sales_agent", how="inner")
        .sort_values("win_pct", ascending=False)
    )
    win_percentage["sales_agent"] = _truncate_names(win_percentage["sales_agent"])

    # Render the four ranked card visuals shown in the screenshots.
    fig, ax = plt.subplots(figsize=(3.3, 2.4))
    _plot_ranked_bar(ax, wins_by_agent, "sales_agent", "wins", "Deals Win by sales agent")
    _save_fig(fig, figures_dir, "deals_won_by_agent.png")

    fig, ax = plt.subplots(figsize=(3.3, 2.4))
    _plot_ranked_bar(
        ax,
        revenue_by_agent,
        "sales_agent",
        "deal_revenue",
        "Sales agent with highest revenue",
        value_fmt="${:,.2f}M",
        show_in_millions=True,
    )
    _save_fig(fig, figures_dir, "revenue_by_agent.png")

    fig, ax = plt.subplots(figsize=(3.3, 2.4))
    _plot_ranked_bar(ax, avg_days_to_win, "sales_agent", "days_to_close", "Avg days to win a deal", value_fmt="{:.0f}")
    _save_fig(fig, figures_dir, "avg_days_to_win.png")

    fig, ax = plt.subplots(figsize=(3.3, 2.4))
    _plot_ranked_bar(ax, win_percentage, "sales_agent", "win_pct", "Win Percentage", percentage=True)
    _save_fig(fig, figures_dir, "win_percentage.png")

    # Build quarter-level metrics across the full closed-outcomes dataset.
    quarter_series = (
        closed_outcomes.groupby("quarter_label", as_index=False)
        .agg(
            win_rate=("deal_stage", lambda s: s.eq("Won").mean()),
            wins=("deal_stage", lambda s: int(s.eq("Won").sum())),
            revenue=("deal_revenue", lambda s: float(s[closed_outcomes.loc[s.index, "deal_stage"] == "Won"].sum())),
            avg_scl=("days_to_close", "mean"),
        )
    )
    quarter_series = (
        quarter_series.assign(
            sort_key=pd.PeriodIndex(
                quarter_series["quarter_label"].str[-4:] + "Q" + quarter_series["quarter_label"].str[1],
                freq="Q",
            )
        )
        .sort_values("sort_key")
        .drop(columns="sort_key")
    )

    # Render four separate quarter-level figures for the main summary KPIs.
    _plot_quarter_series_chart(
        quarter_series,
        "win_rate",
        "Win % by Quarter",
        figures_dir,
        "win_pct_by_quarter_series.png",
        y_as_percent=True,
    )
    _plot_quarter_series_chart(
        quarter_series,
        "wins",
        "Wins by Quarter",
        figures_dir,
        "wins_by_quarter_series.png",
        y_fmt="integer",
    )
    _plot_quarter_series_chart(
        quarter_series,
        "revenue",
        "Revenue by Quarter",
        figures_dir,
        "revenue_by_quarter_series.png",
        y_fmt="currency_m",
    )
    _plot_quarter_series_chart(
        quarter_series,
        "avg_scl",
        "Avg SCL by Quarter",
        figures_dir,
        "avg_scl_by_quarter_series.png",
        y_fmt="days",
    )

    # Aggregate revenue by product category for the categorical revenue chart.
    revenue_by_category = (
        won.groupby("product_category", as_index=False)["deal_revenue"].sum().sort_values("deal_revenue", ascending=False)
    )
    category_order = ["Standard", "Premium", "Basic"]
    revenue_by_category["product_category"] = pd.Categorical(revenue_by_category["product_category"], categories=category_order, ordered=True)
    revenue_by_category = revenue_by_category.sort_values("product_category")

    # Render the product-category column chart.
    fig, ax = plt.subplots(figsize=(4.2, 3.1))
    _style_axis(ax, "Revenue by Product Categories")
    ax.bar(revenue_by_category["product_category"].astype(str), revenue_by_category["deal_revenue"], color="#4698e8", width=0.7)
    ax.grid(axis="y", linestyle=(0, (1, 4)), color="#cfcfcf")
    ax.set_axisbelow(True)
    _apply_million_axis_format(ax, axis="y", decimals=1)
    for position, (_, row) in enumerate(revenue_by_category.iterrows()):
        label_y = row["deal_revenue"] - revenue_by_category["deal_revenue"].max() * 0.08
        label_color = "white"
        if row["deal_revenue"] < revenue_by_category["deal_revenue"].max() * 0.12:
            label_y = row["deal_revenue"] + revenue_by_category["deal_revenue"].max() * 0.03
            label_color = "#666666"
        ax.text(
            position,
            label_y,
            f"${row['deal_revenue'] / 1_000_000:.2f}M",
            ha="center",
            va="center",
            color=label_color,
            fontweight="bold" if label_color == "white" else None,
        )
    _save_fig(fig, figures_dir, "revenue_by_product_categories.png")

    # Aggregate revenue by product for the ranked product chart.
    revenue_by_product = won.groupby("product", as_index=False)["deal_revenue"].sum().sort_values("deal_revenue", ascending=False)
    revenue_by_product["product"] = _truncate_names(revenue_by_product["product"], limit=18)

    # Render the ranked product revenue bar chart.
    fig, ax = plt.subplots(figsize=(4.3, 3.1))
    _plot_ranked_bar(
        ax,
        revenue_by_product.head(7),
        "product",
        "deal_revenue",
        "Revenue by product",
        value_fmt="${:,.1f}M",
        show_in_millions=True,
    )
    _save_fig(fig, figures_dir, "revenue_by_product.png")

    # Build the country comparison between United States and all other countries.
    revenue_by_country = won.groupby("office_location", as_index=False)["deal_revenue"].sum()
    us_revenue = float(revenue_by_country.loc[revenue_by_country["office_location"] == "United States", "deal_revenue"].sum())
    other_revenue = float(revenue_by_country.loc[revenue_by_country["office_location"] != "United States", "deal_revenue"].sum())
    total_country_revenue = us_revenue + other_revenue

    # Render the donut chart comparing United States against all other countries.
    fig, ax = plt.subplots(figsize=(6.4, 4.4))
    ax.text(
        0.0,
        1.12,
        "Comparing US revenue with other countries",
        transform=ax.transAxes,
        fontsize=15,
        fontfamily="serif",
        color=TEXT_DARK,
    )
    ax.text(
        0.0,
        1.03,
        f"US is dominating with {us_revenue / total_country_revenue:.0%} revenue overall",
        transform=ax.transAxes,
        fontsize=12,
        color=TEXT_DARK,
    )
    for spine in ax.spines.values():
        spine.set_visible(False)
    ax.tick_params(axis="both", length=0, colors="#666666", labelsize=10)
    wedges, _ = ax.pie(
        [us_revenue, other_revenue],
        labels=None,
        colors=["#6fa8dc", "#d0d0d0"],
        startangle=90,
        counterclock=False,
        wedgeprops=dict(width=0.48, edgecolor="white"),
    )
    ax.set_aspect("equal")
    ax.set_xlim(-1.8, 1.8)
    ax.set_ylim(-1.35, 1.35)

    # Place external callout labels with connectors so text does not overlap the donut or title.
    ax.annotate(
        f"Others\n{other_revenue / 1_000_000:.2f}M ({other_revenue / total_country_revenue:.2%})",
        xy=(-0.45, 0.78),
        xytext=(-1.65, 0.92),
        ha="left",
        va="center",
        fontsize=10,
        color="#2a5f98",
        arrowprops=dict(arrowstyle="-", color="#9aa6b2", lw=1.0, connectionstyle="angle,angleA=0,angleB=90,rad=0"),
    )
    ax.annotate(
        f"United States\n{us_revenue / 1_000_000:.2f}M ({us_revenue / total_country_revenue:.2%})",
        xy=(0.58, -0.72),
        xytext=(1.12, -0.92),
        ha="left",
        va="center",
        fontsize=10,
        color="#2a5f98",
        arrowprops=dict(arrowstyle="-", color="#9aa6b2", lw=1.0, connectionstyle="angle,angleA=180,angleB=-90,rad=0"),
    )
    _save_fig(fig, figures_dir, "us_vs_other_countries.png")

    # Aggregate revenue by sector for the sector ranking visual.
    revenue_by_sector = won.groupby("sector", as_index=False)["deal_revenue"].sum().sort_values("deal_revenue", ascending=False)

    # Render the sector revenue chart.
    fig, ax = plt.subplots(figsize=(4.7, 3.3))
    _plot_ranked_bar(
        ax,
        revenue_by_sector.head(10),
        "sector",
        "deal_revenue",
        "Revenue by sector",
        value_fmt="${:,.2f}M",
        show_in_millions=True,
    )
    _save_fig(fig, figures_dir, "revenue_by_sector.png")

    # Build the country-sector summary table mirroring the screenshot's metrics.
    country_sector_table = (
        closed_outcomes.groupby(["office_location", "sector"], as_index=False)
        .agg(
            wins=("deal_stage", lambda s: int(s.eq("Won").sum())),
            closed_opportunities=("deal_stage", "count"),
            avg_scl=("days_to_close", "mean"),
            revenue=("deal_revenue", lambda s: float(s[closed_outcomes.loc[s.index, "deal_stage"] == "Won"].sum())),
        )
    )
    country_sector_table["win_pct"] = country_sector_table["wins"] / country_sector_table["closed_opportunities"]
    country_sector_table["avg_revenue"] = country_sector_table["revenue"] / country_sector_table["wins"].replace(0, pd.NA)
    country_sector_table = country_sector_table.sort_values(["office_location", "revenue"], ascending=[True, False]).head(20).copy()
    country_sector_table = country_sector_table.rename(columns={"office_location": "Country", "sector": "Sector"})
    country_sector_table["Win %"] = (country_sector_table["win_pct"] * 100).round(1)
    country_sector_table["Avg SCL"] = country_sector_table["avg_scl"].round(2)
    country_sector_table["Revenue"] = country_sector_table["revenue"].round(0)
    country_sector_table["Avg Revenue"] = country_sector_table["avg_revenue"].round(0)
    export_table = country_sector_table[["Country", "Sector", "wins", "Win %", "Avg SCL", "Revenue", "Avg Revenue"]].rename(columns={"wins": "Wins"})
    tables_dir.mkdir(parents=True, exist_ok=True)
    export_table.to_csv(tables_dir / "country_sector_summary.csv", index=False)

    # Render the country-sector summary table as a dashboard-style PNG.
    fig, ax = plt.subplots(figsize=(7.4, 9.4))
    ax.axis("off")
    fig.subplots_adjust(left=0.02, right=0.90, top=0.98, bottom=0.02)
    table_display = export_table.copy()
    table_display["Revenue"] = table_display["Revenue"].map(lambda v: f"${v:,.0f}")
    table_display["Avg Revenue"] = table_display["Avg Revenue"].map(lambda v: f"${v:,.0f}")
    table_display["Win %"] = table_display["Win %"].map(lambda v: f"{v:.1f}%")
    table = ax.table(cellText=table_display.values, colLabels=table_display.columns, loc="center", cellLoc="left", colLoc="left")
    table.auto_set_font_size(False)
    table.set_fontsize(9)
    table.scale(1, 1.35)
    for (row, col), cell in table.get_celld().items():
        cell.set_edgecolor("white")
        if row == 0:
            cell.set_text_props(color="#1a58a8", weight="bold")
            cell.set_facecolor("white")
        elif col in [3, 4]:
            cell.set_facecolor("#dce8f5")
        else:
            cell.set_facecolor("white")
    _save_fig(fig, figures_dir, "country_sector_summary.png")


def create_charts(gold_tables: dict[str, pd.DataFrame], output_dir: Path) -> None:
    """Create the core time-series charts used in the reporting layer."""

    # Resolve the reporting subfolders used for figures and tabular exports.
    figures_dir = output_dir / "figures"
    tables_dir = output_dir / "tables"
    figures_dir.mkdir(parents=True, exist_ok=True)
    tables_dir.mkdir(parents=True, exist_ok=True)

    # Pull the Gold marts required for trend, cohort, and forecast visuals.
    sales_perf = gold_tables["gold_mart_sales_performance"].sort_values("date")
    customer = gold_tables["gold_mart_customer_performance"].sort_values("date")
    forecast = gold_tables["gold_mart_forecasting"].sort_values("date")

    # Render the monthly revenue trend with a rolling average overlay.
    fig, ax = plt.subplots(figsize=(10, 5))
    ax.plot(sales_perf["date"], sales_perf["closed_won_revenue"], label="Monthly revenue", marker="o")
    ax.plot(sales_perf["date"], sales_perf["closed_won_revenue_rolling_3m"], label="3M rolling avg", marker="o")
    ax.set_title("Monthly Revenue Trend")
    _apply_million_axis_format(ax, axis="y", decimals=1)
    ax.legend()
    fig.tight_layout()
    fig.savefig(figures_dir / "monthly_revenue_trend.png")
    plt.close(fig)

    # Render actual and forecast revenue on a single time-series chart.
    fig, ax = plt.subplots(figsize=(10, 5))
    actual = forecast[forecast["forecast_type"] == "actual"]
    projected = forecast[forecast["forecast_type"] == "forecast"]
    forecast_line = pd.concat(
        [
            actual.tail(1),
            projected,
        ],
        ignore_index=True,
    )
    ax.plot(actual["date"], actual["forecast_revenue"], label="Actual", marker="o")
    ax.plot(forecast_line["date"], forecast_line["forecast_revenue"], label="Forecast", linestyle="--", marker="o")
    ax.set_title("Revenue Forecast")
    _apply_million_axis_format(ax, axis="y", decimals=1)
    ax.legend()
    fig.tight_layout()
    fig.savefig(figures_dir / "revenue_forecast.png")
    plt.close(fig)

    # Render the requested dashboard-style recreations using the same Gold outputs.
    _create_dashboard_recreations(gold_tables, figures_dir, tables_dir)
