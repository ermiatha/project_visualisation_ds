# pycodes/madrid_pollution_plot.py

import os
import glob
from pathlib import Path

import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots


WHO_POLLUTANTS = ["PM10", "O_3", "NO_2", "SO_2", "CO"]


def load_data(folder_path="VDS2526_Madrid"):
    folder_path = Path(folder_path)

    csv_files = [
        str(folder_path / "madrid_2001.csv"),
        str(folder_path / "madrid_2018.csv")
    ]

    for f in csv_files:
        if not Path(f).exists():
            raise FileNotFoundError(f"Missing file: {f}")

    dfs = []

    for file in csv_files:
        year = Path(file).stem.replace("madrid_", "")

        df = pd.read_csv(file)
        df["year"] = int(year)

        dfs.append(df)

    df = pd.concat(dfs, ignore_index=True)

    stations = pd.read_csv(folder_path / "stations.csv")

    df["date"] = pd.to_datetime(
        df["date"].astype(str),
        format="mixed",
        utc=True,
        errors="coerce"
    )

    df["year"] = df["date"].dt.year
    df["month"] = df["date"].dt.month
    df["day"] = df["date"].dt.day
    df["hour"] = df["date"].dt.hour

    df = df.merge(
        stations[["id", "name", "lat", "lon"]],
        left_on="station",
        right_on="id",
        how="left"
    )

    return df, stations


def prepare_data(df, stations):
    df_2001_missing = (
        df[df["year"] == 2001]
        .groupby("station")
        .agg(lambda x: (x.isnull().sum() / len(x)) * 100)[WHO_POLLUTANTS]
    )

    df_2018_missing = (
        df[df["year"] == 2018]
        .groupby("station")
        .agg(lambda x: (x.isnull().sum() / len(x)) * 100)[WHO_POLLUTANTS]
    )

    station_dict = {}

    for year, missing_df in [("2001", df_2001_missing), ("2018", df_2018_missing)]:
        for pollutant in WHO_POLLUTANTS:
            station_dict[(year, pollutant)] = set(
                missing_df[missing_df[pollutant] <= 50].index.tolist()
            )

    common_stations = {}

    for pollutant in WHO_POLLUTANTS:
        common_stations[pollutant] = sorted(
            station_dict[("2001", pollutant)]
            & station_dict[("2018", pollutant)]
        )

    stations_new = stations.set_index("id")
    pollutants = list(common_stations.keys())

    avg_2001 = (
        df[df["year"] == 2001]
        .groupby("station")[pollutants]
        .mean()
        .join(stations_new[["name", "lat", "lon"]])
    )

    avg_2018 = (
        df[df["year"] == 2018]
        .groupby("station")[pollutants]
        .mean()
        .join(stations_new[["name", "lat", "lon"]])
    )

    return common_stations, pollutants, avg_2001, avg_2018


def build_figure(data_dir="VDS2526_Madrid"):
    df, stations = load_data(data_dir)

    common_stations, pollutants, avg_2001, avg_2018 = prepare_data(df, stations)

    def get_plot_data(pollutant):
        stations_list = common_stations[pollutant]

        x_2001 = avg_2001.loc[stations_list, pollutant]
        x_2018 = avg_2018.loc[stations_list, pollutant]

        diff = x_2018 - x_2001

        names = avg_2018.loc[stations_list, "name"]
        lats = avg_2018.loc[stations_list, "lat"]
        lons = avg_2018.loc[stations_list, "lon"]

        return stations_list, x_2001, x_2018, diff, names, lats, lons

    fig = make_subplots(
        rows=1,
        cols=2,
        column_widths=[0.65, 0.35],
        subplot_titles=("Average Value in 2018", "Diverging Bar: Change 2001→2018"),
        specs=[[{"type": "mapbox"}, {"type": "xy"}]]
    )

    for i, pollutant in enumerate(pollutants):
        stations_list, x_2001, x_2018, diff, names, lats, lons = get_plot_data(pollutant)

        scaled_size = (x_2018 / x_2018.max()) * 50
        visible = i == 0

        sorted_idx = diff.sort_values(ascending=True).index
        sorted_diff = diff.loc[sorted_idx]
        sorted_names = names.loc[sorted_idx]
        sorted_2018 = x_2018.loc[sorted_idx]

        fig.add_trace(
            go.Scattermapbox(
                lat=lats,
                lon=lons,
                mode="markers",
                text=names,
                hovertemplate="<b>%{text}</b><br>Mean 2018: %{marker.color:.2f}<extra></extra>",
                visible=visible,
                marker=go.scattermapbox.Marker(
                    size=scaled_size,
                    color=x_2018,
                    colorscale="Reds",
                    showscale=False,
                    cmin=x_2018.min(),
                    cmax=x_2018.max(),
                    opacity=0.8
                )
            )
        )

        fig.add_trace(
            go.Bar(
                x=sorted_diff,
                y=list(range(len(sorted_names))),
                orientation="h",
                visible=visible,
                text=[f"{v:.1f}" for v in sorted_diff],
                textposition="outside",
                textfont=dict(size=10),
                customdata=sorted_names,
                hovertemplate="<b>%{customdata}</b><br>Change: %{x:.2f}<extra></extra>",
                marker=dict(
                    color=sorted_2018,
                    colorscale="Reds",
                    showscale=True,
                    colorbar=dict(x=1.02, len=0.9),
                    cmin=x_2018.min(),
                    cmax=x_2018.max()
                )
            ),
            row=1,
            col=2
        )

    buttons = []

    for i, pollutant in enumerate(pollutants):
        visibility = [False] * (len(pollutants) * 2)
        visibility[i * 2] = True
        visibility[i * 2 + 1] = True

        buttons.append(
            dict(
                label=pollutant,
                method="update",
                args=[
                    {"visible": visibility},
                    {
                        "title": dict(
                            text=f"{pollutant}: Top Polluted Areas in Madrid 2018 & Change 2001→2018",
                            x=0.05,
                            xanchor="left"
                        )
                    }
                ]
            )
        )

    fig.update_layout(
        mapbox=dict(
            style="open-street-map",
            center=dict(lat=40.42, lon=-3.70),
            zoom=11,
            domain=dict(x=[0, 0.65], y=[0, 1])
        ),
        updatemenus=[
            dict(
                buttons=buttons,
                direction="down",
                x=0.95,
                y=1.32,
                xanchor="right",
                showactive=True
            )
        ],
        title=dict(
            text=f"{pollutants[0]}: Top Polluted Areas in Madrid 2018 & Change 2001→2018",
            x=0.05,
            xanchor="left"
        ),
        showlegend=False,
        height=550,
        plot_bgcolor="lightgrey",
        margin=dict(l=50, r=100, t=80, b=100)
    )

    fig.add_annotation(
        text="<b>Pollutant</b>",
        x=0.8,
        y=1.3,
        xref="paper",
        yref="paper",
        showarrow=False,
        font=dict(size=13)
    )

    fig.update_layout(
        yaxis=dict(
            showticklabels=False,
            showgrid=False,
            zeroline=False,
            showline=False
        )
    )

    fig.update_xaxes(
        title_text="Change (2018 - 2001)",
        title_standoff=30,
        zeroline=True,
        zerolinecolor="red",
        zerolinewidth=2,
        automargin=True,
        row=1,
        col=2
    )

    return fig


def save_figure_html(
    data_dir="VDS2526_Madrid",
    output_html="www/generated/madrid_pollution.html"
):
    fig = build_figure(data_dir)

    output_html = Path(output_html)
    output_html.parent.mkdir(parents=True, exist_ok=True)

    fig.write_html(
        str(output_html),
        include_plotlyjs="cdn",
        full_html=True
    )

    return str(output_html)


if __name__ == "__main__":
    save_figure_html()