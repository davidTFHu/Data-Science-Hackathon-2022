def data_subset(input_data, mask, cols, rank_groupby):
    # Subset to mask and cols
    data = input_data.copy().loc[mask, cols]
    data.reset_index(drop=True, inplace=True)

    # Rank by groups
    data["rank"] = data.groupby(rank_groupby)["value"].rank(ascending=False)

    return data


def data_subset_ownership(usage, data_subset):
    mask = (usage["metric_name"] == "Total Users") & (usage["rank"] <= 5)
    cols = ["country", "base", "brand"]
    df_top_ownership = usage.copy().loc[mask, cols]
    df_top_ownership["top_ownership"] = True

    data_subset = data_subset.merge(df_top_ownership, how="outer")
    data_subset["top_ownership"].fillna(False, inplace=True)

    return data_subset


def data_subset_wide(data_subset, index_cols):
    # Reshape data subset to wide format
    return data_subset.pivot(index=index_cols, columns="metric_name", values="value").reset_index()


def plot_scatter(data_subset_wide, x, y, label=None, color=None, facet_col="country", facet_row="base", coord_equal=False):

    plot = (
        data_subset_wide
        .plot.scatter(
            x=x,
            y=y,
            text=label,
            color=color,
            hover_data=["brand"],
            facet_col=facet_col,
            facet_row=facet_row
        )
        .update_traces(marker=dict(size=10))
        .update_layout(height=800)
    )

    if coord_equal:
        plot.update_xaxes(range=[0, 60], nticks=4, constrain='domain')
        plot.update_yaxes(range=[0, 60], nticks=4, scaleanchor="x", scaleratio=1, constrain='domain')

    return plot.show()


def plot_rank(data_subset, color="top_ownership"):
    plot = data_subset.plot.bar(
        x="rank",
        y="value",
        text="brand",
        color=color,
        facet_col="country",
        facet_row="base"
    ).update_layout(height=800)

    return plot.show()
