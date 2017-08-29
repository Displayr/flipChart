#' Streamgraph
#'
#' Area chart centerd around the x-axis rather than on top of it.
#' @param x A BasicTable, with columns containing the dates or other numericx-axis variable.
#' @param colors The colors of the streams.
#' @param y.axis.show If FALSE, the y-axis is not shown.
#' @param y.number.ticks The total number of ticks on the y-axis.
#' @param hover.decimals The number of decimals to show in hovers.
#' @param x.tick.interval The frequency of ticks on the x-axis. Where the data crosses multiple years, re-starts at each year.
#' @param x.number.format THe number format of the x-axis. Only integers ('Number') and dates are supported.
#' @param margin.top Top margin (default should be fine, this allows for fine-tuning plot space)
#' @param margin.right Right margin (default should be fine, this allows for fine-tuning plot space)
#' @param margin.bottom Bottom margin (default should be fine, this allows for fine-tuning plot space)
#' @param margin.left Left margin (default should be fine, this allows for fine-tuning plot space)
#' @importFrom streamgraph streamgraph sg_fill_manual sg_axis_x sg_axis_y
#' @importFrom flipTime ParseDates
#' @export
Streamgraph <- function(x,
                        colors = c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#4473C5", "#70AD46", "#255F91", "#9E480D", "#636365", "#987300", "#26408B", "#42682B"),
                        y.axis.show = TRUE,
                        y.number.ticks = 5,
                        hover.decimals = 2,
                        x.tick.interval = 1,
                        x.number.format = "%d %b %y",
                        margin.top = 20,
                        margin.left = 50,
                        margin.bottom = 30,
                        margin.right = 40)
{

    is.date <- isDate(x.number.format)
    if (!is.date && x.number.format != "Number")
        warning("Streamgraph only supports interger and date x-axes.")
    x <- round(x, hover.decimals)
    columns <- colnames(x)
    columns <- if(is.date) ParseDates(columns) else as.integer(columns)
    df <- data.frame(value = as.numeric(t(x)), date = columns, key = rep(rownames(x), rep(ncol(x), nrow(x))))
    sg <- streamgraph(data = df,
                key = "key",
                value = "value",
                date = "date",
                offset = "silhouette",
                interpolate = "cardinal",
                interactive = TRUE,
                scale = "date",
                top = margin.top,
                right = margin.right,
                left = margin.left,
                bottom = margin.bottom)
    sg <- sg_fill_manual(sg, values = colors)
    if (!y.axis.show)
        sg <- sg_axis_y(sg, 0)
    else
        sg <- sg_axis_y(sg, tick_count = y.number.ticks)
    sg <- sg_axis_x(sg, tick_interval = x.tick.interval, tick_format = if (is.date) x.number.format else NULL)
    sg
    }
