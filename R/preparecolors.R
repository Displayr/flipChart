#' Generates vectors of colors for use by CChart.
#' Wrapper for \code{GetNumColors} and \code{ChartColors}.
#'
#' @param dat The data to be plotted, from which the number of required colors are deduced.
#' @param chart.type The name of plot to create.
#' @param small.multiples Whether the chart is shown as small multiples.i
#' @param multi.color.series Logical; Indicates whether multiple colors will be shown in a Bar or Column chart with a single series. By default this is off and different colors are used to distinguish between different series. However, when chart.type is "Pyramid", then \code{multi.color.series} is always \code{true}.
#' @param palette Specifies the color vector to be used for the chart data. It can be (1) A named palette from grDevices, RColorBrewer colorspace, or colorRamps; or
#' (2) A vector of colors which will be recycled to the length of the number of colors needed; or
#' (3) one of \code{"Custom color"}, \code{"Custom gradient"} or \code{"Custom palette"}.
#' The last option gives the user greater control via additional parameters (see below).  If not specified, the colors used
#' are c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000", "#4473C5", "#70AD46",
#' "#255F91", "#9E480D", "#636365", "#987300", "#26408B", "#42682B")
#' @param scatter.colors.column For scatterplot data, the column of data which is used to determine the colors of each point.
#' @param palette.custom.color A single color provided as a hex or character string. Only used if \code{palette} is \code{"Custom color"}. The output vector will consist of \code{palette} repeated for the number of required colors.
#' @param palette.custom.gradient.start A color specifying the start of the gradient when \code{palette} is set to \code{"Custom gradient"}.
#' @param palette.custom.gradient.end A color specifying the end of the gradient when \code{palette} is set to \code{"Custom gradient"}.
#' @param palette.custom.palette A vector or comma separated list of colors which will be recycled to the desired length. Only used if \code{palette} is \code{"Custom palette"}.
#' @param fit.palette As per \code{palette} except for the chart fit lines (trendlines). An additional option is \code{"Group colors"}, which uses the colors from \code{palette}.
#' @param fit.palette.custom.color As per \code{palette.custom.color}.
#' @param fit.palette.custom.gradient.start As per \code{palette.custom.gradient.start}.
#' @param fit.palette.custom.gradient.end As per \code{palette.custom.gradient.end}.
#' @param fit.palette.custom.palette As per \code{palette.custom.palette}.

#' @param fit.CI.palette As per \code{palette} except for the fit.CI lines (confidence interval around trend). An additional option is \code{"Group colors"}, which uses the colors from \code{palette}.
#' @param fit.CI.palette.custom.color As per \code{palette.custom.color}.
#' @param fit.CI.palette.custom.gradient.start As per \code{palette.custom.gradient.start}.
#' @param fit.CI.palette.custom.gradient.end As per \code{palette.custom.gradient.end}.
#' @param fit.CI.palette.custom.palette As per \code{palette.custom.palette}.

#' @param subslice.palette As per \code{palette} except for pie chart subslice colors. An additional option is \code{"Group colors"}, which uses the colors from \code{palette}.
#' @param subslice.palette.custom.color As per \code{palette.custom.color}.
#' @param subslice.palette.custom.gradient.start As per \code{palette.custom.gradient.start}.
#' @param subslice.palette.custom.gradient.end As per \code{palette.custom.gradient.end}.
#' @param subslice.palette.custom.palette As per \code{palette.custom.palette}.
#' @export
#' @importFrom flipChartBasics GetNumColors ChartColors
PrepareColors <- function(dat, chart.type, small.multiples = FALSE, multi.color.series = FALSE,
                          scatter.colors.column = 4,
                          palette = NULL, palette.custom.color = NULL, palette.custom.gradient.start = NULL,
                          palette.custom.gradient.end = NULL, palette.custom.palette = NULL,
                          fit.palette = NULL, fit.palette.custom.color = NULL, fit.palette.custom.gradient.start = NULL,
                          fit.palette.custom.gradient.end = NULL, fit.palette.custom.palette = NULL,
                          fit.CI.palette = NULL, fit.CI.palette.custom.color = NULL, fit.CI.palette.custom.gradient.start = NULL,
                          fit.CI.palette.custom.gradient.end = NULL, fit.CI.palette.custom.palette = NULL,
                          subslice.palette = NULL, subslice.palette.custom.color = NULL, subslice.palette.custom.gradient.start = NULL,
                          subslice.palette.custom.gradient.end = NULL, subslice.palette.custom.palette = NULL)
{
    if (multi.color.series && chart.type %in% c("Bar", "Column"))
        chart.type <- "Pyramid"
    if (chart.type == "Funnel")
        chart.type <- "Pyramid"

    num.colors <- GetNumColors(dat, chart.type, scatter.colors.column)

    series.colors <- NULL
    fit.line.colors <- NULL
    fit.CI.colors <- NULL
    subslice.colors <- NULL

    # We suppress warnings for Pie charts because missing values mean that num.color
    # may be larger than the number of slices. But we still want warnings if the
    # user specifies custom color (i.e. all slices colored the same)
    # In contrast, for bar pictograph and pyramid, the default option is to have
    # each bar a different color, but it would be equally valid to color them differently
    # For small multiples, it is also equally valid to color all panels the same.

    # For time series, we turn off the warnings for a single color because a single color
    # is usually valid if the option for range.bars is selected

    if (!is.null(palette))
        series.colors <- ChartColors(num.colors[[1]], given.colors = palette,
                                                      custom.color = palette.custom.color,
                                                      custom.gradient.start = palette.custom.gradient.start,
                                                      custom.gradient.end = palette.custom.gradient.end,
                                                      custom.palette = palette.custom.palette,
                                                      silent = chart.type %in% c("Pie", "Donut"),
                                                      silent.single.color = small.multiples || chart.type %in% c("Bar Pictograph", "Time Series", "Pyramid"))

    if (!is.null(fit.palette) && fit.palette != "Group colors")
        fit.line.colors <- ChartColors(num.colors[[1]], given.colors = fit.palette,
                                                        custom.color = fit.palette.custom.color,
                                                        custom.gradient.start = fit.palette.custom.gradient.start,
                                                        custom.gradient.end = fit.palette.custom.gradient.end,
                                                        custom.palette = fit.palette.custom.palette,
                                                        silent.single.color = small.multiples)
    if (!is.null(fit.CI.palette) && fit.CI.palette != "Group colors")
        fit.CI.colors <- ChartColors(num.colors[[1]], given.colors = fit.CI.palette,
                                                        custom.color = fit.CI.palette.custom.color,
                                                        custom.gradient.start = fit.CI.palette.custom.gradient.start,
                                                        custom.gradient.end = fit.CI.palette.custom.gradient.end,
                                                        custom.palette = fit.CI.palette.custom.palette,
                                                        silent.single.color = small.multiples)
    if (is.null(fit.line.colors))
        fit.line.colors <- series.colors
    if (is.null(fit.CI.colors))
        fit.CI.colors <- fit.line.colors
    if (length(num.colors) > 1 && !is.null(subslice.palette) && subslice.palette != "Group colors")
        subslice.colors <- ChartColors(num.colors[[2]], given.colors = subslice.palette,
                                                        custom.color = subslice.palette.custom.color,
                                                        custom.gradient.start = subslice.palette.custom.gradient.start,
                                                        custom.gradient.end = subslice.palette.custom.gradient.end,
                                                        custom.palette = subslice.palette.custom.palette,
                                                        silent = TRUE, silent.single.color = FALSE)

    return(list(series.colors = series.colors,
                fit.line.colors = fit.line.colors,
                fit.CI.colors = fit.CI.colors,
                subslice.colors = subslice.colors))
}
