#' Generates all d3 formats for use by CChart.
#'
#' See \url{https://github.com/d3/d3-format} for more information on d3.
#' Wrapper for \code{ChartNumberFormat}.
#' @param categories.format.list A list of five unnamed items for formatting the categories-axis as
#' described in \code{\link[flipChartBasics]{ChartNumberFormat}}.
#' @param values.format.list As per \code{categories.format.list} except for values-axis formatting.
#' @param hover.format.list As per \code{categories.format.list} except for hovertext formatting.
#' @param data.labels.format.list As per \code{categories.format.list} except for data label formatting.
#' @param as.percentages Whether the default formatting should be as percentages.
#' @export
#' @importFrom flipChartBasics ChartNumberFormat
PrepareNumbers <- function(categories.format.list = NULL,
                           values.format.list = NULL,
                           hover.format.list = NULL,
                           data.labels.format.list = NULL,
                           as.percentages = FALSE) {

    return(list(categories.number.format = ChartNumberFormat(categories.format.list, FALSE),
                values.number.format = ChartNumberFormat(values.format.list, as.percentages),
                hover.number.format = ChartNumberFormat(hover.format.list, as.percentages),
                data.labels.number.format = ChartNumberFormat(data.labels.format.list, as.percentages)))
}
