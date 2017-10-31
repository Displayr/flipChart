#' Generates all d3 formats for use by CChart.
#'
#' See \url{https://github.com/d3/d3-format} for more information on d3.
#' Wrapper for \code{ChartNumberFormat}.
#' @param categories.format.list A list of five unnamed items for formatting the categories-axis as
#' described in \code{\link[flipChartBasics]{ChartNumberFormat}}.
#' @param values.format.list As per \code{categories.format.list} except for values-axis formatting.
#' @param hover.format.list As per \code{categories.format.list} except for hovertext formatting.
#' @param data.labels.format.list As per \code{categories.format.list} except for data label formatting.
#' @export
#' @importFrom flipChartBasics ChartNumberFormat
PrepareNumbers <- function(categories.format.list = NULL,
                           values.format.list = NULL,
                           hover.format.list = NULL,
                           data.labels.format.list = NULL) {

    categories.number.format <- NULL
    values.number.format <- NULL
    hover.number.format <- NULL
    data.labels.number.format <- NULL

    if (!is.null(categories.format.list))
        categories.number.format <- ChartNumberFormat(categories.format.list)

    if (!is.null(values.format.list))
        values.number.format <- ChartNumberFormat(values.format.list)

    if (!is.null(hover.format.list))
        hover.number.format <- ChartNumberFormat(hover.format.list)

    if (!is.null(data.labels.format.list))
        data.labels.number.format <- ChartNumberFormat(data.labels.format.list)


    return(list(categories.number.format = categories.number.format, values.number.format = values.number.format,
                hover.number.format = hover.number.format, data.labels.number.format = data.labels.number.format))
}
