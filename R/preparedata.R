#' PrepareData
#'
#' Prepares input data for charting.
#' @param chart.type Character; chart type to be plotted.
#' @param subset subset An optional vector specifying a subset of
#'     observations to be used in the fitting process, or, the name of
#'     a variable in \code{data}. It may not be an expression.
#' @param weights An optional vector of sampling weights, or, the name
#'     of a variable in \code{data}. It may not be an expression.
#' @param input.data.table Array; typically a table of some kind,
#'     which is then processed using
#'     \code{\link[flipTables]{AsTidyTabularData}}.
#' @param input.data.tables List of array; each component is assumed
#'     to be a Qtable and will be processed using.
#'     \code{\link[flipTables]{AsTidyTabularData}}
#' @param input.data.raw List, containing variables or data.frames.
#' @param input.data.pasted List of length six; the first component of
#'     which is assumed to be from a user-entered/pasted table; will
#'     be processed by \code{\link{ParseUserEnteredTable}}.
#' @param input.data.other A PickAny Multi Q variable.
#' @param data.source Where multiple data inputs are provided, a text
#'     string can be provided to disambiguate. Refer to the source
#'     code for a precise understanding of how this works (it is not
#'     obvious and is not likely to be of any use for most cases, so
#'     should usually be left as a \code{NULL}).
#' @param first.aggregate Logical; whether or not the input data needs
#'     to be aggregated in this function. A single variable is
#'     tabulated, 2 variables are crosstabbed if \code{group.by.last} is selected,
#'     and otherwise the mean is computed. If \code{input.data.raw} contains
#'     two an 'X' variable and a 'Y' variable in the first two elements of the list,
#'     the data is automatically aggregated and crosstabbed.
#' @param scatter.input.columns.order (deprecated) Use \code{scatter.mult.yvals} instead.
#' @param scatter.mult.yvals Logical; When \code{chart.type} is "Scatter',
#'     a \code{TRUE} value indicates that columns of input.data.table or input.data.pasted
#'     should be considered multiple series instead of different attributes (default).
#' @param group.by.last Logical; \code{TRUE} and \code{first.aggregate} and there is data
#'     in either of \code{input.data.table} or \code{input.data.pasted}, the data is aggregated
#'     using the last variable
#' @param tidy Logical; whether or not the input data needs to be
#'     aggregated in this function (e.g., if an x and y variable have
#'     been provided, a contingency table is used to aggregate. This
#'     defaults to \code{TRUE}. It aggressively seeks to turn the data
#'     into a named vector or a matrix using
#'     \code{\link[flipTables]{TidyTabularData}}. This is not applied
#'     when \code{data.input.tables} are provided, or when the chart
#'     type is any of \code{"Scatter"}, \code{"Bean"},
#'     \code{"Histogram"}, \code{"Density"}, \code{"Box"}, or
#'     \code{"Violin"}.
#' @param tidy.labels Logical; whether to remove common prefixes from the
#'     labels of the input data.
#' @param transpose Logical; should the resulting matrix (of created)
#'     be transposed?
#' @param row.names.to.remove Character vector or delimited string of
#'     row labels specifying rows to remove from the returned table;
#'     default is \code{c("NET", "SUM")}
#' @param column.names.to.remove Character vector or delimited string
#'     of column labels specifying columns to remove from the returned
#'     table; default is \code{c("NET", "SUM")}.
#' @param split Character delimiter to split
#'     \code{row.names.to.remove} and \code{col.names.to.remove}
#'     on. Default is to split on either of \code{","} or \code{";"}.
#'     Assumed to be a regular expression; see \code{\link{strsplit}}.
#' @param hide.empty.rows.and.columns Logical; if \code{TRUE} empty
#'     rows and columns will be removed from the data.  Empty here
#'     meaning that a row or column contains all \code{NA} values, or
#'     in the case of percentages, that a row or column contains only
#'     0's. Retained for backwards-compatibility but is superseded by
#'     \code{hide.empty.rows} and \code{hide.empty.columns}.
#' @param hide.empty.rows Logical; hide rows with only NAs or 0's (percentages).
#' @param hide.empty.columns Logical; hide columns with only NAs or 0's (percentages).
#' @param select.rows String; Comma separated list of rows, by name or index
#'     to select from input table. If blank (default), then all rows are selected.
#' @param select.columns String; Comma separated list of columns, by name or index
#'     to select from input table. If blank (default), then all columns are selected.
#' @param auto.order.rows Logical; Automatically order rows by correspondence analysis.
#' @param sort.rows Logical; whether to sort the rows of the table. This operation is
#'     performed after row selection. (Ignored if \code{auto.order.rows} is true).
#' @param sort.rows.column String; If \code{sort.rows} is true, this column
#'     (specified by name or index) is used for sorting the rows. If not specified,
#'     the column with the largest \code{Column n} or the right-most column
#'     will be used for sorting.
#' @param sort.rows.exclude String; If \code{sort.rows} is \code{TRUE}, then rows
#'      in \code{sort.rows.exclude} will be excluded from sorting and
#'      appended at the bottom of the table.
#' @param sort.rows.decreasing Logical; Whether rows should be sorted in decreasing order.
#' @param auto.order.columns Logical; Automatically order columns by correspondence analysis.
#' @param sort.columns Logical; whether to sort the columns of the table.
#'      This operation is performed after column selection (Ignored if
#'      \code{auto.order.columns} is true.
#' @param sort.columns.row String; If \code{sort.columns} is true, this row
#'      (specified by name or index) is used for sorting the columns. If not specified,
#'      the row with the largest \code{n} or the bottom row
#'      will be used for sorting.
#' @param sort.columns.exclude String; If \code{sort.columns} is \code{TRUE}, then columns
#'      in \code{sort.columns.exclude} will be excluded from sorting and
#'      appended at the right of the table.'
#' @param sort.columns.decreasing Logical; Whether columns should be sorted in decreasing order.
#' @param hide.output.threshold Integer; If sample size ('Column n' or 'n') is provided
#'      then each cell in the input table will be checked to ensure
#'      'n' or 'Column n' is larger than specified threshold, otherwise an error
#'      message is given.
#' @param hide.rows.threshold Integer; If sample size ('Column n' or 'n')
#'      is provided, then rows and with sample sizes smaller than threshold
#'      will be removed from table. Vectors will be treated as 1-d matrices
#' @param hide.columns.threshold Integer; If sample size ('Column n' or 'n')
#'      is provided, then columns with sample sizes smaller than threshold
#'      will be removed from table. Vectors will not be affected.
#' @param first.k.rows Integer; Number of rows to select from the top of the input table.
#' @param last.k.rows Integer; Number of rows to select from the bottom of the input table.
#' @param first.k.columns Integer; Number of columns to select from the left of the input table.
#' @param last.k.columns Integer; Number of columns to select from the right of the input table.
#' @param reverse.rows Logical; Whether to reverse order of rows. This operation is
#'      performed after row selection and sorting.
#' @param reverse.columns Logical; Whether to reverse order of columns. This operation
#'      is peformed after column selection and sorting.
#' @param show.labels Logical; If \code{TRUE}, labels are used for
#'     names in the data output if raw data is supplied.
#' @param as.percentages Logical; If \code{TRUE}, aggregate values in the
#' output table are given as percentages summing to 100. If \code{FALSE},
#' column sums are given.
#' @param date.format One of \code{"Automatic", "US" or "International"}.
#' This is used to determine whether strings which are interpreted as dates
#' in the (row)names will be read in the US (month-day-year) or the
#' International (day-month-year) format. By default US format is used
#' if it cannot be deduced from the input data.
#' @param values.title The title for the values axis of a chart (e.g.,
#'     the y-axis of a column chart or the x-axis of a bar chart).
#' @details It is assumed that only one of \code{input.data.pasted},
#'     \code{input.data.table}, \code{input.data.tables},
#'     \code{input.data.other}, \code{input.data.raw} is non-NULL.
#'     They are checked for nullity in that order.
#' @importFrom flipTransformations ParseUserEnteredTable
#'     SplitVectorToList
#' @importFrom flipTables TidyTabularData RemoveRowsAndOrColumns SelectRows SelectColumns SortRows SortColumns ReverseRows ReverseColumns HideOutputsWithSmallSampleSizes HideRowsWithSmallSampleSizes HideColumnsWithSmallSampleSizes AutoOrderRows AutoOrderColumns
#' @importFrom flipData TidyRawData
#' @importFrom flipFormat Labels Names ExtractCommonPrefix
#' @importFrom flipStatistics Table WeightedTable
#' @return A list with components \itemize{ \item \code{data} - If
#'     possible, a named vector or matrix, or if that is not posible
#'     or a data.frame is requested, a data.frame.  \item
#'     \code{weights} - Numeric vector of user-supplied weights.
#'     \item \code{values.title} - Character string to be used for the
#'     y-axis title; will only be a non-empty string if some
#'     aggregation has been performed on \code{data} \item
#'     \code{scatter.variable.indices} A named vector indicating which
#'     columns in \code{data} should be plotted in a scatterplot as
#'     \code{x}, \code{y}, \code{sizes}, and \code{colors}. Is
#'     \code{NULL} if \code{chart.type} does not contain
#'     \code{"Scatter"} or \code{"Bubble"}. \code{NA} is used when the
#'     data does not exist.  }
#' @export
#' @seealso \code{\link[flipTables]{AsTidyTabularData}},
#'     \code{\link[flipData]{TidyRawData}},
#'     \code{\link[flipTransformations]{ParseUserEnteredTable}}
PrepareData <- function(chart.type,
                        subset = TRUE,
                        weights = NULL,
                        input.data.table = NULL,
                        input.data.tables = NULL,
                        input.data.raw = NULL,
                        input.data.pasted = NULL,
                        input.data.other = NULL,
                        data.source = NULL,
                        first.aggregate = NULL,
                        scatter.input.columns.order = NULL,
                        scatter.mult.yvals = FALSE,
                        group.by.last = FALSE,
                        tidy = TRUE,
                        tidy.labels = FALSE,
                        transpose = FALSE,
                        select.rows = NULL,
                        first.k.rows = NA,
                        last.k.rows = NA,
                        select.columns = NULL,
                        first.k.columns = NA,
                        last.k.columns = NA,
                        auto.order.rows = FALSE,
                        sort.rows = FALSE,
                        sort.rows.exclude = c("NET", "SUM", "Total"),
                        sort.rows.column = NULL,
                        sort.rows.decreasing = FALSE,
                        auto.order.columns = FALSE,
                        sort.columns = FALSE,
                        sort.columns.exclude = c("NET", "SUM", "Total"),
                        sort.columns.row = NULL,
                        sort.columns.decreasing = FALSE,
                        hide.output.threshold = 0,
                        hide.rows.threshold = 0,
                        hide.columns.threshold = 0,
                        reverse.rows = FALSE,
                        reverse.columns = FALSE,
                        row.names.to.remove = c("NET", "SUM", "Total"),
                        column.names.to.remove = c("NET", "SUM", "Total"),
                        split = "[;,]",
                        hide.empty.rows.and.columns = TRUE,
                        hide.empty.rows = hide.empty.rows.and.columns,
                        hide.empty.columns = hide.empty.rows.and.columns,
                        as.percentages = FALSE,
                        date.format = "Automatic",
                        show.labels = TRUE,
                        values.title = "")
{

    # Scenarios to address
    # - User provides a single numeric variable and wants to plot a bar for each value.
    # - User provides a single categorical variable and wants to plot a bar for each value.
    # - User provides two numeric variables and wants to plot a stacked bar plot of the unique values.
    # - User provides two numeric variables and wants to plot a stacked column chart of the crosstab.
    # - Data is in a weird format (e.g., JSON) for Venn diagram.
    # - User wants to treat variables or variable sets NOT as 'raw' data. E.g., performing a correspondence analysis of raw data.
    # - User wants to treat pasted data as raw data.
    # - User wants to treat otherData as raw data
    # - Scatterplots of raw data, where separate drop boxes have been used as inputs.
    # - Scatterplots of raw data, where a table has been used as an input.
    # - Scatterplots of raw data, where pasted data has been used as an input.
    # - Venn diagrams of JSON.
    # - Venn diagrams of multiple binary variables
    # - Histogram, Density, Bean, Violin, and Box plots of numeric variables
    # - Histogram, Density, Bean, Violin, and Box plots of an x and a y variable, where the histograms are conditional on the X.
    # - Aggregation by crosstabbing
    # - Sankey requires a data.frame
    # - means of multiple variables of raw data if aggregating
    ## Other things for the future...
    # - Taking the average of multiple numeric variables.
    # - Frequencies of multiple categorical variables (Pick One - Multi)

    #### This function does the following things:
    # 1. Converts the data inputs into a single data object called 'data'.
    # 2. Filters the data and/or removes missing values
    # 3. Aggregate the data if so required.
    # 4. Tailoring the data for the chart type.
    # 5. Transformations of the tidied data (sorting, transposing, removing rows).

    # This function needs to be frequently understood and generalized
    # by multiple people. Consequently, the goal has been to write the code in such a
    # way as to make it as easy to read and maintain as possible. In particular,
    # many obvious ways to make this code more efficent have been ignored in the interests
    # of making it easy to read (and in recognition that the efficiency gains would be trivial anyway).

    ###########################################################################
    # 1. Converts the data inputs into a single data object called 'data'.
    ###########################################################################
    data.source.index <- if (is.null(data.source)) NULL else
        switch(data.source,
                "Link to a table" = 1,
                "Link to a table in 'Pages'" = 1,
                "Link to multiple tables" = 2,
                "Link to multiple tables in 'Pages'" = 2,
                "Link to a variable" = 3,
                "Link to a variable in 'Data'" = 3,
                "Link to variables" = 3,
                "Link to variables in 'Data'" = 3,
                "Question Type: Pick Any" = 3,
                "Variable Set: Binary - Multi" = 3,
                "Question Type: Number - Multi" = 3,
                "Variable Set: Numeric - Multi" = 3,
                "Type or paste in data" = 4,
                "Use an existing R Output" = 5,
                "Use an existing R Output in 'Pages'" = 5,
                "Link to questions" = 3,
                "Link to variable sets in 'Data'" = 3,
                "Link to a question" = 3,
                "Link to a variable in 'Data'" = 3,

                       { # Default
                           warning("'", data.source, "' is not a recognized data source.")
                           3
                       }
                )
    # Convert lists of NULLs into single NULLs.
    if (all(sapply(input.data.raw, is.null)))
        input.data.raw <- NULL
    # Ignore colors/sizes/labels if x and y are not supplied
    if (length(input.data.raw) >= 2 && all(sapply(input.data.raw[1:2], is.null)))
        input.data.raw <- NULL
    if (all(sapply(input.data.pasted, is.null)))
        input.data.pasted <- NULL
    # Check that there is no ambiguity regarding which input to use.
    checkNumberOfDataInputs(data.source.index, input.data.table, input.data.tables,
                            input.data.raw, input.data.pasted, input.data.other)
    # Assign the data to 'data'
    data <- processInputData(input.data.table)
    if (is.null(data))
        data <- input.data.tables
    if (is.null(data))
        data <- coerceToDataFrame(input.data.raw, chart.type)
    if (is.null(data))
        data <- input.data.other
    if (is.null(data))
        data <- processPastedData(input.data.pasted,
                                  warn = tidy,
                                  date.format)

    # Replacing variable names with variable/question labels if appropriate
    if (is.data.frame(data))
        names(data) <- if (show.labels) Labels(data) else Names(data)

    ###########################################################################
    # 2. Filters the data and/or removes missing values
    ###########################################################################
    filt <- length(subset) > 1 && NROW(subset) == NROW(data)
    if (!is.null(input.data.raw) || filt || NROW(weights) == NROW(data))
    {
        missing <- if (chart.type %in% c("Scatter", "Venn", "Sankey"))
            "Exclude cases with missing data" else "Use partial data"
        n <- NROW(data)
        if (invalid.joining <- !is.null(attr(data, "InvalidVariableJoining")))
        {
            if (!isDistribution(chart.type) && length(subset) > 1 || NROW(weights) > 1)
                warning("The variables have been automatically spliced together without ","
                        any knowledge of which case should be matched with which. ",
                        "This may cause the results to be misleading.")
        }
        data <- if (chart.type == "Scatter") # As we can potentially use the variable in two different ways, we suppress the warning
            suppressWarnings(TidyRawData(data, subset = subset, weights = weights, missing = missing))
        else
            TidyRawData(data, subset = subset, weights = weights, missing = missing)
        if (invalid.joining)
            attr(data, "InvalidVariableJoining") <- TRUE
        n.post <- NROW(data)
        if (missing == "Exclude cases with missing data" && n.post < n)
            warning("After removing missing values and/or filtering, ", n.post,
                    " observations remain.")
        weights <- setWeight(data, weights)
    }
    if (filt)
        attr(data, "assigned.rownames") <- FALSE

    ###########################################################################
    # 3. Aggregate the data if so required.
    ###########################################################################
    crosstab <- !(chart.type %in% c("Scatter", "Venn") || isDistribution(chart.type)) &&
                 (rawDataLooksCrosstabbable(input.data.raw) || group.by.last)
    if (is.null(first.aggregate))
        first.aggregate <- crosstab
    if ((chart.type %in% c("Scatter", "Venn") || isDistribution(chart.type)) &&
        first.aggregate)
    {
        warning("Data is not aggregated for this chart type.")
        first.aggregate <- FALSE
    }
    if (crosstab && !first.aggregate)
        warning("Input data is always aggregated when 'Groups' variable is provided.")
    if (crosstab || first.aggregate)
    {
        #crosstab <- NCOL(data) == 2 || group.by.last
        if (crosstab && !is.null(attr(data, "InvalidVariableJoining")))
            warning("The variables being crosstabbed have different lengths; ","
                    it is likely that the crosstab is invalid.")
        data <- aggregateDataForCharting(data, weights, chart.type, crosstab)
        if (crosstab)
            group.by.last <- TRUE
    }

    ###########################################################################
    # 4. Tailoring the data for the chart type.
    ###########################################################################
    data <- prepareForSpecificCharts(data,
                                     multiple.tables = .isTableList(input.data.table) || .isTableList(input.data.tables),
                                     input.data.raw,
                                     chart.type, weights, show.labels,
                                     date.format, scatter.mult.yvals)
    weights <- setWeight(data, weights)
    scatter.mult.yvals <- isTRUE(attr(data, "scatter.mult.yvals"))

    ###########################################################################
    # 5. Transformations of the tidied data (e.g., sorting, transposing, removing rows).
    ###########################################################################
    data <- transformTable(data,
                   chart.type,
                   multiple.tables = .isTableList(input.data.tables) || .isTableList(input.data.table),
                   tidy,
                   is.raw.data = !is.null(input.data.raw) || !is.null(input.data.pasted) || !is.null(input.data.other),
                   row.names.to.remove, column.names.to.remove, split,
                   select.rows, first.k.rows, last.k.rows,
                   select.columns, first.k.columns, last.k.columns,
                   hide.output.threshold,
                   hide.rows.threshold, hide.columns.threshold,
                   transpose,
                   group.by.last || first.aggregate,
                   as.percentages && chart.type != "Venn", #Venn takes care of this itself
                   hide.empty.rows, hide.empty.columns,
                   date.format = date.format)

    # Sort/reordering rows and columns
    # This does not affect input with a list of multiple tables (scatterplot only)
    if (!(is.list(data) && !is.data.frame(data)))
    {
        if (auto.order.rows)
        {
            data <- try(AutoOrderRows(data))
            if (inherits(data, "try-error"))
                stop("Could not perform correspondence analysis on table. Try hiding empty rows.")
        }
        else if (sort.rows)
            data <- SortRows(data, sort.rows.decreasing, sort.rows.column, sort.rows.exclude)
        if (reverse.rows)
            data <- ReverseRows(data)

        if (auto.order.columns)
        {
            data <- try(AutoOrderColumns(data))
            if (inherits(data, "try-error"))
                stop("Could not perform correspondence analysis on table. Try hiding empty columns.")
        }
        else if (sort.columns)
            data <- SortColumns(data, sort.columns.decreasing, sort.columns.row, sort.columns.exclude)
        if (reverse.columns)
            data <- ReverseColumns(data)
    }

    ###########################################################################
    # Finalizing the result.
    ###########################################################################
    if (tidy.labels)
        data <- tidyLabels(data, chart.type)
    data <- setAxisTitles(data, chart.type, tidy, values.title)
    values.title <- attr(data, "values.title")
    categories.title <- attr(data, "categories.title")
    attr(data, "values.title") <- NULL
    attr(data, "categories.title") <- NULL
    if (scatter.mult.yvals)
        attr(data, "scatter.mult.yvals") <- TRUE

    # This is a work around bug RS-3402
    # This is now fixed in Q 5.2.7+, but we retain support for older versions
    # by converting to a matrix if necessary
    if (chart.type == "Table" && !is.null(attr(data, "statistic")) &&
        (is.null(dim(data)) || length(dim(data)) == 1))
    {
        tmp <- attr(data, "statistic")
        data <- as.matrix(data)
        attr(data, "statistic") <- tmp
    }

    list(data = data,
         weights = weights,
         values.title = values.title,
         categories.title = categories.title,
         scatter.variable.indices = attr(data, "scatter.variable.indices"))
}

#' Handle input of table or tables
#' @noRd
#' @description This function allows a list of tables to be supplied
#'  via the \code{input.data.table} argument in the same way as
#'  \code{input.data.tables}.
#' @param x Input data which may be a matrix or list of matrix
unlistTable <- function(x)
{
    if (is.null(x))
        return(x)
    if (is.list(x) && !is.data.frame(x) && length(x) == 1)
        return(x[[1]])
    else
        return(x)
}

.isTableList <- function(x){!is.data.frame(x) && is.list(x) && length(x) > 1 &&
                            (is.matrix(x[[1]]) || is.data.frame(x[[1]]) || is.numeric(x[[1]]))}

isScatter <- function(chart.type)
{
    grepl("Scatter|Bubble", chart.type)
}

#' Aggregrate Raw Data For Charting
#' @param data \code{data.frame} containing raw data
#' @param weights numeric vector of weights
#' @param chart.type character; type of chart to be plotted
#' @param crosstab Aggregate using a contingency table.
#' @return aggregated data
#' @noRd
#' @importFrom flipStatistics Table WeightedTable
#' @importFrom flipTransformations AsNumeric
aggregateDataForCharting <- function(data, weights, chart.type, crosstab)
{
    weighted <- !is.null(weights)
    weights <- if (is.null(weights) || is.function(weights)) rep.int(1L, NROW(data)) else weights
    # In tables that show aggregated tables, only the x-axis title is
    # taken from dimnames. But both names should be set in case
    # the table is transposed
    if (NCOL(data) == 1)
    {
        out <- as.matrix(WeightedTable(unlist(data), weights = weights))
        names(dimnames(out)) <- c(names(data)[1], "")
        attr(out, "statistic") = "Count"
    }
    else if (crosstab)
    {
        data <- as.data.frame(data)
        k <- NCOL(data)
        if (k > 2)
        {
            warning("Multiple variables have been provided. Only the first and last ",
                    "variable have been used to create the crosstab. If you wish to ",
                    "create a crosstab with more than two variables, you need to ",
                    "instead add the data as a 'Data Set' instead add a 'Data Set'.")
            data <- data[, c(1, k)]
        }
        data.is.numeric <- all(sapply(data[,-k], is.numeric)) # exclude grouping variable
        tmp.names <- names(data)
        names(data) <- c("x", "y") # temporarily set names for formula

        if (data.is.numeric)
        {
            if (weighted)
            {
                data$w <- weights
                data$xw <- data$x * weights
                out <- Table(xw~y, data = data, FUN = sum)/Table(w~y, data = data, FUN = sum)
            } else
                out <- Table(x~y, data = data, FUN = mean)
            attr(out, "statistic") <- "Average"
            attr(out, "categories.title") <- tmp.names[2]
        }
        else
        {
            data$w <- weights
            out <- Table(w  ~  x + y, data = data, FUN = sum)
            names(dimnames(out)) <- tmp.names
            attr(out, "statistic") = "Counts"
        }
    }
    else
    {
        if (!is.matrix(data) && !is.numeric(data))
            data <- coerceToDataFrame(data)
        if (is.data.frame(data))
            data <- AsNumeric(data, binary = FALSE)
        if (weighted)
        {
            xw <- sweep(data, 1, weights, "*")
            sum.xw <- apply(xw, 2, sum, na.rm = TRUE)
            w <- matrix(weights, nrow(data), ncol(data))
            w[is.na(data)] <- 0
            sum.w <- apply(w, 2, sum)
            out <- sum.xw / sum.w
        } else
           out <- apply(data, 2, mean, na.rm = TRUE)
        attr(out, "statistic") <- "Average"
    }
    attr(out, "assigned.rownames") <- TRUE
    out
}

#' coerceToDataFrame
#'
#' @description Takes various formats of data (in particular, lists of variables and
#' data.frames, and forces them to become a data frame. Where the coercion
#' involves creating rows in the data frame that are unlikely to be from the same analysis unit, a warning
#' is provided.
#' @param x Input data which may be a list of variables or dataframe
#' @param chart.type For any value except \code{"Scatter"}, x$Y will be
#'      ignored if x$X contains more than one variable
#' @param remove.NULLs Logical; whether to remove null entries
#' @importFrom flipTables TidyTabularData
#' @return A \code{\link{data.frame}})
#' @importFrom stats sd
coerceToDataFrame <- function(x, chart.type = "Column", remove.NULLs = TRUE)
{
    if (is.null(x))
        return(x)
    else if (is.data.frame(x))
        return(x)

    # Check that grouping variable is not used with multiple X variables
    # This is possible in Q5.1.2
    if (chart.type != "Scatter" && !is.null(x$Y) && is.list(x$X) && length(x$X) > 1)
    {
        warning("'Groups' variable ignored if more than one input variable is selected.")
        x$Y <- NULL
    }

    if (is.list(x) && length(x) == 1 && is.matrix(x[[1]])) # List only contains a matrix
        return(as.data.frame(x[[1]]))
    else if (is.character(x))
    {
        x <- TidyTabularData(x)
        return(as.data.frame(x))
    }
    #else if (is.list(x[[1]])) # In Displayr, this is typically true.
    #    x[[1]] <- as.data.frame(x[[1]])

    # if labels are present in raw data, extract and store for later
    rlabels <- x$labels
    x$labels <- NULL

    # Dealing with situation where first element of x is a list containing only one thing.
    if (is.list(x[[1]]) && length(x[[1]]) == 1)
        x[[1]] <- x[[1]][[1]]


    if (length(x) == 1 && is.list(x) && (is.matrix(x[[1]]) || !is.atomic(x[[1]])))
    {
        x = x[[1]]
        if (is.null(rlabels) && !is.atomic(x))
        {
            rlabels <- x$labels
            x$labels <- NULL
        }
    }
    # Checking to see if all the elements of x are single variables.
    all.variables <- all(sapply(x, NCOL) == 1)
    # Remove entries in the list which are null
    if(remove.NULLs)
        x <- Filter(Negate(is.null), x)
    # Extracting variable names
    nms <- if (all.variables) names(x) else unlist(lapply(x, names)) # i.e. 'X', 'Y', 'labels'

    # Splicing together elements of the input list
    # Note that elements of x can contain lists of variables
    invalid.joining <- FALSE
    if (NCOL(x) > 1 || is.list(x) && length(x) > 1)
    {
        lengths <- sapply(x, function(m) NROW(as.data.frame(m)))
        if (invalid.joining <- sd(lengths) != 0)
        {
            k <- length(lengths)
            out <- matrix(NA, max(lengths), k)
            for (i in 1:k)
                out[1:lengths[i], i] <- x[[i]]  # shouldn't work if x$Y is a list?? x$X handled in line 402
            x <- out
        }
    }
    x <- data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)

    # Set column and rownames
    names(x) <- nms
    if (!is.null(rlabels) && nrow(x) == length(rlabels))
         rownames(x) <- make.unique(as.character(rlabels), sep = "")
    if (invalid.joining)
        attr(x, "InvalidVariableJoining") <- TRUE
    x
}


isDistribution <- function(chart.type)
{
    grepl("Bean|Box|Histogram|Density|Violin", chart.type)
}

processInputData <- function(x)
{
    if (is.null(x))
        return(x)

    # Handle list of tables
    if (is.list(x) && !is.data.frame(x))
    {
        if (length(x) == 1)
            x <- x[[1]]
        else
            return(x)
    }

    if (length(attr(x, "tsp")) == 3) # time-series object
    {
        ts.info <- attr(x, "tsp")
        ts.seq <- seq(from = ts.info[1], to = ts.info[2], by = 1/ts.info[3])
        if (length(dim(x)) < 2)
            names(x) <- ts.seq
        else
            rownames(x) <- ts.seq
        attr(x, "tsp") <- NULL     # delete attribute so we can do all matrix operations
        attr(x, "assigned.rownames") <- TRUE
        return(x)
    }

    if (hasUserSuppliedRownames(x))
        attr(x, "assigned.rownames") <- TRUE

    return(x)
}

processPastedData <- function(input.data.pasted, warn, date.format)
{
    us.format <- switch(date.format, US = TRUE, International = FALSE, NULL)
    want.data.frame <- length(input.data.pasted) > 1L && isTRUE(input.data.pasted[[2]])
    processed <- tryCatch(ParseUserEnteredTable(input.data.pasted[[1]],
                                  want.data.frame = want.data.frame,
                                  want.factors = FALSE, #input.data.pasted[[2]], #charts has no concept of factors
                                  want.col.names = input.data.pasted[[3]],
                                  want.row.names = input.data.pasted[[4]],
                                  us.format = us.format,
                                  warn = warn),
             error = function(e) {input.data.pasted[[1]]})
    if (!is.null(processed) && length(input.data.pasted) > 3)
        attr(processed, "assigned.rownames") <- input.data.pasted[[4]]
    if (!is.null(processed) && want.data.frame)
        attr(processed, "assigned.rownames") <- TRUE
    return(processed)
}

checkNumberOfDataInputs <- function(data.source.index, table, tables, raw, pasted, other)
{
    data.provided <- !sapply(list(table, tables, raw, pasted, other), is.null)
    n.data <- sum(data.provided)
    if (n.data == 0)
        stop("No data has been provided.")
    else if (is.null(data.source.index))
    {
        if (n.data > 1)
            stop("There are ", n.data, " data inputs. One and only one data argument may be supplied.")

    } else if (!data.provided[data.source.index])
        stop("The data provided does not match the 'data.source.index'.")
}

scatterVariableIndices <- function(input.data.raw, data, show.labels)
{
    # Creating indices in situations where the user has provided a table.
    len <- length(input.data.raw)
    indices <- c(x = 1, y = 2, sizes = 3, colors = 4, groups = 5)
    if (is.null(input.data.raw) || is.data.frame(input.data.raw) || is.list(input.data.raw) && len == 1)
        return(indices)

    .getColumnIndex <- function(i)
    {
        if (i > len)
            return(NA)
        lst <- input.data.raw[[i]]
        if (is.null(lst))
            return(NA)
        nms <- names(data)

        # Match based on label/variable name to avoid problems with duplicates
        nm <- if (show.labels) Labels(lst) else Names(lst)
        if (is.null(nm))
            return(i)
        match(nm, nms)
    }
    # Indices corresponding to selections in input.raw.data
    indices["x"] <- .getColumnIndex(1)
    indices["y"] <- .getColumnIndex(2)
    indices["sizes"] <- .getColumnIndex(3)
    indices["colors"] <- .getColumnIndex(4)
    indices["groups"] <- .getColumnIndex(5)
    indices
}

asPercentages <- function(data)
{
    ind.negative <- which(data < 0)
    if (length(ind.negative) > 0)
    {
        warning("Percentages calculated ignoring negative values.")
        data[ind.negative] <- 0
    }

    if (NCOL(data) > 1)
    {
        data <- prop.table(data, 1)
        attr(data, "statistic") <- "Row %"
    }
    else
    {
        data <- prop.table(data)
        attr(data, "statistic") <- "%"
    }
    data
}

#' @importFrom flipTables RemoveRowsAndOrColumns HideEmptyRows HideEmptyColumns
#' @importFrom flipTime AsDate AsDateTime
transformTable <- function(data,
                           chart.type,
                           multiple.tables,
                           tidy,
                           is.raw.data,
                           row.names.to.remove, column.names.to.remove, split,
                           select.rows, first.k.rows, last.k.rows,
                           select.columns, first.k.columns, last.k.columns,
                           hide.output.threshold,
                           hide.rows.threshold, hide.columns.threshold,
                           transpose,
                           first.aggregate,
                           as.percentages,
                           hide.empty.rows, hide.empty.columns,
                           date.format,
                           table.counter = 1)
{
    if (multiple.tables)
    {
        for(i in seq_along(data))
            data[[i]] = transformTable(data[[i]],
                                       chart.type,
                                       FALSE,
                                       tidy,
                                       is.raw.data,
                                       row.names.to.remove, column.names.to.remove, split,
                                       select.rows, first.k.rows, last.k.rows,
                                       select.columns, first.k.columns, last.k.columns,
                                       0, 0, 0, # sample size not used
                                       transpose,
                                       first.aggregate,
                                       as.percentages,
                                       hide.empty.rows, hide.empty.columns,
                                       date.format,
                                       i)
        return(data)
    }

    # Selecting rows/columns
    data <- SelectRows(data, select.rows, first.k.rows, last.k.rows)
    data <- SelectColumns(data, select.columns,
                first.k.columns, last.k.columns)

    ## Remove rows and columns
    data <- RemoveRowsAndOrColumns(data, row.names.to.remove = row.names.to.remove,
                                   column.names.to.remove = column.names.to.remove, split = split)

    if (hide.empty.rows)
        data <- if (isListOrRaggedArray(data)) lapply(data, HideEmptyRows)
                else HideEmptyRows(data)

    if (hide.empty.columns)
        data <- if (isListOrRaggedArray(data)) lapply(data, HideEmptyColumns)
                else HideEmptyColumns(data)

    # Checking sample sizes (if available)
    # This needs to happen after row/columns have been (de)selected
    if (sum(hide.output.threshold, na.rm = TRUE) > 0)
        data <- HideOutputsWithSmallSampleSizes(data, hide.output.threshold)
    if (sum(hide.rows.threshold, na.rm = TRUE) > 0)
        data <- HideRowsWithSmallSampleSizes(data, hide.rows.threshold)
    if (sum(hide.columns.threshold, na.rm = TRUE) > 0)
        data <- HideColumnsWithSmallSampleSizes(data, hide.columns.threshold)

    # This must happen after sample sizes have been used
    # (only first statistic is retained after tidying)
    if (tidy && !chart.type %in% c("Venn", "Sankey") &&
        !isScatter(chart.type) && !isDistribution(chart.type))
            data <- tryCatch(TidyTabularData(data), error = function(e) { data })


    ## Switching rows and columns
    if (isTRUE(transpose))
    {
        data <- t(data)
        attr(data, "questions") <- rev(attr(data, "questions"))
    }

    ## If data is already percentages in Qtable then divide by 100
    ## Note that R outputs and pasted data will already be in decimals
    stat <- attr(data, "statistic")
    qst <- attr(data, "questions")
    if (!is.null(stat) && !is.null(qst) && grepl("%$", stat))
        data <- data / 100

    # Convert to percentages - this must happen AFTER transpose and RemoveRowsAndOrColumns
    if (as.percentages)
    {
        percentages.warning <- paste0("The data has not been converted to percentages/proportions. ",
        "To convert to percentages, first convert to a more suitable type (e.g., create a table).")
        if (!is.numeric(data) && !is.data.frame(data))
            warning(percentages.warning)
        else if ((prod(NROW(data)*NCOL(data)) == 1) && table.counter == 1)
            warning(percentages.warning)
        else if (chart.type %in% c("Pie", "Donut"))
            data <- data / sum(data, na.rm = TRUE)
        else
            data <- asPercentages(data)
    }

    if (date.format != "Automatic" && isDate(rownames(data)))
        rownames(data) <- format(suppressWarnings(AsDate(rownames(data), us.format = !grepl("International", date.format))), "%b %d %Y")
    else if (date.format != "Automatic" && isDate(names(data)))
        names(data) <- format(suppressWarnings(AsDate(names(data), us.format = !grepl("International", date.format))), "%b %d %Y")
    return(data)
}

#' @importFrom flipTables TidyTabularData
#' @importFrom flipTransformations AsNumeric
prepareForSpecificCharts <- function(data,
                                     multiple.tables,
                                     input.data.raw,
                                     chart.type,
                                     weights,
                                     show.labels,
                                     date.format,
                                     scatter.mult.yvals)
{
    if (!isDistribution(chart.type) && chart.type != "Table" && !is.null(input.data.raw) &&
        is.list(input.data.raw$X) && length(input.data.raw$X) > 10)
        warning("With a large number of variables, it may be better to first create ",
                 "a table and then create a visualization using the table.")

    # Multiple tables
    if (multiple.tables)
    {
        data <- lapply(data, TidyTabularData)
        # flipStandardCharts::Scatterplot takes an array input, with column numbers indicating how to plot.
        if (isScatter(chart.type))
            attr(data, "scatter.variable.indices") = c(x = 1, y = 2, sizes = 3, colors = 4)
    }
    else if (chart.type == "Table" || chart.type == "Heat")
    {
        # Do nothing
    }
    else if (chart.type == "Venn")
    {
        missing.data.rows <- rowSums(as.matrix(is.na(data))) > 0
        if (any(missing.data.rows))
        {
            data <- data[!missing.data.rows, ]
            warning(sum(missing.data.rows), " case(s) with missing data have been removed.")
        }
    }
    else if (chart.type == "Sankey")
    {
        data <- coerceToDataFrame(data)
    }
    # Scatterplots
    else if (isScatter(chart.type))
    {
        if (isTRUE(scatter.mult.yvals) || (is.list(input.data.raw$Y) && length(input.data.raw$Y) > 1))
        {
            n <- nrow(data)
            y.names <- if (show.labels) Labels(input.data.raw$Y) else Names(input.data.raw$Y)
            if (is.list(input.data.raw$Y) && is.null(input.data.raw$X))
            {
                # No X-coordinates supplied in variables
                m <- length(input.data.raw$Y)
                y.ind <- 1:m
                xvar <- rep(1:n, m)

            } else if (is.null(input.data.raw$Y) && hasUserSuppliedRownames(data))
            {
                # Use rowlabels as X-coordinate if character labels given
                m <- ncol(data)
                y.ind <- 1:m
                xvar <- rep(rownames(data), m)

            } else
            {
                # Otherwise use first column as X-coordinates
                m <- ncol(data) - 1
                y.ind <- (1:m) + 1
                xvar <- rep(data[,1], m)
            }
            if (length(y.names) <= 1)
                y.names <- colnames(data)[y.ind]
            if (length(y.names) <= 1)
                y.names <- paste("Group", 1:m)

            # newdata needs to use data rather than input.data.raw
            # otherwise it will not handle filters etc
            newdata <- data.frame(X = xvar,
                                  Y = as.vector(unlist(data[,y.ind])),
                                  Groups = rep(y.names, each = n))

            if (date.format != "Automatic" && isDate(as.character(newdata[,1])))
                newdata[,1] <- format(AsDate(as.character(newdata[,1]),
                us.format = !grepl("International", date.format)), "%b %d %Y")
            if (!is.null(input.data.raw$X))
                colnames(newdata)[1] <- colnames(data)[1]
            data <- newdata
            attr(data, "scatter.variable.indices") <- c(x = 1, y = 2, sizes = 0, colors = 3)
            attr(data, "scatter.mult.yvals") <- TRUE

        } else
        {
            if (!is.data.frame(data) && !is.matrix(data))
                data <- TidyTabularData(data)
            # Removing duplicate columns
            if (any(d <- duplicated(names(data))))
                data <- data[, !d]
            if (NCOL(data) > 4)
                warning("Columns ", paste(colnames(data)[5:ncol(data)], collapse = ", "),
                    " not used in Scatter plot.",
                    " Consider selecting checkbox for 'Input data contains y-values in multiple columns'.")

            # flipStandardCharts::Scatterplot takes an array input, with column numbers indicating how to plot.
            attr(data, "scatter.variable.indices") = scatterVariableIndices(input.data.raw, data, show.labels)
        }
    }
    # Charts that plot the distribution of raw data (e.g., histograms)
    else if (isDistribution(chart.type))
    {
        len <- sum(!vapply(input.data.raw, is.null, FALSE))
        if (len > 1L)  # variables from multiple GUI controls
        {
            if (NCOL(input.data.raw[[1]]) > 1 && (NCOL(input.data.raw[[2]]) == 1 || len > 2))
                stop("If using a grouping variable, you may only have one additional variable.")
            # Splitting the first variable by the second
            else if (#!is.null(input.data.raw[[2]]) &&
                NCOL(input.data.raw[[1]]) == 1 && NCOL(input.data.raw[[2]]) == 1)
            {
                if (!is.null(weights))
                    weights <- SplitVectorToList(weights, data[[2]])
                data <- SplitVectorToList(data[[1]], data[[2]])
                attr(data, "weights") <- weights
            }
        }
        else # Coercing data to numeric format, if required
            data <- AsNumeric(data, binary = FALSE)
        #if (!is.list(data))
        #    data <- list(data)
    }
    else
    {
        # Set rownames before TidyTabularData so that factor are not converted to numeric
        data <- useFirstColumnAsLabel(data,
            allow.numeric.rownames = chart.type %in% c("Area", "Bar", "Column", "Line", "Stream"))
    }
    data
}


setWeight <- function(x, weights)
{
    if (!is.null(w <-  attr(x, "weights")))
        return(w)
    weights
}

#' Check for object of class list or a \emph{ragged} array
#' @noRd
isListOrRaggedArray <- function(x)
    inherits(x, "list") || (inherits(x, "array") && !all(vapply(x, length, 1L) == 1))


#' @noRd
useFirstColumnAsLabel <- function(x, remove.duplicates = TRUE,
    allow.numeric.rownames = TRUE, allow.duplicate.rownames = TRUE)
{
    if (length(dim(x)) != 2 || ncol(x) == 1)
        return(x)
    if (hasUserSuppliedRownames(x))
        return(x)

    if (!allow.numeric.rownames && is.numeric(x[,1]))
        return(x)

    # What to do with duplicate rownames?
    ind.dup <- duplicated(x[,1])
    if (any(ind.dup))
    {
        if (!allow.duplicate.rownames) # scatterplot
        {
            warning("First column was not used as labels ",
                    "because it contains duplicated values: ",
                    paste(unique(x[ind.dup,1]), collapse=", "))
            return(x)
        }

        # If too many duplicates, then assume it is not expected to be a rowname
        # The exception is when the rownames are QDates
        is.date <- is.factor(x[,1]) &&
            all(!is.na(suppressWarnings(AsDate(levels(x[,1]), on.parse.failure = "silent"))))
        if ((!is.date) && mean(ind.dup, na.rm = T) > 0.9) # too many duplicates
            return(x)
        wmsg <- if (isDate(x[,1])) ". Check aggregation level of date variable '"
                else               ". Consider aggregating on '"

        warning("Duplicated entries in '", colnames(x)[1], "': ",
            paste(unique(x[ind.dup,1]), collapse = ", "),
            wmsg, colnames(x)[1], "'.")
        if (remove.duplicates)
        {
            warning("Only the first unique entry is shown.")
            x <- x[!ind.dup, ]
        }
        else
            return(x)
    }
    if (inherits(x[,1], 'Date') || inherits(x[,1], 'POSIXct') ||
        inherits(x[,1], 'POSIXlt') || inherits(x[,1], 'POSIXt'))
        r.tmp <- format(x[,1], "%b %d %Y")
    else if (is.factor(x[,1])) # QDates are also factors
        r.tmp <- make.unique(as.character(x[,1]))
    else
        r.tmp <- make.unique(as.character(x[,1]))

    is.missing <- is.na(r.tmp)
    if (any(is.missing))
        warning("Rows ", paste(which(is.missing), collapse = ","),
                " have been omitted because of missing values.")
    ind <- which(!is.missing)

    c.title <- colnames(x)[1]
    c2.title <- if (NCOL(x) == 2) colnames(x)[2]
    x <- x[ind, -1, drop = FALSE]
    rownames(x) <- r.tmp[ind]
    attr(x, "categories.title") <- c.title
    if (!is.null(c2.title))
        attr(x, "values.title") <- c2.title
    return(x)
}

setAxisTitles <- function(x, chart.type, tidy, values.title = "")
{
    if (isScatter(chart.type))
    {
        # Charting functions will automatically use column names
        attr(x, "categories.title") <- ""
        attr(x, "values.title") <- ""

    } else
    {
        # Extract categories.title from aggregated data
        if (is.null(attr(x, "categories.title")))
            attr(x, "categories.title") <- names(dimnames(x))[1]
        # Extract categories.title from Qtables
        if (is.null(attr(x, "categories.title")) && !is.null(attr(x, "questions")))
            attr(x, "categories.title") <- attr(x, "questions")[1]
        if (!is.null(attr(x, "statistic")) && grepl("%$", attr(x, "statistic")))
            attr(x, "values.title") <- "%"
        else if (sum(nchar(attr(x, "statistic"))) > 0)
            attr(x, "values.title") <- attr(x, "statistic")
    }
    if (sum(nchar(values.title)) > 0)
        attr(x, "values.title") <- values.title
    if (is.null(attr(x, "values.title")))
        attr(x, "values.title") <- ""
    if (tidy && !is.data.frame(x) && chart.type != "Scatter" && chart.type != "Table")
    {
        # only drop 1 dimension from a 2d matrix
        if (!is.data.frame(x) && length(dim(x)) == 2 && dim(x)[2] == 1)
        {
            tmp.vec <- x[, 1]
            names(tmp.vec) <- rownames(x)
            attr(tmp.vec, "statistic") <- attr(x, "statistic")
            attr(tmp.vec, "categories.title") <- attr(x, "categories.title")
            attr(tmp.vec, "values.title") <- attr(x, "values.title")
            x <- tmp.vec
        }
        else
            x <- drop(x)
    }
    x
}


rawDataLooksCrosstabbable <- function(input.data.raw, data)
{
    if (is.null(input.data.raw))
        return(FALSE)
    not.nulls <- !sapply(input.data.raw, is.null)
    if (length(not.nulls) == 1)
        return(FALSE)
    if (!not.nulls[1] || !not.nulls[2])
        return(FALSE)
    if (length(not.nulls) > 2)
    {
        if (sum(not.nulls) != 2)
            return(FALSE)
        input.data.raw <- input.data.raw[1:2]
    }
    nms <- names(input.data.raw)
    ncols <- sapply(input.data.raw, NCOL)
    if (any(ncols != 1))
        return(FALSE)
    if (is.list(input.data.raw$X) && length(input.data.raw$X) > 1) # Y-variable removed in coerceToDataFrame
        return(FALSE)
    return(nms == c("X", "Y"))
}

hasUserSuppliedRownames <- function(data)
{
    if (is.null(rownames(data)))
        return(FALSE)
    tmp <- attr(data, "assigned.rownames")
    if (isTRUE(tmp))
        return(TRUE)
    if (!is.null(tmp) && !tmp)
        return(FALSE)
    if (length(dim(data)) < 2 && is.null(names(data)))
        return(FALSE)

    # Default row names
    rnames <- gsub("Row ", "", rownames(data))
    if (all(rnames == as.character(1:nrow(data))))
        return(FALSE)

    return(TRUE)
}

# All warnings are suppressed here - warnings are given in the charting functions
isDate <- function(x) return(!is.null(x) && all(!is.na(suppressWarnings(
                AsDateTime(as.character(x), on.parse.failure = "silent")))))


tidyLabels <- function(data, chart.type)
{
    tmp <- NULL
    vertical.chart <- isDistribution(chart.type) || chart.type == "Venn"
    if (length(dim(data)) >= 2)
    {
        orig.names <- if (vertical.chart) colnames(data)
                      else                rownames(data)
        if (!isDate(orig.names))
        {
            tmp <- ExtractCommonPrefix(orig.names)
            if (vertical.chart)
                colnames(data) <- tmp$shortened.labels
            else
            {
                rownames(data) <- tmp$shortened.labels
                if (is.null(attr(data, "categories.title")) && !is.na(tmp$common.prefix))
                    attr(data, "categories.title") <- tmp$common.prefix
            }
        }
    }
    else if (!is.null(names(data))) # lists and vectors
    {
        if (!isDate(names(data)))
        {
            tmp <- ExtractCommonPrefix(names(data))
            names(data) <- tmp$shortened.labels
            if (is.null(attr(data, "categories.title")) && !is.na(tmp$common.prefix))
                attr(data, "categories.title") <- tmp$common.prefix
        }
    }
    data
}
