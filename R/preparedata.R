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
#' @param input.data.raw List, containing variables or data.frames or Regression outputs from flipRegression.
#'     In the case of multiple Regression outputs, the labels default to the R name of the Regression output.
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
#' @param hide.values.threshold Integer; If sample size ('Column n' or 'n') is provided
#'      then each cell in the input table will be checked to ensure
#'      'n' or 'Column n' is larger than specified threshold,
#'      otherwise the cell will be set to \code{NA}.
#' @param hide.rows.threshold Integer; If sample size ('Column n' or 'n')
#'      is provided, then rows and with sample sizes smaller than threshold
#'      will be removed from table. Vectors will be treated as 1-d matrices
#' @param hide.columns.threshold Integer; If sample size ('Column n' or 'n')
#'      is provided, then columns with sample sizes smaller than threshold
#'      will be removed from table. Vectors will not be affected.
#' @param first.k.rows Integer; Number of rows to select from the top of the input table. This occurs after select and sort.
#' @param last.k.rows Integer; Number of rows to select from the bottom of the input table. This occurs after select and sort.
#' @param first.k.columns Integer; Number of columns to select from the left of the input table. This occurs after select and sort.
#' @param last.k.columns Integer; Number of columns to select from the right of the input table. This occurs after select and sort.
#' @param reverse.rows Logical; Whether to reverse order of rows. This operation is
#'      performed after row selection and sorting.
#' @param reverse.columns Logical; Whether to reverse order of columns. This operation
#'      is peformed after column selection and sorting.
#' @param show.labels Logical; If \code{TRUE}, labels are used for
#'     names in the data output if raw data is supplied.
#' @param as.percentages Logical; If \code{TRUE}, aggregate values in the
#' output table are given as percentages summing to 100. If \code{FALSE},
#' column sums are given.
#' @param hide.percent.symbol Percentage data is shown without percentage symbols and the symbol
#'  is also removed from the statistic attribute.
#' @param categorical.as.binary If data is aggregated and this is true, then categorical variables will be converted into indicator variables for each level in the factor.
#' @param date.format One of \code{"Automatic", "US", "International" or "No date formatting"}.
#' This is used to determine whether strings which are interpreted as dates
#' in the (row)names will be read in the US (month-day-year) or the
#' International (day-month-year) format. By default US format is used
#' if it cannot be deduced from the input data.
#' @param values.title The title for the values axis of a chart (e.g.,
#'     the y-axis of a column chart or the x-axis of a bar chart).
#' @param column.labels A comma separated list of names to replace the default column names
#'      of \code{pd$data}. This is applied after all other data manipulations
#' @param row.labels A comma separated list of names to replace the default row names
#'      of \code{pd$data}. This is applied after all other data manipulations
#' @details It is assumed that only one of \code{input.data.pasted},
#'     \code{input.data.table}, \code{input.data.tables},
#'     \code{input.data.other}, \code{input.data.raw} is non-NULL.
#'     They are checked for nullity in that order.
#' @importFrom flipU ConvertCommaSeparatedStringToVector
#' @importFrom flipTransformations ParseUserEnteredTable
#'     SplitVectorToList
#' @importFrom flipTables TidyTabularData RemoveRowsAndOrColumns SelectRows SelectColumns SortRows SortColumns ReverseRows ReverseColumns HideOutputsWithSmallSampleSizes HideValuesWithSmallSampleSizes HideRowsWithSmallSampleSizes HideColumnsWithSmallSampleSizes AutoOrderRows AutoOrderColumns ConvertQTableToArray
#' @importFrom flipData TidyRawData
#' @importFrom flipFormat Labels Names ExtractCommonPrefix
#' @importFrom flipStatistics Table WeightedTable
#' @importFrom verbs Sum
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
                        hide.values.threshold = 0,
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
                        hide.percent.symbol = FALSE,
                        as.percentages = FALSE,
                        categorical.as.binary = NULL,
                        date.format = "Automatic",
                        show.labels = TRUE,
                        column.labels = "",
                        row.labels = "",
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
    data <- processInputData(input.data.table, subset, weights)
    if (is.null(data))
        data <- input.data.tables
    if (is.null(data))
        data <- coerceToDataFrame(input.data.raw, chart.type)
    if (is.null(data))
        data <- input.data.other
    if (is.null(data))
        data <- processPastedData(input.data.pasted,
                                  warn = tidy,
                                  date.format, subset, weights)

    # Replacing variable names with variable/question labels if appropriate
    if (is.data.frame(data))
        names(data) <- if (show.labels) Labels(data) else Names(data)
    chart.title <- attr(data, "title")

    ###########################################################################
    # 2. Filters the data and/or removes missing values
    ###########################################################################
    if (isScatter(chart.type) && !is.null(input.data.raw) && containsQTable(input.data.raw))
        subset <- TRUE
    filt <- length(subset) > 1 && NROW(subset) == NROW(data)
    if (!is.null(input.data.raw) || filt || NROW(weights) == NROW(data))
    {
        missing <- if (chart.type %in% c("Venn", "Sankey") && !any(checkRegressionOutput(input.data.raw)))
            "Exclude cases with missing data" else "Use partial data"
        n <- NROW(data)
        if (invalid.joining <- !is.null(attr(data, "InvalidVariableJoining")))
        {
            if (!isDistribution(chart.type) && length(subset) > 1 || NROW(weights) > 1)
                warning("The variables have been automatically spliced together without ","
                        any knowledge of which case should be matched with which. ",
                        "This may cause the results to be misleading.")
        }
        # As we can potentially use the variable in two different ways, we suppress the warning
        if (isScatter(chart.type))
        {
            # Make sure column names are unique otherwise TidyData will remove
            # them WITHOUT warning
            #colnames(data) <- make.unique(colnames(data))
            data <- suppressWarnings(TidyRawData(data, subset = subset,
                    weights = weights, missing = missing, error.if.insufficient.obs = FALSE,
                    remove.missing.levels = FALSE))
        }
        if (!isScatter(chart.type))
            data <- TidyRawData(data, subset = subset, weights = weights,
                        missing = missing, error.if.insufficient.obs = FALSE,
                        remove.missing.levels = isDistribution(chart.type))
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
    if (crosstab || first.aggregate)
    {
        #crosstab <- NCOL(data) == 2 || group.by.last
        if (crosstab && !is.null(attr(data, "InvalidVariableJoining")))
            warning("The variables being crosstabbed have different lengths; ","
                    it is likely that the crosstab is invalid.")
        data <- aggregateDataForCharting(data, weights, chart.type,
                    crosstab, categorical.as.binary, as.percentages)
        if (crosstab)
            group.by.last <- TRUE
    }

    ###########################################################################
    # 4. Tailoring the data for the chart type.
    ###########################################################################
    multiple.tables <- .isTableList(input.data.table) || .isTableList(input.data.tables)
    data <- prepareForSpecificCharts(data, multiple.tables, input.data.raw, chart.type,
                                     weights, show.labels, scatter.mult.yvals)
    weights <- setWeight(data, weights)
    scatter.mult.yvals <- isTRUE(attr(data, "scatter.mult.yvals"))

    ###########################################################################
    # 5. Transformations of the tidied data (e.g., sorting, transposing, removing rows).
    ###########################################################################
    if (isTRUE(transpose) && isScatter(chart.type))
    {
        warning("Data was not transposed. This option is incompatible with Scatter charts")
        transpose <- FALSE
    }

    # Do not drop 1-column table to keep name for legend
    drop <- (tidy && (chart.type %in% c("Pie", "Donut") ||
            !any(nchar(select.columns), na.rm = TRUE) &&
            !any(nchar(column.labels), na.rm = TRUE)))
    data <- transformTable(data, chart.type, multiple.tables, tidy, drop,
                   is.raw.data = !is.null(input.data.raw) || !is.null(input.data.pasted) || !is.null(input.data.other),
                   hide.output.threshold, hide.values.threshold, hide.rows.threshold, hide.columns.threshold,
                   transpose, group.by.last || first.aggregate,
                   hide.empty.rows, hide.empty.columns, date.format)

    # Sort must happen AFTER tidying
    data <- RearrangeRowsColumns(data,
                                 multiple.tables =  multiple.tables,
                                 select.rows, first.k.rows, last.k.rows,
                                 select.columns, first.k.columns, last.k.columns,
                                 row.names.to.remove, column.names.to.remove, split,
                                 auto.order.rows, auto.order.columns,
                                 sort.rows, sort.rows.decreasing, sort.rows.column,
                                 sort.rows.exclude, reverse.rows,
                                 sort.columns, sort.columns.decreasing, sort.columns.row,
                                 sort.columns.exclude, reverse.columns)

    if (any(nchar(column.labels)))
        data <- replaceDimNames(data, 2, column.labels)
    if (any(nchar(row.labels)))
        data <- replaceDimNames(data, 1, row.labels)

    if (scatter.mult.yvals)
        data <- convertScatterMultYvalsToDataFrame(data, input.data.raw, show.labels, date.format)


    # Calculate percentages after all the select/hide operations are completed
    data <- convertPercentages(data, as.percentages, hide.percent.symbol, chart.type, multiple.tables)

    ###########################################################################
    # Finalizing the result.
    ###########################################################################
    if (tidy.labels)
        data <- tidyLabels(data, chart.type)
    if (filt && !is.null(attr(subset, "label")) && !is.null(input.data.raw) && NCOL(data) == 1 &&
        chart.type %in% c("Table", "Area", "Bar", "Column", "Line", "Radar", "Palm", "Time Series"))
    {
        # Do not drop 1-column table (from aggregated data) to keep name for legend
        data <- CopyAttributes(as.matrix(data), data)
        colnames(data) <- attr(subset, "label")
        drop <- FALSE
    }
    data <- setAxisTitles(data, chart.type, drop, values.title)
    values.title <- attr(data, "values.title")
    categories.title <- attr(data, "categories.title")
    attr(data, "values.title") <- NULL
    attr(data, "categories.title") <- NULL
    if (multiple.tables)
    {
        for (i in seq_along(data))
        {
            attr(data[[i]], "values.title") <- NULL
            attr(data[[i]], "categories.title") <- NULL
            if (NCOL(data[[i]]) > 2)
                attr(data[[i]], "statistic") <- NULL
        }
    }
    if (isScatter(chart.type) && !is.null(input.data.raw))
        data <- rmScatterDefaultNames(data)
    if (scatter.mult.yvals)
        attr(data, "scatter.mult.yvals") <- TRUE

    # Do not re-assign scatter variable indices if it already
    # exists (this is sometimes set in ExtractChartData
    # for some S3 classes) unless specifically requested
    if (isScatter(chart.type) && !scatter.mult.yvals &&
        (is.null(attr(data, "scatter.variable.indices")) ||
         any(nchar(select.columns), na.rm = TRUE)))
        attr(data, "scatter.variable.indices") <- scatterVariableIndices(input.data.raw, data, show.labels)

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

    # Modify multi-stat QTables so they are 3 dimensional arrays
    # and statistic attribute from the primary statistic
    # This is needed to correctly export chart to powerpoint and
    # R GUI code checks the statistic attribute to determine axis formatting
    if (!tidy && is.array(data) && !is.null(attr(data, "questions")) &&
        is.null(attr(data, "statistic")))
    {
        data <- ConvertQTableToArray(data)
        #attr(data, "statistic") <- dimnames(data)[[3]][1]
        #attr(data, "multi-stat") <- TRUE

    }

    list(data = data,
         weights = weights,
         values.title = values.title,
         categories.title = categories.title,
         chart.title = chart.title,
         chart.footer = attr(data, "footer"),
         scatter.variable.indices = attr(data, "scatter.variable.indices"))
}

replaceDimNames <- function(x, dim, labels)
{
    if (length(dim(x)) < dim)
        x <- CopyAttributes(as.matrix(x), x)

    new.labels <- paste0(dimnames(x)[[dim]], rep("", dim(x)[dim])) # get length right
    tmp.labels <- ConvertCommaSeparatedStringToVector(labels)
    tmp.len <- min(length(tmp.labels), length(new.labels))
    new.labels[1:tmp.len] <- tmp.labels[1:tmp.len]
    dimnames(x)[[dim]] <- new.labels
    return(x)
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

.isTableList <- function(x){"list" %in% class(x) && !is.data.frame(x) && is.list(x) && length(x) > 1 &&
                            (is.matrix(x[[1]]) || is.data.frame(x[[1]]) || is.numeric(x[[1]]))}

isScatter <- function(chart.type)
{
    grepl("Scatter|Bubble", chart.type)
}

#' @importFrom verbs Sum
crosstabOneVariable <- function(x, group, weights = NULL,
        categorical.as.binary = FALSE, as.percentages = FALSE)
{
    data <- data.frame(x = x, y = group)
    data$w <- if (is.null(weights)) rep.int(1L, NROW(data)) else weights

    if (is.numeric(x) || !categorical.as.binary)
    {
        data$x <- AsNumeric(data$x, binary = FALSE)
        if (!is.null(weights))
        {
            data$xw <- data$x * weights
            out <- Table(xw~y, data = data, FUN = sum)/Table(w~y, data = data, FUN = sum)

        } else
            out <- Table(x~y, data = data, FUN = mean)
        attr(out, "statistic") <- "Average"
    } else
    {
        out <- Table(w~x+y, data = data, FUN = sum)
        if (as.percentages)
        {
            out <- out / Sum(data$w * !is.na(data$x), remove.missing = FALSE) * 100
            attr(out, "statistic") <- "%"

        } else
                attr(out, "statistic") <- "Counts"
    }
    return(out)
}





#' Aggregrate Raw Data For Charting
#' @param data \code{data.frame} containing raw data
#' @param weights numeric vector of weights
#' @param chart.type character; type of chart to be plotted
#' @param crosstab Aggregate using a contingency table.
#' @param categorical.as.binary Whether to convert factors to indicator variables
#' @param as.percentages Whether to return percentages instead of counts.
#'     This is only used if the chart.type is "Heat". The difference between these
#'     calculations is this percentage uses the number of observations in the dataframe
#'     as the denomicator. For bar/column charts, it is computing row percentages.
#' @return aggregated data
#' @noRd
#' @importFrom flipStatistics Table WeightedTable
#' @importFrom flipTransformations AsNumeric
aggregateDataForCharting <- function(data, weights, chart.type, crosstab,
        categorical.as.binary, as.percentages)
{
    if (chart.type != "Heat")
        as.percentages <- FALSE

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
        if (is.null(categorical.as.binary))
            categorical.as.binary <- TRUE

        data <- as.data.frame(data)
        tmp.names <- names(data)
        k <- NCOL(data)
        group.var <- data[,k]

        if (k <= 2)
        {
            out <- crosstabOneVariable(data[,1], group.var, weights,
                        categorical.as.binary, as.percentages)
            if (attr(out, "statistic") == "Average")
                attr(out, "categories.title") <- tmp.names[2]
            else
                names(dimnames(out)) <- tmp.names
        }
        else
        {
            res <- lapply(data[,-k], crosstabOneVariable, group = group.var,
                        weights = weights, categorical.as.binary = categorical.as.binary,
                        as.percentages = as.percentages)
            out <- do.call("rbind", res)

            if (chart.type == "Heat")
                names(dimnames(out)) <- c("", attr(group.var, "question"))
            else
                names(dimnames(out)) <- c("", tmp.names[2])

            attr.list <- sapply(res, function(x) attr(x, "statistic"))
            if (all(attr.list == attr.list[1]))
                attr(out, "statistic") <- attr.list[1]
        }
    }
    else # first.aggregate
    {
        if (is.null(categorical.as.binary))
            categorical.as.binary <- FALSE

        if (categorical.as.binary)
        {
            tmp.dat <- data
            tmp.names <- Names(data)
            tmp.numeric <- sapply(data, is.numeric)
        }
        if (is.data.frame(data))
            data <- AsNumeric(data, binary = categorical.as.binary)
        if (!is.null(weights))
        {
            xw <- sweep(data, 1, weights, "*")
            sum.xw <- apply(xw, 2, sum, na.rm = TRUE)
            w <- matrix(weights, nrow(data), ncol(data))
            w[is.na(data)] <- 0
            sum.w <- apply(w, 2, sum)
            out <- sum.xw / sum.w
        } else
           out <- apply(data, 2, mean, na.rm = TRUE)

        if (categorical.as.binary && any(!tmp.numeric))
        {
            ind <- which(!tmp.numeric)
            for (ii in ind)
            {
                tmp.pos <- grep(paste0("^", tmp.names[ii]), names(out))
                names(out)[tmp.pos] <- levels(tmp.dat[[ii]])
            }
        }
        out <- as.matrix(out)

        # If ANY of the variables have been converted to percentages
        # label 'statistic' attribute to prevent mixed summary
        # statistics from being summed
        if (categorical.as.binary && any(!tmp.numeric))
            attr(out, "statistic") <- "%"
        else if (!categorical.as.binary || all(tmp.numeric))
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
#' @importFrom flipChartBasics MatchTable
#' @importFrom flipFormat TidyLabels
#' @importFrom flipU MakeUniqueNames
coerceToDataFrame <- function(x, chart.type = "Column", remove.NULLs = TRUE)
{
    if (is.null(x))
        return(x)
    else if (is.data.frame(x))
        return(x)

    if (is.list(x) && length(x) == 1 && is.matrix(x[[1]])) # List only contains a matrix
        return(as.data.frame(x[[1]]))
    else if (is.character(x))
    {
        x <- TidyTabularData(x)
        return(as.data.frame(x))
    }

    # For plotting regression output in a scatterplot, coerce regression object to chart data
    if (any(reg.outputs <- checkRegressionOutput(x)) && isScatter(chart.type) && is.list(x))
    {
        if(reg.outputs[1])
            x[[1]] <- extractRegressionScatterData(x[[1]])
        if(reg.outputs[2])
        {
            # Always expect names attributes of the models or table to be passed by Q/Displayr
            # However, catch case where names arent provided in the Y element of input.data.raw
            reg.names <- if (!is.null(names(x[[2]]))) names(x[[2]]) else LETTERS[1:length(x[[2]])]
            x[[2]] <- mapply(extractRegressionScatterData,
                             x = x[[2]], y.axis = TRUE, name = reg.names, SIMPLIFY = FALSE)
        }

    }


    # if labels are present in raw data, extract and store for later
    rlabels <- x$labels
    x$labels <- NULL

    # Dealing with situation where x$X is a list containing only one thing.
    if (is.list(x[[1]]) && length(x[[1]]) == 1)
        x[[1]] <- x[[1]][[1]]

    # For Scatterplot, y-coordinates are entered by a multi comboBox
    # Remove duplicates before rownames are messed up
    if (isScatter(chart.type) && length(x) >= 2 && is.list(x[[2]]))
    {
        if (length(x[[2]]) > 1 || NCOL(x[[2]][[1]]) > 1)
            names(x[[2]]) <- NULL
        for (i in 1:length(x[[2]]))
        {
            y.rnames <- rownames(x[[2]][[i]])
            is.dup <- duplicated(y.rnames)

            # No warnings required because you may want points with the same label
            if (any(is.dup))
                rownames(x[[2]][[i]]) <- MakeUniqueNames(y.rnames)
        }
        # Remap all Y elements to common array and keep attributes
        if (!is.null(unlist(lapply(x[[2]], rownames))) && length(x[[2]]) >= 2 && any(reg.outputs))
        {
            y.all.rownames <- unique(unlist(lapply(x[[2]], rownames)))
            base.values <- rep(NA, length(y.all.rownames))
            x[[2]] <- lapply(seq_along(x[[2]]), function(i) {
                vals <- base.values
                indices <- match(names(x[[2]][[i]]), y.all.rownames, nomatch = 0)
                vals[indices] <- x[[2]][[i]]
                names(vals) <- y.all.rownames
                CopyAttributes(vals, x[[2]][[i]])
            })
        }
        x[[2]] <- data.frame(x[[2]], check.names = FALSE, check.rows = FALSE,
                        fix.empty.names = FALSE, stringsAsFactors = FALSE)
        ind.autonames <- grep("^structure\\(|^c\\(", colnames(x[[2]]), perl = TRUE)
        for (ii in ind.autonames)
        {
            tmp.name <- attr(x[[2]][,ii], "name")
            colnames(x[[2]])[ii] <- if (!is.null(tmp.name)) tmp.name else " "
        }
    }

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
    x.rows <- sapply(x, function(m) NROW(as.data.frame(m)))
    k <- length(x.rows)
    extra.cols <- NULL
    if (isScatter(chart.type))
    {
        # Trim Y if sizes or color variable is provided
        if (NCOL(x$Y) > 1 && (!is.null(x$Z1) || !is.null(x$Z2) || !is.null(x$groups)))
        {
            warning("Only the first column of '", scatterDefaultNames(2),
                    "' variables is used'")
            extra.cols <- x$Y[,-1,drop = FALSE]
            x$Y <- x$Y[,1,drop = FALSE]
        }
        for (i in 1:k)
        {
            if (names(x)[i] != "Y" && NCOL(x[[i]]) > 1)
            {
                warning("Only the first column of '", scatterDefaultNames(i),
                        "' variables is used")
                x[[i]] <- x[[i]][,1,drop = FALSE]
            }
        }
    }

    # Extracting variable names
    if (isScatter(chart.type))
        nms <- unlist(lapply(1:k, function(i) {
            if (length(dim(x[[i]])) < 2) tidyScatterDefaultNames(names(x)[i])
            else                         colnames(x[[i]]) }))
    else
        nms <- if (all.variables) names(x) else unlist(lapply(x, names)) # i.e. 'X', 'Y', 'labels'

    # Check for row names to match on
    x.all.rownames <- NULL
    removed.rownames <- NULL
    if (isScatter(chart.type) && length(x) > 1)
    {
        # Check for row names to match on
        .getRowNames <- function(x)
        {
            if (is.null(nrow(x)) && !is.list(x))
                return(names(x))
            else if (hasUserSuppliedRownames(x))
                return(rownames(x))
            else
                return(NULL)
        }
        x.all.rownames <- .getRowNames(x[[1]])
        for (i in 2:k)
        {
            if (length(x.all.rownames) == 0)
                x.all.rownames <- .getRowNames(x[[i]])
            else
            {
                tmp.names <- .getRowNames(x[[i]])
                if (length(tmp.names) > 0)
                {
                    removed.rownames <- unique(c(setdiff(x.all.rownames, tmp.names),
                                                 setdiff(tmp.names, x.all.rownames)))
                    x.all.rownames <- intersect(x.all.rownames, tmp.names)
                }

            }
        }

        # This is only rearranging the tables into the right order/dimensions
        # Note that we don't use MergeTables because this forces tables into the same type
        if (length(x.all.rownames) > 0)
        {
            for (i in 1:k)
                x[[i]] <- MatchTable(x[[i]], ref.names = x.all.rownames,
                                as.matrix = FALSE, trim.whitespace = FALSE,
                                silent.remove.duplicates = TRUE)

            if (!is.null(extra.cols))
                extra.cols <- MatchTable(extra.cols, ref.names = x.all.rownames,
                                as.matrix = FALSE, trim.whitespace = FALSE,
                                silent.remove.duplicates = TRUE)

            if (length(x.all.rownames) < max(x.rows))
            {
                discarded.rows <- if(length(removed.rownames) == 0) NULL else {
                    paste0(": ", paste0(removed.rownames, collapse = ", "))
                }
                if (any(reg.outputs))
                    base.warning <- paste0("Y input coefficients that did not appear in the list of X input ",
                                           "coefficients were discarded")
                else
                {
                    # Suppress warnings when removed rows are named "NET"
                    # This happens often when inputs are BANNERS
                    if (length(removed.rownames) > 0)
                        removed.rownames <- removed.rownames[trimws(removed.rownames) != "NET"]
                    base.warning <- "Rows that did not occur in all of the input tables were discarded"
                }

                if (length(removed.rownames) > 0)
                    warning(base.warning, discarded.rows)
            }
            if (length(rlabels) > 0)
                warning("The 'Labels' variable has been ignored. Using row names of ",
                        "'X-coordinates' and 'Y-coordinates' instead")
            rlabels <- x.all.rownames
        }
    }

    if (any(reg.outputs) && length(x.all.rownames) == 0 && length(x) > 1)
    {
        x.names <- paste0(sQuote(names(x[[1]])), collapse = ", ")
        y.names <- paste0(sQuote(rownames(x[[2]])), collapse = ", ")
        stop("The X coordinate and Y coordinate inputs don't have any variables with matching names. ",
             "Please ensure that there is matching input for both the X and Y coordinate input. ",
             "The X coordinate input has names: ", x.names, ". ",
             "The Y coordinate input has names: ", y.names, ".")
    }

    num.obs <- sapply(x, NROW)
    if (isScatter(chart.type) && is.null(x.all.rownames) &&
        length(unique(num.obs[num.obs > 0])) > 1)
    {
        # If data is aggregated (e.g. the mean of each variable) then
        # the length can differ
        names(num.obs) <- sapply(names(num.obs), tidyScatterDefaultNames)
        ind.diff <- which(num.obs > 0 & num.obs != num.obs[1])
        stop("Variables for '", paste(names(num.obs)[ind.diff], collapse = "', '"),
            "' differ in length from variables for '", names(num.obs)[1], "'. ",
            "Check that all variables are from the same data set.")
    }

    # Splicing together elements of the input list if lengths vary
    # Note that elements of x can contain lists of variables
    invalid.joining <- FALSE
    if (!isScatter(chart.type) && (NCOL(x) > 1 || is.list(x) && length(x) > 1))
    {
        if (invalid.joining <- sd(x.rows) != 0)
        {
            k <- length(x.rows)
            out <- matrix(NA, max(x.rows), k)
            for (i in 1:k)
                out[1:x.rows[i], i] <- x[[i]]
            x <- out
        }
    }
    x <- data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
    names(x) <- MakeUniqueNames(nms)
    if (!is.null(extra.cols))
        x <- data.frame(x, extra.cols, stringsAsFactors = FALSE, check.names = FALSE)

    # Set rownames
    if (!is.null(rlabels) && nrow(x) == length(rlabels))
         rownames(x) <- MakeUniqueNames(as.character(rlabels))
    if (invalid.joining)
        attr(x, "InvalidVariableJoining") <- TRUE
    return(x)
}


isDistribution <- function(chart.type)
{
    grepl("Bean|Box|Histogram|Density|Violin", chart.type)
}

#' @importFrom flipStatistics ExtractChartData
#' @importFrom verbs FlattenTableAndDropStatisticsIfNecessary
processInputData <- function(x, subset, weights)
{
    if (is.null(x))
        return(x)

    if (length(subset) > 1)
        warning("Filters have not been used. They can only be applied to variables and questions")
    if (length(weights) > 0)
        warning("Weights have not been used. They can only be applied to variables and questions")

    # Simplify input if only a single table has been specified
    if ("list" %in% class(x) && is.list(x) && !is.data.frame(x))
    {
        if (length(x) == 1)
            x <- x[[1]]
    }

    # Try to use S3 method to extract data
    x <- ExtractChartData(x)

    # Flatten tables with spans or grid questions
    has.mult.stats <- is.null(attr(x, "statistic")) && !is.null(attr(x, "questiontypes"))
    ndim <- length(dim(x)) - has.mult.stats
    if (ndim >= 2)
    {
        if (has.mult.stats)
        {
            x0 <- suppressWarnings(FlattenTableAndDropStatisticsIfNecessary(x))
            stat.names <- dimnames(x)[[ndim + 1]]
            new.names <- (dimnames(x0))
            new.names[[length(new.names) + 1]] <- stat.names
            new.x <- array(x, dim = c(dim(x0), length(stat.names)), dimnames = new.names) 
            x <- CopyAttributes(new.x, x)
        } else
            x <- FlattenTableAndDropStatisticsIfNecessary(x)
    }

    if (hasUserSuppliedRownames(x))
        attr(x, "assigned.rownames") <- TRUE

    return(x)
}

processPastedData <- function(input.data.pasted, warn, date.format, subset, weights)
{
    if (length(subset) > 1)
        warning("Filters have not been used. They can only be applied to variables and questions")
    if (length(weights) > 0)
        warning("Weights have not been used. They can only be applied to variables and questions")

    us.format <- switch(date.format, US = TRUE, International = FALSE, Automatic = NULL, "No date formatting")
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
    if (!is.null(attr(processed, "row.column.names")))
        names(dimnames(processed)) <- attr(processed, "row.column.names")
    return(processed)
}

#' @importFrom verbs Sum
checkNumberOfDataInputs <- function(data.source.index, table, tables, raw, pasted, other)
{
    data.provided <- !sapply(list(table, tables, raw, pasted, other), is.null)
    n.data <- Sum(data.provided, remove.missing = TRUE)
    if (n.data == 0)
        stop("No data has been provided.")
    else if (is.null(data.source.index))
    {
        if (n.data > 1)
            stop("There are ", n.data, " data inputs. One and only one data argument may be supplied.")

    } else if (!data.provided[data.source.index])
        stop("The data provided does not match the 'data.source.index'.")
}

# For error messages, etc
scatterDefaultNames <- function(i)
{
    return(switch(i,
         "X",
         "Y",
         "Sizes",
         "Colors",
         "Groups"))
}

tidyScatterDefaultNames <- function(x)
{
    return(switch(x,
        X = "X coordinates",
        Y = "Y coordinates",
        Z1 = "Sizes",
        Z2 = "Colors"))
}

rmScatterDefaultNames <- function(data)
{
    # Remove default names so they are not shown in the axis
    if (is.data.frame(data) && !is.null(colnames(data)))
    {
        if (colnames(data)[1] == "X coordinates")
            colnames(data)[1] <- " "
        if (NCOL(data) >= 2 && colnames(data)[2] == "Y coordinates")
            colnames(data)[2] <- "  "
    }
    return(data)
}


scatterVariableIndices <- function(input.data.raw, data, show.labels)
{
    # Use ExtractChartData to convert any raw Regression input
    if (any(reg.outputs <- checkRegressionOutput(input.data.raw)))
    {
        if(reg.outputs[1])
            input.data.raw[[1]] <- extractRegressionScatterData(input.data.raw[[1]])
        if(reg.outputs[2])
            input.data.raw[[2]] <- lapply(input.data.raw[[2]], extractRegressionScatterData, y.axis = TRUE)
    }

    # Creating indices in situations where the user has provided a table.
    len <- length(input.data.raw)
    indices <- c(x = 1,
                 y = 2,
                 sizes = if (NCOL(data) >= 3) 3 else NA,
                 colors = if (NCOL(data) >= 4) 4 else NA,
                 groups = NCOL(data))
    if (is.null(input.data.raw) || is.data.frame(input.data.raw) || is.list(input.data.raw) && len == 1)
        return(indices)

    .getColumnIndex <- function(i)
    {
        if (i > len)
            return(NA)
        if (raw.is.null[i])
            return(NA)
        ind <- cumsum(!raw.is.null)[i]
        lst <- input.data.raw[[i]]
        if (is.null(lst))
            return(NA)
        nms <- names(data)

        # If inputs are variables, match on label/variable name to avoid problems with duplicates
        # This should not be applied on tables which do not necessarily have unique names
        if (!is.null(attr(lst, "label")) && is.null(attr(lst, "questions")))
        {
            nm <- if (show.labels) Labels(lst) else Names(lst)
            if (is.null(nm) || length(nm) != 1)
                return(ind)
            pos <- match(nm, nms)
            if (!is.na(pos))
                return(pos)
        }
        return(ind)
    }


    # Indices corresponding to selections in input.raw.data
    raw.is.null <- sapply(input.data.raw, is.null)
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

    if (length(dim(data)) == 2 && is.null(attr(data, "statistic")) &&
        length(attr(data, "questions")) == 2 && attr(data, "questions")[2] == "SUMMARY")
    {
        # 1-dimensional table with multiple statistics
        data[,1] <- prop.table(data[,1]) * 100
    }
    else if (length(dim(data)) > 2)
    {
        # 2-dimensional table with statistics
        data[,,1] <- prop.table(suppressWarnings(TidyTabularData(data)), 1) * 100
    }
    else if (NCOL(data) > 1)
    {
        # 2-dimensional table without statistics
        data <- prop.table(data, 1) * 100
        attr(data, "statistic") <- "Row %"
    }
    else
    {
        # 1-dimensional table without statistics
        data <- prop.table(data) * 100
        attr(data, "statistic") <- "%"
    }
    data
}

RearrangeRowsColumns <- function(data,
                                 multiple.tables,
                                 select.rows, first.k.rows, last.k.rows,
                                 select.columns, first.k.columns, last.k.columns,
                                 row.names.to.remove, column.names.to.remove, split,
                                 auto.order.rows, auto.order.columns,
                                 sort.rows, sort.rows.decreasing, sort.rows.column,
                                 sort.rows.exclude, reverse.rows,
                                 sort.columns, sort.columns.decreasing, sort.columns.row,
                                 sort.columns.exclude, reverse.columns)
{
    if (multiple.tables)
    {
        for(i in seq_along(data))
            data[[i]] = RearrangeRowsColumns(data[[i]], FALSE,
                                 select.rows, first.k.rows, last.k.rows,
                                 select.columns, first.k.columns, last.k.columns,
                                 row.names.to.remove, column.names.to.remove, split,
                                 auto.order.rows, auto.order.columns,
                                 sort.rows, sort.rows.decreasing, sort.rows.column,
                                 sort.rows.exclude, reverse.rows,
                                 sort.columns, sort.columns.decreasing, sort.columns.row,
                                 sort.columns.exclude, reverse.columns)
        return(data)
    }

    # Select first so that sorting only occurs in rows/columns of interest
    data <- SelectRows(data, select = select.rows)
    data <- SelectColumns(data, select = select.columns)

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

    # Keep hidden rows/columns until after sorting
    # Sort is often performed on the NET values
    data <- RemoveRowsAndOrColumns(data,
                row.names.to.remove = row.names.to.remove,
                column.names.to.remove = column.names.to.remove, split = split)

    # Keep last to retain order from sorting
    data <- SelectRows(data, first.k = first.k.rows, last.k = last.k.rows)
    data <- SelectColumns(data, first.k = first.k.columns, last.k = last.k.columns)

}

#' @importFrom flipTables RemoveRowsAndOrColumns HideEmptyRows HideEmptyColumns
#' @importFrom flipTime AsDate AsDateTime IsDateTime
#' @importFrom flipU CopyAttributes
#' @importFrom verbs Sum
transformTable <- function(data,
                           chart.type,
                           multiple.tables,
                           tidy,
                           drop,
                           is.raw.data,
                           hide.output.threshold,
                           hide.values.threshold,
                           hide.rows.threshold, hide.columns.threshold,
                           transpose,
                           first.aggregate,
                           hide.empty.rows, hide.empty.columns,
                           date.format,
                           table.counter = 1)
{
    if (multiple.tables)
    {
        for (i in seq_along(data))
            data[[i]] = transformTable(data[[i]],
                                       chart.type,
                                       FALSE,
                                       FALSE,
                                       FALSE,
                                       is.raw.data,
                                       0, 0, 0, 0, # sample size not used
                                       transpose,
                                       first.aggregate,
                                       hide.empty.rows, hide.empty.columns,
                                       date.format,
                                       i)
        return(data)
    }

    if (hide.empty.rows)
        data <- if (isListOrRaggedArray(data)) lapply(data, HideEmptyRows)
                else HideEmptyRows(data)

    if (hide.empty.columns)
    {
        if (isScatter(chart.type))
            old.names <- colnames(data)
        data <- if (isListOrRaggedArray(data)) lapply(data, HideEmptyColumns)
                else HideEmptyColumns(data)
        if (FALSE && isScatter(chart.type))
        {
            ind.rm <- which(!old.names %in% colnames(data))
            if (length(ind.rm) > 0)
            {
                new.indices <- attr(data, "scatter.variable.indices")
                for (i in 1:length(new.indices))
                    new.indices[i] <- new.indices[i] - Sum(ind.rm <= new.indices[i], remove.missing = TRUE)
                attr(data, "scatter.variable.indices") <- new.indices
            }
        }
    }

    # Switching rows and columns
    # This is the first operation performed to ensure that both
    # hide.rows.threshold and row.names.to.remove refer to rows AFTER tranposing
    if (isTRUE(transpose))
    {
        if (length(dim(data)) > 2)
            new.data <- aperm(data, c(2,1,3))
        else
            new.data <- t(data)
        data <- CopyAttributes(new.data, data)
        attr(data, "questions") <- rev(attr(data, "questions"))
    }

    # Checking sample sizes (if available)
    # This needs to happen after row/columns have been (de)selected
    if (any(as.integer(hide.output.threshold), na.rm = TRUE))
        data <- HideOutputsWithSmallSampleSizes(data, hide.output.threshold)
    if (any(as.integer(hide.values.threshold), na.rm = TRUE))
        data <- HideValuesWithSmallSampleSizes(data, hide.values.threshold)
    if (any(as.integer(hide.rows.threshold), na.rm = TRUE))
        data <- HideRowsWithSmallSampleSizes(data, hide.rows.threshold)
    if (any(as.integer(hide.columns.threshold), na.rm = TRUE))
        data <- HideColumnsWithSmallSampleSizes(data, hide.columns.threshold)

    # Set axis names before dropping dimensions (but AFTER transpose)
    data <- setAxisTitles(data, chart.type, drop)
    if (chart.type == "Scatter" && is.null(dim(data)))
    {
        tmp.names <- names(data)
        dim(data) <- c(length(data), 1)
        if (!is.null(tmp.names))
            rownames(data) <- tmp.names
    }

    # Convert to matrix to avoid state names from being turned into numeric values
    # when TidyTabularData is called
    if (gsub(" ", "", chart.type) == "GeographicMap")
        data <- CopyAttributes(as.matrix(data), data)

    # This must happen after sample sizes have been used
    # (only first statistic is retained after tidying)
    if (tidy && !chart.type %in% c("Venn", "Sankey", "Heat") &&
        !isScatter(chart.type) && !isDistribution(chart.type))
            data <- tryCatch(TidyTabularData(data), error = function(e) { data })


    if (!grepl("^No date", date.format) && date.format != "Automatic")
    {
        input.us.format <- !grepl("International", date.format)
        output.format.str <- if (!grepl("International", date.format)) "%b %d %Y" else "%d %b %Y"
        if (!is.null(rownames(data)) && IsDateTime(rownames(data)))
        {
            tmp.dates <- try(suppressWarnings(AsDate(rownames(data), us.format = input.us.format)), silent = TRUE)
            if (inherits(tmp.dates, "try-error"))
                tmp.dates <- suppressWarnings(AsDate(rownames(data)))
            rownames(data) <- format(tmp.dates, output.format.str)
        }
        else if (IsDateTime(names(data)))
        {
            tmp.dates <- try(suppressWarnings(AsDate(names(data), us.format = input.us.format)), silent = TRUE)
            if (inherits(tmp.dates, "try-error"))
                tmp.dates <- suppressWarnings(AsDate(names(data)))
            names(data) <- format(tmp.dates, output.format.str)
        }
    }
    return(data)
}

convertPercentages <- function(data, as.percentages, hide.percent.symbol, chart.type,
                               multiple.tables, table.counter = 1)
{
    if (multiple.tables)
    {
        for(i in seq_along(data))
            data[[i]] = convertPercentages(data[[i]], as.percentages, hide.percent.symbol,
                            chart.type, FALSE, i)
        return(data)
    }

    ### If data is already percentages in Qtable then divide by 100
    ### Note that R outputs and pasted data will already be in decimals
    #stat <- attr(data, "statistic")
    #qst <- attr(data, "questions")
    #if (!is.null(stat) && !is.null(qst) && grepl("%)?$", stat))
    #    data <- data / 100

    # Convert to percentages - this must happen AFTER transpose and RemoveRowsAndOrColumns
    if (as.percentages && chart.type != "Venn")
    {
        percentages.warning <- paste0("The data has not been converted to percentages/proportions. ",
        "To convert to percentages, first convert to a more suitable type (e.g., create a table).")
        if (!is.numeric(data) && !is.data.frame(data) &&
            (is.null(attr(data, "questions")) || chart.type %in% c("Pie", "Donut", "Heat")))
            warning(percentages.warning)
        else if (chart.type %in% c("Pie", "Donut"))
        {
            data <- data / Sum(data) * 100
            attr(data, "statistic") <- "%"
        }
        else if (chart.type == "Heat" && isTRUE(grepl("%$", attr(data, "statistic"))))
            data <- data
        else
            data <- asPercentages(data) # converts character QTables to numeric
    }

    if (hide.percent.symbol)
    {
        if (isTRUE(grepl("%", attr(data, "statistic"))))
            attr(data, "statistic") <- "Percent"
        else if (!is.null(attr(data, "questions")) && !is.null(attr(data, "name")) &&
                  is.null(attr(data, "statistic")))
        {
            dlen <- length(dim(data))
            primary.stat <- dimnames(data)[[dlen]][1]
            if (grepl("%", primary.stat))
                dimnames(data)[[dlen]][1] <- gsub("%", "Percent", primary.stat)
        }
    }
    return(data)
}

#' @importFrom flipTables TidyTabularData
#' @importFrom flipTransformations AsNumeric
#' @importFrom flipU MakeUniqueNames
#' @importFrom verbs SumRows
prepareForSpecificCharts <- function(data,
                                     multiple.tables,
                                     input.data.raw,
                                     chart.type,
                                     weights,
                                     show.labels,
                                     scatter.mult.yvals)
{
    if (!isDistribution(chart.type) && chart.type != "Table" && !is.null(input.data.raw) &&
        is.list(input.data.raw$X) && length(input.data.raw$X) > 10 && !inherits(input.data.raw$X, "Regression"))
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
        missing.data.rows <- SumRows(as.matrix(is.na(data))) > 0
        if (any(missing.data.rows))
        {
            data <- data[!missing.data.rows, ]
            warning(Sum(missing.data.rows), " case(s) with missing data have been removed.")
        }
    }
    else if (chart.type == "Sankey")
    {
        data <- coerceToDataFrame(data)
    }
    # Scatterplots
    else if (isScatter(chart.type))
    {
        .isQTableWithMultStatistic <- function(x)
        {
            !is.null(attr(x, "questions")) && !is.null(attr(x, "name")) && is.null(attr(x, "statistic"))
        }


        if (isTRUE(scatter.mult.yvals) ||
            (is.list(input.data.raw$Y) && length(input.data.raw$Y) > 1))
        {
            # Tag data for reformatting but this is preformed later after
            # Row/column manipulations
            attr(data, "scatter.mult.yvals") <- TRUE

        } else if (NCOL(input.data.raw$Y[[1]]) > 1 && is.null(input.data.raw$Z1) &&
            is.null(input.data.raw$Z2) && is.null(input.data.raw$groups))
        {
            if (!(.isQTableWithMultStatistic(input.data.raw$Y[[1]]) &&
                  length(dim(input.data.raw$Y[[1]])) < 3))
                attr(data, "scatter.mult.yvals") <- TRUE

            if (.isQTableWithMultStatistic(input.data.raw$Y[[1]]))
            {
                if (length(dim(input.data.raw$Y[[1]])) < 3)
                    attr(data, "ycol") <- 1
                else
                    attr(data, "ycol") <- NCOL(input.data.raw$Y[[1]])
            }

        } else
        {
            if (!is.data.frame(data) && !is.matrix(data))
                data <- TidyTabularData(data)

            # Removing duplicate columns
            if (length(dim(data)) == 2 && any(d <- duplicated(names(data))))
                data <- data[, !d]

            # flipStandardCharts::Scatterplot takes an array input, with column numbers indicating how to plot.
            if (is.null(attr(data, "scatter.variable.indices")))
                attr(data, "scatter.variable.indices") = scatterVariableIndices(input.data.raw, data, show.labels)
        }
    }
    # Charts that plot the distribution of raw data (e.g., histograms)
    else if (isDistribution(chart.type))
    {
        len <- Sum(!vapply(input.data.raw, is.null, FALSE))
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
        tmp.stat <- attr(data, "statistic")
        data <- useFirstColumnAsLabel(data,
            allow.numeric.rownames = chart.type %in% c("Area", "Bar", "Column", "Line", "Stream"))
        attr(data, "statistic") <- tmp.stat
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
    if (NROW(x) == 1) # single row input
        return(x)
    if (hasUserSuppliedRownames(x))
        return(x)

    if (!allow.numeric.rownames && is.numeric(x[,1]))
        return(x)

    # What to do with duplicate rownames?
    ind.dup <- duplicated(x[,1])

    # Duplicated numeric vectors are most likely data variables, not rownames
    if (any(ind.dup) && is.numeric(x[,1]))
        return(x)

    # For duplicated character vectors, we remove duplicates
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
        wmsg <- if (IsDateTime(x[,1])) ". Check aggregation level of date variable '"
                else                   ". Consider aggregating on '"

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

setAxisTitles <- function(x, chart.type, drop, values.title = "")
{
    if (isScatter(chart.type))
    {
        # Charting functions will automatically use column names
        attr(x, "categories.title") <- ""
        attr(x, "values.title") <- ""

    } else if (chart.type == "Heat")
    {
        # No default axis labels for summary tables
        # Because it depends on the question type used to create the table
        if (length(attr(x, "questions")) == 2 &&
            "SUMMARY" %in% attr(x, "questions"))
        {
            attr(x, "categories.title") <- ""
            attr(x, "values.title") <- ""
        }

        if (is.null(attr(x, "categories.title")))
            attr(x, "categories.title") <- names(dimnames(x))[2]
        if (is.null(attr(x, "categories.title")))
            attr(x, "categories.title") <- attr(x, "questions")[2]

        if (is.null(attr(x, "values.title")))
            attr(x, "values.title") <- names(dimnames(x))[1]
        if (is.null(attr(x, "values.title")))
            attr(x, "values.title") <- attr(x, "questions")[1]
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
        else if (!is.null(attr(x, "statistic")) && grepl("Percent", attr(x, "statistic")))
            attr(x, "values.title") <- ""
        else if (any(nchar(attr(x, "statistic"))))
            attr(x, "values.title") <- attr(x, "statistic")
    }
    if (sum(nchar(values.title)) > 0)
        attr(x, "values.title") <- values.title
    if (is.null(attr(x, "values.title")))
        attr(x, "values.title") <- ""
    if (drop && !is.data.frame(x) && !chart.type %in% c("Scatter", "Heat"))
    {
        # only drop 1 dimension from a 2d matrix
        if (!is.data.frame(x) && length(dim(x)) == 2 && dim(x)[2] == 1)
        {
            tmp.vec <- x[, 1]
            names(tmp.vec) <- rownames(x)
            attr(tmp.vec, "statistic") <- attr(x, "statistic")
            attr(tmp.vec, "questions") <- attr(x, "questions")
            attr(tmp.vec, "categories.title") <- attr(x, "categories.title")
            attr(tmp.vec, "values.title") <- attr(x, "values.title")
            x <- tmp.vec
        }
        else
            x <- CopyAttributes(drop(x), x)
    }
    x
}

#' Helps tidy Q variables and tables
#' @description Inputs supplied via input.data.raw can be in a range of
#'  formats. This function does a minimal job of checking for attribute
#'  and using these as names when appropriate. Currently, it does
#'  two functions. (1) Returns the span instead of the values and
#'  (2) assigns column names to 1-dimensional Q tables. Inputs which
#'  cannot be safely converted to a matrix (e.g. date/time or factors)
#'  are returned as is without any changes.
#'
#' @param x Q table or variable
#' @param use.span Logical; Whether the span categories should be returned
#'  instead of the values in the table. Row names will be preserved.
#'  A warning will be given if this option is selected but no span
#'  attribute is found in \code{x}.
#' @param show.labels This option is only relevant for Q variables.
#'   For tables, the resulting variable will always be named by
#'   by the 'name' attribute, but for variables both the 'label' and
#'   'name' attribute can be used.
#' @param is.scatter.annot.data This condition is applied to input
#'   data expected to be used for annotation data for scatterplots.
#'   it checks that the data is one-dimensional and stops immediately
#'   and gives an error if this condition is not met. This avoid
#'   some nonsense output or misleading error messages that might
#'   be given by PrepareData.
#'
#' @export
PrepareForCbind <- function(x, use.span = FALSE, show.labels = TRUE,
                        is.scatter.annot.data = FALSE)
{
    if (is.null(x))
        return(x)
    if (is.scatter.annot.data && NCOL(x) > 1)
        stop("Annotation data for Scatterplots should be a single-column table or variable with the same number of values as the number of points in the chart")

    if (use.span && is.null(attr(x, "span")))
        warning("Spans were not used as this attribute was not found in the data.")

    new.dat <- NULL
    cname.prefix <- ""
    if (inherits(x, c("POSIXct", "POSIXt", "Date")) || is.factor(x))
    {
        # For variables, this function is not really required
        # and for non-atomic types it results in info being lost
        new.dat <- data.frame(x)

    } else if (use.span && !is.null(attr(x, "span")))
    {
        # Q tables can always be converted to a matrix
        new.dat <- as.matrix(attr(x, "span")$rows[,1])
        if (!is.null(rownames(x)))
            rownames(new.dat) <- rownames(x)
        else
            rownames(new.dat) <- names(x)

        # Assign a blank name, so this column is not
        # accidentally used for another variable
        # The space is needed to avoid ugly R defaults
        colnames(new.dat) <- " "
        return(new.dat)
    }
    else if (!is.list(x))
    {
        # Avoid trying to convert complex data structures
        # including dataframes which might have different types
        new.dat <- as.matrix(x)

    } else
        new.dat <- x

    # Multi-column tables are generally already correctly named
    if ((is.data.frame(x) || !is.list(x)) && ncol(new.dat) == 1)
    {
        if (!is.null(attr(x, "label")) && show.labels)     # x is a variable
            colnames(new.dat) <- Labels(x)
        else if (!is.null(attr(x, "name")) && length(Names(x)) == 1) # x is a table or a variable
            colnames(new.dat) <- Names(x)
        else
            colnames(new.dat) <- " "
    }
    new.dat <- CopyAttributes(new.dat, x)
    return(new.dat)
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
        if (Sum(not.nulls) != 2)
            return(FALSE)
        input.data.raw <- input.data.raw[1:2]
    }
    nms <- names(input.data.raw)
    ncols <- sapply(input.data.raw, NCOL)
    if (any(ncols != 1))
        return(FALSE)
    #if (is.list(input.data.raw$X) && length(input.data.raw$X) > 1) # Y-variable removed in coerceToDataFrame
    #    return(FALSE)
    return(all(nms == c("X", "Y")))
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


tidyLabels <- function(data, chart.type)
{
    tmp <- NULL
    vertical.chart <- isDistribution(chart.type) || chart.type == "Venn"
    if (length(dim(data)) >= 2)
    {
        orig.names <- if (vertical.chart) colnames(data)
                      else                rownames(data)
        if (!IsDateTime(orig.names))
        {
            tmp <- ExtractCommonPrefix(orig.names)
            if (!is.na(tmp$common.prefix))
            {
                warning(sprintf("'%s' has been removed from labels. To turn off de-select 'DATA MANIPULATION > Tidy labels'", tmp$common.prefix))
                if (vertical.chart)
                    colnames(data) <- tmp$shortened.labels
                else
                {
                    rownames(data) <- tmp$shortened.labels
                    if (is.null(attr(data, "categories.title")))
                        attr(data, "categories.title") <- tmp$common.prefix
                }
            }
        }
    }
    else if (!is.null(names(data))) # lists and vectors
    {
        if (!IsDateTime(names(data)))
        {
            tmp <- ExtractCommonPrefix(names(data))
            if (!is.na(tmp$common.prefix))
            {
                warning(sprintf("'%s' has been removed from labels. To turn off de-select 'DATA MANIPULATION > Tidy labels'", tmp$common.prefix))
                names(data) <- tmp$shortened.labels
                if (is.null(attr(data, "categories.title")))
                    attr(data, "categories.title") <- tmp$common.prefix
            }
        }
    }
    data
}


checkRegressionOutput <- function(x)
{
    # First element always a single element
    # Second element is a list of elements
    # Last four elements are Z1, Z2, groups and labels that should never be regression outputs
    return(c(inherits(x$X, "Regression"), any(sapply(x$Y, function(e) inherits(e, "Regression")))))
}

#' @importFrom flipFormat TidyLabels
extractRegressionScatterData <- function(x, y.axis = FALSE, name = NULL)
{
    if (!inherits(x, "Regression"))
        return(x)
    chart.data <- ExtractChartData(x)
    if (!is.null(x$importance))
        names(chart.data) <- TidyLabels(names(chart.data))
    if (y.axis)
    {
        chart.data <- as.array(chart.data)
        attr(chart.data, "name") <- name
    }
    return(chart.data)
}


# This function is used when scatter.mult.yvals = TRUE
# It converts the input data frame which a data series in each column
# into a the standard input format, where the data series
# is indicated by the value in the "Groups" column
# If x-coordinates are supplied in input.data.raw$X, then
# rownames attached to the x-coordinates will be used as
# rownames of the resulting data frame
# Otherwise, the rownames of data will be used as the
# x-coordinates and the rownames of the output data
# will be blank (with spaces as padding for uniqueness)
# This function also updates the attribute "scatter.variable.indices"
# to describe the format of the output data frame

convertScatterMultYvalsToDataFrame <- function(data, input.data.raw, show.labels, date.format)
{
    data.row.labels <- rownames(data)
    n <- nrow(data)
    if (any(reg.outputs <- sapply(input.data.raw$Y, function(e) inherits(e, "Regression"))))
    {
        extracted.data.raw.Y <- input.data.raw$Y
        extracted.data.raw.Y[reg.outputs] <- lapply(input.data.raw$Y[reg.outputs], ExtractChartData)
        regression.names <- names(input.data.raw$Y)
        idx <- which(reg.outputs)
        for(i in seq_along(idx))
            attr(extracted.data.raw.Y[[idx[i]]], "label") <- regression.names[idx[i]]
        y.names <- if (show.labels) Labels(extracted.data.raw.Y) else Names(extracted.data.raw.Y)
    } else
        y.names <- if (show.labels) Labels(input.data.raw$Y) else Names(input.data.raw$Y)

    # Figure out which columns to use as the X and Y coordinates
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
        if (!is.null(attr(data, "ycol")))
            m <- attr(data, "ycol")
        y.ind <- 1:m
        xvar <- rep(rownames(data), m)
        data.row.labels <- rep("", nrow(data))
    } else
    {
        # Otherwise use first column as X-coordinates
        m <- ncol(data) - 1
        if (!is.null(attr(data, "ycol")))
            m <- attr(data, "ycol") - 1
        y.ind <- (1:m) + 1
        xvar <- rep(data[,1], m)
    }

    if (!hasUserSuppliedRownames(data))
        data.row.labels <- rep("", nrow(data))
    if (length(y.names) < m)
        y.names <- colnames(data)[y.ind]
    if (length(y.names) < m)
        y.names <- paste("Group", 1:m)
    if (any(checkRegressionOutput(input.data.raw)) && length(y.names) >= m)
        y.names <- colnames(data)[y.ind]

    # Data from other statistics is restructured and appended separately
    extravar <- NULL
    if (!is.null(attr(data, "ycol")))
    {
        # Other statistics are in the rest of input.data.raw$Y[[1]]
        # But we need to take from data because we may have removed row/cols
        yvar <- as.vector(unlist(data[,y.ind]))
        y.names <- dimnames(input.data.raw$Y[[1]])[[2]]
        tmp.ind <- charmatch(y.names, colnames(data)[y.ind])
        y.names <- y.names[!is.na(tmp.ind)]
        y.names.patt <- paste(paste0("\\Q", y.names, "\\E"), collapse = "|")
        stat.names <- dimnames(input.data.raw$Y[[1]])[[3]]
        extravar <- matrix(NA, nrow = length(yvar), ncol = length(stat.names) - 1)

        for (i in 2:length(stat.names))
        {
            stat.names.patt <- paste0("\\Q.", stat.names[i], "\\E$") # make patt strict (e.g 'p')!
            tmp.ind <- intersect(grep(stat.names.patt, colnames(data)),
                                 grep(y.names.patt, colnames(data)))
            if (length(tmp.ind) > 0)
                extravar[,i-1] <- unlist(data[,tmp.ind])
        }
        colnames(extravar) <- stat.names[-1]

    } else if (length(dim(data)) >= 3)
    {
        # Other statistics are in the 3rd dimension of table
        yvar <- as.vector(unlist(data[,y.ind,1]))
        extravar <- apply(data[, y.ind, -1, drop = FALSE], 3, unlist)

    } else # simple case with no other statistics
        yvar <- as.vector(unlist(data[,y.ind]))


    # newdata needs to use data rather than input.data.raw
    # otherwise it will not handle filters etc
    newdata <- data.frame(X = xvar,
                          Y = yvar,
                          Groups = factor(rep(y.names, each = n), levels = y.names),
                          stringsAsFactors = FALSE)

    if (length(extravar) > 0)
        newdata <- cbind(newdata, extravar)
    rownames(newdata) <- if (length(unique(data.row.labels)) <= 1) NULL
                         else                                      MakeUniqueNames(rep(data.row.labels, m))
    if (!grepl("^No date", date.format) && date.format != "Automatic")
    {
        if (IsDateTime(as.character(newdata[,1])))
            newdata[,1] <- format(AsDate(as.character(newdata[,1]),
            us.format = !grepl("International", date.format)), "%b %d %Y")
    }

    # Preserve column names where possible
    if (!is.null(input.data.raw$X))
        colnames(newdata)[1] <- colnames(data)[1]
    else if (!is.null(qst <- attr(data, "questions")))
    {
        colnames(newdata)[1] <- qst[1]
        if (length(qst) >= 2)
            colnames(newdata)[3] <- qst[2]
    }
    if (length(dim(data)) == 3)
        colnames(newdata)[2] <- dimnames(data)[[3]][1]
    else if (!is.null(attr(data, "statistic")))
        colnames(newdata)[2] <- attr(data, "statistic")


    data <- newdata
    attr(data, "scatter.variable.indices") <- c(x = 1, y = 2, sizes = 0, colors = 3, groups = 3)
    return(data)
}

containsQTable <- function(x)
{
    if (!is.list(x))
        return(!is.null(attr(x, "questions")) && !is.null(attr(x, "name")))
    else
        return(any(sapply(x, containsQTable)))

}
