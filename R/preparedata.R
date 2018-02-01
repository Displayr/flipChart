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
#' @param scatter.input.columns.order Character; This is used to determine
#'     the input format of \code{input.data.table/input.data.pasted/input.data.other}
#'     for scatterplots. Allowable values are
#'     "X coordinates, Y coordinates in multiple columns", "Data labels, X coordinates,
#'     Y coordinates, Size, Colors" (default), "X coordinates, Y coordinates, Sizes, Colors".
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
#'     0's.
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
#' @importFrom flipTables TidyTabularData RemoveRowsAndOrColumns
#' @importFrom flipData TidyRawData
#' @importFrom flipFormat Labels Names
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
                        group.by.last = FALSE,
                        tidy = TRUE,
                        transpose = FALSE,
                        row.names.to.remove = c("NET", "SUM"),
                        column.names.to.remove = c("NET", "SUM"),
                        split = "[;,]",
                        hide.empty.rows.and.columns = TRUE,
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
    if (all(sapply(input.data.pasted, is.null)))
        input.data.pasted <- NULL
    # Check that there is no ambiguity regarding which input to use.
    checkNumberOfDataInputs(data.source.index, input.data.table, input.data.tables,
                            input.data.raw, input.data.pasted, input.data.other)
    # Assign the data to 'data'
    data <- input.data.table
    if (is.null(data))
        data <- input.data.tables
    if (is.null(data))
        data <- coerceToDataFrame(input.data.raw, chart.type)
    if (is.null(data))
        data <- input.data.other
    if (is.null(data))
        data <- processPastedData(
                                  input.data.pasted,
                                  warn = tidy,
                                  date.format = date.format)

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

    ###########################################################################
    # 3. Aggregate the data if so required.
    ###########################################################################
    crosstab <- !chart.type %in% c("Bubble", "Scatter", "Venn") && 
                (rawDataLooksCrosstabbable(input.data.raw) || group.by.last)
    if (is.null(first.aggregate))
        first.aggregate <- crosstab
    if (crosstab && !first.aggregate)
        warning("Input data is always aggregated when 'Groups' variable is provided.")
    if (!isDistribution(chart.type) && (crosstab || first.aggregate))
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
    data <- prepareForSpecificCharts(data, input.data.tables, input.data.raw,
                                     chart.type, weights, tidy, show.labels,
                                     date.format, scatter.input.columns.order)
    weights <- setWeight(data, weights)

    ###########################################################################
    # 5. Transformations of the tidied data (e.g., sorting, transposing, removing rows).
    ###########################################################################
    data <- transformTable(data,
                   chart.type,
                   multiple.tables = !is.null(input.data.tables),
                   is.raw.data = !is.null(input.data.raw) || !is.null(input.data.pasted) || !is.null(input.data.other),
                   row.names.to.remove, column.names.to.remove, split,
                   transpose,
                   group.by.last || first.aggregate,
                   as.percentages && chart.type != "Venn", #Venn takes care of this itself
                   hide.empty.rows.and.columns = hide.empty.rows.and.columns,
                   date.format = date.format)

    ###########################################################################
    # Finalizing the result.
    ###########################################################################
    data <- setAxisTitles(data, chart.type, tidy, values.title)
    values.title <- attr(data, "values.title")
    categories.title <- attr(data, "categories.title")
    attr(data, "values.title") <- NULL
    attr(data, "categories.title") <- NULL

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
        tmp.names <- names(data)
        names(data) <- c("x", "y") # temporarily set names for formula
        data$w <- weights
        out <- flipStatistics::Table(w  ~  x + y, data = data, FUN = sum)
        names(dimnames(out)) <- tmp.names
        attr(out, "statistic") = "Counts"
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

processPastedData <- function(input.data.pasted, warn, date.format)
{
    us.format <- switch(date.format, US = TRUE, International = FALSE, NULL)
    want.data.frame <- length(input.data.pasted) > 1L && isTRUE(input.data.pasted[[2]])
    processed <- tryCatch(ParseUserEnteredTable(input.data.pasted[[1]],
                                  want.data.frame = want.data.frame,
                                  want.factors = input.data.pasted[[2]],
                                  want.col.names = input.data.pasted[[3]],
                                  want.row.names = input.data.pasted[[4]],
                                  us.format = us.format,
                                  warn = warn),
             error = function(e) {input.data.pasted[[1]]})
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

scatterVariableIndices <- function(input.data.raw, data, scatter.input.columns.order, show.labels)
{
    # Creating indices in situations where the user has provided a table.
    len <- length(input.data.raw)
    if (scatter.input.columns.order == "Groups, X coordinates, Y coordinates, Sizes, Colors")
        indices <- c(x = 2, y = 3, sizes = 4, colors = 1)
    else if (scatter.input.columns.order == "X coordinates, Y coordinates, Colors, Sizes")
        indices <- c(x = 1, y = 2, sizes = 4, colors = 3)
    else if (scatter.input.columns.order == "Data labels, X coordinates, Y coordinates, Colors, Sizes")
        indices <- c(x = 1, y = 2, sizes = 4, colors = 3)
    else
        indices <- c(x = 1, y = 2, sizes = 3, colors = 4)
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
        match(nm, nms)
    }
    # Indices corresponding to selections in input.raw.data
    indices["x"] <- .getColumnIndex(1)
    indices["y"] <- .getColumnIndex(2)
    indices["sizes"] <- .getColumnIndex(3)
    indices["colors"] <- .getColumnIndex(4)
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

#' @importFrom flipTables RemoveRowsAndOrColumns HideEmptyRowsAndColumns
#' @importFrom flipTime AsDate
transformTable <- function(data,
                           chart.type,
                           multiple.tables,
                           is.raw.data,
                           row.names.to.remove, column.names.to.remove, split,
                           transpose,
                           first.aggregate,
                           as.percentages,
                           hide.empty.rows.and.columns,
                           date.format,
                           table.counter = 1)
{
    if (multiple.tables)
    {
        for(i in seq_along(data))
            data[[i]] = transformTable(data[[i]],
                                       chart.type,
                                       FALSE,
                                       is.raw.data,
                                       row.names.to.remove, column.names.to.remove, split,
                                       transpose,
                                       first.aggregate,
                                       as.percentages,
                                       hide.empty.rows.and.columns,
                                       date.format,
                                       i)
        return(data)
    }

    ## Remove rows and columns
    data <- RemoveRowsAndOrColumns(data, row.names.to.remove = row.names.to.remove,
                                   column.names.to.remove = column.names.to.remove, split = split)

    if (hide.empty.rows.and.columns)
        data <- if (isListOrRaggedArray(data))
                         lapply(data, HideEmptyRowsAndColumns)
                     else
                         HideEmptyRowsAndColumns(data)

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
        else if (FALSE && is.raw.data && !first.aggregate) # currently turned off
        {
            # This clause is turned off because I can't understand
            # when it would be useful to divide by the number of rows
            # It is also making pasted data behave badly
            # No tests failed with this always set to FALSE
            if (is.null(nrow(data)))
                warning(percentages.warning)
            else
            {
                data <- data / nrow(data)
                warning("Percentages have been computed by dividing the data values by the ",
                        "number of rows in the data. If this is not appropriate, first convert to a ",
                        "more suitable type (e.g., create a table).")
                attr(data, "statistic") = "%"
            }
        }
        else
            data <- asPercentages(data)
    }

    # Convert dates in row/column names if format is NOT automatic
    # Pattern matching to allow for more flexible controls, e.g. "International (dd/mm/yyyy)"
    # All warnings are suppressed here - warnings are given in the charting functions
    .isDate <- function(x) return(!is.null(x) && all(!is.na(suppressWarnings(AsDate(x,
                                                                                    on.parse.failure = "silent")))))

    if (date.format != "Automatic" && .isDate(rownames(data)))
        rownames(data) <- format(suppressWarnings(AsDate(rownames(data), us.format = !grepl("International", date.format))), "%b %d %Y")
    else if (date.format != "Automatic" && .isDate(names(data)))
        names(data) <- format(suppressWarnings(AsDate(names(data), us.format = !grepl("International", date.format))), "%b %d %Y")
    return(data)
}

#' @importFrom flipTables TidyTabularData
#' @importFrom flipTransformations AsNumeric
prepareForSpecificCharts <- function(
                                     data,
                                     input.data.tables,
                                     input.data.raw,
                                     chart.type,
                                     weights,
                                     tidy,
                                     show.labels,
                                     date.format,
                                     scatter.input.columns.order)
{
    # Multiple tables
    if (!is.null(input.data.tables))
    {
        data <- lapply(data, TidyTabularData)
        # flipStandardCharts::Scatterplot takes an array input, with column numbers indicating how to plot.
        if (isScatter(chart.type))
            attr(data, "scatter.variable.indices") = c(x = 1, y = 2, sizes = 3, colors = 4)
    }
    else if (chart.type == "Table")
    {
        data <- data
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
        if (is.null(scatter.input.columns.order))
            scatter.input.columns.order = "Data labels, X coordinates, Y coordinates, Sizes, Colors"

        if ((scatter.input.columns.order == "X coordinates, Y coordinates in multiple columns") ||
            (is.list(input.data.raw$Y) && length(input.data.raw$Y) > 1))
        {
            n <- nrow(data)
            y.names <- if (show.labels) Labels(input.data.raw$Y) else Names(input.data.raw$Y)
            if (is.list(input.data.raw$Y) && is.null(input.data.raw$X))
            {
                # No X-coordinates supplied in variables
                m <- length(input.data.raw$Y)
                y.ind <- 1:m
                xvar <- rep(1:n, m)

            } else if (is.null(input.data.raw$Y) && (!is.null(rownames(data))) && suppressWarnings(any(!is.numeric(rownames(data)))))
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
            .isDate <- function(x) return(!is.null(x) && all(!is.na(suppressWarnings(AsDate(x,
                                                              on.parse.failure = "silent")))))

            if (date.format != "Automatic" && .isDate(as.character(newdata[,1])))
                newdata[,1] <- format(AsDate(as.character(newdata[,1]),
                us.format = !grepl("International", date.format)), "%b %d %Y")
            if (!is.null(input.data.raw$X))
                colnames(newdata)[1] <- colnames(data)[1]
            data <- newdata
            attr(data, "scatter.variable.indices") <- c(x = 1, y = 2, sizes = 0, colors = 3)

        } else
        {
            if (is.null(input.data.raw) && grepl("^Data labels", scatter.input.columns.order))
                data <- useFirstColumnAsLabel(data, allow.numeric.rownames = TRUE,
                            allow.duplicate.rownames = FALSE)
            if (!is.data.frame(data) && !is.matrix(data))
                data <- TidyTabularData(data)
            # Removing duplicate columns
            if (any(d <- duplicated(names(data))))
                data <- data[, !d]
            if (NCOL(data) > 4)
                warning("Columns ", paste(colnames(data)[5:ncol(data)], collapse = ", "),
                    " not used in Scatter plot.",
                    " Consider setting column order to 'X coordinates, Y coordinates in multiple columns'.")

            # flipStandardCharts::Scatterplot takes an array input, with column numbers indicating how to plot.
            attr(data, "scatter.variable.indices") = scatterVariableIndices(input.data.raw, data,
                        scatter.input.columns.order, show.labels)
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
        if (!is.list(data))
            data <- list(data)
    }
    else
    {
        data <- useFirstColumnAsLabel(data) # Set rownames before TidyTabularData so that factor are not converted to numeric
        if (tidy)
            data <- tryCatch(TidyTabularData(data), error = function(e) { data })
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
    allow.numeric.rownames = FALSE, allow.duplicate.rownames = TRUE)
{
    if (length(dim(x)) != 2 || ncol(x) == 1)
        return(x)
    if (!allow.numeric.rownames && is.numeric(x[,1]))
        return(x)

    # Preserve existing rownames if they are non-numeric
    # Unnamed matrices would have been given default names 'Row 1', 'Row 2',
    # Filtered variables would have numeric rownames
    # corresponding to index in original dataset
    tmp.names <- gsub("Row ", "", rownames(x))
    if (any(suppressWarnings(is.na(as.numeric(tmp.names)))))
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

        warning("Duplicated entries in '", colnames(x)[1], "': ",
            paste(unique(x[ind.dup,1]), collapse = ", "),
            ". Consider aggregating on '", colnames(x)[1], "'.")
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
        rownames(x) <- format(x[,1], "%b %d %Y")
    else if (is.factor(x[,1])) # QDates are also factors
        rownames(x) <- make.unique(as.character(x[,1]))
    else
        rownames(x) <- make.unique(as.character(x[,1]))
    c.title <- colnames(x)[1]
    x <- x[,-1, drop = FALSE]
    attr(x, "categories.title") <- c.title
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
        else
            attr(x, "values.title") <- attr(x, "statistic")
    }
    if (sum(nchar(values.title)) > 0)
        attr(x, "values.title") <- values.title
    if (is.null(attr(x, "values.title")))
        attr(x, "values.title") <- ""
    if (tidy)
        x <- drop(x)
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
