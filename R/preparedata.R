#' PrepareData
#'
#' Prepares input data for charting.
#' @param chart.type Character; chart type to be plotted.
#' @param subset subset An optional vector specifying a subset of
#'     observations to be used in the fitting process, or, the name of
#'     a variable in \code{data}. It may not be an expression.
#' @param weights An optional vector of sampling weights, or, the name
#'     of a variable in \code{data}. It may not be an expression.
#' @param input.data.table Array; typically a table of some kind, which is then
#' processed using \code{\link[flipTables]{AsTidyTabularData}}.
#' @param input.data.tables List of array; each component is assumed to be a
#'     Qtable and will be processed using.
#'     \code{\link[flipTables]{AsTidyTabularData}}
#' @param input.data.raw List, containing variables or data.frames.
#' @param input.data.pasted List of length six; the first component of which is
#'     assumed to be from a user-entered/pasted table; will be
#'     processed by \code{\link{ParseUserEnteredTable}}.
#' @param input.data.other  A PickAny Multi Q variable.
#' @param data.source Where multiple data inputs are provided, a text string can be provided
#' to disambiguate. Refer to the source code for a precise understanding
#' of how this works (it is not obvious and is not likely to be of any use for
#' most cases, so should usually be left as a \code{NULL}).
#' @param first.aggregate Logical; whether or not the input data
#' needs to be aggregated in this function. A single variable is tabulated,
#' 2 variables are crosstabbed, and with 3 or more the mean is computed.
#' been provided, a contingency table is used to aggregate.
#' @param tidy Logical; whether or not the input data
#' needs to be aggregated in this function (e.g., if an x and y variable have
#' been provided, a contingency table is used to aggregate. This defaults
#' to \code{TRUE}. It aggressively seeks to turn the data into a named vector
#' or a matrix using \code{\link[flipTables]{TidyTabularData}}. This is not applied
#' when \code{data.input.tables} are provided, or when the chart type is
#' any of \code{"Scatter"}, \code{"Bean"}, \code{"Histogram"}, \code{"Density"},
#' \code{"Box"}, or \code{"Violin"}.
#' @param transpose Logical; should the resulting matrix (of created)  be
#'     transposed?
#' @param row.names.to.remove Character vector or delimited string
#' of row labels specifying rows to remove from the returned table; default
#' is \code{c("NET", "SUM")}
#' @param column.names.to.remove Character vector or delimited string
#' of column labels specifying columns to remove from the returned table;
#' default is \code{c("NET", "SUM")}.
#' @param split Character delimiter to split \code{row.names.to.remove}
#' and \code{col.names.to.remove} on. Default is to split on either of
#' \code{","} or \code{";"}.  Assumed to be a regular expression; see
#' \code{\link{strsplit}}.
#' @param show.labels Logical; If \code{TRUE}, labels are used for
#'     names in the data output if raw data is supplied.
#' @param as.percentages Logical; If \code{TRUE}, aggregate values in the
#' output table are given as percentages summing to 100. If \code{FALSE},
#' column sums are given.
#' @param values.title The title for the values axis of a chart (e.g.,
#' the y-axis of a column chart or the x-axis of a bar chart).
#' @details It is assumed that only one of \code{pasted},
#'     \code{input.data.table}, \code{input.data.tables}, \code{input.data.other},
#'     \code{input.data.raw} is non-NULL.  They are checked for nullity in
#'     that order.
#' @importFrom flipTransformations ParseUserEnteredTable SplitVectorToList
#' @importFrom flipTables TidyTabularData RemoveRowsAndOrColumns
#' @importFrom flipData TidyRawData
#' @importFrom flipFormat Labels Names
#' @importFrom flipStatistics Table WeightedTable
#' @return A list with components
#' \itemize{
#' \item \code{data} - If possible, a named vector or matrix, or if that is not
#'     posible or a data.frame is requested, a data.frame.
#' \item  \code{weights} - Numeric vector of user-supplied weights.
#' \item \code{values.title} - Character string to be used for the y-axis title; will
#' only be a non-empty string if some aggregation has been performed on
#' \code{data}
#' \item  \code{scatter.variable.indices} A named vector indicating which columns in
#' \code{data} should be plotted in a scatterplot as \code{x}, \code{y}, \code{sizes},
#' and \code{colors}. Is \code{NULL} if \code{chart.type} does not contain \code{"Scatter"}
#' or  \code{"Bubble"}. \code{NA} is used when the data does not exist.
#' }
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
                        first.aggregate = FALSE,
                        tidy = TRUE,
                        transpose = FALSE,
                        row.names.to.remove = c("NET", "SUM"),
                        column.names.to.remove = c("NET", "SUM"),
                        split = "[;,]",
                        as.percentages = FALSE,
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
    # - Scattersplots of raw data, where separate drop boxes have been used as inputs.
    # - Scattersplots of raw data, where a table has been used as an input.
    # - Scattersplots of raw data, where pasted data has been used as an input.
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

    # This function need to be frequently understood and generalized
    # by multiple people. Consequently, the goal has been to write the code in such a
    # way as to make it as easy to read and maintain as possible. In particular,
    # many obvious ways to make this code more efficent have been ignored in the interests
    # of making it easy to read (and in recognition that the efficiency gains would be trivial anyway).

    ###########################################################################
    # 1. Converts the data inputs into a single data object called 'data'.
    ###########################################################################
    data.source.index <- if (is.null(data.source)) NULL else
        switch(data.source,
                "Link to a table in 'Pages'" = 1,
                "Link to multiple tables in 'Pages'" = 2,
                "Link to a variable in 'Data'" = 3,
                "Link to variables in 'Data'" = 3,
                "Link to variables in 'Data'" = 3,
                "Variable Set: Binary - Multi" = 3,
                "Variable Set: Pick Any" = 3,
                "Variable Set: Numeric - Multi" = 3,
                "Variable Set: Number - Multi" = 3,
                "Type or paste in data" = 4,
                "Use an existing R Output in 'Pages'" = 5,
                "Link to variable sets in 'Data'" = 3,
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
    # Check that there is no ambiguity rearding which input to use.
    checkNumberOfDataInputs(data.source.index, input.data.table, input.data.tables, input.data.raw, input.data.pasted, input.data.other)
    # Assign the data to 'data'
    data <- input.data.table
    if (is.null(data))
        data <- input.data.tables
    if (is.null(data))
        data <- asDataFrame(input.data.raw)
    if (is.null(data))
        data <- input.data.other
    if (is.null(data))
        data <- processPastedData(input.data.pasted, first.aggregate)
    # Replacing variable names with variable/question labels if appropriate
    if (is.data.frame(data))
        names(data) <- if (show.labels) Labels(data) else Names(data)

    ###########################################################################
    # 2. Filters the data and/or removes missing values
    ###########################################################################
    filt <- NROW(subset) == NROW(data)
    if (!is.null(input.data.raw) || filt || NROW(weights) == NROW(data))
    {
        missing <- if (chart.type %in% c("Scatter", "Venn", "Sankey"))
            "Exclude cases with missing data" else "Use partial data"
        n <- NROW(data)
        if (!is.null(attr(data, "InvalidVariableJoining")))
        {
            if (!isDistribution(chart.type))
                warning("The variables have been automatically spliced together, without any knowledge of which case should be matched with which. This may cause the results to be misleading.")
        }
        data <- TidyRawData(data, subset = subset, weights = weights, missing = missing)
        n.post <- NROW(data)
        if (missing == "Exclude cases with missing data" && n.post < n)
            warning("After removing missing values and/or filtering, ", n.post, " observations remain.")
        weights <- setWeight(data, weights)
    }

    ###########################################################################
    # 3. Aggregate the data if so required.
    ###########################################################################
    if (first.aggregate)
    {
        null.inputs <- sapply(input.data.raw, is.null)
        nms <- names(input.data.raw)[!null.inputs]
        crosstab <- length(nms) == 2 && nms == c("X", "Y") && ncol(data) == 2
        data <- aggregateDataForCharting(data, weights, chart.type, crosstab)
    }

    ###########################################################################
    # 4. Tailoring the data for the chart type.
    ###########################################################################
    data <- prepareForSpecificCharts(data, input.data.tables, input.data.raw, chart.type, weights, tidy)
    weights <- setWeight(data, weights)

    ###########################################################################
    # 5. Transformations of the tidied data (e.g., sorting, transposing, removing rows).
    ###########################################################################
    data <- transformTable(data,
                   !is.null(input.data.tables),
                   row.names.to.remove, column.names.to.remove, split,
                   transpose,
                   as.percentages)

    ###########################################################################
    # Finalizing the result.
    ###########################################################################

    # values.title and statistic are set in asPercentages and aggregateDataForCharting. Note that 'statistic'
    # is set as an attribute so that other functions (e.g., table rendering) can use this information
    # later (i.e., it is not just a lazy way of avoiding a list).
    if (values.title == "")
        values.title = if (is.null(yt <- attr(data, "values.title")))
            attr(data, "statistic") else yt
    if (is.null(values.title))
        values.title <- ""
    list(data = data,
         weights = weights,
         values.title = values.title,
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
    # In tables that show aggregated tables, only the x-axis title is
    # taken from dimnames. But both names should be set in case
    # the table is transposed
    if (NCOL(data) == 1)
    {
        out <- WeightedTable(data[[1]]) #, weights)
        d.names <- list(names(out), NULL)
        names(d.names) <- c(names(data)[1], "")
        out <- matrix(out, dimnames=d.names)
        attr(out, "statistic") = "Count"
    }
    else if (ncol(data) == 2 && crosstab)
    {
        tmp.names <- names(data)
        names(data) <- c("x", "y") # temporarily set names for formula
        data$w <- if (is.null(weights)) rep.int(1L, nrow(data)) else weights
        out <- flipStatistics::Table(w  ~  x + y, data = data, FUN = sum)
        names(dimnames(out)) <- tmp.names
        attr(out, "statistic") = "Counts"
    }
    else
    {
        if (!is.matrix(data) && !is.numeric(data))
            data <- asDataFrame(data)
        if (is.data.frame(data))
            data <- AsNumeric(data, binary = FALSE)
        if (weighted <- !is.null(weights))
        {
            xw <- sweep(data, 1, weights, "*")
            sum.xw <- apply(xw, 2, sum, na.rm = TRUE)
            w <- matrix(weights, nrow(data), ncol(data))
            w[!is.na(data)] <- 0
            sum.w <- apply(w, 2, sum)
            out <- sum.xw / w
        } else
           out <- apply(data, 2, mean, na.rm = TRUE)
        attr(out, "statistic") <- "Average"
    }
    out
}


#' @importFrom flipTables TidyTabularData
#' @importFrom stats sd
asDataFrame <- function(x, remove.NULLs = TRUE)
{
    if (is.character(x))
        x <- TidyTabularData(x)
    if (is.null(x))
        return(x)
    if (is.data.frame(x))
        return(x)
    if (is.list(x[[1]])) # In Displayr, this is typically true.
    {
        x[[1]] <- as.data.frame(x[[1]])
    }
    all.variables <- all(sapply(x, NCOL) == 1)
    if(remove.NULLs)
        x <- Filter(Negate(is.null), x)
    nms <- if (all.variables) names(x) else unlist(lapply(x, names))
    if (NCOL(x) > 1 || is.list(x) && length(x) > 1)
    {
        lengths <- sapply(x, NROW)
        if (invalid.joining <- sd(lengths) != 0)
        {
            k <- length(lengths)
            out <- matrix(NA, max(lengths), k)
            for (i in 1:k)
                out[1:lengths[i], i] <- x[[i]]
            x <- out
        }
    } else
        invalid.joining <- FALSE
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    names(x) <- nms
    if (invalid.joining)
        attr(x, "InvalidVariableJoining")
    x
}


isDistribution <- function(chart.type)
{
    grepl("Bean|Box|Histogram|Density|Violin", chart.type)
}

processPastedData <- function(input.data.pasted, first.aggregate)
{
    tryCatch(suppressWarnings(ParseUserEnteredTable(input.data.pasted[[1]],
                                  want.data.frame = first.aggregate,
                                  want.factors = input.data.pasted[[2]],
                                  want.col.names = input.data.pasted[[3]],
                                  want.row.names = input.data.pasted[[4]],
                                  us.format = input.data.pasted[[5]])),
             error = function(e) {input.data.pasted[[1]]})
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
        stop("The data provided does not mach the 'data.source.index'.")
}

scatterVariableIndices <- function(input.data.raw, data)
{
    indices <- c(x = 1, y = 2, sizes = 3, colors = 4)
    # Creating indices in situations where the user has provided a table.
    if (is.null(input.data.raw))
        return(indices[1:max(4, NCOL(data))])
    # Indices corresponding to selections in input.raw.data
    indices["x"] <- if (is.null(input.data.raw$X)) NA else 1
    indices["y"] <- if (is.null(input.data.raw$Y)) NA else 1 + max(indices[1], 0, na.rm = TRUE)
    indices["sizes"] <- if (is.null(input.data.raw$Z)) NA else 1 + max(indices[1:2], 0, na.rm = TRUE)
    indices["colors"] <- if (is.null(input.data.raw$Z2)) NA else 1 + max(indices[1:3], 0, na.rm = TRUE)
    indices
}

useLabelsAsRowNames <- function(input.data.raw, data)
{
    if (!is.null(input.data.raw) && !is.null(label.variable.name <- names(input.data.raw$labels)))
    {
        labels <- data[[label.variable.name]]
        data[[label.variable.name]]  <- NULL # Deleting the variable
        if (nrow(data) == NROW(labels))
            rownames(data) <- make.unique(as.character(labels), sep="")
    }
    data
}

asPercentages <- function(data)
{
    ind.negative <- which(data < 0)
    if (length(ind.negative) > 0)
    {
        warning("Percentages calculated ignoring negative values.")
        data[ind.negative] <- 0
    }

    if (is.matrix(data))
    {
        data <- prop.table(data, 2)
        attr(data, "statistic") <- "Column %"
        attr(data, "values.title") <- "%"
    }
    else
    {
        data <- prop.table(data)
        attr(data, "statistic") <- "%"
    }
    data
}

transformTable <- function(data,
                           multiple.tables,
                           row.names.to.remove, column.names.to.remove, split,
                           transpose,
                           as.percentages,
                           table.counter = 1)
{
    if (multiple.tables)
    {
        for(i in seq_along(data))
            data[[i]] = transformTable(data[[i]], FALSE,
                                       row.names.to.remove, column.names.to.remove, split,
                                       transpose,
                                       as.percentages,
                                       i)
        return(data)
    }
    ## Remove rows and columns
    data <- RemoveRowsAndOrColumns(data, row.names.to.remove = row.names.to.remove,
                                 column.names.to.remove = column.names.to.remove, split = split)
    ## Switching rows and columns
    if (isTRUE(transpose))
        data <- t(data)

    # Convert to percentages - this must happen AFTER transpose and RemoveRowsAndOrColumns
    if (as.percentages)
    {
        if ((!is.numeric(data) || prod(NROW(data)*NCOL(data)) == 1) && table.counter == 1)
            warning("The data has not been converted to percentages. To convert to percentages, ",
                    "first convert to a more suitable type (e.g., create a table)")
        else
            data <- asPercentages(data)
    }
    return(data)
}

#' @importFrom flipTables TidyTabularData
#' @importFrom flipTransformations AsNumeric
prepareForSpecificCharts <- function(data, input.data.tables, input.data.raw, chart.type, weights, tidy)
{
    # Multiple tables
    if (!is.null(input.data.tables))
    {
        data <- lapply(data, TidyTabularData)

    }
    else if (chart.type == "Venn")
    {
        data <- data # Do nothing.

    }
    else if (chart.type == "Sankey")
    {
        data <- asDataFrame(data)
    }
    # Scatterplots
    else if (isScatter(chart.type))
    {
        if (!is.data.frame(data) && !is.matrix(data))
            data <- TidyTabularData(data)
        # Appending the labels to the data.frame as row names
        data <- useLabelsAsRowNames (input.data.raw, data)
        # flipStandardCharts::Scatterplot takes an array input, with column numbers indicating how to plot.
        attr(data, "scatter.variable.indices") = scatterVariableIndices(input.data.raw, data)
    }
    # Charts that plot the distribution of raw data (e.g., histograms)
    else if (isDistribution(chart.type))
    {
        if (len <- length(input.data.raw) > 1)
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
    }
    else if (!tidy) # Do nothing
    {
        data <- data
    }
    else  # Everything else. We try and turn it into a table if we can.
    {
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
