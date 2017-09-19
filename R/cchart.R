#' CChart
#'
#' Creates charts
#' @param chart.function.name The name of the function used for creating the chart (aka plot).
#' @param x The data to be plotted.
#' @param ... Arguments to the function \code{chart.type}
#' @param warn.if.no.match If TRUE, a warning is shown if any arugments are not matched.
#' @param append.data If TRUE, appends the chart data as an attribute called "ChartData".
#' @details Where \code{chart.type} is not the name of an existing function. It is always assumed that the first parameter
#' in the signature is a data object, which is assigned the value of \code{x}.
#' @importFrom methods formalArgs
#' @return A chart object that can be printed. Most often, a plotly object.
#' @export

CChart <- function(chart.function.name, x,  ..., warn.if.no.match = TRUE, append.data = FALSE)
{
    fun.and.pars <- getFunctionAndParameters(chart.function.name)
    arguments <- substituteArgumentNames(fun.and.pars$parameters.o, list(...), warn.if.no.match)
    args <- paste0("c(list(", fun.and.pars$parameter.1, " = x), arguments)")
    if (!append.data)
        return(do.call(fun.and.pars$chart.function, eval(parse(text = args))))
    result <- do.call(fun.and.pars$chart.function, eval(parse(text = args)))
    attr(result,  "ChartData") <- x #Used by Displayr to permit exporting of the raw data.
    result
}

#' getFunctionNameAndParameters
#'
#' Gets the function, loading parameters if necessary, and the parameters of the function.
#' @param chart.function.name The name of the function used for creating the chart (aka plot).
#' @return A list witht he following elements:
#' \item{\code{chart.function}}{The function}.
#' \item{\code{parameter.1}}{The first parameter in \code{chart.function}}.
#' \item{\code{parameter.o}}{The other parameters in \code{chart.function}}.
#' @export

getFunctionAndParameters <- function(chart.function.name)
{
    if (!is.character(chart.function.name))
        stop("'chart.function.name' must be of type 'character'.")

    # Getting the function.
    chart.function <- gsub('"', "", chart.function.name, fixed = TRUE) # fixing mess created when 'type' is already a character
    loadPackage(chart.function)
    chart.function <- get0(chart.function, mode = "function")
    if (!is.function(chart.function))
        stop(paste0("Cannot find ", chart.function,"."))
    parameters <- formalArgs(chart.function)
    list(chart.function = chart.function, parameter.1 = parameters[1], parameters.o = parameters[-1])
}


#' #' RGUIControls
#' #'
#' #' Writes the JavaScript to create the RGUI Controls for Displayr and Q. Note that the first parameter of the function
#' #' is not automatically written (as it is assumed to be data, to be  addressed by the remaining arguments).
#' #' @param chart.function.name The name of the function used for creating the chart (aka plot).
#' #' @param vector \code{TRUE} if the function accepts a vector as the sole data input.
#' #' @param matrix \code{TRUE} if the function accepts a \code{matrix} as the sole data input.
#' #' @param raw.data.1 \code{TRUE} if the function accepts a single variable of 'raw' (non-aggregated) data as an input.
#' #' @param raw.data.2 \code{TRUE} if the function accepts a pair of variables of 'raw' data.
#' #' @param raw.data.multi \code{TRUE} if the function accepts multiple variables of 'raw' data as an input.
#' #' @return A \code{character} object of JavaScript code.
#' #' @export
#'
#' RGUIControls <- function(chart.function.name,
#'                          vector = FALSE,
#'                          matrix = FALSE,
#'                          raw.data.1 = FALSE,
#'                          raw.data.2 = FALSE,
#'                          raw.data.multi = FALSE,
#'                          r.object = FALSE,
#'                          scalar = FALSE)
#' {
#'     parameters <- getFunctionAndParameters(chart.function.name)$parameters.o
#'     parameters
#' }
#'


#' substituteArgumentNames
#'
#' @param parameter.names The names of parameters.
#' @param arguments The arguments to match to the parameters.
#' @param warn.if.no.match If TRUE, a warning is shown if any arugments are not matched.
#'
substituteArgumentNames <- function(parameter.names, arguments, warn.if.no.match = TRUE)
{
    a.names <- names(arguments)
    p.names <- parameter.names
    a.unmatched <- !a.names %in% p.names
    p.unmatched <- !p.names %in% a.names
    if (sum(a.unmatched) > 0) # Some argument names do not match parameter names
    {
        # Perform matches and update a.names
        .replaceMatches <- function(aa, pp)
        {
            for (a in aa[a.unmatched])
                for(p in pp[p.unmatched]){
                    if (parametersEqual(p, a))
                        a.names[match(a, aa)] <- p.names[match(p, pp)]
                }
            a.names <<- a.names
        }
        ## Permuting order and ignoring case
        a.regularized <- tolower(a.names)
        p.regularized <- tolower(p.names)
        .replaceMatches(a.regularized, p.regularized)

        # Substituting synonyms
        a.unmatched <- !a.names %in% p.names
        p.unmatched <- !p.names %in% a.names
        if (sum(a.unmatched) > 0)
        {
            .replaceSynonyms <- function(names)
            {
                for (i in seq_along(synonyms))
                {
                    syns <- synonyms[[i]]
                    init.syn <- syns[1]
                    for (s in syns[-1])
                        names <- gsub(s, init.syn, names, fixed = TRUE)
                }
                names
            }
            a.regularized[a.unmatched] <- .replaceSynonyms(tolower(a.names[a.unmatched]))
            p.regularized[p.unmatched] <- .replaceSynonyms(tolower(p.names[p.unmatched]))
            .replaceMatches(a.regularized, p.regularized)
        }
    }
    a.unmatched <- !a.names %in% p.names
    if (sum(a.unmatched) > 0 && warn.if.no.match)
        warning("The following arguments have been ignored: ", paste(a.names[a.unmatched], collapse = ", "))
    names(arguments) <- a.names
    arguments[!a.unmatched]
}



#' Make sure that the first synonym is the briefiest, and from there they are ordered from
#' most more verbose to any roots.
#' These are swapped out in order. E.g., after the first line, all "colours" will be changed to 'col'.
synonyms <- list(c("col", "colours", "colour", "colors", "color"),
      c("size", "sizes"),
      c("label", "labels"),
      c("xtitle", "x.axis.title",  "x.title",   "xlab"),
      c("ytitle", "y.axis.title",  "y.title",  "ylab"),
      c("title", "main"),
      c("label.show", "data.label.show"),
      c("fontsize", "font.size")
    )

#' parametersEqual
#'
#' Checks if parameters are equal
#' @param recipient The name of the parameter in the function.
#' @param donor The provided name of the parameter.
parametersEqual <- function(recipient, donor)
{
    # Exact match
    if (recipient == donor)
        return(TRUE)
    # Matching after re-ordering full stops
    recipient.split <- sort(strsplit(recipient, ".", fixed = TRUE)[[1]])
    donor.split <- sort(strsplit(donor, ".", fixed = TRUE)[[1]])
    if(length(recipient.split) == length(donor.split))
        return((all(recipient.split == donor.split)))
    return(FALSE)
}

#' getChartFunction
#'
#' @param type The type of chart (i.e., function name mainly)
#' Returns the function that creates the chart
getChartFunction <- function(type)
{

    if (missing(type) | !is.character(type))
        type <- deparse(substitute(type))

    if (!exists(eval(parse(text=type)), mode = "function"))
        loadPackage(type)
    if (!exists(eval(parse(text=type)), mode = "function"))
        stop(paste0("Chart type '", type, " not found."))
    get0(type, mode = "function")
}


#' loadPackage
#'
#' Loads the package in which a chart is located. Only does this if the chart's function is registerd
#' in this function as having a package requiring loading.
#'
#' @param chart.type The name of the function of the chart type to be loaded.
#' @importFrom flipU LookupName
loadPackage <- function(chart.type)
{
    # Make sure that the package is specified in suggest in the descriptoins file
    #
    #              package           chart function
    package.type.lookup <- c("rhtmlMoonPlot" = "moonplot",
                  "rhtmlLabeledScatter" = "LabeledScatter",
                  "flipStandardCharts" = "Chart")
    package <- LookupName(chart.type, package.type.lookup)
    if (!is.null(package))
        require(package, character.only = TRUE)
}
