#' CChart
#'
#' Creates charts
#' @param chart.type The name of the function, used for creating the chart (aka plot).
#' @param x The data to be plotted.
#' @param ... Arguments to the function \code{chart.type}
#' @param warn.if.no.match If TRUE, a warning is shown if any arugments are not matched.
#' @details Where \code{chart.type} is not the name of an existing function, ea c
#' @importFrom methods formalArgs
#' @export

CChart <- function(chart.type, x,  ..., warn.if.no.match = TRUE)
{
    # Getting the function.
    chart.type <- deparse(substitute(chart.type)) # converting to a character
    chart.type <- gsub('"', "", chart.type, fixed = TRUE) # fixing mess created when 'type' is already a character
    loadPackage(chart.type)
    chart.function <- get0(chart.type, mode = "function")
    if (!is.function(chart.function))
        stop(paste0("Cannot find ", chart.type,"."))
    parameters <- formalArgs(chart.function)
    first.parameter.name <- parameters[1]
    arguments <- substituteArgumentNames(parameters, list(...), warn.if.no.match)
    args <- paste0("c(list(", first.parameter.name, " = x), arguments)")
    do.call(chart.type, eval(parse(text = args)))
}

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
