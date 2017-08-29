#' CChart
#'
#' Creates charts
#' @param chart.type The name of the function, used for creating the chart (aka plot).
#' @param x The data to be plotted.
#' @param ... Arguments to the function \code{chart.type}
#' @details Where \code{chart.type} is not the name of an existing function, ea c
#' @importFrom methods formalArgs
#' @export

CChart <- function(chart.type, x,  ...)
{
    # Getting the function.
    chart.type <- deparse(substitute(chart.type)) # converting to a character
    chart.type <- gsub('"', "", chart.type, fixed = TRUE) # fixing mess created when 'type' is already a character
    loadPackage(chart.type)
    chart.function <- get0(chart.type, mode = "function")
    parameters <- formalArgs(chart.function)
    first.parameter.name <- parameters[1]
    arguments <- substituteArgumentNames(parameters, list(...))
    args <- paste0("c(list(", first.parameter.name, " = x), arguments)")
    do.call(chart.type, eval(parse(text = args)))
}

substituteArgumentNames <- function(parameter.names, arguments)
{
    a.names <- names(arguments)
    p.names <- parameter.names
    a.unmatched <- a.names[!a.names %in% p.names]
    if (length(a.unmatched) == 0)
        return(arguments)
    p.unmatched <- p.names[!p.names %in% a.names]
    # Substituting exact matches (after substitution and reordering).
    for (a in a.unmatched)
        for(p in p.unmatched)
        {
            if (parametersEqual(p, a) || parametersEqualAfterSubstitution(p, a))
            {
                #cat(p, "<=", a, "\n")
                a.names[match(a, a.names)] <- p

            }
        }

    a.unmatched <- a.names[!a.names %in% p.names]
    # if (length(a.unmatched) > 0)
    # {
    #     p.unmatched <- p.names[!p.names %in% a.names]
    #     # Substituting subordinate matches
    #     for (a in a.unmatched)
    #         for(p in p.unmatched)
    #             if (recipientIsSubordinateOrEqual(p, a) || recipientIsSubordinateOrEqualAfterSubstitution(p, a))
    #                 a.names[match(a, a.names)] <- p
    #     # Checking to see if there are any unmatched arguments
    #     a.unmatched <- a.names[!a.names %in% p.names]
    #     if (length(a.unmatched) > 0)
    #         paste0("The following arguments have been provided but may not be supported: ", paste0(a.unmatched, collapse = ", "))
    # }
    # names(arguments) <- a.names
    #print(arguments)
    arguments
}



#' We do not change out the first parameter, as there is no good name for it. E.g., height in barplot, y in Chart, x in pie.
synonyms <- list(c("col", "cols", "color", "colors", "colour", "colours" ),
      c("title", "main"),
      c("xlab", "x.title", "x.axis.title", "xtitle"),
      c("ylab", "y.title", "y.axis.title", "ytitle"),
      c("data.label.show","data.labels.show", "label.show","labels.show", "labels"),
      c("fontsize", "fontSize", "font.size"),
      c("label", "labels")

    )
s.vector <- unlist(synonyms)
s.lookup <- rep(1:length(synonyms), lapply(synonyms, length))




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


#' parametersEqualAfterSubstitution
#'
#' Checks if parameters are equal after substitution of synonyms
#' @param recipient The name of the parameter in the function.
#' @param donor The provided name of the parameter.
parametersEqualAfterSubstitution <- function(recipient, donor)
{
    i <- s.lookup[match(donor, s.vector)]
    if (!is.na(i))
    {
        for (d in synonyms[[i]])
            if (parametersEqual(recipient, d))
                return(TRUE)
    }
    # Matching after re-ordering full stops

    recipient.split <- strsplit(recipient, ".", fixed = TRUE)[[1]]
    donor.split <- strsplit(donor, ".", fixed = TRUE)[[1]]
    if(length(recipient.split) == length(donor.split))
        return((all(recipient.split == donor.split)))
    return(FALSE)
}



#' getChartFunction
#' @param type The type of chart (i.e., function name mainly)
#' Returns the function that creates the chart
getChartFunction <- function(type)
{

    if (missing(type) | !is.character(type))
        type <- deparse(substitute(type))
    if (!exists(eval(parse(text=type)), mode = "function"))
    {
        #print("dog")
        loadPackage(type)
        #type <- replaceChartSynonyms(type)
    }
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
        requireNamespace(package)
}

#' #' replaceChartSynonyms
#' #'
#' #' @param type The name of the chart type.
#' replaceChartSynonyms <- function(type)
#' {
#'     d <- list(barplot = c("barchart", "bar chart", "bar plot"),
#'               radarChart = c("radar","spider"),
#'               pieChart = c("Pie Chart", "pie chart", "pie plot"),
#'               pie = "pie"
#'               )
#'
#' }


#' #' recipientIsSubordinateOrEqual
#' #'
#' #' All the sub.parts of the donor are in the recipient
#' #' @param recipient The name of the parameter in the function.
#' #' @param donor The provided name of the parameter.
#' recipientIsSubordinateOrEqual <- function(recipient, donor) #, require.equal = FALSE)
#' {
#'     recipient.split <- strsplit(recipient, ".", fixed = TRUE)[[1]]
#'     donor.split <- strsplit(donor, ".", fixed = TRUE)[[1]]
#'     all(donor.split %in% recipient.split)
#' }
#'
#' #' recipientIsSubordinateOrEqualAfterSubstitution
#' #'
#' #' All the sub.parts of the donor are in the recipient
#' #' @param recipient The name of the parameter in the function.
#' #' @param donor The provided name of the parameter.
#' recipientIsSubordinateOrEqualAfterSubstitution <- function(recipient, donor) #, require.equal = FALSE)
#' {
#'     i <- s.lookup[match(recipient, s.vector)]
#'     if (!is.na(i))
#'     {
#'         for (d in synonyms[[i]])
#'             if (parametersEqualAfterSubstitution(recipient, d))
#'                 return(TRUE)
#'     }
#'     return(FALSE)}



     #       return(true)
    #
    #     return(TRUE)
    #
    # if (all(donor.split %in% recipient.split))
    #
    #
    #     (!is.na(length(donot.split) <= recipient.split)
    #
    #     if (!exact.match)
    #
    #
    # reordered.match <- FALSE
    # if ()
    # recipient.split




# Matching after replacing synonms
# All match

#
# term = "ylab"
# synonyms[[s.lookup[match(term, s.vector)]]]
#
#
# getSynonym <- function(term, s.vector, s.lookup)
# {
#
#
# }
#
# match("xlab", parameterSynonyms())
#
#
#
# }
