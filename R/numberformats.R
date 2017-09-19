
#' We do not change out the first parameter, as there is no good name for it. E.g., height in barplot, y in Chart, x in pie.
NumberFormats <- c("Number" = "Number",
    "Percentage (%)" = "%",
    "$" = "$",
    "Custom" = "Custom", # Custom must remain the final non-date format, as it is used as a lookup in other functions.
    "Day" = "%d",
    "Month" = "%m",
    "Year (2 digit)" = "%y",
    "Year (4 digit)" = "%Y",
    "YYYY MM DD" = "%Y %m %d",
    "YYYY MMM DD" = "%Y %b %d",
    "YYYY MMMM DD" = "%Y %B %d",
    "YY MM DD" = "%y %m %d",
    "YY MMM DD" = "%y %b %d",
    "YY MMMM DD" = "%y %B %d",
    "MM DD YYYY " = "%m %d %Y",
    "MMM DD YYYY" = "%b %d %Y",
    "MMMM DD YYYY" = "%B %d %Y",
    "MM DD YY" = "%m %d %y",
    "MMM DD YY" = "%b %d %y" ,
    "MMMM DD YY" = "%B %d %y" ,
    "DD MM YYYY" = "%d %m %Y",
    "DD MMM YYYY" = "%d %b %Y",
    "DD MMMM YYYY" = "%d %B %Y",
    "DD MM YY " = "%d %m %y",
    "DD MMM YY" = "%d %b %y",
    "DD MMMM YY " = "%d %B %y")

isDateFormat <- function(x) {match(x, NumberFormats) > match("Custom", NumberFormats)}
