# The ones that don't depend on Distribution are in the testthat directory
library(flipStandardCharts)
library(flipChart)


data(colas, package = "flipExampleData")

###############################################################################
########                Instructions for testing                     ##########
###############################################################################
# For each charting function, the units tests need to have have complete coverage
# of all the paramters. However, a single assertion can be used to test multiple
# paramters.
#
# The data inputs needs should be adde dat the beginning in the Example data
# section, with names that adequately explpain their structure.
#
# Where possible, tests should be in test-cchart-nice



##############################################################################
########                        Example data                       ###########
##############################################################################
asBinary <- function(x) as.integer(unclass(x) == 2)
data(colas, package = "flipExampleData")

RawData.XFactor.YFactor = list(X = colas$d1, Y = colas$d2)
RawData.XPickAny = list(X = data.frame(a = asBinary(colas$Q5_5_1),  b = asBinary(colas$Q5_5_2), c = asBinary(colas$Q5_5_3)))




##############################################################################
########                        Distribution plots                 ###########
##############################################################################

chart.type <- "Histogram"
pd <- PrepareData(chart.type, input.data.raw = RawData.XFactor.YFactor)

#test_that("Violin", {
    set.seed(1223)
    z = list(Normal = rnorm(1000), "Poisson with unit lamda" = rpois(1000, 1), Exponential = rexp(1000))
    Violin(z, title = "Comparing distributions",
           values.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red")
    CChart("Violin", z, title = "Comparing distributions",
           values.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red")
    CChart("Violin", z, title = "Comparing distributions",
           y.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red")
    CChart("Histogram", z, title = "Comparing distributions",
           y.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red")
#})
