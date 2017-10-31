# The ones that don't depend on Distribution are in the testthat directory
library(flipStandardCharts)
library(flipChart)


###############################################################################
########                Instructions for testing                     ##########
###############################################################################
# For each charting function, the units tests need to have have complete coverage
# of all data input arguments. The charting parameters should be covered in
# flipStandarrdCharts.
#
# Please:
# - Use the 'Distribution plots' tests below as a tempplate for your testing.
# - Put ALL your example data sets into the Example data section at the top,
#   re-using existing ones where you can and following the naming conventions.
#   Avoid using dput, as it makes the tests hard to read.
# - Where possible, tests should be in test-cchart-nice



##############################################################################
########                        Example data                       ###########
##############################################################################
asBinary <- function(x) as.integer(unclass(x) == 2)
data(colas, package = "flipExampleData")

# Table
Table.Vector = c(a = 1, b = 2, c = 3)
Table.MatrixUnlabeled = matrix(1:10, 5)
Table.MatrixLabeled = matrix(1:10, 5, dimnames = list(LETTERS[1:5], LETTERS[6:7]))

# Tables

# Raw data
RawData.XFactor = list(X = colas$d1)
RawData.XFactor.YFactor = list(X = colas$d1, Y = colas$d2)
RawData.XPickAny = list(X = data.frame(a = asBinary(colas$Q5_5_1),  b = asBinary(colas$Q5_5_2), c = asBinary(colas$Q5_5_3)))
RawData.XPickOneMulti = suppressWarnings(list(X = flipU::Select(colas, "q4a", "q4f")))
RawData.XNumberMulti = suppressWarnings(list(X = flipTransformations::AsNumeric(RawData.XPickOneMulti[[1]], binary = FALSE)))

# Pasted
dat <- rbind(c("", LETTERS[1:4]), cbind(letters[1:3], matrix(as.character(1:12), 3, 4)))
Pasted.Vector <- list(dat[, 1:2], TRUE, TRUE, TRUE, TRUE, TRUE)
Pasted.Matrix <- list(dat, TRUE, TRUE, TRUE, TRUE, TRUE)

# Other
Other.List <- list(Normal = rnorm(1000), "Poisson with unit lamda" = rpois(1000, 1), Exponential = rexp(1000))
Other.ListUnequal <- list(Normal = rnorm(20), "Poisson with unit lamda" = rpois(1000, 1))
Other.Unnamed.Vector <- Other.List[[1]]
Other.data.frame = as.data.frame(Other.List)
Other.Named.Vector <- Table.Vector
Other.Matrix <- Table.MatrixLabeled

##############################################################################
########                        Distribution plots                 ###########
##############################################################################


# Table inputs
for (input in list(Table.Vector, Table.MatrixUnlabeled, Table.MatrixLabeled))
    for (chart.type in c("Bean", "Box", "Density", "Histogram", "Violin"))
    {
        pd <- suppressWarnings(PrepareData(chart.type, input.data.table = input))
        print(CChart(chart.type, pd$data, title = "Comparing distributions",
                values.title = "Values",
                global.font.family = "Courier",
                global.font.color = "Red"))
}

# Raw data inputs
for (input in list( RawData.XFactor,  RawData.XFactor.YFactor, RawData.XPickAny, RawData.XPickOneMulti, RawData.XNumberMulti))
    for (chart.type in c("Bean", "Box", "Density", "Histogram", "Violin"))
    {
        pd <- suppressWarnings(PrepareData(chart.type, input.data.raw = input))
        print(CChart(chart.type, pd$data, title = "Comparing distributions",
                values.title = "Values",
                global.font.family = "Courier",
                global.font.color = "Red"))
}

# Pasted
for (input in list(Pasted.Vector, Pasted.Matrix))
    for (chart.type in c("Bean", "Box", "Density", "Histogram", "Violin"))
    {
        pd <- suppressWarnings(PrepareData(chart.type, input.data.pasted = input))
        print(CChart(chart.type, pd$data, title = "Comparing distributions",
                values.title = "Values",
                global.font.family = "Courier",
                global.font.color = "Red"))
    }

# Other
for (input in list(Other.List, Other.ListUnequal, Other.Unnamed.Vector, Other.data.frame, Other.Named.Vector, Other.Matrix))
    for (chart.type in c("Bean", "Box", "Density", "Histogram", "Violin"))
    {
        pd <- suppressWarnings(PrepareData(chart.type, input.data.other = input))
        print(CChart(chart.type, pd$data, title = "Comparing distributions",
                values.title = "Values",
                global.font.family = "Courier",
                global.font.color = "Red"))
    }


