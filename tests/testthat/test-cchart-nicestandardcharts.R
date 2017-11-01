context("Test 'nice' standard charts via CChart")
# The ones that depend on Distribution, which does not test well, are in the test directory: test-cchart-uglystandardcharts
library(flipStandardCharts)


###############################################################################
########                Instructions for testing                     ##########
###############################################################################
# For each charting function, the units tests need to have have complete coverage
# of all data input arguments. The charting parameters should be covered in
# flipStandarrdCharts.
#
# Please:
# - Use the 'Distribution plots' tests in test-cchart-uglystandardcharts
#   as a tempplate for your testing.
# - Put ALL your example data sets into the Example data section at the top,
#   re-using existing ones where you can and following the naming conventions.
#   Avoid using dput, as it makes the tests hard to read.
# - Where possible, tests should be in test-cchart-nice


##############################################################################
########                        Example data                       ###########
##############################################################################
set.seed(1223)

asBinary <- function(x) as.integer(unclass(x) == 2)
data(colas, package = "flipExampleData")

# Table
Table.Vector = c(a = 1, b = 2, c = 3)
Table.MatrixUnlabeled = matrix(1:10, 5)
Table.MatrixLabeled = matrix(1:10, 5, dimnames = list(LETTERS[1:5], LETTERS[6:7]))
Table.MatrixTimeSeries <- t(apply(matrix(runif(200), nrow = 4), 1, cumsum))
rownames(Table.MatrixTimeSeries) <- c('Aardvark', 'Three toed sloth', 'Camel', 'Dog')
colnames(Table.MatrixTimeSeries) <- as.character(seq(as.Date("1910/1/1"), by = "month", length.out = ncol(Table.MatrixTimeSeries)))
Table.VectorTimeSeries <- Table.MatrixTimeSeries[1, ]

# Tables

# Raw data
RawData.XFactor = list(X = colas$d1)
RawData.XFactor.YFactor = list(X = colas$d1, Y = colas$d2)
RawData.XPickAny = list(X = data.frame(a = asBinary(colas$Q5_5_1),  b = asBinary(colas$Q5_5_2), c = asBinary(colas$Q5_5_3)))
RawData.XPickOneMulti = suppressWarnings(list(X = flipU::Select(colas, "q4a", "q4f")))
RawData.XNumberMulti = suppressWarnings(list(X = flipTransformations::AsNumeric(RawData.XPickOneMulti[[1]], binary = FALSE)))
set.seed(1223)
RawData.XNumberMulti.1 = list("Normal" = rnorm(10) * 1000,
                       "Poisson(Lambda = 1)" = rpois(20, lambda = 1)  * 1000,
                       "Poisson(Lambda = 10) / 10" = rpois(20, lambda = 10) / 10  * 1000,
                       Gamma = rgamma(20, 1)  * 1000,
                       Exponential = rexp(20)  * 1000,
                       Uniform = runif(50)  * 1000                      )


# Pasted
dat <- rbind(c("", LETTERS[1:4]), cbind(letters[1:3], matrix(as.character(1:12), 3, 4)))
Pasted.Vector <- list(dat[, 1:2], TRUE, TRUE, TRUE, TRUE, TRUE)
Pasted.Matrix <- list(dat, TRUE, TRUE, TRUE, TRUE, TRUE)
z <- Table.MatrixTimeSeries
z <- matrix(as.character(z), ncol = ncol(z))
z <- cbind(rownames(Table.MatrixTimeSeries), z)
z <- rbind(c("", colnames(Table.MatrixTimeSeries)), z)
Pasted.MatrixTimeSeries <- list(z, TRUE, FALSE, TRUE, TRUE, TRUE)

# Other
Other.List <- list(Normal = rnorm(1000), "Poisson with unit lamda" = rpois(1000, 1), Exponential = rexp(1000))
Other.ListUnequal <- list(Normal = rnorm(20), "Poisson with unit lamda" = rpois(1000, 1))
Other.Unnamed.Vector <- Other.List[[1]]
Other.data.frame = as.data.frame(Other.List)
Other.Named.Vector <- Table.Vector
Other.Matrix <- Table.MatrixLabeled
Other.MatrixTimeSeries <- Table.MatrixTimeSeries
Other.JSON <- r.output <- list(
                    list("sets"= list(0), "label"= "Like", "size"= 99.9),
                    list("sets"= list(1), "label"= "Love", "size"= 50.1),
                    list("sets"= list(2), "label"= "Dislike", "size"= 100),
                    list("sets"= list(3), "label"= "Hate", "size"= 50),
                    list("sets"= list(0, 1), "size"= 50),
                    list("sets"= list(0, 2), "size"= 0),
                    list("sets"= list(2, 3), "size"= 50))

##############################################################################
########                        Line, Bar, Area, Column            ###########
##############################################################################

test_that("Line, Bar, Area, Column",{

# Table inputs
for (input in list(Table.Vector, Table.MatrixUnlabeled, Table.MatrixLabeled))
    for (chart.type in c("Line", "Bar", "Area", "Column"))
    {
        pd <- suppressWarnings(PrepareData(chart.type, input.data.table = input))
        c = suppressWarnings(CChart(chart.type, pd$data, y.zero = FALSE, y.zero.line.width = 1,  grid.show = FALSE, y.title = "Dog"))
        expect_error(print(c), NA)
}

# Raw data inputs - first aggregate = FALSE
for (input in list( RawData.XFactor,  RawData.XFactor.YFactor, RawData.XPickAny,
                    RawData.XPickOneMulti, RawData.XNumberMulti, RawData.XNumberMulti.1))
    for (chart.type in c("Line", "Bar", "Area", "Column"))
    {
        pd <- suppressWarnings(PrepareData(chart.type, input.data.raw = input, first.aggregate = FALSE))
        c = suppressWarnings(CChart( chart.type, pd$data, y.zero = FALSE, y.zero.line.width = 1,  grid.show = FALSE, y.title = "Dog"))
        suppressWarnings(print(c))
        #expect_error(print(c), NA)
     }

# Raw data inputs - first aggregate = TRUE
for (input in list( RawData.XFactor,  RawData.XFactor.YFactor, RawData.XPickAny, RawData.XPickOneMulti, RawData.XNumberMulti))
    for (chart.type in c("Line", "Bar", "Area", "Column"))
    {
        pd <- suppressWarnings(PrepareData(chart.type, input.data.raw = input, first.aggregate = TRUE))
        c = (CChart( chart.type, pd$data, y.zero = FALSE, y.zero.line.width = 1,  grid.show = FALSE, y.title = "Dog"))
        expect_error(print(c), NA)
    }

# Raw data inputs - first aggregate = TRUE, as.percentages = TRUE
for (input in list( RawData.XFactor,  RawData.XFactor.YFactor, RawData.XPickAny, RawData.XPickOneMulti, RawData.XNumberMulti))
    for (chart.type in c("Line", "Bar", "Area", "Column"))
    {
        pd <- suppressWarnings(PrepareData(chart.type, input.data.raw = input, first.aggregate = TRUE, as.percentages = TRUE))
        pn <- PrepareNumbers(categories.format.list = list(get0("formCategoriesNumberType"), get0("formCategoriesDateType"), get0("formCategoriesNumberCustom"),                                          get0("formCategoriesSeparateThousands"), get0("formCategoriesDecimals")),
                     values.format.list = list("Percentage", get0("formValuesDateType"), get0("formValuesNumberCustom"),                                          get0("formValuesSeparateThousands"),
                                               0),
                     hover.format.list = list(get0("formHoverNumberType"), get0("formHoverDateType"), get0("formHoverNumberCustom"),                                              get0("formHoverSeparateThousands"), get0("formHoverDecimals")),
                     data.labels.format.list = list(get0("formDataLabelsNumberType"), get0("formDataLabelsDateType"),                                                    get0("formDataLabelsCustom"), get0("formDataLabelsSeparateThousands"),                                                    get0("formDataLabelsDecimals")))

        c = (CChart(chart.type, pd$data, y.zero = FALSE, values.tick.format = pn$values.number.format, y.zero.line.width = 1,  grid.show = FALSE, y.title = "Dog"))
        expect_error(print(c), NA)
    }

# Pasted
for (input in list(Pasted.Vector, Pasted.Matrix))
    for (chart.type in c("Line", "Bar", "Area", "Column"))
    {
        pd <- suppressWarnings(PrepareData(chart.type, input.data.pasted = input))
        c = (CChart( chart.type, pd$data, y.zero = FALSE, y.zero.line.width = 1,  grid.show = FALSE, y.title = "Dog"))
        expect_error(print(c), NA)
    }

# Other
for (input in list(Other.Unnamed.Vector, Other.Named.Vector, Other.Matrix))
    for (chart.type in c("Line", "Bar", "Area", "Column"))
    {
        pd <- suppressWarnings(PrepareData(chart.type, input.data.other = input))
        c  = (CChart(chart.type, pd$data, title = "Comparing distributions",
                values.title = "Values",
                global.font.family = "Courier",
                global.font.color = "Red"))
        expect_error(print(c), NA)
    }
})


##############################################################################
########                        Venn                               ###########
##############################################################################


test_that("Venn",
          {
                pd <- suppressWarnings(PrepareData("Venn", input.data.other = Other.JSON))
                CChart("Venn", pd$data, as.percentages = TRUE, data.label.decimals = 2)

                pd <- suppressWarnings(PrepareData("Venn", input.data.raw = RawData.XPickAny))
                CChart("Venn", pd$data, as.percentages = TRUE, data.label.decimals = 2)

                pd <- suppressWarnings(PrepareData("Venn", input.data.raw = RawData.XPickAny))
                CChart("Venn", pd$data, as.percentages = FALSE, data.label.decimals = 2)

                set.seed(1223)
                pd <- suppressWarnings(PrepareData("Venn", input.data.raw = RawData.XPickAny, weights = runif(nrow(RawData.XPickAny[[1]]))))
                expect_equal(names(pd$data), letters[1:3])
                CChart("Venn", pd$data, weights = pd$weights, as.percentages = FALSE, data.label.decimals = 2)
          })


##############################################################################
########                        Streamgraph                        ###########
##############################################################################
#Other.TimeSeries <- Table.MatrixTimeSeries  Table.VectorTimeSeries

#Pasted.MatrixTimeSeries

test_that("Stream",
          {
              pn <- PrepareNumbers(categories.format.list = list("Date/Time", "YYYY (Year, 4 digit)", get0("formCategoriesNumberCustom"),                                          get0("formCategoriesSeparateThousands"), get0("formCategoriesDecimals")),
                                   values.format.list = list("Number", get0("formValuesDateType"), get0("formValuesNumberCustom"),                                          get0("formValuesSeparateThousands"),
                                                             0),
                                   hover.format.list = list(get0("formHoverNumberType"), get0("formHoverDateType"), get0("formHoverNumberCustom"),                                              get0("formHoverSeparateThousands"), get0("formHoverDecimals")),
                                   data.labels.format.list = list(get0("formDataLabelsNumberType"), get0("formDataLabelsDateType"),                                                    get0("formDataLabelsCustom"), get0("formDataLabelsSeparateThousands"),                                                    get0("formDataLabelsDecimals")))


              pd <- PrepareData("Stream", input.data.table = Table.MatrixTimeSeries)
              CChart("Stream", pd$data,  x.tick.interval = 2, x.tick.units = "year", y.tick.format = pn$values.number.format, x.tick.format = pn$categories.number.format)

              pd <- PrepareData("Stream", input.data.table = Table.VectorTimeSeries)
              expect_error(CChart("Stream", pd$data,  x.tick.interval = 2, x.tick.units = "year", y.tick.format = pn$values.number.format, x.tick.format = pn$categories.number.format))

              pd <- PrepareData("Stream", input.data.pasted = Pasted.MatrixTimeSeries)
              CChart("Stream", pd$data,  x.tick.interval = 2, x.tick.units = "year", y.tick.format = pn$values.number.format, x.tick.format = pn$categories.number.format)

              pd <- PrepareData("Stream", input.data.other = Other.MatrixTimeSeries)
              CChart("Stream", pd$data,  x.tick.interval = 2, x.tick.units = "year", y.tick.format = pn$values.number.format, x.tick.format = pn$categories.number.format)

          })



