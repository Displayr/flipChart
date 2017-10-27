context("PrepareData")

QFilter <- structure(TRUE, name = "", label = "Total sample")
QPopulationWeight <- NULL
chart.type <- "Pie"

test_that("JSON list (Bug DS-1608)", {
    JSON = list(
        list("sets" =  list(0), "label" =  "Like", "size" =  100),
        list("sets" =  list(1), "label" =  "Love", "size" =  50),
        list("sets" =  list(2), "label" =  "Dislike", "size" =  100),
        list("sets" =  list(3), "label" =  "Hate", "size" =  50),
        list("sets" =  list(0, 1), "size" =  50),
        list("sets" =  list(0, 2), "size" =  0),
        list("sets" =  list(2, 3), "size" =  50))
     out <- PrepareData("Venn", input.data.other = JSON)
     expect_equal(JSON, out$data)
})

test_that("PrepareData: single table, single stat",
{
    input.data.table <- structure(c(48.3870967741936, 51.6129032258064, 100, 52.6315789473684,
        47.3684210526316, 100, 48.936170212766, 51.063829787234, 100,
        42.3076923076923, 57.6923076923077, 100, 55.3191489361702, 44.6808510638298,
        100, 50, 50, 100, 41.3793103448276, 58.6206896551724, 100, 58.0645161290323,
        41.9354838709677, 100, 50, 50, 100), .Dim = c(3L, 9L), statistic = "Column %", .Dimnames = list(
        c("Male", "Female", "NET"), c("Less than 18 + 18 to 24 + 25 to 29",
        "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
        "55 to 64", "65 or more", "NET")), name = "Q1 by Q2", questions = c("Q1", "Q2"))

    out <- PrepareData("Area", NULL, NULL,
                       get0("input.data.table"),
                       get0("input.data.tables"),
                       NULL, NULL, #input.data.raw = #as.data.frame(Filter(Negate(is.null), list(get0("formX"), get0("formY")))),
                      # input.data.pasted = list(get0("formPastedData"), get0("formPastedRawData"), get0("formPastedFactor"),
                     #                get0("formPastedColumnNames"), get0("formPastedRowNames"), get0("formPastedDateConvention")),
                       get0("input.data.other"),
                       transpose = get0("transpose"),
                       missing = "Exclude cases with missing data", row.names.to.remove = NULL,
                       column.names.to.remove = NULL)
    expect_equal(attr(out$data, "statistic"), attr(input.data.table, "statistic"))
    expect_is(out$data,  "matrix")
    expect_equal(dim(out$data), dim(input.data.table) )
})

test_that("PrepareData: single table, single stat",
{
    input.data.table <- structure(c(100, 22.2222222222222, 100, 0, 77.7777777777778,
        100, 84.375, 15.625, 100, 3.125, 78.125, 100, 90, 16.6666666666667,
        100, 0, 76.6666666666667, 100, 96.2962962962963, 11.1111111111111,
        100, 0, 85.1851851851852, 100, 94.2028985507246, 14.4927536231884,
        100, 1.44927536231884, 72.463768115942, 100, 90, 17.5, 100, 0,
        80, 100, 91.304347826087, 17.3913043478261, 100, 0, 78.2608695652174,
        100, 89.4736842105263, 5.26315789473684, 100, 0, 78.9473684210526,
        100, 92.8571428571429, 21.4285714285714, 100, 0, 78.5714285714286,
        100, 92.0689655172414, 14.8275862068966, 100, 0.689655172413793,
        78.2758620689655, 100, 9, 9, 9, 9, 9, 9, 32, 32, 32, 32, 32,
        32, 30, 30, 30, 30, 30, 30, 54, 54, 54, 54, 54, 54, 69, 69, 69,
        69, 69, 69, 40, 40, 40, 40, 40, 40, 23, 23, 23, 23, 23, 23, 19,
        19, 19, 19, 19, 19, 14, 14, 14, 14, 14, 14, 290, 290, 290, 290,
        290, 290, 267, 43, 290, 2, 227, 290, 267, 43, 290, 2, 227, 290,
        267, 43, 290, 2, 227, 290, 267, 43, 290, 2, 227, 290, 267, 43,
        290, 2, 227, 290, 267, 43, 290, 2, 227, 290, 267, 43, 290, 2,
        227, 290, 267, 43, 290, 2, 227, 290, 267, 43, 290, 2, 227, 290,
        267, 43, 290, 2, 227, 290), .Dim = c(3L, 2L, 10L, 3L), .Dimnames = list(
        c("Coke", "Diet Coke", "NET"), c("Traditional", "Weight-conscious"
        ), c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
        "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
        "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
        "NET"), c("Column %", "Column Sample Size", "Row Sample Size"
        )), name = "PickAnyGrid by Income", questions = c("PickAnyGrid",
                                                          "Income [Colas edited]"))
    QFilter <- structure(TRUE, name = "", label = "Total sample")
    QPopulationWeight <- NULL
    chart.type <- "Scatter"

    expect_error(PrepareData(chart.type, QFilter, QPopulationWeight,
                               get0("input.data.table"),
                               get0("input.data.tables"),
                               input.data.pasted = list(get0("formPastedData"), get0("formPastedRawData"), get0("formPastedFactor"),
                                     get0("formPastedColumnNames"), get0("formPastedRowNames"), get0("formPastedDateConvention")),
                               input.data.raw = as.data.frame(Filter(Negate(is.null), list(get0("formX"), get0("formY")))),
                               input.data.other = get0("input.data.other"),
                                transpose = get0("transpose"),
                                missing = "Exclude cases with missing data"), "There are 3 data inputs. One and only one data argument may be supplied.")
    out <- suppressWarnings(PrepareData(chart.type, QFilter, QPopulationWeight, get0("input.data.table")))
    expect_warning(PrepareData(chart.type, QFilter, QPopulationWeight, get0("input.data.table")),
                   "^Multiple statistics detected")
    out <- suppressWarnings(PrepareData(chart.type, QFilter, QPopulationWeight,
                               get0("input.data.table"),
                               get0("input.data.tables"),
                               input.data.pasted = NULL,
                               input.data.raw = NULL,
                               input.data.other = get0("input.data.other"),
                                transpose = get0("transpose"),
                                missing = "Exclude cases with missing data"))





    dims <- dim(input.data.table)
    n.dim <- length(dims)
    expect_equal(attr(out$data, "statistic"), dimnames(input.data.table)[[n.dim]][1])
    expect_is(out$data, "matrix")
    expect_equal(dim(out$data), c(dims[1]*dims[3], dims[2]))
})

test_that("PrepareData: multiple existing tables",
{
    input.data.tables <- list(structure(c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12), .Names = c("Blueberry",
                      "Cherry", "Apple", "Boston Cream", "Other", "Vanilla Cream")),
                        structure(c(42.625, 11.125, 17.875, 9, 2.5, 14.875, 0.75,
                        1.25, 100), .Dim = 9L, statistic = "%", .Dimnames = list(
                            c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
                              "Pepsi Max", "Dislike all cola", "Don't care", "NET")), name = "Preferred cola",
                        questions = c("Preferred cola",
                                      "SUMMARY")))
    out <- PrepareData(input.data.table = NULL, input.data.raw = NULL, input.data.tables = input.data.tables, input.data.other = NULL,
                       chart.type = "Scatter")
    expect_length(out$data, 2)
    expect_equal(attr(out$data[[2]], "statistic"), "%")
})

test_that("PredpareData: pasted raw data",
{
    ## list(get0("formPastedData"), get0("formPastedRawData"), get0("formPastedFactor"), get0("formPastedColumnNames"),
    ##      get0("formPastedRowNames"), get0("formPastedDateConvention"))
    dat <- rbind(c("", LETTERS[1:4]), cbind(letters[1:3], matrix(as.character(1:12), 3, 4)))
    pasted <- list(dat, TRUE, TRUE, TRUE, TRUE, TRUE)
    out <- PrepareData(input.data.table = NULL, input.data.raw = NULL, input.data.tables = NULL, input.data.other = NULL,
                       input.data.pasted = pasted, chart.type = "Column")
    expect_is(out$data, "matrix")
    expect_equal(colnames(out$data), LETTERS[1:4])
})

test_that("PrepareData: crappy input to crappy data",
{
    ## list(get0("formPastedData"), get0("formPastedRawData"), get0("formPastedFactor"), get0("formPastedColumnNames"),
    ##      get0("formPastedRowNames"), get0("formPastedDateConvention"))
    dat <- rbind(c("", LETTERS[1:4]), cbind(letters[1:3], matrix(as.character(1:12), 3, 4)))
    dat[-1, 3] <- c("dog", "cat", "dog")
    pasted <- list(dat, TRUE, FALSE, TRUE, TRUE, TRUE)
    suppressWarnings(PrepareData(input.data.pasted = pasted, chart.type = "Bar"))


})

test_that("PrepareData: pasted, non-raw data",
{
    dat <- structure(c("", "a", "v", "c", "d", "col 1", "2", "3", "1", "2",
                      "col 2", "3", "2", "1", "1", "col 3", "3", "2", "1", "1", "col 4",
                      "3", "2", "1", "1", "col 5", "3", "2", "1", "1", "col 6", "3",
                      "2", "1", "1", "col 7", "3", "2", "1", "1", "col 8", "3", "2",
                      "1", "1"), .Dim = c(5L, 9L))
    pasted <- list(dat, FALSE, NULL, NULL, NULL, NULL)
    QFilter <- structure(TRUE, name = "", label = "Total sample")
    QPopulationWeight <- NULL
    chart.type <- "Scatter"
    out <- PrepareData(input.data.pasted = pasted, chart.type = chart.type, subset = QFilter,
                       weights = QPopulationWeight)
    expect_is(out$data, "matrix")
    expect_equal(dim(out$data), dim(dat) - c(1, 1))
})

test_that("PrepareData: Binary variable for Venn",
{
    input.data.raw <- structure(list(`Coca-Cola` = c(0, 1, 0, 0, 1, 1, 1, 0, 1, 1,
    0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1,
    1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
    0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
    1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1,
    1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1,
    1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0,
    1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1,
    0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1,
    1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
    1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0,
    1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1,
    0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
    1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1,
    1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0,
    0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0,
    1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1,
    1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1,
    0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0,
    1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1,
    1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1,
    1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1,
    1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1,
    1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1,
    0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
    1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1,
    1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1,
    0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0), `Diet Coke` = c(1, 0,
    1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1,
    0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0,
    0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1,
    0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0,
    0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1,
    0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0,
    1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1,
    1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0,
    1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0,
    0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0,
    0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,
    1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0,
    0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1,
    0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0,
    1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0,
    1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1,
    0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1,
    0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1,
    1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0,
    1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0,
    1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0,
    0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1,
    1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0,
    1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0,
    0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0,
    0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1,
    0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0,
    0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
    1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0,
    0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0,
    0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0
    ), `Coke Zero` = c(0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0,
    0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0,
    1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0,
    1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1,
    0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0,
    1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0,
    0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0,
    0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0,
    0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1,
    1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1,
    1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1,
    0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0,
    0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0,
    0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1,
    0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0,
    0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1,
    0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1,
    1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0,
    1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1,
    0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0,
    0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1,
    0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0,
    1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1,
    0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1,
    0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0,
    0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1,
    0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0,
    0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1,
    0, 0, 1, 1, 1, 0, 0, 0, 0), `Pepsi + Pepsi Max + Diet Pepsi` = c(1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0,
    1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0,
    1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1,
    1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
    0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0,
    1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1,
    0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0,
    0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0,
    0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1,
    1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1,
    0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1,
    0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1,
    1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1,
    1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1,
    1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1,
    0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1,
    1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0,
    1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0,
    1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1,
    1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0,
    0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1,
    0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0,
    1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0,
    1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
    0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0,
    0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0,
    1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0,
    1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1,
    0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1,
    1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1,
    1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1,
    1)), class = "data.frame", .Names = c("Coca-Cola", "Diet Coke",
                                          "Coke Zero", "Pepsi + Pepsi Max + Diet Pepsi"),
    row.names = c(NA,
                  800L), questiontype = "PickAny", question = "Brand attitude: Love + Like")

    QFilter <- rbinom(nrow(input.data.raw), 1, .25)
    n.filter <- sum(QFilter ==  1)
    out <- PrepareData(input.data.raw = input.data.raw, chart.type = "Venn")
    expect_is(out$data, "data.frame")
    expect_named(out$data, names(input.data.raw))
    expect_is(out$data[[2]], "numeric")

    out <- PrepareData(input.data.raw = input.data.raw, chart.type = "Venn", subset = QFilter)
    expect_is(out$data, "data.frame")
    expect_named(out$data, names(input.data.raw))
    expect_is(out$data[[2]], "numeric")
    expect_equal(nrow(out$data), n.filter)
})

test_that("PrepareData works with aggregation",
{

})

test_that("PrepareData works with pasted vector",
{
    dat <- cbind(letters[1:5], 1:5)
    pasted <- list(dat, FALSE, NULL, NULL, NULL, NULL)
    QFilter <- structure(TRUE, name = "", label = "Total sample")
    QPopulationWeight <- NULL
    chart.type <- "Scatter Plot"
    out <- PrepareData(input.data.pasted = pasted, chart.type = chart.type, subset = QFilter,
                       weights = QPopulationWeight)
    expect_is(out$data, "numeric")
    expect_null(dim(out$data))
    expect_named(out$data, dat[, 1])
})

dat <- structure(list(structure.c.2L..1L..2L..2L..2L..1L..1L..1L..2L..2L..2L..1L..2L.. = structure(c(2L,
    1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L,
    1L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L,
    2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L,
    2L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 1L,
    1L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L,
    2L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 1L,
    2L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L,
    1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L,
    1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L,
    2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 2L,
    1L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 1L,
    1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 1L,
    1L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L,
    2L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L,
    1L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L,
    2L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L,
    2L, 1L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L,
    1L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L,
    2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L,
    2L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L,
    2L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 1L,
    2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L,
    2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L,
    2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 1L,
    2L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 2L,
    1L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L,
    2L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 2L,
    2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 2L,
    2L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L,
    1L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L,
    1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 2L,
    1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 1L,
    1L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 1L,
    1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L,
    1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 2L,
    1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 2L,
    1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L,
    1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L,
    2L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 1L,
    1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 1L,
    2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L,
    2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 1L,
    2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 2L,
    1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L,
    2L, 2L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 1L,
    1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L,
    1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
    1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 2L,
    1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 2L,
    2L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L), class = "factor", .Label = c("Male",
    "Female"), questiontype = "PickOne", name = "d3", label = "Gender", question = "Gender"),
    structure.c.7L..4L..7L..2L..NA..3L..7L..5L..5L..7L..2L..5L..2L.. = structure(c(7L,
    4L, 7L, 2L, NA, 3L, 7L, 5L, 5L, 7L, 2L, 5L, 2L, 6L, 4L, 4L,
    4L, 2L, 8L, 6L, 5L, 5L, 4L, 3L, NA, 2L, 6L, 5L, 9L, NA, 6L,
    NA, 5L, 4L, 7L, NA, 4L, NA, 4L, 3L, 7L, 6L, 1L, 4L, 1L, 4L,
    2L, 5L, NA, NA, 4L, 2L, 6L, 4L, 7L, 8L, 9L, 4L, 5L, 6L, 5L,
    6L, 2L, 3L, 2L, 3L, 2L, 5L, NA, 4L, 4L, 2L, 3L, 9L, 5L, 7L,
    8L, 8L, 9L, 5L, 2L, 7L, 5L, 1L, 3L, 3L, 8L, 4L, 5L, 2L, 1L,
    6L, 4L, 6L, 5L, 2L, 3L, 2L, 6L, 9L, 7L, NA, NA, 2L, 8L, 2L,
    9L, 3L, 2L, 8L, 2L, 4L, 8L, 6L, 6L, 6L, 3L, 3L, 5L, 1L, 4L,
    8L, 6L, 2L, 9L, 7L, NA, 3L, 6L, NA, 4L, 6L, NA, 4L, NA, 2L,
    3L, 5L, 6L, 5L, 5L, 6L, 5L, 4L, 1L, 6L, NA, NA, 7L, NA, 5L,
    3L, 6L, 8L, 5L, 5L, 8L, NA, 4L, 5L, 2L, 3L, 3L, 4L, 6L, 5L,
    NA, 4L, 6L, 7L, 6L, 5L, 4L, 6L, 3L, 5L, 4L, 4L, 8L, 2L, 5L,
    5L, 5L, 4L, NA, 6L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L,
    7L, 4L, 4L, 3L, 7L, 4L, NA, 7L, 3L, 7L, 5L, 4L, 4L, 7L, 5L,
    2L, 7L, 6L, 5L, 4L, 4L, 2L, 8L, 6L, 5L, 3L, 4L, 5L, 6L, NA,
    6L, NA, 5L, 4L, NA, NA, 4L, NA, 4L, 4L, 2L, 3L, 5L, 3L, 4L,
    6L, 4L, 4L, 8L, 2L, 5L, NA, 5L, 4L, NA, 5L, 2L, 4L, 8L, 9L,
    4L, 6L, NA, 6L, 9L, 3L, NA, 5L, NA, NA, 7L, 4L, 6L, 4L, 4L,
    NA, 2L, 5L, 5L, 8L, 8L, 2L, 9L, 3L, 2L, 5L, 5L, 2L, 7L, 5L,
    1L, 5L, 3L, 9L, 4L, 5L, 2L, 7L, 6L, 6L, 5L, 4L, 2L, 2L, 5L,
    6L, 9L, NA, NA, 3L, NA, 6L, NA, 2L, 2L, 9L, 8L, 6L, 2L, 9L,
    4L, 8L, 6L, 6L, 5L, 3L, 5L, 3L, 9L, 5L, 1L, 8L, 5L, 9L, 7L,
    9L, NA, 3L, 6L, NA, 4L, 6L, NA, 8L, 7L, 4L, 5L, 5L, 2L, 1L,
    5L, 5L, 6L, 1L, 6L, NA, NA, 7L, 5L, 3L, 6L, 9L, 5L, 8L, 4L,
    9L, 3L, 2L, 5L, 5L, 5L, NA, 6L, 7L, 4L, 5L, 5L, NA, 3L, 4L,
    8L, 5L, 2L, 5L, 5L, 4L, 4L, 4L, 5L, 7L, 8L, 5L, 6L, 4L, 6L,
    6L, 6L, 6L, 7L, 4L, 4L, 5L, 4L, 3L, 7L, 4L, NA, 2L, 3L, NA,
    3L, 7L, 5L, 5L, 4L, 7L, 2L, 5L, 2L, 7L, 4L, 5L, 4L, 8L, 3L,
    4L, 2L, 5L, 2L, 5L, 9L, 6L, 5L, 5L, NA, 7L, NA, NA, NA, 4L,
    5L, NA, 4L, 7L, 4L, 1L, 4L, 8L, 1L, 2L, 5L, NA, 5L, 5L, 5L,
    NA, 2L, 4L, 7L, 5L, 6L, 5L, 3L, 9L, 3L, 5L, 5L, NA, NA, 7L,
    4L, 4L, 3L, 9L, 5L, 8L, 9L, 3L, 2L, 5L, 2L, NA, 8L, 7L, 5L,
    1L, 5L, 3L, 3L, 4L, 8L, 9L, 1L, 2L, 7L, 1L, 6L, 4L, 6L, 2L,
    5L, 6L, 9L, NA, 6L, 7L, 3L, 3L, 6L, NA, 2L, 8L, 9L, 3L, 2L,
    6L, 9L, 6L, NA, 6L, 5L, 3L, 4L, 3L, NA, 5L, 5L, 1L, 4L, 5L,
    8L, 9L, 7L, 9L, NA, 7L, 3L, NA, 6L, 5L, NA, 2L, 3L, NA, 4L,
    5L, 5L, 6L, 1L, 5L, 4L, 1L, 6L, 8L, NA, NA, 7L, NA, 5L, 3L,
    6L, 5L, 5L, 5L, 8L, 4L, 4L, 9L, 5L, 3L, 4L, 5L, 5L, 4L, 7L,
    5L, 4L, 6L, 3L, 5L, 5L, 3L, 4L, 3L, 4L, 5L, 5L, 4L, NA, 4L,
    7L, 8L, 5L, 6L, 6L, 3L, 6L, 5L, 6L, 7L, 4L, 4L, 5L, 3L, NA,
    7L, 2L, NA, 3L, 7L, 7L, 7L, 5L, 4L, 7L, 5L, 7L, 4L, 5L, 4L,
    8L, 6L, 5L, 5L, 5L, 3L, NA, 5L, 6L, 9L, NA, 6L, 5L, 5L, NA,
    4L, 4L, NA, 3L, NA, 4L, 6L, 4L, 5L, 5L, NA, 5L, NA, 5L, 4L,
    6L, 4L, 8L, 4L, 5L, NA, 6L, 2L, 9L, 2L, 2L, NA, 7L, 4L, 4L,
    NA, 2L, 9L, 7L, 8L, 8L, 2L, 9L, 3L, 5L, 5L, 5L, 2L, NA, 8L,
    7L, 5L, 1L, 4L, 9L, 4L, 7L, 1L, 5L, 2L, 7L, 1L, 6L, 4L, 2L,
    3L, 4L, 2L, 5L, 6L, NA, NA, 6L, 3L, 6L, 2L, 2L, 9L, 3L, 2L,
    6L, 2L, 4L, NA, 8L, 6L, 6L, NA, 3L, 3L, 4L, 9L, 5L, 4L, 5L,
    6L, 8L, 2L, 9L, 7L, 9L, NA, 7L, 3L, 6L, NA, 4L, 6L, NA, 8L,
    4L, NA, 2L, 3L, NA, 6L, 5L, 6L, 2L, 1L, 4L, 1L, 8L, NA, 3L,
    8L, 5L, 9L, 5L, NA, 9L, 2L, 5L, 2L, 5L, 6L, 5L, 5L, 4L, 6L,
    2L, 6L, 6L, 3L, 5L, 5L, 3L, 4L, 4L, 5L, 4L, 5L, 2L, 5L, 5L,
    4L, 4L, 5L, 5L, 6L, 4L, 6L, 6L, 6L, 2L, 3L, 5L, 6L, 7L, 4L,
    4L, 4L, 4L, 3L), class = "factor", .Label = c("Less than $15,000",
    "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
    "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
    "$150,001 to $200,000", "$200,001 or more"), questiontype = "PickOne",
    name = "d2", label = "Income", question = "Income")),
    .Names = c("structure.c.2L..1L..2L..2L..2L..1L..1L..1L..2L..2L..2L..1L..2L..",
               "structure.c.7L..4L..7L..2L..NA..3L..7L..5L..5L..7L..2L..5L..2L.."),
    row.names = c(NA, -800L), class = "data.frame")

test_that("PrepareData: input.data.raw with missing vals",
{
    out <- suppressWarnings(PrepareData(input.data.raw = dat, chart.type = "Area"))
    expect_is(out$data, "matrix")
    num.na <- sum(rowSums(is.na(dat)) > 0)
    expect_equal(dim(out$data), dim(dat) - c(num.na, 0))
    expect_error(PrepareData(input.data.raw = dat, chart.type = "Bar",
                             missing = "Error if missing data"),
                 "^The data contains missing values.")
})

test_that("PrepareData: input.data.raw subset and weights",
{
    QPopulationWeight <- prop.table(runif(nrow(dat)))
    QFilter <- rbinom(nrow(dat), 1, .5)
    n.filter <- sum(QFilter ==  1L)
    out <- PrepareData(input.data.raw = dat, subset = QFilter, chart.type = "Scatter Plot",
                       missing = "Use partial data")
    expect_equal(nrow(out$data), n.filter)
    expect_is(out$data, "data.frame")

    out <- PrepareData(input.data.raw = dat, subset = QFilter, chart.type = "Scatter Plot",
                       missing = "Use partial data", weights = QPopulationWeight)
    expect_equal(nrow(out$data), n.filter)
    expect_is(out$data, "data.frame")
    expect_equal(attr(out$data, "weights"), QPopulationWeight[QFilter ==  1])
})

test_that("PrepareData uses Labels",
{
    dat <- structure(list(Q6_A = structure(c(3L, 5L, 5L, 6L, 4L, 1L, 3L,
    6L, 5L, 6L, 6L, 5L, 5L, 4L, 3L, 6L, 6L, 5L, 5L, 4L), .Label = c("Don t Know",
    "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
    ), class = "factor", label = structure("Q6. Coca Cola", .Names = "Q6_A")),
        Q6_B = structure(c(5L, 2L, 6L, 3L, 6L, 1L, 4L, 3L, 5L, 6L,
        2L, 3L, 3L, 3L, 6L, 5L, 5L, 3L, 3L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Diet Coke", .Names = "Q6_B")),
        Q6_C = structure(c(3L, 5L, 3L, 3L, 4L, 1L, 5L, 5L, 1L, 6L,
        2L, 3L, 3L, 5L, 3L, 5L, 5L, 3L, 5L, 6L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Coke Zero", .Names = "Q6_C")),
        Q6_D = structure(c(4L, 5L, 4L, 3L, 4L, 1L, 3L, 4L, 5L, 5L,
        6L, 5L, 4L, 4L, 5L, 5L, 3L, 5L, 4L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Pepsi", .Names = "Q6_D")),
        Q6_E = structure(c(2L, 4L, 2L, 3L, 6L, 6L, 3L, 3L, 5L, 5L,
        2L, 3L, 3L, 4L, 6L, 1L, 2L, 3L, 4L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Pepsi Light", .Names = "Q6_E")),
        Q6_F = structure(c(6L, 6L, 2L, 3L, 3L, 6L, 3L, 5L, 4L, 4L,
        2L, 3L, 3L, 5L, 3L, 1L, 2L, 3L, 4L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Pepsi Max", .Names = "Q6_F"))), .Names = c("Q6_A",
    "Q6_B", "Q6_C", "Q6_D", "Q6_E", "Q6_F"), row.names = c(NA, 20L
                                                           ), class = "data.frame")
    expect_warning(out <-PrepareData(input.data.raw = dat, chart.type = "Bubble"),
                   "^Some categories do not appear")
    expect_is(out$data, "data.frame")
    expect_equal(names(out$data), flipFormat::Labels(dat), check.attributes = FALSE)
})

test_that("PrepareData: No data has been provided",
{
    expect_error(PrepareData(chart.type = "Bar"),
                 "No data has been provided.")
})

test_that("PrepareData: input and output format of raw data",
{
    set.seed(1234)
    xx <- rpois(100, 4)
    yy <- rpois(100, 2)
    attr(xx, "label") <- "VarA"
    attr(yy, "label") <- "VarB"

    res1 <- PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = FALSE)
    expect_equal(res1$y.title, NULL)
    res1 <- PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = TRUE)
    expect_equal(res1$y.title, "Count")
    expect_equal(names(dimnames(res1$data)), c("VarA", ""))

    res2 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = FALSE)
    expect_equal(res2$y.title, NULL)
    res2 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = TRUE)
    expect_equal(res2$y.title, "Counts")
    res2 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = TRUE, as.percentages = TRUE)
    expect_equal(res2$y.title, "%")
    expect_equal(names(dimnames(res2$data)), c("VarA", "VarB"))

    res3 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = FALSE,
                        as.percentages = TRUE, transpose = TRUE)
    expect_equal(res3$y.title, "%")
    expect_equal(rownames(res3$data), c("VarA", "VarB"))

    res3 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = FALSE,
                        as.percentages = TRUE, transpose = FALSE)
    expect_equal(res3$y.title, "%")
    expect_equal(colnames(res3$data), c("VarA", "VarB"))

    res3 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = TRUE,
                        as.percentages = TRUE, transpose = TRUE)
    expect_equal(res3$y.title, "%")
    expect_equal(rownames(res3$data), as.character(0:7))


    res4 <- PrepareData("Scatter", input.data.raw = list(X = NULL, Y = xx))
    expect_equal(ncol(res4$data), 1)
    expect_equal(res4$y.title, NULL)
    expect_true(is.na(res4$scatter.variable.indices["x"]))
    expect_equivalent(res4$scatter.variable.indices["y"], 1)
    expect_true(is.na(res4$scatter.variable.indices["sizes"]))
    expect_true(is.na(res4$scatter.variable.indices["colors"]))

    res5 <- PrepareData("Scatter", input.data.raw = list(X = xx, Y = yy, Z = NULL, Z2 = yy))
    expect_equal(colnames(res5$data), c("VarA", "VarB"))
    expect_equal(res5$y.title, NULL)
    expect_equivalent(res5$scatter.variable.indices["x"], 1)
    expect_equivalent(res5$scatter.variable.indices["y"], 2)
    expect_true(is.na(res5$scatter.variable.indices["sizes"]))
    expect_equivalent(res5$scatter.variable.indices["colors"], 3)

    res <- suppressWarnings(PrepareData("Column", input.data.raw = list(X = factor(1:5), Y = factor(1:5), Z = factor(1:5)),
                       as.percentages = TRUE, transpose = FALSE))
    expect_equal(res$y.title, "%")
    expect_equal(colnames(res$data), c("X", "Y","Z"))
})


# context("Distribution")
#
# TrialOpens = structure(c("UK/Europe", "Asia", "US/Canada", "UK/Europe", "UK/Europe",
# "US/Canada", "Australia/NZ", "US/Canada", "Australia/NZ", "Australia/NZ",
# "US/Canada", "US/Canada", "US/Canada", "Asia", "UK/Europe", "Australia/NZ",
# "Australia/NZ", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
# "Asia", "UK/Europe", "US/Canada", "Australia/NZ", "Australia/NZ",
# "US/Canada", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
# "US/Canada", "Asia", "US/Canada", "US/Canada", "UK/Europe", "US/Canada",
# "UK/Europe", "UK/Europe", "Africa/ME", "US/Canada", "US/Canada",
# "US/Canada", "US/Canada", "US/Canada", "US/Canada", "US/Canada",
# "US/Canada", "US/Canada", "US/Canada", "US/Canada", "US/Canada",
# "US/Canada", "US/Canada", "US/Canada", "US/Canada", "US/Canada",
# "UK/Europe", "Australia/NZ", "UK/Europe", "US/Canada", "US/Canada",
# "US/Canada", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
# "UK/Europe", "UK/Europe", "US/Canada", "Australia/NZ", "US/Canada",
# "US/Canada", "UK/Europe", "UK/Europe", "UK/Europe", "US/Canada",
# "US/Canada", "Asia", "US/Canada", "UK/Europe", "UK/Europe", "US/Canada",
# "Australia/NZ", "UK/Europe", "US/Canada", "US/Canada", "Australia/NZ",
# "Africa/ME", "Australia/NZ", "Australia/NZ", "Australia/NZ",
# "US/Canada", "Australia/NZ", "UK/Europe", "US/Canada", "US/Canada",
# "Australia/NZ", "US/Canada", "US/Canada", "US/Canada", "US/Canada",
# "Australia/NZ", "UK/Europe", "Australia/NZ", "UK/Europe", "UK/Europe",
# "UK/Europe", "UK/Europe", "US/Canada", "US/Canada", "US/Canada",
# "Asia", "Africa/ME", "Africa/ME", "UK/Europe", "US/Canada", "US/Canada",
# "Australia/NZ", "US/Canada", "Australia/NZ", "US/Canada", "UK/Europe",
# "UK/Europe", "UK/Europe", "US/Canada", "US/Canada", "Missing data",
# "US/Canada", "US/Canada", "Australia/NZ", "Australia/NZ", "UK/Europe",
# "Asia", "Asia", "Asia", "Asia", "Australia/NZ", "UK/Europe",
# "US/Canada", "US/Canada", "Australia/NZ", "Australia/NZ", "Asia",
# "Africa/ME", "US/Canada", "US/Canada", "Asia", "UK/Europe", "UK/Europe",
# "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
# "Africa/ME", "US/Canada", "US/Canada", "US/Canada", "US/Canada",
# "US/Canada", "US/Canada", "US/Canada", "UK/Europe", "Asia", "UK/Europe",
# "UK/Europe", "UK/Europe", "Australia/NZ", "US/Canada", "US/Canada",
# "US/Canada", "US/Canada", "US/Canada", "UK/Europe", "US/Canada",
# "US/Canada", "US/Canada", "US/Canada", "US/Canada", "UK/Europe",
# "UK/Europe", "UK/Europe", "US/Canada", "Asia", "Asia", "Asia",
# "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia",
# "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia",
# "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia",
# "Africa/ME", "US/Canada", "US/Canada", "US/Canada", "Australia/NZ",
# "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
# "UK/Europe", "UK/Europe", "US/Canada", "UK/Europe", "UK/Europe",
# "UK/Europe", "UK/Europe", "Australia/NZ", "UK/Europe", "Asia",
# "US/Canada", "UK/Europe", "US/Canada", "US/Canada", "US/Canada",
# "UK/Europe", "US/Canada", "US/Canada", "UK/Europe", "Australia/NZ",
# "Asia", "UK/Europe", "US/Canada", "UK/Europe", "UK/Europe", "UK/Europe",
# "UK/Europe", "US/Canada", "US/Canada", "US/Canada", "UK/Europe",
# "US/Canada", "US/Canada", "UK/Europe", "US/Canada", "US/Canada",
# "US/Canada", "UK/Europe", "US/Canada", "US/Canada", "Australia/NZ",
# "Asia", "US/Canada", "US/Canada", "US/Canada", "UK/Europe", "Asia",
# "Asia", "US/Canada", "Australia/NZ", "UK/Europe", "Australia/NZ",
# "Asia", "Australia/NZ", "US/Canada", "Asia", "Asia", "US/Canada",
# "US/Canada", "Asia", "US/Canada", "UK/Europe", "UK/Europe", "US/Canada",
# "Australia/NZ", "Australia/NZ", "Asia", "UK/Europe", "US/Canada",
# "US/Canada", "Australia/NZ", "Australia/NZ", "UK/Europe", "UK/Europe",
# "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
# "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
# "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "Asia", "Australia/NZ",
# "UK/Europe", "US/Canada", "US/Canada", "US/Canada", "Australia/NZ",
# "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "Australia/NZ",
# "Australia/NZ", "Australia/NZ", "US/Canada", "US/Canada", "US/Canada",
# "US/Canada", "US/Canada", "Australia/NZ", "Australia/NZ", "US/Canada",
# "Asia", "Australia/NZ", "UK/Europe", "US/Canada", "UK/Europe",
# "Australia/NZ", "Australia/NZ", "Asia", "Asia", "Asia", "Asia",
# "UK/Europe", "Australia/NZ", "Australia/NZ", "UK/Europe", "US/Canada",
# "US/Canada", "US/Canada", "US/Canada", "UK/Europe", "US/Canada",
# "US/Canada", "UK/Europe", "US/Canada", "Africa/ME", "Africa/ME",
# "UK/Europe", "UK/Europe", "UK/Europe", "US/Canada", "US/Canada",
# "US/Canada", "US/Canada", "US/Canada", "UK/Europe", "US/Canada",
# "US/Canada", "US/Canada", "US/Canada", "Australia/NZ", "UK/Europe",
# "US/Canada", "US/Canada", "US/Canada", "Australia/NZ", "US/Canada",
# "US/Canada", "US/Canada", "UK/Europe", "UK/Europe", "US/Canada",
# "US/Canada", "UK/Europe", "Australia/NZ", "US/Canada", "US/Canada",
# "Australia/NZ", "Australia/NZ", "Australia/NZ", "Australia/NZ",
# "UK/Europe", "UK/Europe", "US/Canada", "UK/Europe", "US/Canada",
# "US/Canada", "US/Canada", "US/Canada", "US/Canada", "US/Canada",
# "US/Canada", "UK/Europe", "Asia", "Australia/NZ", "UK/Europe",
# "Missing data", "US/Canada", "Asia", "Australia/NZ", "Australia/NZ",
# "UK/Europe", "Asia", "Missing data", "Australia/NZ", "Asia",
# "Australia/NZ", "US/Canada", "US/Canada", "Australia/NZ", "Asia",
# "Asia", "UK/Europe", "Asia", "US/Canada", "UK/Europe", "Australia/NZ",
# "Asia", "UK/Europe", "UK/Europe", "Australia/NZ", "Australia/NZ",
# "Australia/NZ", "Asia", "Africa/ME", "Missing data", "US/Canada",
# "UK/Europe", "Missing data", "Africa/ME", "UK/Europe", "Africa/ME",
# "UK/Europe", "Africa/ME", "US/Canada", "Australia/NZ", "UK/Europe",
# "UK/Europe", "Asia", "US/Canada", "UK/Europe", "UK/Europe", "Australia/NZ",
# "UK/Europe", "UK/Europe", "Asia", "Asia", "Australia/NZ", "US/Canada",
# "US/Canada", "Australia/NZ", "US/Canada", "Australia/NZ", "US/Canada",
# "UK/Europe", "Australia/NZ", "US/Canada", "Australia/NZ", "US/Canada",
# "US/Canada", "Asia", "US/Canada", "US/Canada", "Missing data",
# "UK/Europe", "US/Canada", "Australia/NZ", "Australia/NZ", "Asia",
# "UK/Europe", "Australia/NZ", "UK/Europe", "UK/Europe", "US/Canada",
# "US/Canada", "Australia/NZ", "UK/Europe", "Asia", "US/Canada",
# "US/Canada", "UK/Europe", "Australia/NZ", "Asia", "US/Canada",
# "Australia/NZ", "Africa/ME", "US/Canada", "Asia", "Missing data",
# "US/Canada", "Australia/NZ", "Asia", "UK/Europe", "Australia/NZ",
# "US/Canada", "UK/Europe", "UK/Europe", "UK/Europe", "Asia", "Asia",
# "Africa/ME", "US/Canada", "Australia/NZ", "US/Canada", "Asia",
# "Asia", "Africa/ME", "Africa/ME", "Australia/NZ", "Asia", "Asia",
# "US/Canada", "UK/Europe", "Australia/NZ", "UK/Europe", "Asia",
# "Australia/NZ", "Asia", "Missing data", "US/Canada", "US/Canada",
# "US/Canada", "Australia/NZ", "Australia/NZ", "US/Canada", "Asia",
# "Australia/NZ", "Missing data", "US/Canada", "Australia/NZ",
# "Australia/NZ", "US/Canada", "UK/Europe", "Asia", "US/Canada",
# "Africa/ME", "Africa/ME", "Australia/NZ", "UK/Europe", "US/Canada",
# "US/Canada", "Australia/NZ", "Australia/NZ", "UK/Europe", "UK/Europe",
# "US/Canada", "Asia", "Asia", "UK/Europe", "UK/Europe", "UK/Europe",
# "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
# "US/Canada", "US/Canada", "US/Canada", "US/Canada", "US/Canada",
# "Australia/NZ", "US/Canada", "UK/Europe", "US/Canada", "US/Canada",
# "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
# "Australia/NZ", "UK/Europe", "UK/Europe", "UK/Europe", "Asia",
# "Asia", "Asia", "Asia", "Australia/NZ", "US/Canada", "US/Canada",
# "US/Canada", "US/Canada", "Australia/NZ", "US/Canada", "US/Canada",
# "Asia", "UK/Europe", "US/Canada", "US/Canada", "UK/Europe", "US/Canada",
# "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
# "US/Canada", "US/Canada", "US/Canada", "Australia/NZ", "Australia/NZ",
# "US/Canada", "UK/Europe", "Australia/NZ", "Australia/NZ", "UK/Europe",
# "UK/Europe", "UK/Europe", "US/Canada", "Africa/ME", "Africa/ME",
# "UK/Europe", "6", "24", "5", "0", "8", "40", "12", "0", "10",
# "1", "2", "6", "80", "10", "0", "0", "8", "2", "7", "1", "7",
# "0", "14", "0", "1", "10", "31", "1", "9", "1", "13", "3", "12",
# "1", "0", "77", "25", "25", "13", "0", "0", "1", "13", "26",
# "13", "13", "3", "0", "11", "13", "18", "5", "19", "2", "16",
# "5", "0", "3", "0", "1", "21", "2", "8", "9", "0", "20", "57",
# "0", "0", "2", "28", "1", "2", "0", "34", "0", "0", "0", "3",
# "124", "14", "4", "3", "27", "0", "3", "4", "36", "0", "1", "7",
# "16", "0", "0", "2", "0", "2", "0", "3", "0", "0", "30", "8",
# "13", "8", "6", "25", "28", "0", "0", "1", "7", "1", "0", "32",
# "0", "25", "0", "25", "1", "11", "7", "2", "0", "10", "45", "2",
# "3", "9", "0", "32", "11", "3", "2", "1", "4", "4", "4", "2",
# "11", "0", "20", "16", "15", "1", "3", "0", "3", "50", "0", "0",
# "0", "0", "6", "0", "19", "30", "6", "0", "0", "0", "25", "0",
# "25", "0", "10", "0", "4", "10", "1", "0", "0", "0", "25", "1",
# "0", "3", "4", "16", "11", "5", "4", "4", "0", "258", "0", "0",
# "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",
# "0", "0", "0", "0", "0", "0", "0", "0", "1", "1", "11", "99",
# "0", "5", "1", "0", "14", "5", "10", "1", "6", "0", "0", "9",
# "2", "5", "4", "32", "0", "3", "3", "59", "0", "0", "0", "7",
# "0", "31", "139", "2", "14", "17", "17", "16", "2", "7", "1",
# "28", "1", "0", "0", "1", "1", "0", "7", "2", "0", "11", "0",
# "0", "3", "0", "0", "10", "0", "14", "31", "11", "0", "12", "0",
# "1", "0", "31", "2", "2", "0", "21", "1", "0", "0", "3", "0",
# "17", "21", "7", "0", "4", "0", "11", "9", "0", "4", "11", "0",
# "25", "0", "48", "0", "0", "12", "12", "9", "10", "9", "0", "7",
# "26", "14", "7", "0", "0", "0", "0", "0", "7", "0", "0", "15",
# "1", "17", "18", "28", "2", "0", "6", "4", "5", "0", "0", "4",
# "1", "1", "7", "1", "24", "0", "64", "7", "0", "18", "0", "6",
# "0", "0", "0", "1", "8", "7", "6", "1", "3", "2", "2", "7", "1",
# "5", "1", "5", "4", "46", "1", "8", "0", "0", "11", "11", "5",
# "73", "37", "0", "2", "0", "2", "10", "12", "0", "249", "0",
# "10", "0", "1", "64", "4", "23", "0", "22", "12", "40", "1",
# "12", "1", "4", "8", "0", "3", "2", "2", "0", "2", "6", "4",
# "2", "2", "2", "5", "0", "4", "1", "0", "4", "6", "3", "0", "0",
# "9", "6", "0", "76", "0", "1", "0", "1", "1", "12", "2", "10",
# "5", "2", "0", "3", "0", "3", "6", "3", "1", "0", "1", "0", "0",
# "1", "0", "0", "0", "0", "0", "15", "1", "2", "2", "3", "2",
# "1", "17", "0", "0", "0", "0", "0", "31", "2", "5", "3", "4",
# "8", "0", "9", "8", "0", "0", "1", "1", "17", "2", "22", "0",
# "0", "7", "1", "10", "46", "0", "0", "1", "28", "18", "5", "20",
# "0", "2", "2", "0", "0", "0", "2", "2", "10", "32", "0", "23",
# "1", "0", "0", "0", "2", "1", "2", "2", "5", "0", "4", "0", "1",
# "5", "1", "6", "5", "2", "1", "13", "9", "0", "0", "1", "1",
# "0", "2", "1", "0", "4", "15", "1", "10", "3", "1", "0", "17",
# "0", "14", "25", "13", "15", "9", "15", "2", "57", "21", "1",
# "2", "0", "0", "0", "0", "0", "24", "0", "30", "1", "0", "6",
# "0", "4", "37", "19", "25", "7", "1", "0", "13", "12", "7", "1",
# "3", "0", "8", "13", "2", "0", "39", "8", "21", "7", "1", "4",
# "21", "0", "0", "24", "13", "1", "2", "2", "0", "18", "0", "5",
# "0", "3", "1", "0", "2", "0", "1", "2", "2", "4", "0", "0", "0",
# "12", "9", "30", "10", "10", "2", "5", "0", "0", "1", "9", "1",
# "0", "1", "3"), .Dim = c(624L, 2L), .Dimnames = list(NULL, c("Region",
# "numOpens")))
# TrialOpens = as.data.frame(TrialOpens)
# z = TrialOpens$Region
# TrialOpens = tapply(TrialOpens[[2]], TrialOpens[[1]], c)
# TrialOpens <- TrialOpens[-4] # Removing missing data
# group.sizes <- sapply(TrialOpens, length)
# weights <- runif(sum(group.sizes))
# TrialOpenWeights = tapply(weights, z[z ! =  'Missing data'], c)
#
#           })
#
