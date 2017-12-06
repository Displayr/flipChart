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
                       row.names.to.remove = NULL,
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

    out <- suppressWarnings(PrepareData(chart.type, QFilter, QPopulationWeight, get0("input.data.table")))
    expect_warning(PrepareData(chart.type, QFilter, QPopulationWeight, get0("input.data.table")),
                   "^Multiple statistics detected")
    out <- suppressWarnings(PrepareData(chart.type, QFilter, QPopulationWeight,
                               get0("input.data.table"),
                               get0("input.data.tables"),
                               input.data.pasted = NULL,
                               input.data.raw = NULL,
                               input.data.other = get0("input.data.other"),
                                transpose = get0("transpose")))





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

    out <- suppressWarnings(PrepareData(input.data.raw = input.data.raw, chart.type = "Venn", subset = QFilter))
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
    expect_equal(nrow(out$data), 800)
    # expect_error(PrepareData(input.data.raw = dat, chart.type = "Bar",
    #                          missing = "Error if missing data"),
    #              "^The data contains missing values.")
})

test_that("PrepareData: input.data.raw subset and weights",
{
    QPopulationWeight <- prop.table(runif(nrow(dat)))
    QFilter <- rbinom(nrow(dat), 1, .5)
    n.filter <- sum(QFilter ==  1L)
    out <- PrepareData(input.data.raw = list(dat), subset = QFilter, chart.type = "Scatter Plot")

    expect_equal(nrow(out$data), n.filter)
    expect_is(out$data, "data.frame")

    out <- PrepareData(input.data.raw = list(dat), subset = QFilter, chart.type = "Scatter Plot",
                       weights = QPopulationWeight)
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

test_that("PrepareData: aggregate works for all formats",
{
    set.seed(23456)
    zvec <- rpois(20, 4)
    zlen <- length(table(zvec))
    resV <- PrepareData("Column", input.data.table = zvec, first.aggregate = TRUE)
    resM <- PrepareData("Column", input.data.table = as.matrix(zvec), first.aggregate = TRUE)
    resL <- PrepareData("Column", input.data.raw = list(X = zvec), first.aggregate = TRUE)
    expect_equal(length(resV$data), zlen)
    expect_equal(length(resM$data), zlen)
    expect_equal(length(resL$data), zlen)
})

test_that("PrepareData: input and output format of raw data",
{
    set.seed(1234)
    xx <- rpois(100, 4)
    yy <- rpois(100, 2)
    attr(xx, "label") <- "VarA"
    attr(yy, "label") <- "VarB"

    res1 <- PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = FALSE)
    expect_equal(res1$values.title, "")
    res1 <- PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = TRUE)
    expect_equal(res1$values.title, "Count")
    expect_true(is.null(dimnames(res1$data)))

    res2 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = FALSE)
    expect_equal(res2$values.title, "")
    res2 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = TRUE)
    expect_equal(res2$values.title, "Counts")
    res2 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = TRUE, as.percentages = TRUE)
    expect_equal(res2$values.title, "%")
    expect_equal(names(dimnames(res2$data)), c("VarA", "VarB"))

    res3 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = FALSE,
                        as.percentages = TRUE, transpose = TRUE)
    expect_equal(res3$values.title, "%")
    expect_equal(rownames(res3$data), c("VarA", "VarB"))

    res3 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = FALSE,
                        as.percentages = TRUE, transpose = FALSE)
    expect_equal(res3$values.title, "%")
    expect_equal(colnames(res3$data), c("VarA", "VarB"))

    res3 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = TRUE,
                        as.percentages = TRUE, transpose = TRUE)
    expect_equal(res3$values.title, "%")
    expect_equal(rownames(res3$data), as.character(0:7))


    res4 <- PrepareData("Scatter", input.data.raw = list(X = NULL, Y = xx))
    #expect_equal(ncol(res4$data), 1)
    expect_equal(res4$values.title, "")
    expect_true(is.na(res4$scatter.variable.indices["x"]))
    expect_equivalent(res4$scatter.variable.indices["y"], 1)
    expect_true(is.na(res4$scatter.variable.indices["sizes"]))
    expect_true(is.na(res4$scatter.variable.indices["colors"]))

    res5 <- PrepareData("Scatter", input.data.raw = list(X = xx, Y = yy, Z1 = NULL, Z2 = yy))
    expect_equal(colnames(res5$data), c("VarA", "VarB"))
    expect_equal(res5$values.title, "")
    expect_equivalent(res5$scatter.variable.indices["x"], 1)
    expect_equivalent(res5$scatter.variable.indices["y"], 2)
    expect_true(is.na(res5$scatter.variable.indices["sizes"]))
    expect_equivalent(res5$scatter.variable.indices["colors"], 2)

    res <- suppressWarnings(PrepareData("Column", input.data.raw = list(X = factor(1:5), Y = factor(1:5), Z = factor(1:5)),
                       as.percentages = TRUE, transpose = FALSE, show.labels = TRUE))
    expect_equal(res$values.title, "%")
    expect_equal(colnames(res$data), c("X", "Y","Z"))
})


test_that("PrepareData: incorrect data.source.index",
{
    set.seed(1234)
    xx <- rpois(100, 4)
    yy <- rpois(100, 2)
    attr(xx, "label") <- "VarA"
    attr(yy, "label") <- "VarB"

    expect_error(PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = FALSE, data.source = "Link to a table in 'Pages'"), "The data provided does not match the 'data.source.index'.")
    expect_error(PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = FALSE, data.source = "Link to a table"), "The data provided does not match the 'data.source.index'.")
    expect_error(PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = FALSE, data.source = "Link to multiple tables in 'Pages'"), "The data provided does not match the 'data.source.index'.")
    expect_error(PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = FALSE, data.source = "Link to multiple tables"), "The data provided does not match the 'data.source.index'.")
    expect_error(PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = FALSE,
                             data.source ="Link to a variable in 'Data'"), NA)
    expect_error(PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = FALSE,
                             data.source = "Type or paste in data"), "The data provided does not match the 'data.source.index'.")
    expect_error(PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = FALSE, data.source = "Use an existing R Output in 'Pages'"), "The data provided does not match the 'data.source.index'.")
    expect_error(PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = FALSE, data.source = "Use an existing R Output"), "The data provided does not match the 'data.source.index'.")

    set.seed(1223)
    MultipleNumeric = list("Normal" = rnorm(500) * 1000,
                       "Poisson(Lambda = 1)" = rpois(1000, lambda = 1)  * 1000,
                       "Poisson(Lambda = 10) / 10" = rpois(1000, lambda = 10) / 10  * 1000,
                       Gamma = rgamma(1000, 1)  * 1000,
                       Exponential = rexp(1000)  * 1000,
                       Uniform = runif(1000)  * 1000
                      )
    expect_error(PrepareData("Histogram", input.data.other = MultipleNumeric, first.aggregate = FALSE,
                             data.source = "Use an existing R Output in 'Pages'"), NA)
    expect_error(PrepareData("Histogram", input.data.other = MultipleNumeric, first.aggregate = FALSE,
                             data.source = "Use an existing R Output"), NA)
    expect_error(PrepareData("Histogram", input.data.other = MultipleNumeric, input.data.raw = list(NULL), first.aggregate = FALSE,
                             data.source = "Use an existing R Output in 'Pages'"), NA)
    expect_error(PrepareData("Histogram", input.data.other = MultipleNumeric, input.data.raw = list(NULL), first.aggregate = FALSE,
                             data.source = "Use an existing R Output"), NA)

})


# values.title
Q.table <- structure(c(48.3870967741936, 51.6129032258064, 100, 52.6315789473684,
    47.3684210526316, 100, 48.936170212766, 51.063829787234, 100,
    42.3076923076923, 57.6923076923077, 100, 55.3191489361702, 44.6808510638298,
    100, 50, 50, 100, 41.3793103448276, 58.6206896551724, 100, 58.0645161290323,
    41.9354838709677, 100, 50, 50, 100), .Dim = c(3L, 9L), statistic = "Column %", .Dimnames = list(
    c("Male", "Female", "NET"), c("Less than 18 + 18 to 24 + 25 to 29",
    "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
    "55 to 64", "65 or more", "NET")), name = "Q1 by Q2", questions = c("Q1", "Q2"))
x <- rep(1:2, 15)
y <- rep(1:3, 10)
m <- matrix(1:15, 3, dimnames = list(1:3, 1:5))
for(ct in c("Bar", "Column"))
    test_that(paste("CChart: values.title ", ct),{
        expect_equal(PrepareData(ct, input.data.table = m)$values.title, "")
        pd <- PrepareData(ct, input.data.raw = list(X = x, Y = y), first.aggregate = TRUE)
        expect_equal(pd$values.title, "Counts")
        pd <- PrepareData(ct, input.data.table = Q.table, first.aggregate = TRUE)
        expect_equal(pd$values.title, "Average")
        pd <- PrepareData(ct, input.data.table = Q.table, first.aggregate = TRUE, values.title = "dog")
        expect_equal(pd$values.title, "dog")
        pd <- PrepareData(ct, input.data.table = Q.table, first.aggregate = FALSE, values.title = "dog")
        expect_equal(pd$values.title, "dog")
        pd <- PrepareData(ct, input.data.table = Q.table, first.aggregate = FALSE)
        expect_equal(pd$values.title, "Column %")

})

test_that("Basic crosstab input",{
    data(colas, package = "flipExampleData")
    z = list(X = list(colas$d1), Y = colas$d2)
    pd <- suppressWarnings(PrepareData("Column", TRUE, NULL, input.data.raw = z,
                      transpose = FALSE, first.aggregate = TRUE,
                      tidy = FALSE, data.source = "Link to variables in 'Data'"))
    expect_equal(dim(pd$data), 9:8)
    z = list(X = list(colas$d1), Y = colas$d2, Z = NULL)
    pd <- suppressWarnings(PrepareData("Column", TRUE, NULL, input.data.raw = z,
                      transpose = FALSE, first.aggregate = TRUE,
                      tidy = FALSE, data.source = "Link to variables in 'Data'"))
    expect_equal(dim(pd$data), 9:8)
    z = list(X = list(colas$d1), Y = colas$d2, Z = NULL, Z1 = NULL)
    pd <- suppressWarnings(PrepareData("Column", TRUE, NULL, input.data.raw = z,
                      transpose = FALSE, first.aggregate = TRUE,
                      tidy = FALSE, data.source = "Link to variables in 'Data'"))
    expect_equal(dim(pd$data), 9:8)
    expect_equal(dim(pd$data), 9:8)
    z = list(X = list(colas$d1), Y = colas$d2, Z = NULL, Z1 = NULL)
    pd <- suppressWarnings(PrepareData("Column", TRUE, NULL, input.data.raw = z,
                      transpose = FALSE, first.aggregate = FALSE,
                      tidy = FALSE, data.source = "Link to variables in 'Data'"))
    expect_equal(dim(pd$data), c(327, 2))
    pd <- suppressWarnings(PrepareData("Column", input.data.raw = list(X = list(colas$d1, colas$d2)),
                      transpose = TRUE, first.aggregate = TRUE))
    expect_equal(dim(pd$data), 1:2)


    set.seed(123456)
    yy <- table(rpois(20, 5))
    ylen <- length(yy)
    res1 <- PrepareData("Pie", input.data.table = yy, tidy = TRUE)
    res2 <- PrepareData("Pie", input.data.table = yy, tidy = FALSE)
    expect_true(is.null(dim(res1$data)))
    expect_true(!is.null(dim(res2$data)))
})

test_that("Scatterplot with duplicated variable",{
    data(colas, package = "flipExampleData")

    z = list(X = colas$d1, Y = colas$d2, Z1 = colas$d3, Z2 = colas$d1)
    w = capture_warnings(pd <- PrepareData("Column", TRUE, NULL, input.data.raw = z,
                      transpose = FALSE, first.aggregate = FALSE,
                      tidy = FALSE, data.source = "Link to variables in 'Data'"))
    expect_equal(NCOL(pd$data), 3)
    expect_equal(length(w), 2)
    expect_equal(w[1], "Variables containing duplicated variable names have been removed (give the variables unique names if you do not want this to happen): Age.")
    expect_true(grepl("^Some categories do not appear ", w[2]))
    w = capture_warnings(pd <- PrepareData("Scatter", TRUE, NULL, input.data.raw = z,
                      transpose = FALSE, first.aggregate = FALSE,
                      tidy = FALSE, data.source = "Link to variables in 'Data'"))
    expect_equal(NCOL(pd$data), 3)
    expect_equal(length(w), 1)
    expect_true(grepl("^After removing missing ", w[1]))
})


test_that("Scatterplot with only a Y variable",{
    #data(colas, package = "flipExampleData")
    #z = list(X = NULL, Y = colas$d2, Z1 = NULL, Z2 = NULL, labels = sprintf("Num %d", 1:nrow(colas)))
    z = structure(list(X = NULL, Y = structure(c(1327795200, 1330041600,
        1328486400, 1330214400, 1331596800, 1325376000, 1326412800, 1329609600),
        class = c("POSIXct", "POSIXt", "QDate"), QDate = structure(c(3L,
        5L, 4L, 5L, 7L, 1L, 2L, 5L), class = c("ordered", "factor"),
        .Label = c("19-Dec-11-01-Jan-12",
"02-Jan-12-15-Jan-12", "16-Jan-12-29-Jan-12", "30-Jan-12-12-Feb-12",
"13-Feb-12-26-Feb-12", "27-Feb-12-11-Mar-12", "12-Mar-12-25-Mar-12",
"26-Mar-12-08-Apr-12", "09-Apr-12-22-Apr-12", "23-Apr-12-06-May-12",
"07-May-12-20-May-12", "21-May-12-03-Jun-12", "04-Jun-12-17-Jun-12",
"18-Jun-12-01-Jul-12", "02-Jul-12-15-Jul-12", "16-Jul-12-29-Jul-12",
"30-Jul-12-12-Aug-12", "13-Aug-12-26-Aug-12", "27-Aug-12-09-Sep-12",
"10-Sep-12-23-Sep-12", "24-Sep-12-07-Oct-12", "08-Oct-12-21-Oct-12",
"22-Oct-12-04-Nov-12", "05-Nov-12-18-Nov-12", "19-Nov-12-02-Dec-12",
"03-Dec-12-16-Dec-12", "17-Dec-12-30-Dec-12")),
    questiontype = "Date", name = "date", label = "Interview Date", question = "Interview Date"),
    Z1 = NULL, Z2 = NULL, labels = structure(sprintf("Num %d", 1:8),
    questiontype = "Text", name = "IDstring", label = "ID string", question = "ID string")), .Names = c("X",
"Y", "Z1", "Z2", "labels"))
    w = capture_warnings(pd <- PrepareData("Scatter", TRUE, NULL, input.data.raw = z,
                      transpose = FALSE, first.aggregate = FALSE,
                      tidy = FALSE, data.source = "Link to variables in 'Data'"))
    expect_equal(NCOL(pd$data), 1)
    expect_equal(rownames(pd$data)[1], "Num 1")
})


test_that("Pasted data",{
    x = matrix(rep("", 20), 5)
    x[3:5, 2] = LETTERS[1:3]
    x[2, 3:4] = c("G1", "G2")
    x[3:5, 3:4] = c(1:3, 5, 3, 1)
    # Radar chart
    data(colas, package = "flipExampleData")
    #z = list(X = NULL, Y = colas$d2, Z1 = NULL, Z2 = NULL)
    pd <- PrepareData("Radar", TRUE, NULL,
        input.data.pasted = list(x, NULL, NULL, NULL, NULL),
                      transpose = FALSE, first.aggregate = FALSE,
                      tidy = TRUE, data.source = "Type or paste in data",
        values.title = NULL)
    expect_equal(NCOL(pd$data), 2)
    # Line chart
    data(colas, package = "flipExampleData")
    #z = list(X = NULL, Y = colas$d2, Z1 = NULL, Z2 = NULL)
    pd <- PrepareData("Pie", TRUE, NULL,
    input.data.pasted = list(x, NULL, NULL, NULL, NULL),
                      transpose = FALSE, first.aggregate = FALSE,
                      tidy = TRUE, data.source = "Type or paste in data",
                      as.percentages = TRUE,
                      values.title = NULL)
    expect_equal(NCOL(pd$data), 2)
})

test_that("DS-1659: histogram, variables from data",
{
    dat <- structure(list(X = list(structure(c(3025.844, 5961.405605, 2114.4025,
    -181.9484, 4071.847, 2620.7452, -11853.0269, -97.49, -1291.5211,
    395.71, 4252.2578, -6310.5432, 5661.68435
    ), questiontype = "Number", name = "TotalProfit", label = "Total Profit", question = "Total Profit")),
        Y = NULL, Z1 = NULL, Z2 = NULL, labels = NULL), .Names = c("X",
                                                                   "Y", "Z1", "Z2", "labels"))

    expect_silent(out <- PrepareData(chart.type = "Histogram", input.data.raw = dat))
    expect_is(out$data, "data.frame")
    expect_is(out$data[[1]], "numeric")

    v2 <- dat[[1L]][[1L]]
    v2 <- v2 + 11
    attr(v2, "name") <- "TotalSales"
    attr(v2, "labels") <- "Total Sales"
    attr(v2, "question") <- "Total Sales"
    dat[[1L]] <- list(dat[[1L]][[1L]], v2)

    out <- PrepareData(chart.type = "Histogram", input.data.raw = dat)
    expect_is(out$data, "data.frame")
    expect_equal(ncol(out$data), 2L)
})

test_that("DS-1689 Bar chart from one variable raw data",{
    data(colas, package = "flipExampleData")

    z = list(X = colas$d1, Y = NULL, Z1 = NULL, Z2 = NULL)
    w = capture_warnings(pd <- PrepareData("Column", TRUE, NULL, input.data.raw = z,
                                           transpose = FALSE, first.aggregate = TRUE, as.percent = TRUE,
                                           tidy = FALSE, data.source = "Link to variables in 'Data'"))
    expect_equal(unname(pd$data[1]), 0.13455657, tol = 0.000001)
})
