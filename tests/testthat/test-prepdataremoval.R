context("PrepareData Removing Rows and Columns")

test_that("PrepareData R+C removal: single table, single stat",
{
    input.data.table <- structure(c(48.3870967741936, 51.6129032258064, 100, 52.6315789473684,
    47.3684210526316, 100, 48.936170212766, 51.063829787234, 100,
    42.3076923076923, 57.6923076923077, 100, 55.3191489361702, 44.6808510638298,
    100, 50, 50, 100, 41.3793103448276, 58.6206896551724, 100, 58.0645161290323,
    41.9354838709677, 100, 50, 50, 100), .Dim = c(3L, 9L), statistic = "Column %", .Dimnames = list(
    c("Male", "Female", "NET"), c("Less than 18 + 18 to 24 + 25 to 29",
    "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
    "55 to 64", "65 or more", "NET")), name = "Q1 by Q2", questions = c("Q1", "Q2"))
    QFilter <- structure(TRUE, name = "", label = "Total sample")
    QPopulationWeight <- NULL
    chart.type <- "Pie"

    out <- PrepareData(chart.type, QFilter, QPopulationWeight, input.data.table = input.data.table, column.names.to.remove = NULL,
                       row.names.to.remove = NULL)
    expect_equal(attr(out$data, "statistic"), attr(input.data.table, "statistic"))
    expect_is(out$data, "matrix")
    expect_equal(dim(out$data), dim(input.data.table))

    out <- PrepareData(chart.type, QFilter, QPopulationWeight, input.data.table = input.data.table, row.names.to.remove = "NET",
                       column.names.to.remove = "NET")
    expect_equal(attr(out$data, "statistic"), attr(input.data.table, "statistic"))
    expect_is(out$data, "matrix")
    expect_equal(dim(out$data), dim(input.data.table) - c(1, 1))
})

test_that("PrepareData R+C removal: multiple existing tables",
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
                       chart.type = "Scatter Plot", row.names.to.remove = "NET", column.names.to.remove = "NET")
    expect_length(out$data, 2)
    expect_equal(attr(out$data[[2]], "statistic"), "%")
    expect_length(out$data[[1]], length(input.data.tables[[1]] ))
    expect_length(out$data[[2]], length(input.data.tables[[2]]) - 1)
})

test_that("PredpareData R+C removal: pasted raw data",
{
    ## list(get0("formPastedData"), get0("formPastedRawData"), get0("formPastedFactor"), get0("formPastedColumnNames"),
    ##      get0("formPastedRowNames"), get0("formPastedDateConvention"))
    dat <- rbind(c("", LETTERS[1:4]), cbind(letters[1:3], matrix(as.character(1:12), 3, 4)))
    pasted <- list(dat, TRUE, TRUE, TRUE, TRUE, TRUE)
    out <- PrepareData(input.data.table = NULL, input.data.raw = NULL, input.data.tables = NULL, input.data.other = NULL,
                       input.data.pasted = pasted, chart.type = "Bubble Chart", row.names.to.remove = "a",
                       column.names.to.remove = LETTERS[2:3])
    expect_is(out$data, "matrix")
    expect_equal(colnames(out$data), LETTERS[c(1, 4)])
    expect_equal(rownames(out$data), letters[2:3])
})

test_that("PrepareData R+C removal: pasted raw data factor handling",
{
    ## list(get0("formPastedData"), get0("formPastedRawData"), get0("formPastedFactor"), get0("formPastedColumnNames"),
    ##      get0("formPastedRowNames"), get0("formPastedDateConvention"))
    dat <- rbind(c("", LETTERS[1:4]), cbind(letters[1:3], matrix(as.character(1:12), 3, 4)))
    dat[-1, 3] <- c("dog", "cat", "dog")
    pasted <- list(dat, TRUE, FALSE, TRUE, TRUE, TRUE)
    out <- suppressWarnings(PrepareData(input.data.pasted = pasted, chart.type = "Bar Chart", column.names.to.remove = "D"))
    expect_is(out$data, "matrix")
    expect_is(out$data[[2]], "character")
    #expect_equal(names(out$data), LETTERS[1:3])
})

test_that("PrepareData R+C removal: pasted, non-raw table",
{
    dat <- structure(c("", "a", "v", "NET", "SUM", "col 1", "2", "3", "1", "2",
                      "col 2", "3", "2", "1", "1", "col 3", "3", "2", "1", "1", "col 4",
                      "3", "2", "1", "1", "col 5", "3", "2", "1", "1", "col 6", "3",
                      "2", "1", "1", "SUM", "3", "2", "1", "1", "NET", "3", "2",
                      "1", "1"), .Dim = c(5L, 9L))
    pasted <- list(dat, FALSE, NULL, NULL, NULL, NULL)
    QFilter <- structure(TRUE, name = "", label = "Total sample")
    QPopulationWeight <- NULL
    chart.type <- "Scatter"
    expect_warning(out <- PrepareData(input.data.pasted = pasted, chart.type = chart.type,
                   subset = QFilter, weights = QPopulationWeight))
    expect_is(out$data, "matrix")
    expect_equal(dim(out$data), dim(dat) - c(3, 3))
})

test_that("PrepareData R+C removal: Binary variable for venn diagram",
{
    input.data.other <- structure(list(`Coca-Cola` = c(0, 1, 0, 0, 1, 1, 1, 0, 1, 1,
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

    QFilter <- rbinom(nrow(input.data.other), 1, .25)
    n.filter <- sum(QFilter==  1)

    out <- PrepareData(input.data.other = input.data.other, chart.type = "Scatter Plot",
                       subset = QFilter, column.names.to.remove = colnames(input.data.other)[1:2])
    expect_is(out$data, "data.frame")
    expect_named(out$data, names(input.data.other)[3:4])
    expect_equal(nrow(out$data), n.filter)
})

test_that("PrepareData pasted vector entry removal",
{
    dat <- cbind(letters[1:5], 1:5)
    pasted <- list(dat, FALSE, NULL, NULL, NULL, NULL)
    QFilter <- structure(TRUE, name = "", label = "Total sample")
    QPopulationWeight <- NULL
    chart.type <- "Scatter Plot"
    out <- PrepareData(input.data.pasted = pasted, chart.type = chart.type, subset = QFilter,
                       weights = QPopulationWeight, row.names.to.remove = "a", column.names.to.remove = "b")
    expect_is(out$data, "numeric")
    expect_null(dim(out$data))
 #   expect_named(names(out$data), letters[2:5])

    pasted[[1]] <- t(dat)
    out <- PrepareData(input.data.pasted = pasted, chart.type = chart.type, subset = QFilter,
                       weights = QPopulationWeight, row.names.to.remove = c("b", "e", "q"),  column.names.to.remove = "e")
    expect_named(out$data, c("a", "c", "d"))
})

test_that("PrepareData R+C removal: input.data.raw with missing vals",
{
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
  .Names = c("Gender", "Income"),
  row.names = c(NA, -800L), class = "data.frame")
    out <- PrepareData(input.data.raw = dat, chart.type = "Bubble Chart",
                                             column.names.to.remove = "Income")
    expect_is(out$data, "data.frame")
    ## Bubble Charts don't have aggre. so will remove Income var.
    expect_equal(ncol(out$data), ncol(dat) - 1)
    expect_equal(dim(out$data), dim(dat) - c(0, 1))
    ## Bar Charts have aggregation, so Gender, Income aren't removed
    ## as aggregation happens before removal
    expect_error(suppressWarnings(PrepareData(input.data.raw = dat, chart.type = "Bar Chart",
                             first.aggregate = FALSE,
                             column.names.to.remove = c("Gender", "Income"))),
                 "Removing rows/columns gives empty input matrix")
    pd <- suppressWarnings(PrepareData(input.data.raw = dat, chart.type = "Bar Chart",
                             first.aggregate = TRUE,
                             column.names.to.remove = c("Gender", "Income")))
    expect_equal(names(pd$data),  c("Gender", "Income"))
    expect_error(suppressWarnings(PrepareData(input.data.raw = dat, chart.type = "Bar Chart",
                             first.aggregate = TRUE,
                             row.names.to.remove = c("Gender", "Income"))),  # As aggregating implicitly transposes
                 "Hiding empty elements gives empty input vector.")




    # Note this unit test is a bit ambitious as it assumes that users will provide a vector
    # which is pretty unlikely. However, it is left in as it tests the more general splitting mechanism.
    out <- suppressWarnings(PrepareData(input.data.raw = dat, chart.type = "Bar Chart",
                       column.names.to.remove = levels(dat[[2L]])[2:3],
                       split = "$")  )# don't split on "," since levels have commas

})

input.data.table <- structure(c(7.08868501529052, 3.84709480122324, 17.4617737003058
), .Dim = 3L, statistic = "Average", .Dimnames = list(c("Colas (e.g., Coca-Cola, Pepsi Max)?",
"Sparkling mineral water", "SUM")), name = "Number Multi", questions = c("Number Multi",
                                                                         "SUMMARY"))

test_that("PrepareData R+C removal: 1D Q-Table",
{
    out <- PrepareData(input.data.table = input.data.table, chart.type = "Bubble Chart")
    expect_equal(names(out$data), names(input.data.table)[-3])
})

test_that("PrepareData hide empty rows and columns QTable",
{
    input.data.table[1L] <- NA
    out <- PrepareData(input.data.table = input.data.table, chart.type = "Bubble Chart")
    expect_equal(out$data, input.data.table[2], check.attributes = FALSE)
})

test_that("PrepareData hide empty rows and columns list of tables",
{
    input.data.table[1] <- NA
    d2 <- input.data.table
    attr(d2, "statistic") <- "Column %"
    d2[2] <- 0
    x <- list(input.data.table, d2)
    expect_error(PrepareData(input.data.tables = x, chart.type = "Bubble Chart"),
                 "Hiding empty elements gives empty input vector.")
    x[[2]][1] <- 5.3
    out <- PrepareData(input.data.tables = x, chart.type = "Donut Chart",
                       row.names.to.remove = NULL, column.names.to.remove = NULL)

    expect_equal(length(out$data[[1]]), sum(!is.na(input.data.table)))
    expect_equal(length(out$data[[2]]), sum(!is.na(x[[2]]) & x[[2]] != 0))
})

test_that("PrepareData, hide empty rows and columns, list of factors",
{
    data(colas, package = "flipExampleData")
    RawData.XFactor.YFactor = list(X = colas$d1, Y = colas$d2)
    pd <- suppressWarnings(PrepareData("Bean", input.data.raw = RawData.XFactor.YFactor))
    expect_is(pd$data, "list")
    expect_is(pd$data[[1L]], "factor")
    expect_equal(levels(pd$data[[2L]]), levels(RawData.XFactor.YFactor$X))
    expect_true(all(names(pd$data) %in% levels(RawData.XFactor.YFactor$Y)))
})
