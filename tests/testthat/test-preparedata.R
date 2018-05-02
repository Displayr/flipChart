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

test_that("PrepareData: number multi",
{
    tab <- structure(c(2.98165137614679, 4.11009174311927, 3.07339449541284,
        2.63302752293578, 3.34862385321101, 2.45565749235474, 3.40366972477064,
        3.52905198776758, 4.02752293577982, 2.28440366972477), .Dim = 10L,
        statistic = "Average",
        .Dimnames = list(c("My friends would describe me as cultured, and refined",
        "I think it is important to be honest when giving complements",
        "I can be a little naïve at times", "I am the life of the party",
        "I am relaxed most of the time and not easily worried",
        "Living in a big city is important to me",
        "I think it is important to follow and maintain traditions",
        "I enjoy being attractive to the opposite sex", "I am young at heart",
        "I follow all the latest fashions")), name = "Q25. Respondent image (number multi)",
        questions = c("Q25. Respondent image (number multi)","SUMMARY"))
    res <- PrepareData("Column", input.data.table = tab)
    expect_equal(res$categories.title, "Q25. Respondent image (number multi)")
    expect_equal(res$values.title, "Average")
})


test_that("PrepareData: single table, single stat",
{
    singleQ <- structure(c(13.4556574923547, 11.9266055045872, 10.0917431192661,
        11.0091743119266, 10.7033639143731, 8.25688073394496, 12.2324159021407,
        15.5963302752294, 6.72782874617737, 100), .Dim = 10L, statistic = "%",
        .Dimnames = list(c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
        "45 to 49", "50 to 54", "55 to 64", "65 or more", "NET")), name = "Q3. Age",
        questions = c("Q3. Age", "SUMMARY"))

    expect_error(PrepareData("Column", input.data.table = singleQ), NA)
    expect_error(PrepareData("Column", input.data.table = singleQ, tidy = FALSE), NA)

    input.data.table <- structure(c(48.3870967741936, 51.6129032258064, 100, 52.6315789473684,
        47.3684210526316, 100, 48.936170212766, 51.063829787234, 100,
        42.3076923076923, 57.6923076923077, 100, 55.3191489361702, 44.6808510638298,
        100, 50, 50, 100, 41.3793103448276, 58.6206896551724, 100, 58.0645161290323,
        41.9354838709677, 100, 50, 50, 100), .Dim = c(3L, 9L), statistic = "Column %",
        .Dimnames = list(c("Male", "Female", "NET"), c("Less than 18 + 18 to 24 + 25 to 29",
        "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
        "55 to 64", "65 or more", "NET")), name = "Gender by Age", questions = c("Gender", "Age"))

    out <- PrepareData("Area", NULL, NULL, input.data.table = input.data.table,
                       transpose = get0("transpose"),
                       row.names.to.remove = NULL,
                       column.names.to.remove = NULL)
    expect_equal(out$categories.title, "Gender")
    expect_equal(out$values.title, "%")
    expect_equal(attr(out$data, "statistic"), attr(input.data.table, "statistic"))
    expect_is(out$data,  "matrix")
    expect_equal(dim(out$data), dim(input.data.table))
    expect_equal(round(out$data[1,1],3), 0.484)

    out2 <- PrepareData("Column", input.data.table = input.data.table, transpose = TRUE)
    expect_equal(out2$categories.title, "Age")
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
                               input.data.table = input.data.table,
                               input.data.pasted = NULL,
                               input.data.raw = NULL,
                               input.data.other = get0("input.data.other"),
                                transpose = get0("transpose")))





    dims <- dim(input.data.table)
    n.dim <- length(dims)
    expect_equal(attr(out$data, "statistic"), dimnames(input.data.table)[[n.dim]][1])
    expect_is(out$data, "matrix")
    expect_equal(dim(out$data), c(dims[1]*dims[3], dims[2]))

    tab.with.stat <- structure(c(1, 3, 5, 7, 2, 4, 6, 8), .Dim = c(4L, 2L), statistic = "Counts",
                               .Dimnames = list(c("A", "B", "C", "D"), c("Column 1", "Column 2")))
    pdColumn <- PrepareData("Column", input.data.table = tab.with.stat)
    pdScatter <- PrepareData("Scatter", input.data.table = tab.with.stat)
    expect_equal(pdColumn$values.title, "Counts")
    expect_equal(pdScatter$values.title, "")

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


    out2 <- PrepareData(input.data.table = input.data.tables, chart.type = "Scatter")
    expect_equal(out2, out)
})

test_that("PrepareData: pasted raw data",
{
    ## list(get0("formPastedData"),get0("formPastedFactor"), get0("formPastedColumnNames"),
    ##      get0("formPastedRowNames"))
    dat <- rbind(c("", LETTERS[1:4]), cbind(letters[1:3], matrix(as.character(1:12), 3, 4)))
    pasted <- list(dat, TRUE, TRUE, TRUE)
    out <- PrepareData(input.data.table = NULL, input.data.raw = NULL, input.data.tables = NULL, input.data.other = NULL,
                       input.data.pasted = pasted, chart.type = "Column")
    expect_is(out$data, "matrix")
    expect_equal(colnames(out$data), LETTERS[1:4])

    dat2 <- list(structure(c("", "", "", "", "", "", "", "", "", "", "",
        "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
        "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "Total %",
        "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
        "50 to 54", "55 to 64", "65 or more", "", "", "", "Colas (e.g., Coca Cola, Pepsi Max)?",
        "11.31%", "10.70%", "7.65%", "9.48%", "7.95%", "7.65%", "9.17%",
        "12.54%", "4.89%", "", "", "", "Sparkling mineral water", "",
        "1.22%", "3.36%", "2.45%", "2.75%", "3.06%", "3.67%", "4.59%",
        "1.83%", "", "", "", "Coffee", "7.34%", "7.65%", "4.59%", "8.87%",
        "7.34%", "6.42%", "8.87%", "11.93%", "5.81%"), .Dim = c(13L,
        7L)), NULL, NULL, NULL, NULL)
    out2 <- PrepareData("Scatter", input.data.pasted = dat2)
    expect_equal(out2$data[1,1], 0.1131)
})

test_that("PrepareData: raw data with labels",
{
    pp <- PrepareData("TimeSeries", input.data.raw = list(X = list(Date=Sys.Date()+1:10, A=1:10, B=2:11)))
    expect_equal(dim(pp$data), c(10, 2))
    expect_equal(pp$categories.title, "Date")
    expect_error(CChart("Time Series", pp$data), NA)
})

test_that("PrepareData: crappy input to crappy data",
{
    ## list(get0("formPastedData"), get0("formPastedFactor"), get0("formPastedColumnNames"),
    ##      get0("formPastedRowNames"))
    dat <- rbind(c("", LETTERS[1:4]), cbind(letters[1:3], matrix(as.character(1:12), 3, 4)))
    dat[-1, 3] <- c("dog", "cat", "dog")
    pasted <- list(dat, FALSE, TRUE, TRUE)
    expect_error(suppressWarnings(PrepareData(input.data.pasted = pasted, chart.type = "Bar")), NA)


})

test_that("PrepareData: pasted, non-raw data",
{
    dat <- structure(c("", "a", "v", "c", "d", "col 1", "2", "3", "1", "2",
                      "col 2", "3", "2", "1", "1", "col 3", "3", "2", "1", "1", "col 4",
                      "3", "2", "1", "1", "col 5", "3", "2", "1", "1", "col 6", "3",
                      "2", "1", "1", "col 7", "3", "2", "1", "1", "col 8", "3", "2",
                      "1", "1"), .Dim = c(5L, 9L))
    pasted <- list(dat, NULL, NULL, NULL, NULL)
    QFilter <- structure(TRUE, name = "", label = "Total sample")
    QPopulationWeight <- NULL
    chart.type <- "Scatter"
    expect_warning(out <- PrepareData(input.data.pasted = pasted, chart.type = chart.type,
                   subset = QFilter, weights = QPopulationWeight))
    expect_is(out$data, "matrix")
    expect_equal(dim(out$data), dim(dat) - c(1, 1))
})

test_that("Tidy data tries to convert matrix to numeric",
{
    dat <- structure(c("", "a", "v", "c", "d", "col 1", "2", "3", "1", "2",
                      "col 2", "3", "2", "1", "1", "col 3", "3", "2", "1", "1", "col 4",
                      "3", "2", "1", "1", "col 5", "3", "2", "1", "1", "col 6", "3",
                      "2", "1", "1", "col 7", "3", "2", "1", "1", "col 8", "3", "2",
                      "1", "1"), .Dim = c(5L, 9L))
    dat[2, 2] <- "TEXT"
    pasted <- list(dat, FALSE, TRUE, TRUE)

    expect_warning(out <- PrepareData(input.data.pasted = pasted, chart.type = "Table", tidy = TRUE,
                               first.aggregate = FALSE),
                   "Data has been automatically been converted to being numeric.")
    expect_is(out$data, "matrix")
    expect_true(is.numeric(out$data))
    expect_equal(dim(out$data), dim(dat) - c(1, 1))

    expect_silent(out <- PrepareData(input.data.pasted = pasted, chart.type = "Table", tidy = FALSE,
                               first.aggregate = FALSE))
    expect_is(out$data[[1]], "character")
    expect_is(out$data[[2]], "numeric")
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

test_that("PrepareData works with pasted vector",
{
    dat <- cbind(letters[1:5], 1:5)
    pasted <- list(dat, FALSE, NULL, NULL, NULL)
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
    out <- suppressWarnings(PrepareData(input.data.raw = list(dat), subset = QFilter, chart.type = "Scatter Plot"))

    expect_equal(nrow(out$data), n.filter)
    expect_is(out$data, "data.frame")

    out <- suppressWarnings(PrepareData(input.data.raw = list(dat), subset = QFilter, chart.type = "Scatter Plot",
                       weights = QPopulationWeight))
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

test_that("PrepareData: 1-row table",
{
    xx <- matrix(1:5, 1, 5)
    expect_error(res1 <- PrepareData("Column", input.data.table = xx, subset = TRUE, tidy = TRUE), NA)
    expect_error(res2 <- PrepareData("Column", input.data.table = xx, subset = TRUE, tidy = FALSE), NA)
    expect_equal(length(dim(res1$data)), 0)
    expect_equal(length(dim(res2$data)), 2)
})

test_that("PrepareData: aggregate works for all formats",
{
    set.seed(23456)
    zvec <- rpois(20, 4)
    zlen <- length(table(zvec))
    res.vector <- PrepareData("Column", input.data.table = zvec, first.aggregate = TRUE)
    res.matrix <- PrepareData("Column", input.data.table = as.matrix(zvec), first.aggregate = TRUE)
    res.list <- PrepareData("Column", input.data.raw = list(X = zvec), first.aggregate = TRUE)
    expect_equal(length(res.vector$data), zlen)
    expect_equal(length(res.matrix$data), zlen)
    expect_equal(length(res.list$data), zlen)
})

test_that("PrepareData: input and output format of raw data",
{
    set.seed(1234)
    xx <- rpois(100, 4)
    yy <- rpois(100, 2)
    y2 <- rpois(100, 2)
    attr(xx, "label") <- "VarA"
    attr(yy, "label") <- "VarB"
    attr(y2, "label") <- "VarC"

    # Multiple variables in Y are concatenated
    res6 <- PrepareData("Scatter", input.data.raw = list(X = xx, Y = list(yy, y2)), tidy.labels = TRUE)
    expect_equal(dim(res6$data), c(200, 3))
    expect_true(attr(res6$data, "scatter.mult.yvals"))
    expect_equal(res6$scatter.variable.indices, c(x = 1, y = 2, sizes = 0, colors = 3))
    expect_equal(as.character(res6$data[101,3]), "VarC")

    # Duplicated variables
    res7 <- PrepareData("Scatter", input.data.raw = list(X = yy, Y = yy), tidy.labels = TRUE)
    expect_equal(dim(res7$data), c(100, 1))
    expect_equal(res7$scatter.variable.indices, c(x = 1, y = 1, sizes = NA, colors = NA))

    res8 <- PrepareData("Scatter", input.data.raw = list(X = xx, Y = yy), tidy.labels = TRUE)
    expect_equal(dim(res8$data), c(100, 2))
    expect_equal(res8$scatter.variable.indices, c(x = 1, y = 2, sizes = NA, colors = NA))

    res1 <- PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = FALSE)
    expect_equal(res1$values.title, "")
    res1 <- PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = TRUE)
    expect_equal(res1$values.title, "Count")
    expect_equal(res1$categories.title, "VarA")
    expect_true(is.null(dimnames(res1$data)))

    expect_warning(res2 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = FALSE),
                   "Input data is always aggregated when 'Groups' variable is provided")
    expect_equal(res2$values.title, "Counts")
    res2 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy))
    expect_equal(res2$values.title, "Counts")
    res2 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = TRUE, as.percentages = TRUE)
    expect_equal(res2$values.title, "%")
    expect_equal(res2$categories.title, "VarA")
    expect_equal(names(dimnames(res2$data)), c("VarA", "VarB"))

    res3 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = TRUE,
                        group.by.last = TRUE,
                        as.percentages = TRUE, transpose = TRUE)
    expect_equal(res3$values.title, "%")
    expect_equal(sum(sum(res3$data)),8)

    res3 <- suppressWarnings(PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = FALSE,
                        group.by.last = TRUE,
                        as.percentages = TRUE, transpose = TRUE))
    expect_equal(res3$values.title, "%")
    expect_equal(rownames(res3$data), as.character(0:7))

    res3 <- suppressWarnings(PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = FALSE,
                        as.percentages = TRUE, transpose = FALSE))
    expect_equal(res3$values.title, "%")
    expect_equal(rownames(res3$data), as.character(c(0:7,10)))

    res3 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = TRUE,
                        as.percentages = TRUE, transpose = TRUE, tidy.labels = TRUE)
    expect_equal(res3$values.title, "%")
    expect_equal(rownames(res3$data), as.character(0:7))


    res4 <- PrepareData("Scatter", input.data.raw = list(X = NULL, Y = xx), tidy.labels = TRUE)
    #expect_equal(ncol(res4$data), 1)
    expect_equal(res4$values.title, "")
    expect_true(is.na(res4$scatter.variable.indices["x"]))
    expect_equivalent(res4$scatter.variable.indices["y"], 1)
    expect_true(is.na(res4$scatter.variable.indices["sizes"]))
    expect_true(is.na(res4$scatter.variable.indices["colors"]))

    res5 <- suppressWarnings(PrepareData("Scatter", input.data.raw = list(X = xx, Y = yy, Z1 = NULL, Z2 = yy)))
    expect_equal(colnames(res5$data), c("VarA", "VarB"))
    expect_equal(res5$values.title, "")
    expect_equivalent(res5$scatter.variable.indices["x"], 1)
    expect_equivalent(res5$scatter.variable.indices["y"], 2)
    expect_true(is.na(res5$scatter.variable.indices["sizes"]))
    expect_equivalent(res5$scatter.variable.indices["colors"], 2)

    res <- suppressWarnings(PrepareData("Column", input.data.raw = list(X = 2:6, Y = factor(1:5), Z = factor(1:5)),
                       as.percentages = TRUE, transpose = FALSE, show.labels = TRUE))
    expect_equal(res$values.title, "%")
    expect_equal(colnames(res$data), c("Y","Z"))
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
        expect_equal(pd$values.title, "%")

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
    z = list(X = list(colas$d1), Y = colas$d2, Z = NULL, Z1 = NULL)
    pd <- suppressWarnings(PrepareData("Column", TRUE, NULL, input.data.raw = z,
                      transpose = FALSE, first.aggregate = FALSE,
                      tidy = FALSE, data.source = "Link to variables in 'Data'"))
    z = list(X = list(colas$d1), Y = colas$d2, Z = NULL, Z1 = NULL)
    pd <- suppressWarnings(PrepareData("Column", TRUE, NULL, input.data.raw = z,
                      transpose = FALSE, first.aggregate = NULL,
                      tidy = FALSE, data.source = "Link to variables in 'Data'"))
    expect_equal(dim(pd$data), 9:8)
    pd <- suppressWarnings(PrepareData("Column", input.data.raw = list(X = list(colas$d1, colas$d2)),
                      transpose = TRUE, first.aggregate = TRUE))
    expect_equal( dim(as.matrix(pd$data)), 2:1)


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
        input.data.pasted = list(x, NULL, NULL, NULL),
                      transpose = FALSE, first.aggregate = FALSE,
                      tidy = TRUE, data.source = "Type or paste in data",
        values.title = NULL)
    expect_equal(NCOL(pd$data), 2)
    # Line chart
    data(colas, package = "flipExampleData")
    #z = list(X = NULL, Y = colas$d2, Z1 = NULL, Z2 = NULL)
    pd <- suppressWarnings(PrepareData("Pie", TRUE, NULL,
        input.data.pasted = list(x, NULL, NULL, NULL),
                      transpose = FALSE, first.aggregate = FALSE,
                      tidy = TRUE, data.source = "Type or paste in data",
                      as.percentages = TRUE,
                      values.title = NULL))
    expect_equal(NCOL(pd$data), 2)

    pst <- list(structure(c("", "", "", "", "", "", "", "", "", "Aardvark",
        "Bear", "Zebra", "", "", "", "3%", "7%", "5%"), .Dim = c(6L,
        3L)), NULL, NULL, NULL)
    pd <- PrepareData("Column", input.data.pasted = pst, as.percentages = T)
    expect_equal(sum(pd$data), 1)
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

test_that("as.percentages from pasted data and raw data work by dividing by nrow if NOT venn",{
 z = matrix(c(1,1,1,1,1,0,1,0,0),3, dimnames = list(1:3, LETTERS[1:3]))
 zz = PrepareData("Venn", input.data.raw = list(z), as.percentages = TRUE, first.aggregate = FALSE)
 expect_equal(zz$data[1,1], 1)
 zz = PrepareData("Venn", input.data.pasted = list(z), as.percentages = TRUE, first.aggregate = FALSE)
 expect_equal(zz$data[1,1], 1)
 z = matrix(c(1,1,1,1,1,0,1,0,0),3, dimnames = list(1:3, LETTERS[1:3]))
 zz = suppressWarnings(PrepareData("Column", input.data.raw = list(z), as.percentages = TRUE,
                                   first.aggregate = FALSE))
 #expect_equal(zz$data[1,1], 1/3)
 #zz = suppressWarnings(PrepareData("Column", input.data.pasted = list(z), as.percentages = TRUE,
 #                                  first.aggregate = FALSE))
 #expect_equal(zz$data[1,1], 1/3)
})



test_that("crosstabs from pasted data and table",{
 z = matrix(c(1,1,1,1,2,2,2,2,rep(NA,8), 1,1,2,2,1,1,2,2), ncol = 3, dimnames = list(1:8, LETTERS[1:3]))
 # Computing the average - variable with all missing data
 zz = PrepareData("Column", input.data.raw = list(z), as.percentages = FALSE, first.aggregate = TRUE,
                  group.by.last = FALSE)
 expect_equal(as.numeric(zz$data), c(1.5, 1.5))
 # Computing the average - variable with all missing data and weights
 zz = PrepareData("Column", input.data.raw = list(z), weights = z[,1], as.percentages = FALSE,
                  first.aggregate = TRUE, group.by.last = FALSE)
 expect_equal(as.numeric(zz$data), c(1 + 2/3, 1.5))
 # Computing the average - variable with all missing data and some dodgy weights
 zz = PrepareData("Column", input.data.raw = list(z), weights = c(0,0,NA,-1,1,1,1,1), as.percentages = FALSE,
                  first.aggregate = TRUE, group.by.last = FALSE)
 expect_equal(as.numeric(zz$data), c(2, 1.5))


 # Creating a crosstab - with three variables
 expect_warning(PrepareData("Column", input.data.raw = list(z), as.percentages = FALSE, first.aggregate = TRUE, group.by.last = TRUE),
              "Multiple variables have been provided. Only the first and last variable have been used to create the crosstab. If you wish to create a crosstab with more than two variables, you need to instead add the data as a 'Data Set' instead add a 'Data Set'.")
 zzz = suppressWarnings(PrepareData("Column", input.data.raw = list(z), as.percentages = FALSE, first.aggregate = TRUE, group.by.last = TRUE))
 expect_equal(zzz$data[1,1], 2)
 # Creating a crosstab with two variables
 zz = PrepareData("Column", input.data.raw = list(z[, -2]), as.percentages = FALSE, first.aggregate = TRUE, group.by.last = TRUE)
 expect_equal(zz$data[1,1], 2)
 # Creating a crosstab with two variables
 zz = PrepareData("Column", input.data.pasted = list(z[, -2]), as.percentages = FALSE, first.aggregate = TRUE, group.by.last = TRUE)
 expect_equal(zz$data[1,1], 2)
})

test_that("PrepareData, automatic rownames",
{
    # Basic test
    res <- PrepareData("Time Series", input.data.raw = list(X = list(
        date=as.Date("2017-01-01") + 0:9, score=1:10)))
    expect_equal(names(res$data), sprintf("Jan %02d 2017", 1:10))

    # Check all-numeric matrix with numeric rownames is retained
    # Checks for Scatter
})

test_that("Prepare data with as.percentages and Pick Any inputs to a Venn Diagram",{
    b1 = structure(list(`Coca Cola` = c(0, 1, 1, 1, 0, 0, 0, 1, 1, 1,
        1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1,
        1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0,
        1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1,
        1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1,
        1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1,
        1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1,
        0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0,
        1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1,
        1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0,
        0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1,
        1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1,
        1, 0), `Diet Coke` = c(1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0,
        0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1,
        0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0,
        0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
        1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1,
        0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1,
        0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0,
        0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0,
        0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
        0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0,
        0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0,
        1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0,
        0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
        1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0,
        0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0,
        0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0),
            `Coke Zero` = c(0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1,
            0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0,
            0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0,
            1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1,
            0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0,
            1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1,
            0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
            1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1,
            1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0,
            0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1,
            0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1,
            0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0,
            0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
            1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1,
            1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1,
            0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0,
            0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0,
            0, 1, 1, 1, 0, 0, 1, 0, 0), Pepsi = c(0, 1, 0, 0, 0, 0, 0,
            0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1,
            0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0,
            0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1,
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
            0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0,
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0,
            1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0,
            1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0,
            1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1,
            1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1,
            1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0,
            1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0,
            1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1,
            1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0,
            1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
            0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
            0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0), `Pepsi Max` = c(1,
            1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
            1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1,
            0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0,
            1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1,
            0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1,
            1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,
            0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0,
            1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0,
            1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1,
            0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0,
            0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1,
            0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0,
            0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
            0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0,
            0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0,
            0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1,
            1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0,
            0, 1, 1), NET = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1,
            1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), class = "data.frame", .Names = c("Coca Cola",
        "Diet Coke", "Coke Zero", "Pepsi", "Pepsi Max", "NET"), row.names = c(NA,
        327L), questiontype = "PickAny", question = "Q6. Brand preference")
     zz = PrepareData("Venn", input.data.raw = list(X = b1), as.percentages = TRUE)
     expect_error(Venn(zz$data), NA)
})

test_that("Invalid joining",
{
    expect_warning(PrepareData("Column", input.data.raw = list(X = list(A = 1:10, B = 1:9)),
                      first.aggregate = TRUE, group.by.last = FALSE), NA)
    expect_warning(PrepareData("Column", input.data.raw = list(X = list(A = 1:10, B = 1:9)),
                      first.aggregate = TRUE, group.by.last = FALSE, subset = TRUE), NA)
    expect_warning(PrepareData("Column", input.data.raw = list(X = list(A = 1:10, B = 1:9)),
                      first.aggregate = TRUE, group.by.last = FALSE, subset = rep(TRUE, 10)),
                   "The variables have been automatically spliced")
    expect_error(suppressWarnings(PrepareData("Column", input.data.raw = list(X = list(A = 1:10, B = 1:9)),
                      first.aggregate = TRUE, group.by.last = FALSE, subset = rep(TRUE, 11))),
                   "'subset' and 'data' are required to have the same number of observations. They do not")
    expect_warning(PrepareData("Column", input.data.raw = list(X = list(A = 1:10, B = 1:9)),
                      first.aggregate = TRUE, group.by.last = FALSE, weights = 1:10),
                   "The variables have been automatically spliced together")
    expect_error(suppressWarnings(PrepareData("Column", input.data.raw = list(X = list(A = 1:10, B = 1:9)),
                      first.aggregate = TRUE, group.by.last = FALSE, weights = 1:11)),
                   "'weights' and 'data' are required to have the same number of observations. They do not.")
    expect_warning(PrepareData("Column", input.data.raw = list(X = list(A = 1:10, B = 1:9)),
                      first.aggregate = TRUE, group.by.last = TRUE),
                     "The variables being crosstabbed have different lengths")
    expect_warning(PrepareData("Column", input.data.raw = list(X = list(A = 1:10, B = 1:9)),
                      first.aggregate = TRUE, group.by.last = TRUE),
                     "The variables being crosstabbed have ")
})


test_that("Weighting of a frequency table",
{
    z = PrepareData("Column", input.data.raw = list(X = rep(1:2,5)),
                               first.aggregate = TRUE, group.by.last = FALSE)
    expect_equal(as.numeric(z$data), c(5,5))
    z = PrepareData("Column", input.data.raw = list(X = rep(1:2,5)), weights = rep(1000,10),
                               first.aggregate = TRUE, group.by.last = FALSE)
    expect_equal(as.numeric(z$data), c(5000,5000))

})

test_that("Automatic crosstab of two input variables",
{
    # Raw data
    z = PrepareData("Column", input.data.raw = list(X = c(1,2,1,1,1), Y = c(1,2,1,2,1)),
                               first.aggregate = TRUE, group.by.last = TRUE)
    expect_equal(z$data[1,1], 3)
    z = PrepareData("Column", input.data.raw = list(X = c(1,2,1,1,1), Y = c(1,2,1,2,1)),
                               first.aggregate = TRUE, group.by.last = FALSE)
    expect_equal(z$data[1,1], 3)
    z = PrepareData("Column", input.data.raw = list(X = c(1,2,1,1,1), Y = c(1,2,1,2,1)),
                               first.aggregate = NULL, group.by.last = FALSE)
    expect_equal(z$data[1,1], 3)
    expect_warning(z <- PrepareData("Column", input.data.raw = list(X = list(A = 1:5, B = 2:6), Y = c(1,2,1,2,1))),
                   "'Groups' variable ignored if more than one input variable is selected")
    expect_equal(z$categories.title, "A")
    expect_equal(z$values.title, "B")

    # Pasted data
    zz = list(matrix(c("X", 1,2,1,1,1,"Y", 1,2,1,2,1), ncol = 2))
    z = PrepareData("Column", input.data.pasted = zz, first.aggregate = TRUE, group.by.last = TRUE)
    expect_equal(z$data[1,1], 3)
    z = PrepareData("Column", input.data.pasted = zz,first.aggregate = TRUE, group.by.last = FALSE)
    expect_equal(as.numeric(z$data), c(1.2, 1.4))
    z = PrepareData("Scatter", input.data.pasted = zz, first.aggregate = FALSE, group.by.last = FALSE)
    expect_equal(as.numeric(z$data), as.numeric(zz[[1]][-1,]))
    z = expect_warning(PrepareData("Colunm", input.data.pasted = zz, first.aggregate = FALSE,
                        group.by.last = FALSE), "Duplicated entries in 'X'")
    expect_equal(z$categories.title, "X")
    expect_equal(z$values.title, "Y")

    # Pasted data with an irrelevant middle column
    zz = list(matrix(c(1,2,1,1,1,NA, 4, NA, 3, NA, 1,2,1,2,1), ncol = 3, dimnames = list(1:5, c("X","Irrelevant", "Y"))))
    z = suppressWarnings(PrepareData("Column", input.data.pasted = zz, first.aggregate = TRUE, group.by.last = TRUE))
    expect_equal(z$data[1,1], 3)
    z = PrepareData("Column", input.data.table = zz, first.aggregate = TRUE, group.by.last = FALSE)
    expect_equal(as.numeric(z$data), c(1.2, 3.5, 1.4))
    #z = PrepareData("Column", input.data.table = zz, first.aggregate = FALSE, group.by.last = FALSE)
    #expect_equal(dim(z$data), dim(zz[[1]]))

    # Checking histograms still work (as they should never be aggregated)
    zz = c(1,2,1,1,1)
    z = suppressWarnings(PrepareData("Histogram",
                                input.data.raw = list(X = zz, Y = c(1,2,1,2,1)),
                               first.aggregate = TRUE, group.by.last = TRUE))
    expect_equal(sum(unlist(z$data)), sum(zz))

})

test_that("Pasted data with dates and date.format arg",
{
    ## list(get0("formPastedData"), get0("formPastedFactor"), get0("formPastedColumnNames"),
    ##      get0("formPastedRowNames"))
    x <- matrix(c("Date times", "22/06/2007 5:29:41 PM", "22/06/2007 6:09:10 PM",
                  "22/06/2007 5:36:35 PM", "22/06/2007 5:30:29 PM", "22/06/2007 5:40:53 PM",
                  "22/06/2007 5:32:22 PM", "22/06/2007 5:39:32 PM", "22/06/2007 5:39:14 PM",
                  "22/06/2007 5:40:11 PM", "22/06/2007 5:54:34 PM"), ncol = 1)
    pasted <- list(x, TRUE, TRUE, FALSE)
    out <- PrepareData(chart.type = "Table", input.data.pasted = pasted, tidy = FALSE,
                       hide.empty.rows.and.columns = FALSE, date.format = "Automatic")$data
    expect_is(out, "data.frame")
    expect_named(out, "Date times")
    expect_is(out[[1L]], "POSIXct")

    ## wrong date format specified so char. dates becomes factor
    out <- PrepareData(chart.type = "Table", input.data.pasted = pasted, tidy = FALSE,
                       hide.empty.rows.and.columns = FALSE, date.format = "US")$data
    expect_is(out, "data.frame")
    expect_named(out, "Date times")
    expect_is(out[[1L]], "character")

    ## tidy requested - data is forced into numeric format
    pasted <- list(x, FALSE, TRUE, FALSE)
    expect_silent(out <- PrepareData(chart.type = "Table", input.data.pasted = pasted, tidy = TRUE,
                                      hide.empty.rows.and.columns = FALSE, date.format = "US")$data)
    expect_true(is.numeric(out))
    expect_equal(length(out), 10)
})

test_that("Date formatting",
{
    # test for vector and matrix separately because they are coded in different places
    x1d <- 1:10
    names(x1d) <- sprintf("%02d/01/2017", 1:10)
    x2d <- cbind(A = x1d, B = x1d + 1)
    x3 <- cbind(Time = names(x1d), A = unname(x1d))

    res1 <- PrepareData("Column", input.data.table = x1d, date.format = "International (dd/mm/yyyy)")
    res2 <- PrepareData("Column", input.data.table = x2d, date.format = "International (dd/mm/yyyy)")
    res3 <- PrepareData("Column", input.data.table = x1d, date.format = "US (mm/dd/yyyy)")
    res4 <- PrepareData("Column", input.data.table = x2d, date.format = "US (mm/dd/yyyy)")
    res5 <- PrepareData("Column", input.data.table = x3)

    us.dates <- format(as.Date(sprintf("2017-%02d-01", 1:10)), "%b %d %Y")
    intl.dates <- format(as.Date(sprintf("2017-01-%02d", 1:10)), "%b %d %Y")

    expect_equal(names(res1$data), intl.dates)
    expect_equal(rownames(res2$data), intl.dates)
    expect_equal(names(res3$data), us.dates)
    expect_equal(rownames(res4$data), us.dates)
})

test_that("Scatter input data column order",
{
    pst <- list(structure(c("A", "0", "1", "2", "3", "B", "5", "6", "7",
"8", "C", "9", "10", "11", "12"), .Dim = c(5L, 3L)), NULL, NULL,
    NULL, NULL)
    pstDF <- list(structure(c("A", "0", "1", "2", "3", "B", "5", "6", "7",
"8", "C", "9", "10", "11", "12"), .Dim = c(5L, 3L)), TRUE, TRUE,
    TRUE, NULL)
    p.unnamed <- list(structure(c("", "", "", "", "", "", "1", "2", "3", "4",
"", "5", "6", "7", "8", "", "9", "10", "11", "12"), .Dim = c(5L,
4L)), NULL, NULL, NULL, NULL)
    p.2col <- list(structure(c("A", "1", "2", "3", "4", "B", "5", "6", "7", "8"),
        .Dim = c(5L, 2L)), NULL, NULL, NULL, NULL)
    p.dates <- list(structure(c("A", "1/1/2017", "2/1/2017", "3/1/2017", "4/1/2017",
        "B", "5", "6", "7", "8", "C", "9", "10", "11", "12"),
        .Dim = c(5L, 3L)), NULL, NULL, NULL, NULL)
    tb <- cbind(A = rnorm(10), B = rnorm(10), C = rnorm(10))
    rownames(tb) <- letters[1:10]

    res <- PrepareData("Scatter", input.data.other = tb, scatter.mult.yvals = TRUE)
    expect_equal(dim(res$data), c(30, 3))
    expect_equal(attr(res$data, "scatter.mult.yvals"), TRUE)
    res <- PrepareData("Scatter", input.data.pasted = pst, scatter.mult.yvals = TRUE)
    expect_equal(levels(res$data$Groups), c('B','C'))
    res <- PrepareData("Scatter", input.data.pasted = p.dates, date.format = "International", scatter.mult.yvals = TRUE)
    expect_equal(res$data[,1], sprintf("Jan %02d 2017", c(1:4, 1:4)))
    res <- PrepareData("Scatter", input.data.pasted = p.unnamed, scatter.mult.yvals = TRUE)
    expect_equal(levels(res$data$Groups), c('Group 1','Group 2'))
    res <- PrepareData("Scatter", input.data.pasted = pstDF)
    expect_equal(ncol(res$data), 2)
    res <- PrepareData("Scatter", input.data.pasted = pst, scatter.mult.yvals = TRUE)
    expect_equal(ncol(res$data), 3)

})

test_that("Tidy labels",
{
    mat.date <- structure(c(1, 2, 3, 4, 5, 2.2, 4.7, 3.1, 5, 6.2), .Dim = c(5L,
2L), .Dimnames = list(c("Jan 12 2007 00:00", "Jan 12 2007 12:00",
"Jan 13 2007 00:00", "Jan 13 2007 12:00", "Jan 14 2007 00:00"), NULL))
    res <- PrepareData("Table", input.data.table = mat.date, tidy.labels = TRUE)
    expect_true(is.null(res$categories.title))

    mat.string <- structure(c(1, 2, 3, 4, 5, 2.2, 4.7, 3.1, 5, 6.2), .Dim = c(5L,
2L), .Dimnames = list(c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5"), NULL))
    res <- PrepareData("Table", input.data.table = mat.string, tidy.labels = TRUE)
    expect_equal(res$categories.title, "Row")

    pst <- structure(c("Q1 - A", "1", "2", "3", "4", "5", "2", "5", "7",
        "5", "", "Q1 - B", "0", "0", "0", "0", "5", "8", "7", "1", "2",
        "1", "Q1 - C", "9", "9", "9", "9", "7", "7", "6", "", "", ""),
        .Dim = c(11L, 3L))
    res <- PrepareData("Density", input.data.pasted=list(pst, FALSE), tidy.labels = TRUE)
    expect_equal(colnames(res$data), c('A','B','C'))

    datL <- list('Q1 - A' = rnorm(20), 'Q1 - B' = rnorm(30), 'Q1 - C' = rnorm(30))
    resL <- PrepareData("Density", input.data.table = datL, tidy.labels = TRUE)
    expect_equal(names(resL$data), c('A','B','C'))

    datV = list('Q1 - A' = rbinom(10, 1, 0.5), 'Q1 - B' = rbinom(10, 1, 0.2),
                'Q1 - C' = rbinom(10, 1, 0.9))
    resV = PrepareData("Venn", input.data.raw = datV, tidy.labels = TRUE)
    expect_equal(colnames(resV$data), c('A','B','C'))
})

test_that("PrepareData with lists and dataframes",
{
    data("LifeCycleSavings")
    expect_silent(res <- PrepareData("Column", input.data.table = LifeCycleSavings))
    expect_silent(CChart("Column", res$data))

    x1 <- matrix(c(1:9, 1, 1, 2), 3, 4, dimnames = list(LETTERS[1:3], letters[1:4]))
    x2 <- x1 + 0.2
    x3 <- x2 + 0.2
    expect_silent(res <- PrepareData("Scatter", input.data.table = list(x1, x2, x3)))
    expect_warning(CChart("Scatter", res$data, trend.lines = FALSE),
                   "Tables have been automatically assigned names")

    df <- structure(list(`col 1` = c(19.5, 29.3, 1.1, 28.7, 11, 10.4)), .Names = "col 1",
                    row.names = c("A", "B", "C", "D", "E", "F"), class = "data.frame")
    expect_silent(res <- PrepareData("Donut", input.data.table = df))
    expect_equal(names(res$data), rownames(df))


    dfL <- list(structure(list(sr = c(11.43, 12.07, 13.17, 5.75, 12.88),
        pop15 = c(29.35, 23.32, 23.8, 41.89, 42.19)), .Names = c("sr",
        "pop15"), row.names = c("Australia", "Austria", "Belgium", "Bolivia",
        "Brazil"), class = "data.frame"), structure(list(sr = c(12.43,
        12.07, 14.17, 5.75, 13.88), pop15 = c(29.35, 24.32, 23.8, 42.89,
        42.19)), .Names = c("sr", "pop15"), row.names = c("Australia",
        "Austria", "Belgium", "Bolivia", "Brazil"), class = "data.frame"),
        structure(list(sr = c(12.43, 13.07, 14.17, 6.75, 13.88),
        pop15 = c(30.35, 24.32, 24.8, 42.89, 43.19)), .Names = c("sr",
        "pop15"), row.names = c("Australia", "Austria", "Belgium",
        "Bolivia", "Brazil"), class = "data.frame"))
    expect_silent(PrepareData("Scatter", input.data.table = dfL))
})

test_that("Heatmap allows numeric rownames",
{
    mat <- structure(c(0.971335243200883, 0.296968459384516, 0.0549077750183642,
                0.113298856886104, 0.618921121116728, 0.880021639866754, 0.976773890899494,
                0.435112230246887, 0.627550506265834, 0.947816838743165, 0.839256793726236,
                0.882670084480196, 0.868620664346963, 0.704373944085091, 0.502894431818277,
                0.753273667767644, 0.433141203364357, 0.551834018435329, 0.864853468956426,
                0.738900367636234, 0.85217569116503, 0.245717079611495, 0.22966141323559,
                0.703907362418249, 0.200267365435138), .Dim = c(5L, 5L), .Dimnames = list(
                    c("1", "2", "3", "4", "5"), c("1", "2", "3", "4", "5")))
    res <- PrepareData("Heat", input.data.table = mat)
    expect_equal(rownames(res$data), as.character(1:5))
    expect_equal(ncol(res$data), 5)
})

test_that("Retain numeric rownames unless they are the default",
{
    x0 <- structure(c(4.68663338879645, 4.88175494917068, 4.82555178268251,
                4.95379965457686, 5.03145973154362, 5.06707855251545, 4.97760859829825,
                4.00892359174568, 4.12285407725322, 4.18324829931973, 4.1969696969697,
                4.25999158603281, 4.44154118689105, 4.35534591194969, 4.87296233839236,
                5.07708779443255, 5.05692438402719, 5.04362850971922, 5.14974832214765,
                5.1682119205298, 5.10653536257833), .Dim = c(7L, 3L),
                .Dimnames = list(c("1","2","3","4","5","6","7"),
                c("to help people and care for others well-being",
                  "to be humble and modest, not draw attention",
                "to be loyal to friends and devote to people close")))

    x1 <- structure(c(4.68663338879645, 4.88175494917068, 4.82555178268251,
                4.95379965457686, 5.03145973154362, 5.06707855251545, 4.97760859829825,
                4.00892359174568, 4.12285407725322, 4.18324829931973, 4.1969696969697,
                4.25999158603281, 4.44154118689105, 4.35534591194969, 4.87296233839236,
                5.07708779443255, 5.05692438402719, 5.04362850971922, 5.14974832214765,
                5.1682119205298, 5.10653536257833), .Dim = c(7L, 3L),
                .Dimnames = list(c("2002", "2004", "2006", "2008", "2010", "2012", "2014"),
                c("to help people and care for others well-being",
                  "to be humble and modest, not draw attention",
                  "to be loyal to friends and devote to people close")))

    res0 <- PrepareData("Column", input.data.table = x0)
    expect_equal(dim(res0$data), c(7, 2))
    res1 <- PrepareData("Column", input.data.table = x1)
    expect_equal(dim(res1$data), c(7, 3))
})
