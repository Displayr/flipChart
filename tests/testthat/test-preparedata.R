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
     expect_equal(JSON, out$data, check.attributes = FALSE)
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
                       transpose = FALSE,
                       row.names.to.remove = NULL,
                       column.names.to.remove = NULL)
    expect_equal(out$categories.title, "Gender")
    expect_equal(out$values.title, "%")
    expect_equal(attr(out$data, "statistic"), attr(input.data.table, "statistic"))
    expect_is(out$data,  "matrix")
    expect_equal(dim(out$data), dim(input.data.table))
    expect_equal(round(out$data[1,1],1), 48.4)

    out2 <- PrepareData("Column", input.data.table = input.data.table, transpose = TRUE)
    expect_equal(out2$categories.title, "Age")

    out3 <- PrepareData("Column", input.data.table = input.data.table, hide.percent.symbol = TRUE)
    expect_equal(out3$categories.title, "Gender")
    expect_equal(out3$values.title, "")
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
    expect_equal(out2$data[1,1], 11.31)

    dat3 <- list(structure(c("", "Main title", "", "", "", "Product", "", "",
                "", "", "", "", "", "", "", "", "", "", "Coke", "Diet Coke",
                "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max", "None of these",
                "NET", "", "", "", "Attribute", "Feminine", "0.064220183", "0.574923547",
                "0.22324159", "0.085626911", "0.605504587", "0.100917431", "0.097859327",
                "1", "", "", "", "", "Health-conscious", "0.018348624", "0.587155963",
                "0.550458716", "0.021406728", "0.577981651", "0.308868502", "0.174311927",
                "1", "", "", "", "", "Innocent", "0.091743119", "0.229357798",
                "0.128440367", "0.097859327", "0.434250765", "0.073394495", "0.29969419",
                "1", "", "", "", "", "Older", "0.651376147", "0.217125382", "0.04587156",
                "0.379204893", "0.091743119", "0.064220183", "0.085626911", "1",
                "", "", "", "", "Open to new experiences", "0.226299694", "0.091743119",
                "0.519877676", "0.155963303", "0.162079511", "0.504587156", "0.119266055",
                "1", "", "", "", "", "Rebellious", "0.262996942", "0.04587156",
                "0.314984709", "0.177370031", "0.039755352", "0.44648318", "0.159021407",
                "1", "", "", "", "", "Sleepy", "0.091743119", "0.235474006",
                "0.091743119", "0.143730887", "0.296636086", "0.064220183", "0.388379205",
                "1", "", "", "", "", "Traditional", "0.923547401", "0.146788991",
                "0.03058104", "0.5382263", "0.033639144", "0.039755352", "0.027522936",
                "1", "", "", "", "", "Weight-conscious", "0.006116208", "0.764525994",
                "0.645259939", "0", "0.764525994", "0.406727829", "0.055045872",
                "1", "", "", "", "", "NET", "0.981651376", "0.923547401", "0.908256881",
                "0.788990826", "0.951070336", "0.868501529", "0.574923547", "1"
                ), .Dim = c(13L, 12L)), NULL, NULL, NULL, NULL)
        out3 <- PrepareData("Column", input.data.pasted = dat3)
        expect_equal(out3$chart.title, "Main title")
        expect_equal(out3$categories.title, "Product")

        out4 <- PrepareData("Column", input.data.pasted = dat3, transpose = TRUE)
        expect_equal(out4$categories.title, "Attribute")
})

test_that("PrepareData: raw data with labels",
{
    pp <- PrepareData("Time Series", input.data.raw = list(X = list(Date=Sys.Date()+1:10, A=1:10, B=2:11)))
    expect_equal(dim(pp$data), c(10, 2))
    expect_equal(pp$categories.title, "Date")
    expect_error(CChart("Time Series", pp$data), NA)

    filt <- rep(c(0, 1), 5)
    attr(filt, "label") <- "Every second day"
    pp <- PrepareData("Time Series", input.data.raw = list(X = list(Date=Sys.Date()+1:10, A=1:10)),
        subset = filt)
    expect_equal(dim(pp$data), c(5, 1))
    expect_equal(colnames(pp$data), "Every second day")
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
    expect_error(out <- PrepareData(input.data.pasted = pasted, chart.type = chart.type,
                   subset = QFilter, weights = QPopulationWeight), NA)
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
                   "Data has been automatically converted to numeric.")
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

    out <- suppressWarnings(PrepareData(input.data.raw = input.data.raw, chart.type = "Venn", subset = QFilter, hide.empty.rows = FALSE, hide.empty.columns = FALSE))
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
    expect_error(out <-PrepareData(input.data.raw = dat, chart.type = "Bubble"), NA)
                   #"^Some categories do not appear", NA)
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

    res.filter.counts <- PrepareData("Column", input.data.raw = list(X = zvec), first.aggregate = TRUE,
        as.percentages = FALSE, subset = zvec ==3)$data
    res.filter.percent <- PrepareData("Column", input.data.raw = list(X = zvec), first.aggregate = TRUE,
        as.percentages = TRUE, subset = zvec ==3)$data
    expect_equal(res.filter.counts, structure(c(`3` = 3L), statistic = "Count"))
    expect_equal(res.filter.percent, structure(c(`3` = 100L), statistic = "%"))
})

test_that("PrepareData: input and output format of raw data",
{
    set.seed(1234)
    xx <- rpois(100, 4)
    xf <- as.factor(xx)
    yy <- rpois(100, 2)
    y2 <- rpois(100, 2)
    attr(xx, "label") <- "VarA"
    attr(xf, "label") <- "VarA"
    attr(yy, "label") <- "VarB"
    attr(y2, "label") <- "VarC"

    gender <- as.factor(sample(c("Male", "Female"), 100, replace = TRUE))
    fav.food <- as.factor(sample(c("Chocolate", "Ice cream", "Chips", "Fruit", "Nuts"), 100, replace = TRUE))
    fav.drink <- as.factor(sample(c("Cola", "Juice", "Water"), 100, replace = TRUE))
    attr(gender, "label") <- "Gender"
    attr(fav.food, "label") <- "Favourite Food"
    attr(fav.drink, "label") <- "Favourite Drink"

    res1 <- PrepareData("Table", input.data.raw = list(X = list(fav.food)),
                first.aggregate = TRUE, categorical.as.binary = TRUE)$data
    expect_equal(rownames(res1), levels(fav.food))
    res2 <- PrepareData("Table", input.data.raw = list(X = list(fav.food, fav.drink)),
                first.aggregate = TRUE, categorical.as.binary = TRUE)$data
    expect_equal(rownames(res2), c(levels(fav.food), levels(fav.drink)))
    expect_warning(res3 <- PrepareData("Table", input.data.raw = list(X = list(fav.food, fav.drink)),
                first.aggregate = TRUE, categorical.as.binary = FALSE)$data)
    expect_equal(rownames(res3), c("Favourite Food", "Favourite Drink"))


    res4 <- PrepareData("Table", input.data.raw = list(X = list(fav.food), Y = gender),
                first.aggregate = TRUE, categorical.as.binary = TRUE)$data
    expect_equal(rownames(res4), rownames(res1))
    res5 <- PrepareData("Table", input.data.raw = list(X = list(fav.food, fav.drink), Y = gender),
                first.aggregate = TRUE, categorical.as.binary = TRUE)$data
    expect_equal(rownames(res5), rownames(res2))
    expect_warning(res6 <- PrepareData("Table", input.data.raw = list(X = list(fav.food, fav.drink), Y = gender),
                first.aggregate = TRUE, categorical.as.binary = FALSE)$data)
    expect_equal(rownames(res6), rownames(res3))


    # Multiple variables in Y are concatenated
    res6 <- PrepareData("Scatter", input.data.raw = list(X = xx, Y = list(yy, y2)), tidy.labels = TRUE)
    expect_equal(dim(res6$data), c(200, 3))
    expect_true(attr(res6$data, "scatter.mult.yvals"))
    expect_equal(res6$scatter.variable.indices, c(x = 1, y = 2, sizes = 0, colors = 3, groups = 3))
    expect_equal(as.character(res6$data[101,3]), "VarC")

    expect_warning(res7 <- PrepareData("Scatter", input.data.raw = list(X = xx, Y = list(yy, y2)),
        tidy.labels = TRUE, transpose = TRUE))
    expect_equal(res6$data, res7$data)

    # Duplicated variables
    res7 <- PrepareData("Scatter", input.data.raw = list(X = yy, Y = yy), tidy.labels = TRUE,
                        hide.empty.rows = FALSE, hide.empty.columns = FALSE)
    expect_equal(dim(res7$data), c(100, 1))
    expect_equal(res7$scatter.variable.indices, c(x = 1, y = 1, sizes = NA, colors = NA, groups = NA))

    res8 <- PrepareData("Scatter", input.data.raw = list(X = xx, Y = yy), tidy.labels = TRUE)
    expect_equal(dim(res8$data), c(100, 2))
    expect_equal(res8$scatter.variable.indices, c(x = 1, y = 2, sizes = NA, colors = NA, groups = NA))

    res1 <- PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = FALSE)
    expect_equal(res1$values.title, "")
    res1 <- PrepareData("Column", input.data.raw = list(X = xx), first.aggregate = TRUE)
    expect_equal(res1$values.title, "Count")
    expect_equal(res1$categories.title, "VarA")
    expect_true(is.null(dimnames(res1$data)))

    expect_error(res2 <- PrepareData("Column", input.data.raw = list(X = xf, Y = yy), first.aggregate = FALSE), NA)
    expect_equal(res2$values.title, "Counts")
    expect_error(res2 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = FALSE), NA)
    expect_equal(res2$values.title, "Average")

    res2 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy))
    expect_equal(res2$values.title, "Average")
    res2 <- PrepareData("Column", input.data.raw = list(X = xf, Y = yy), first.aggregate = TRUE, as.percentages = TRUE)
    expect_equal(res2$values.title, "%")
    expect_equal(res2$categories.title, "VarA")
    expect_equal(names(dimnames(res2$data)), c("VarA", "VarB"))
    res2b <- PrepareData("Column", input.data.raw = list(X = xf, Y = yy), first.aggregate = TRUE,
                hide.percent.symbol = TRUE, as.percentages = TRUE)
    expect_equal(res2b$values.title, "")
    expect_equal(res2b$categories.title, "VarA")
    expect_equal(names(dimnames(res2b$data)), c("VarA", "VarB"))

    res3 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = TRUE,
                        group.by.last = TRUE,
                        as.percentages = TRUE, transpose = TRUE)
    expect_equal(res3$values.title, "%")
    expect_equal(res3$categories.title, "VarB")
    expect_equal(length(res3$data), length(unique(yy)))

    res3 <- suppressWarnings(PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = FALSE,
                        group.by.last = TRUE,
                        as.percentages = TRUE)) # transpose = TRUE))
    expect_equal(res3$values.title, "%")
    expect_equal(names(res3$data), as.character(0:7))
    expect_equal(res3$categories.title, "VarB")

    #res3 <- suppressWarnings(PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = FALSE,
    #                    as.percentages = TRUE, transpose = FALSE))
    #expect_equal(res3$values.title, "%")
    #expect_equal(names(res3$data), as.character(c(0:7,10)))
    #expect_equal(res3$categories.title, "VarA")

    res3 <- PrepareData("Column", input.data.raw = list(X = xx, Y = yy), first.aggregate = TRUE,
                        as.percentages = TRUE, tidy.labels = TRUE)
    expect_equal(res3$values.title, "%")
    expect_equal(names(res3$data), as.character(0:7))


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

    PrepareData("Table", input.data.raw = list(X = list(fav.food, fav.drink), Y = gender))
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
        pd <- PrepareData(ct, input.data.raw = list(X = as.factor(x), Y = y), first.aggregate = TRUE)
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

    filt <- structure(rep(c(0,0,1), length = nrow(colas)), label = "Random subset")
    pd <- PrepareData("Column", input.data.raw = list(X=colas$d1), first.aggregate = T, subset = filt)
    expect_equal(colnames(pd$data), "Random subset")

    pd <- PrepareData("Donut", input.data.raw = list(X=colas$d1), first.aggregate = T, subset = filt)
    expect_true(is.null(dim(pd$data)))

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
    expect_equal(length(w), 1)
    expect_equal(w[1], "Variables containing duplicated variable names have been removed (give the variables unique names if you do not want this to happen): Age.")
    pd <- PrepareData("Scatter", TRUE, NULL, input.data.raw = z,
                      transpose = FALSE, first.aggregate = FALSE,
                      tidy = FALSE, data.source = "Link to variables in 'Data'")
    expect_equal(NCOL(pd$data), 3)
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
    expect_equal(sum(pd$data), 100)
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
    expect_equal(unname(pd$data[1]), 13.455657, tol = 0.000001)
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

 # Creating a crosstab with two variables
 zz = PrepareData("Column", input.data.raw = list(z[, -2]), as.percentages = FALSE, first.aggregate = TRUE, group.by.last = TRUE)
 expect_equal(zz$data, c('1' = 1.5, '2' = 1.5), check.attributes = FALSE)
 zz = PrepareData("Column", input.data.pasted = list(z[, -2]), as.percentages = FALSE, first.aggregate = TRUE, group.by.last = TRUE)
 expect_equal(zz$data, c('1' = 1.5, '2' = 1.5), check.attributes = FALSE)

 # Creating a crosstab with three variables
 z = matrix(c(1,1,1,1,2,2,2,2,rep(4,8), 1,1,2,2,1,1,2,2), ncol = 3, dimnames = list(1:8, LETTERS[1:3]))
 zz = PrepareData("Column", input.data.raw = list(z), as.percentages = FALSE, first.aggregate = TRUE, group.by.last = TRUE)
 expect_equal(rownames(zz$data), LETTERS[1:2])

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
     expect_warning(Venn(zz$data))
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
    z1 = PrepareData("Column", input.data.raw = list(X = 10:6, Y = c(1,1,1,2,2)),
                               first.aggregate = TRUE, group.by.last = TRUE)
    expect_equal(z1$data[1:2], c('1' = 9.0, '2' = 6.5))
    z2 = PrepareData("Column", input.data.raw = list(X = 10:6, Y = c(1,1,1,2,2)),
                               first.aggregate = TRUE, group.by.last = FALSE)
    expect_equal(z1$data, z2$data)
    z3 = PrepareData("Column", input.data.raw = list(X = 10:6, Y = c(1,1,1,2,2)),
                               first.aggregate = NULL, group.by.last = FALSE)
    expect_equal(z1$data, z3$data)
    #expect_warning(z <- PrepareData("Column", input.data.raw = list(X = list(A = 1:5, B = 2:6), Y = c(1,2,1,2,1))),
    #               "'Groups' variable ignored if more than one input variable is selected")
    #expect_equal(z$categories.title, "A")
    #expect_equal(z$values.title, "B")

    # Pasted data
    zz = list(matrix(c("X", 1,2,1,1,1,"Y", 1,2,1,2,1), ncol = 2))
    z = PrepareData("Column", input.data.pasted = zz,first.aggregate = TRUE, group.by.last = FALSE)
    expect_equal(as.numeric(z$data), c(1.2, 1.4))
    z = PrepareData("Scatter", input.data.pasted = zz, first.aggregate = FALSE, group.by.last = FALSE)
    expect_equal(as.numeric(z$data), as.numeric(zz[[1]][-1,]))

    z2 = list(matrix(c("X", 5,4,3,2,1,"Y", 1,2,1,2,1), ncol = 2))
    z = expect_error(PrepareData("Column", input.data.pasted = z2, first.aggregate = FALSE,
                        group.by.last = FALSE), NA)
    expect_equal(z$categories.title, "X")
    expect_equal(z$values.title, "Y")

    # Pasted data with an irrelevant middle column
    zz = list(matrix(c(1,2,1,1,1,NA, 4, NA, 3, NA, 1,2,1,2,1), ncol = 3, dimnames = list(1:5, c("X","Irrelevant", "Y"))))
    z = suppressWarnings(PrepareData("Column", input.data.pasted = zz, first.aggregate = TRUE, group.by.last = TRUE))
    #expect_equal(z$data[1,1], 3)
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

    out <- PrepareData(chart.type = "Table", input.data.pasted = pasted, tidy = FALSE,
                       hide.empty.rows.and.columns = FALSE, date.format = "No date formatting")$data
    expect_is(out, "data.frame")
    expect_named(out, "Date times")
    expect_is(out[[1L]], "character")

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
    intl.dates <- format(as.Date(sprintf("2017-01-%02d", 1:10)), "%d %b %Y")

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

test_that("Scatter x-axis tick order",
{
    raw.dat <- list(X = c("Value for money", "Internet speed", "Network coverage",
"Understand the plans", "Support", "Upgrade/downgrade plans",
"Understand your bill", "Cancel", "Check internet usage"), Y = list(
    table.Performance.by.Main.phone.company.3 = structure(c(3.85304054054054,
    4.02364864864865, 4.11993243243243, 3.83304347826087, 3.87033747779751,
    3.86213235294118, 3.98795180722892, 3.67901234567901, 4.03368794326241,
    3.75087108013937, 4.13414634146341, 4.33449477351916, 3.89087656529517,
    3.8690036900369, 3.93371212121212, 4.08152173913043, 3.68913043478261,
    4.10575139146568, 4.2483660130719, 4.2156862745098, 4.24183006535948,
    4.09797297297297, 4.09756097560976, 4, 4.12627986348123,
    3.97254901960784, 4.18118466898955, 3.66824644549763, 3.7345971563981,
    3.66824644549763, 3.57, 3.65151515151515, 3.65957446808511,
    3.85643564356436, 3.4327485380117, 4.01041666666667, 4.35757575757576,
    3.98787878787879, 4.01818181818182, 4.33128834355828, 4.09554140127389,
    4.25625, 4.37654320987654, 4.16447368421053, 4.28125, 4.44715447154472,
    3.95121951219512, 3.89430894308943, 4.47933884297521, 4.05932203389831,
    4.29310344827586, 4.475, 4.09259259259259, 4.24137931034483,
    4.06903353057199, 4.05877712031558, 4.15897435897436, 4.0514378290806,
    3.90654985398415, 3.99698795180723, 4.15860105734038, 3.87815326035221,
    4.13693998309383, 4.50531914893617, 4.09929078014184, 4.25886524822695,
    4.41261261261261, 3.88533834586466, 4.18110236220472, 4.41165755919854,
    4.23880597014925, 4.23818897637795), .Dim = 9:8, statistic = "Average", .Dimnames = list(
        c("Value for money", "Internet speed", "Network coverage",
        "Understand the plans", "Support", "Upgrade/downgrade plans",
        "Understand your bill", "Cancel", "Check internet usage"
        ), c("AT&T", "Verizon", "T-Mobile", "Sprint", "Metro PCS",
        "Boost Mobile", "NET", "Other")), name = "table.Performance.by.Main.phone.company.3", questions = c("Performance",
    "Main phone company"))), Z1 = NULL, Z2 = NULL, groups = NULL,
    labels = NULL)
    pd <- PrepareData("Scatter", input.data.raw = raw.dat)
    expect_is(pd$data[,1], "character")
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
    expect_warning(res <- PrepareData("Table", input.data.table = mat.string, tidy.labels = TRUE),
        "'Row' has been removed from labels.")
    expect_equal(res$categories.title, "Row")

    pst <- structure(c("Q1 - A", "1", "2", "3", "4", "5", "2", "5", "7",
        "5", "", "Q1 - B", "0", "0", "0", "0", "5", "8", "7", "1", "2",
        "1", "Q1 - C", "9", "9", "9", "9", "7", "7", "6", "", "", ""),
        .Dim = c(11L, 3L))
    expect_warning(res <- PrepareData("Density", input.data.pasted=list(pst, FALSE), tidy.labels = TRUE),
        "'Q1' has been removed from labels.")
    expect_equal(colnames(res$data), c('A','B','C'))

    datL <- list('Q1 - A' = rnorm(20), 'Q1 - B' = rnorm(30), 'Q1 - C' = rnorm(30))
    expect_warning(resL <- PrepareData("Density", input.data.table = datL, tidy.labels = TRUE))
    expect_equal(names(resL$data), c('A','B','C'))

    datV = list('Q1 - A' = rbinom(10, 1, 0.5), 'Q1 - B' = rbinom(10, 1, 0.2),
                'Q1 - C' = rbinom(10, 1, 0.9))
    expect_warning(resV <- PrepareData("Venn", input.data.raw = datV, tidy.labels = TRUE))
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
    expect_silent(res <- PrepareData("Scatter", input.data.table = dfL))
    expect_equal(rownames(res$data[[2]]), rownames(dfL[[2]]))
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


    # Default rownames which shouldn't be discarded
    x2 <- structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 35,
            51, 39, 13, 1, 0, 1, 19, 125, 218, 117, 19, 1, 0, 1, 2, 17, 19,
            5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(7L,
            7L), .Dimnames = list(Observed = c("1", "2", "3", "4", "5", "6",
            "7"), Predicted = c("1", "2", "3", "4", "5", "6", "7")), type = "count", accuracy = 0.419825072886297, outcome.label = "Overall", description = "Fitted model : n = 686 cases used in estimation of a total sample size of 896; cases containing missing values have been excluded;  686 observed/predicted pairs with 41.98% accuracy;", decimals = 0, title = "Prediction-Accuracy Table: Overall", footer = "Fitted model : n = 686 cases used in estimation of a total sample size of 896; cases containing missing values have been excluded;  686 observed/predicted pairs with 41.98% accuracy;")
    expect_error(res2 <- PrepareData("Column", input.data.table = x2, hide.empty.columns = FALSE), NA)
    expect_equal(dim(x2), dim(res2$data))
})

test_that("Discard rownames from filtered raw data",
{
    filt <- structure(c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
               name = "largefilter", label = "Very large values")
    raw <- structure(list(X = list(structure(c(0.287577520124614, 0.788305135443807,
               0.896738682640716, 0.308119554305449, 0.363300543511286, 0.783946478739381),
               questiontype = "Number", name = "v1", label = "Variable A", question = "Variable A"),
               structure(c(0.386757359839976, 0.0355316237546504, 0.813215732574463,
               1.74976477492601, 0.1, 0.2), questiontype = "Number", name = "v2",
               label = "Variable B", question = "Variable B")), Y = NULL, Z1 = NULL, Z2 = NULL,
               labels = NULL), .Names = c("X","Y", "Z1", "Z2", "labels"))

    res.unfilt <- PrepareData("Column", input.data.raw = raw)
    res.filt <- PrepareData("Column", input.data.raw = raw, subset = filt)
    expect_equal(NCOL(res.unfilt$data), 1)
    expect_equal(NCOL(res.filt$data), 1)
})


test_that("Aggregate numeric data",
{
    rain.by.month <- structure(list(X = list(structure(c(177.6, 183.5, 25.7, 11.7,
        24.9, 109.8, 119.2, 7.6, 277.3, 7.7, 33.4, 92.4,
        166.9, 275.8, 132.4, 508, 4.5, 72, 303.9, 240.8, 72.3, 103.5, 185.5, 44.9,
        91.8, 83.1, 111.7, 622.1, 40.4, 47, 121.7, 196.3, 45.3, 69, 41.3, 15.4,
        94.4, 120.6, 47.8, 35.3, 37.4, 84.7, 3.2, 49.2, 15.7, 18.8, 25.8, 11.2),
        questiontype = "Number", name = "MonthlyRainfall", label = "Monthly Rainfall",
        question = "Monthly Rainfall")),
        Y = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L,
                        1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L,
                        1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L,
                        1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L),
            class = "factor", .Label = c("Jan", "Feb", "Mar", "Apr",
            "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), questiontype = "PickOne",
            name = "Month", label = "Month", question = "Month"),  Z1 = NULL, Z2 = NULL,
            labels = NULL), .Names = c("X", "Y",  "Z1", "Z2", "labels"))
    wgts <- 1:48

    res1 <- PrepareData("Column", input.data.raw = rain.by.month)
    expect_equal(res1$data[1:12], structure(c(132.675, 165.75, 79.4, 294.275,
        26.8, 78.375, 137, 123.475, 102.65, 49.75, 71.5, 40.975),
        .Names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
        "Sep", "Oct", "Nov", "Dec")))

    res2 <- PrepareData("Column", input.data.raw = rain.by.month, weights = wgts)
    expect_equal(res2$data[1:12], structure(c(107.040789473684, 137.145,
        82.6571428571429, 306.881818181818, 31.5869565217391, 72.10625, 105.188,
        128.107692307692, 57.55, 49.6857142857143, 62.8620689655172, 27.32),
        .Names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
        "Sep", "Oct", "Nov", "Dec")))
})

df <- structure(list(Country = c("Macau", "Qatar", "Singapore", "Australia",
"Hong Kong", "New Zealand", "Israel", "Japan", "United Arab Emirates",
"South Korea", "Brunei", "Kuwait", "Taiwan", "Bahrain", "Saudi Arabia",
"Oman", "Palau", "Seychelles", "Maldives", "Lebanon", "Turkey",
"Malaysia", "Mauritius", "Kazakhstan", "China", "Nauru", "Turkmenistan",
"Thailand", "Suriname", "Fiji", "Jordan", "Iran", "Iraq", "Samoa",
"Tonga", "Azerbaijan", "Sri Lanka", "Indonesia", "Armenia", "Mongolia",
"Tuvalu", "Marshall Islands", "Micronesia", "Vanuatu", "Philippines",
"Bhutan", "Papua New Guinea", "Laos", "Vietnam", "East Timor",
"Solomon Islands", "India", "Kiribati", "Bangladesh", "Pakistan",
"Myanmar", "Kyrgyzstan", "Nepal", "Tajikistan", "Afghanistan",
"Yemen"), `GDP USD` = c(77451, 60804, 57713, 55707, 46109, 41593,
40258, 38440, 37226, 29891, 29712, 27319, 24577, 24029, 21120,
17973, 17096, 15686, 12527, 11409, 10512, 9813, 9794, 8841, 8643,
8575, 6643, 6591, 5746, 5740, 5678, 5305, 5088, 4253, 4177, 4141,
4084, 3876, 3861, 3640, 3638, 3625, 3200, 3094, 2976, 2903, 2861,
2542, 2354, 2104, 2081, 1983, 1721, 1602, 1541, 1264, 1144, 834,
824, 588, 551)), row.names = c(3L, 7L, 9L, 11L, 16L, 21L, 22L,
25L, 26L, 30L, 31L, 33L, 36L, 37L, 40L, 44L, 47L, 53L, 62L, 64L,
67L, 70L, 71L, 74L, 75L, 76L, 86L, 87L, 93L, 94L, 95L, 98L, 100L,
111L, 112L, 113L, 115L, 118L, 119L, 120L, 121L, 122L, 126L, 128L,
129L, 130L, 131L, 134L, 136L, 139L, 140L, 143L, 146L, 150L, 151L,
159L, 161L, 166L, 168L, 180L, 181L), class = "data.frame")

pasted.data <- list(structure(c("", "", "", "", "", "", "", "", "", "France",
"Germany", "Spain", "", "", "", "3%", "7%", "5%"), .Dim = c(6L,
3L)), NULL, NULL, NULL)

test_that("PrepareData: Data frame with country names",
{
    expect_error(res <- PrepareData("Geographic Map", input.data.table = df), NA)
    expect_equal(names(res$data)[1], "Macau")

    expect_error(res <- PrepareData("Geographic Map", input.data.pasted = pasted.data), NA)
    expect_equal(attr(res$data, "statistic"), "%")
})

test_that("Question attribute is not accidently dropped",
{
    tb <- structure(c(86.5979381443299, 84.8130841121495, 80.3446075303127,
80.2355303998539, 79.6610169491525, 79.3420416061926, 78.9456252800561,
77.970592923175, 76.4833261152014, 74.2729306487696, 72.7629773340273,
70.7792207792208, 68.0959302325581, 66.887417218543, 63.6178861788618,
63.013698630137), .Dim = c(16L, 1L), statistic = "Row %", .Dimnames = list(
    c("Pantry", "Hospital", "Private Club/Event", "School", "Kiosk/Vending",
    "Daycare/Assisted Living", "Restaurants", "NET", "Specialty Food Store",
    "Other", "Grocery", "Retail Store", "Mobile Food", "Gas Station",
    "Wholesale", "Bar"), "Overall Pass Rate"), name = "table.Facility.Type.Coded1.by.Pass", questions = c("Facility Type - Coded1",
"Pass"))
    expect_error(res <- PrepareData("Column", input.data.table = tb), NA)
    expect_equal(attr(res$data, "questions"), c("Facility Type - Coded1", "Pass"))
    expect_equal(res$data[1], c(Pantry = 86.5979381443299))
})

test_that("Dimensions are dropped consistently",
{
    pasted1d <- list(structure(c("", "", "", "", "", "", "", "", "", "", "",
    "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
    "50 to 54", "55 to 64", "65 or more", "", "13.45565749", "11.9266055",
    "10.09174312", "11.00917431", "10.70336391", "8.256880734", "12.2324159",
    "15.59633028", "6.727828746"), .Dim = c(10L, 3L)), FALSE, NULL,
        NULL)
    res1 <- PrepareData("Column", input.data.pasted = pasted1d)
    res2 <- PrepareData("Table", input.data.pasted = pasted1d)
    expect_equal(length(dim(res1$data)), 0)
    expect_equal(length(dim(res2$data)), 0)

    tab2d <- structure(c(7, 15.5, 15.5, 22, 20.5, 18, 1.5, 100, 11, 16.5,
        17, 17, 17, 21.5, 0, 100, 33.3333333333333, 0, 33.3333333333333,
        0, 0, 0, 33.3333333333333, 100, 9.18114143920596, 15.8808933002481,
        16.3771712158809, 19.3548387096774, 18.6104218362283, 19.6029776674938,
        0.992555831265509, 100), statistic = "Column %", .Dim = c(8L,
        4L), .Dimnames = list(c("15 to 24 years", "25 to 34 years", "35 to 44 years",
        "45 to 54 years", "55 to 64 years", "65 years and over", "Under 15 years",
        "NET"), c("Female", "Male", "Other / Prefer not to say", "NET"
        )), name = "table.Q3.What.is.your.age.by.Q1.Are.you.", questions = c("Q3. What is your age?",
        "Q1. Are you..."))

    res1 <- PrepareData("Column", input.data.table = tab2d, select.columns = "Female")
    expect_equal(dim(res1$data), c(7, 1))

    res2 <- PrepareData("Pie", input.data.table = tab2d, select.columns = "Female")
    expect_equal(length(res2$data), 7)
    expect_equal(colnames(res2$data), NULL)

    res3 <- PrepareData("Pie", input.data.table = tab2d, select.columns = "Female", tidy = FALSE)
    expect_equal(dim(res3$data), c(7, 1))

    res4 <- PrepareData("Column", input.data.pasted = pasted1d, tidy = TRUE, column.labels = "LabelOne")
    expect_equal(colnames(res4$data), "LabelOne")
})


test_that("Axis and Series names are both preserved",
{
    dat <- list(X = list(`Variable A` = structure(c(0.287577520124614, 0.788305135443807,
    0.323344993172213, 0.835255319951102, 0.143817043630406, 0.192815946880728,
    0.896738682640716, 0.308119554305449, 0.363300543511286, 0.783946478739381
    ), questiontype = "Number", name = "v1", label = "Variable A", question = "Variable A"),
    `Variable B` = structure(c(0.386757359839976, 0.0355316237546504,
    0.837338316719979, 1.43576909322292, 1.4850782644935, 1.74399764742702,
    1.58644833555445, 0.0457093669101596, 0.833829099312425,
    1.74976477492601), questiontype = "Number", name = "v2", label = "Variable B", question = "Variable B")),
    Y = NULL, Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL)
    filt <- structure(rep(c(0, 1), length = 10), label = "Filter ABC")

    pd <- PrepareData("Column", input.data.raw = dat, subset = filt)
    expect_equal(pd$categories.title, "Variable A")
    expect_equal(pd$values.title, "Variable B")
    expect_equal(colnames(pd$data), "Filter ABC")

    dat1 <- list(X = list(Age = structure(c(2L, 6L, 8L, 8L, 4L, 9L, 6L, 3L,
        9L, 9L, 5L, 9L, 1L, 9L, 5L, 8L, 8L, 8L, 7L, 8L, 2L, 8L, 1L, 4L,
        2L, 9L, 5L, 6L, 4L, 1L, 2L, 5L, 4L, 5L, 8L, 4L, 2L, 4L, 2L, 6L,
        5L, 6L, 6L, 8L, 1L, 2L, 9L, 6L, 7L, 8L, 9L, 8L, 3L, 5L, 4L, 6L,
        8L, 4L, 5L, 6L, 4L, 8L, 4L, 7L, 7L, 4L, 2L, 3L, 3L, 7L, 1L, 7L,
        2L, 8L, 8L, 4L, 9L, 6L, 6L, 8L, 3L, 3L, 4L, 7L, 8L, 3L, 2L, 4L,
        8L, 7L, 8L, 3L, 8L, 3L, 6L, 5L, 6L, 5L, 6L, 2L, 4L, 5L, 5L, 1L,
        5L, 3L, 1L, 5L, 6L, 3L, 3L, 6L, 7L, 5L, 8L, 9L, 3L, 1L, 1L, 7L,
        3L, 9L, 6L, 1L, 8L, 8L, 8L, 1L, 1L, 1L, 3L, 9L, 6L, 5L, 4L, 5L,
        7L, 4L, 2L, 5L, 2L, 4L, 2L, 1L, 1L, 9L, 3L, 1L, 7L, 5L, 8L, 3L,
        1L, 1L, 3L, 9L, 5L, 1L, 5L, 1L, 1L, 9L, 4L, 1L, 7L, 1L, 1L, 3L,
        2L, 2L, 2L, 6L, 4L, 8L, 7L, 7L, 8L, 5L, 2L, 5L, 4L, 8L, 5L, 4L,
        5L, 2L, 8L, 8L, 1L, 3L, 5L, 9L, 8L, 3L, 6L, 7L, 3L, 8L, 8L, 8L,
        1L, 2L, 5L, 7L, 9L, 2L, 3L, 4L, 2L, 9L, 9L, 5L, 9L, 1L, 7L, 8L,
        1L, 8L, 7L, 8L, 8L, 1L, 6L, 8L, 5L, 7L, 5L, 7L, 4L, 8L, 2L, 9L,
        6L, 7L, 5L, 8L, 6L, 4L, 7L, 7L, 2L, 9L, 6L, 7L, 8L, 9L, 2L, 3L,
        3L, 8L, 8L, 7L, 5L, 5L, 4L, 2L, 4L, 7L, 1L, 2L, 8L, 4L, 7L, 7L,
        4L, 2L, 3L, 3L, 7L, 3L, 3L, 7L), class = "factor", .Label = c("18 to 24",
        "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
        "55 to 64", "65 or more"), questiontype = "PickOne", name = "d1", label = "Age", question = "Age")),
    Y = NULL, Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL)
    filt1 <- structure(rep(c(0, 1), length = length(dat1$X[[1]])), label = "Another random filter")
    pd <- PrepareData("Column", input.data.raw = dat1, first.aggregate = TRUE, subset = filt1)
    expect_equal(colnames(pd$data), "Another random filter")
    expect_equal(pd$values.title, "Count")
    expect_equal(pd$categories.title, "Age")

    dat2 <- list(X = list(`How many SMS sent in typical week` = structure(c(2,
        10, 10, 50, 10, 15, 3, 0, 5, 0, NA, 6, 1, 0, 35, 0, 20, 15, 0,
        20, 10, 10, 30, 0, 0, 0, 12, 0, 20, 0, 0, 30, 0, 2, 5, 0, 20,
        100, 70, 0, 4, NA, 10, 8, 50, 0, 0, 35, 30, 10, 0, 0, 30, 20,
        2, 15, 25, 20, 2, 10, 20, 2, 15, 30, 20, 0, 50, 2, 70, 10, 20,
        1, 3, 10, 7, 10, 3, 5, NA, 1, 10, NA, NA, 40, 20, 20, 5, 30,
        6, 1, NA, 10, 15, 0, 5, 0, 0, 3, 2, 4, 20, 50, 12, 10, 6, 0,
        1, 10, 5, 1, 40, 30, 0, 20, 0, 0, 8, 30, 20, 21, 15, 3, 0, 0,
        48, 30, 20, 25, 0, 0, 10, 30, 0, 70, 0, 10, 5, 10, 10, 0, 5,
        10, 2, 10, 5, 5, 30, 15, 15, 0, 0, 0, 2, 15, 6), questiontype = "Number",
        name = "q25", label = "How many SMS sent in typical week",
        question = "How many SMS sent in typical week")),
        Y = NULL, Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL)
    filt2 <- structure(rep(c(0, 1, 1), length = length(dat2$X[[1]])), label = "More random numbers")
    pd <- PrepareData("Histogram", input.data.raw = dat2, subset = filt2)
    expect_equal(attr(pd$data[[1]], "label"), "How many SMS sent in typical week")

    expect_error(pd <- PrepareData("Scatter", input.data.raw = dat2, subset = filt2), NA)
    expect_equal(length(pd$scatter.variable.indices), 5)

    wrong.dataset <- list(X = structure(c(5.1, 4.9, 4.7, 4.6, 5, 5.4, 4.6, 5, 4.4,
        4.9, 5.4, 4.8, 4.8, 4.3, 5.8, 5.7, 5.4, 5.1, 5.7, 5.1, 5.4, 5.1,
        4.6, 5.1, 4.8, 5, 5, 5.2, 5.2, 4.7, 4.8, 5.4, 5.2, 5.5, 4.9,
        5, 5.5, 4.9, 4.4, 5.1, 5, 4.5, 4.4, 5, 5.1, 4.8, 5.1, 4.6, 5.3,
        5, 7, 6.4, 6.9, 5.5, 6.5, 5.7, 6.3, 4.9, 6.6, 5.2, 5, 5.9, 6,
        6.1, 5.6, 6.7, 5.6, 5.8, 6.2, 5.6, 5.9, 6.1, 6.3, 6.1, 6.4, 6.6,
        6.8, 6.7, 6, 5.7, 5.5, 5.5, 5.8, 6, 5.4, 6, 6.7, 6.3, 5.6, 5.5,
        5.5, 6.1, 5.8, 5, 5.6, 5.7, 5.7, 6.2, 5.1, 5.7, 6.3, 5.8, 7.1,
        6.3, 6.5, 7.6, 4.9, 7.3, 6.7, 7.2, 6.5, 6.4, 6.8, 5.7, 5.8, 6.4,
        6.5, 7.7, 7.7, 6, 6.9, 5.6, 7.7, 6.3, 6.7, 7.2, 6.2, 6.1, 6.4,
        7.2, 7.4, 7.9, 6.4, 6.3, 6.1, 7.7, 6.3, 6.4, 6, 6.9, 6.7, 6.9,
        5.8, 6.8, 6.7, 6.7, 6.3, 6.5, 6.2, 5.9), questiontype = "NumberGrid",
        name = "Sepal.Length", label = "Sepal.Length", question = "Grid"),
        Y = list(pop15 = structure(c(29.35, 23.32, 23.8, 41.89, 42.19,
        31.72, 39.74, 44.75, 46.64, 47.64, 24.42, 46.31, 27.84, 25.06,
        23.31, 25.62, 46.05, 47.32, 34.03, 41.31, 31.16, 24.52, 27.01,
        41.74, 21.8, 32.54, 25.95, 24.71, 32.61, 45.04, 43.56, 41.18,
        44.19, 46.26, 28.96, 31.94, 31.92, 27.74, 21.44, 23.49, 43.42,
        46.12, 23.27, 29.81, 46.4, 45.25, 41.12, 28.13, 43.69, 47.2
        ), questiontype = "Number", name = "pop15", label = "pop15", question = "pop15")),
        Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL)
    expect_error(pd <- PrepareData("Scatter", input.data.raw = wrong.dataset),
        "Check that all variables are from the same data set.")
})

test_that("Scatter accepts tables as variables",
{
    raw.named <- list(X = structure(c(`Coca-Cola` = 42.625, `Diet Coke` = 11.125,
    `Coke Zero` = 17.875, `Pepsi ` = 9, `Diet Pepsi` = 2.5, `Pepsi Max` = 14.875,
    `Dislike all cola` = 0.75, `Don't care` = 1.25, NET = 100), statistic = "%", .Dim = 9L, .Dimnames = list(
        c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
        "Pepsi Max", "Dislike all cola", "Don't care", "NET")), name = "Preferred cola", questions = c("Preferred cola",
    "SUMMARY")), Y = list(`Preferred cola by Gender` = structure(c(42.7848101265823,
    8.60759493670886, 14.9367088607595, 11.3924050632911, 1.77215189873418,
    17.9746835443038, 0.759493670886076, 1.77215189873418, 100, 42.4691358024691,
    13.5802469135802, 20.7407407407407, 6.66666666666667, 3.20987654320988,
    11.8518518518519, 0.740740740740741, 0.740740740740741, 100,
    42.625, 11.125, 17.875, 9, 2.5, 14.875, 0.75, 1.25, 100), statistic = "Column %", .Dim = c(9L,
    3L), .Dimnames = list(c("Coca-Cola", "Diet Coke", "Coke Zero",
    "Pepsi ", "Diet Pepsi", "Pepsi Max", "Dislike all cola", "Don't care",
    "NET"), c("Male", "Female", "NET")), name = "Preferred cola by Gender", questions = c("Preferred cola",
    "Gender"))), Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL)

    pd <- PrepareData("Scatter", input.data.raw = raw.named)
    expect_equal(dim(pd$data), c(16, 3))
    expect_equal(levels(pd$data[,3]), c("Male", "Female"))
    expect_equal(rownames(pd$data), c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
        "Pepsi Max", "Dislike all cola", "Don't care", "Coca-Cola ",
        "Diet Coke ", "Coke Zero ", "Pepsi  ", "Diet Pepsi ", "Pepsi Max ",
        "Dislike all cola ", "Don't care "))

    raw.unordered <- list(X = structure(1:6, .Names = c("a", "b", "c", "d", "e", "f"
    )), Y = list(v2 = structure(1:6, .Names = c("f", "e", "d", "c",
    "b", "a"))), Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL)
    pd <- PrepareData("Scatter", input.data.raw = raw.unordered)
    expect_equal(rownames(pd$data), letters[1:6])
    expect_equal(pd$data[,1], 1:6)
    expect_equal(pd$data[,2], 6:1)

    raw.qtables <- list(X = structure(c(`Coca-Cola` = 42.625, `Diet Coke` = 11.125,
    `Coke Zero` = 17.875, `Pepsi ` = 9, `Diet Pepsi` = 2.5, `Pepsi Max` = 14.875,
    `Dislike all cola` = 0.75, `Don't care` = 1.25, NET = 100), statistic = "%", .Dim = 9L, .Dimnames = list(
        c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
        "Pepsi Max", "Dislike all cola", "Don't care", "NET")), name = "Preferred cola", questions = c("Preferred cola",
    "SUMMARY")), Y = list(`Preferred cola` = structure(c(`Coca-Cola` = 42.625,
    `Diet Coke` = 11.125, `Coke Zero` = 17.875, `Pepsi ` = 9, `Diet Pepsi` = 2.5,
    `Pepsi Max` = 14.875, `Dislike all cola` = 0.75, `Don't care` = 1.25,
    NET = 100), statistic = "%", .Dim = 9L, .Dimnames = list(c("Coca-Cola",
    "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi", "Pepsi Max",
    "Dislike all cola", "Don't care", "NET")), name = "Preferred cola", questions = c("Preferred cola",
    "SUMMARY"))), Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL)
    pd <- PrepareData("Scatter", input.data.raw = raw.qtables)
    expect_equal(dim(pd$data), c(8, 2))
    expect_equal(rownames(pd$data), names(raw.qtables[[1]])[1:8])

    pd <- PrepareData("Scatter", input.data.raw = raw.qtables, row.names.to.remove = "")
    expect_equal(rownames(pd$data), names(raw.qtables[[1]]))

    raw.2dtable <- list(X = structure(c(5.25, 15, 13.375, 9.5, 17.5, 12.75, 7.875,
    30.75, 20.375, 13, 26, 18, 15.625, 22.5, 26.375, 35, 41.5, 31.625,
    33.875, 12.125, 18.125, 8.875, 5.875, 17.25, 42.375, 24.625,
    26.75, 38.625, 14.125, 25.375, 101, 101, 101, 101, 101, 101), statistic = "Row %", name = "Brand attitude", questions = c("Brand attitude",
    "SUMMARY"), .Dim = c(6L, 6L), .Dimnames = list(c("Coca-Cola",
    "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max"),
        c("Hate", "Dislike", "Neither like nor dislike", "Love",
        "Like", "NET"))), Y = list(tb2 = structure(c(4.89752730700302,
    8.41726680321191, 8.33626305153556, 4.31725350161315, 19.709641146487,
    7.79573592941548, 4.53338740327936, 23.4252473618557, 11.9091996864563,
    -9.45259535649704, 22.0736469813906, 23.9086553156465, 32.3330697551121,
    5.63180932965314, 21.587683239724, 35.0462047743282, 55.3185147044897,
    26.6734769629446, 47.4076082580378, 3.43999988705846, 16.6588705708903,
    0.999241315968322, -11.5619147893862, 26.3789955242066, 36.2421015808696,
    42.8098181057809, 7.45452348744325, 35.4245115147211, 10.8071196148842,
    27.936362298369, 116.426875472851, 103.58300378163, 100.555752828396,
    97.2427029660797, 111.304601659112, 105.671922004027), .Dim = c(6L,
    6L), statistic = "Row %", name = "Brand attitude", questions = c("Brand attitude",
    "SUMMARY"), .Dimnames = list(c("Coca-Cola", "Diet Coke", "Coke Zero",
    "Pepsi", "Diet Pepsi", "Pepsi Max"), c("Hate", "Dislike", "Neither like nor dislike",
    "Love", "Like", "NET")))), Z1 = NULL, Z2 = NULL, groups = NULL,
        labels = NULL)
    expect_warning(pd <- PrepareData("Scatter", input.data.raw = raw.2dtable))
    expect_equal(dim(pd$data), c(30, 3))
    expect_equal(colnames(pd$data)[1], "Hate")
    expect_equal(rownames(pd$data), c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi",
        "Pepsi Max", "Coca-Cola ", "Diet Coke ", "Coke Zero ", "Pepsi ",
        "Diet Pepsi ", "Pepsi Max ", "Coca-Cola  ", "Diet Coke  ", "Coke Zero  ",
        "Pepsi  ", "Diet Pepsi  ", "Pepsi Max  ", "Coca-Cola   ", "Diet Coke   ",
        "Coke Zero   ", "Pepsi   ", "Diet Pepsi   ", "Pepsi Max   ",
        "Coca-Cola    ", "Diet Coke    ", "Coke Zero    ", "Pepsi    ",
        "Diet Pepsi    ", "Pepsi Max    "))

    raw.multiY.and.size <- list(X = structure(c(`Coca-Cola` = 42.625, `Diet Coke` = 11.125,
    `Coke Zero` = 17.875, `Pepsi ` = 9, `Diet Pepsi` = 2.5, `Pepsi Max` = 14.875,
    `Dislike all cola` = 0.75, `Don't care` = 1.25, NET = 100), statistic = "%", .Dim = 9L, .Dimnames = list(
        c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
        "Pepsi Max", "Dislike all cola", "Don't care", "NET")), name = "Preferred cola", questions = c("Preferred cola",
    "SUMMARY")), Y = list(`Preferred cola by Gender` = structure(c(42.7848101265823,
    8.60759493670886, 14.9367088607595, 11.3924050632911, 1.77215189873418,
    17.9746835443038, 0.759493670886076, 1.77215189873418, 100, 42.4691358024691,
    13.5802469135802, 20.7407407407407, 6.66666666666667, 3.20987654320988,
    11.8518518518519, 0.740740740740741, 0.740740740740741, 100,
    42.625, 11.125, 17.875, 9, 2.5, 14.875, 0.75, 1.25, 100), statistic = "Column %", .Dim = c(9L,
    3L), .Dimnames = list(c("Coca-Cola", "Diet Coke", "Coke Zero",
    "Pepsi ", "Diet Pepsi", "Pepsi Max", "Dislike all cola", "Don't care",
    "NET"), c("Male", "Female", "NET")), name = "Preferred cola by Gender", questions = c("Preferred cola",
    "Gender [Cola Tracking - January to December.sav]"))), Z1 = structure(c(`Coca-Cola` = 42.625,
    `Diet Coke` = 11.125, `Coke Zero` = 17.875, `Pepsi ` = 9, `Diet Pepsi` = 2.5,
    `Pepsi Max` = 14.875, `Dislike all cola` = 0.75, `Don't care` = 1.25,
    NET = 100), statistic = "%", .Dim = 9L, .Dimnames = list(c("Coca-Cola",
    "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi", "Pepsi Max",
    "Dislike all cola", "Don't care", "NET")), name = "Preferred cola", questions = c("Preferred cola",
    "SUMMARY")), Z2 = NULL, groups = NULL, labels = NULL)
    expect_warning(pd <- PrepareData("Scatter", input.data.raw = raw.multiY.and.size))
    expect_equal(dim(pd$data), c(8, 4))
    expect_equal(colnames(pd$data)[2], "Male")
    expect_equal(rownames(pd$data), c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ",
                "Diet Pepsi", "Pepsi Max", "Dislike all cola", "Don't care"))
    expect_equal(pd$scatter.variable.indices, c(1, 2, 3, NA, NA), check.attributes = FALSE)

    raw.multi.ytable <- list(X = structure(c(`Less than 18` = 0, `18 to 24` = 13.4556574923547,
        `25 to 29` = 11.9266055045872, `30 to 34` = 10.0917431192661,
        `35 to 39` = 11.0091743119266, `40 to 44` = 10.7033639143731,
        `45 to 49` = 8.25688073394496, `50 to 54` = 12.2324159021407,
        `55 to 64` = 15.5963302752294, `65 or more` = 6.72782874617737,
        NET = 100), statistic = "%", .Dim = 11L, .Dimnames = list(c("Less than 18",
        "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
        "50 to 54", "55 to 64", "65 or more", "NET")), name = "Q3. Age", questions = c("Q3. Age",
        "SUMMARY")), Y = list(`Q3. Age by Q2. Gender` = structure(c(0,
        13.75, 11.25, 9.375, 10, 11.875, 8.125, 11.25, 16.875, 7.5, 100,
        0, 13.1736526946108, 12.5748502994012, 10.7784431137725, 11.9760479041916,
        9.58083832335329, 8.38323353293413, 13.1736526946108, 14.3712574850299,
        5.98802395209581, 100, 0, 13.4556574923547, 11.9266055045872,
        10.0917431192661, 11.0091743119266, 10.7033639143731, 8.25688073394496,
        12.2324159021407, 15.5963302752294, 6.72782874617737, 100), statistic = "Column %", .Dim = c(11L,
        3L), .Dimnames = list(c("Less than 18", "18 to 24", "25 to 29",
        "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
        "65 or more", "NET"), c("Male", "Female", "NET")), name = "Q3. Age by Q2. Gender", questions = c("Q3. Age",
        "Q2. Gender")), `Q3. Age by Q12. How  often do you drink cola with alcohol` = structure(c(0,
        22.2222222222222, 22.2222222222222, 22.2222222222222, 0, 11.1111111111111,
        11.1111111111111, 11.1111111111111, 0, 0, 100, 0, 37.5, 12.5,
        12.5, 25, 0, 0, 0, 12.5, 0, 100, 0, 14.2857142857143, 19.047619047619,
        14.2857142857143, 4.76190476190476, 0, 19.047619047619, 0, 14.2857142857143,
        14.2857142857143, 100, 0, 17.9487179487179, 5.12820512820513,
        12.8205128205128, 15.3846153846154, 10.2564102564103, 5.12820512820513,
        25.6410256410256, 5.12820512820513, 2.56410256410256, 100, 0,
        14.6341463414634, 9.75609756097561, 12.1951219512195, 7.31707317073171,
        12.1951219512195, 14.6341463414634, 12.1951219512195, 12.1951219512195,
        4.8780487804878, 100, 0, 17.6470588235294, 17.6470588235294,
        5.88235294117647, 17.6470588235294, 11.7647058823529, 2.94117647058824,
        8.82352941176471, 8.82352941176471, 8.82352941176471, 100, 0,
        28.5714285714286, 21.4285714285714, 10.7142857142857, 3.57142857142857,
        10.7142857142857, 0, 14.2857142857143, 7.14285714285714, 3.57142857142857,
        100, 0, 7.54716981132075, 13.2075471698113, 7.54716981132075,
        5.66037735849057, 16.9811320754717, 5.66037735849057, 16.9811320754717,
        22.6415094339623, 3.77358490566038, 100, 0, 5.31914893617021,
        7.4468085106383, 8.51063829787234, 14.8936170212766, 9.57446808510638,
        10.6382978723404, 8.51063829787234, 24.468085106383, 10.6382978723404,
        100, 0, 13.4556574923547, 11.9266055045872, 10.0917431192661,
        11.0091743119266, 10.7033639143731, 8.25688073394496, 12.2324159021407,
        15.5963302752294, 6.72782874617737, 100), statistic = "Column %", .Dim = 11:10, .Dimnames = list(
            c("Less than 18", "18 to 24", "25 to 29", "30 to 34", "35 to 39",
            "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
            "NET"), c("Every or nearly every day", "4 to 5 days a week",
            "2 to 3 days a week", "Once a week", "Once every 2 weeks",
            "Once a month", "Once every 3 months", "Once or twice a year",
            "Never", "NET")), name = "Q3. Age by Q12. How  often do you drink cola with alcohol", questions = c("Q3. Age",
        "Q12. How  often do you drink cola with alcohol"))), Z1 = NULL,
            Z2 = NULL, groups = NULL, labels = NULL)
    pd <- PrepareData("Scatter", input.data.raw = raw.multi.ytable)
    expect_equal(nlevels(pd$data$Groups), sum(sapply(raw.multi.ytable$Y, ncol)) - 2)
    expect_equal(rownames(pd$data), MakeUniqueNames(rep(rownames(raw.multi.ytable$X)[2:10], 11)))


    raw.ytable.only <- list(X = NULL, Y = list(`Preferred cola` = structure(c(`Coca-Cola` = 42.625,
    `Diet Coke` = 11.125, `Coke Zero` = 17.875, `Pepsi ` = 9, `Diet Pepsi` = 2.5,
    `Pepsi Max` = 14.875, `Dislike all cola` = 0.75, `Don't care` = 1.25,
    NET = 100), statistic = "%", .Dim = 9L, .Dimnames = list(c("Coca-Cola",
    "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi", "Pepsi Max",
    "Dislike all cola", "Don't care", "NET")), name = "Preferred cola", questions = c("Preferred cola",
    "SUMMARY"))), Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL)
    pd <- PrepareData("Scatter", input.data.raw = raw.ytable.only)
    expect_equal(dim(pd$data), c(8, 1))
    expect_equal(pd$scatter.variable.indices, c(NA, 1, NA, NA, NA), check.attributes = FALSE)
    expect_equal(rownames(pd$data), names(raw.ytable.only$Y[[1]])[-9])

    b.raw <- list(X = rep(c("Age", "Gender", "Location"), c(8, 3, 9)),
    Y = list(b1 = structure(c(5.29313929313929, 5.57701421800948,
    5.45131086142322, 4.69718309859155, 4.47361647361647, 4.22584541062802,
    3.84094256259205, 4.75623325777869, 4.9765984120351, 4.54186991869919,
    4.75623325777869, 4.57254901960784, 5.05172413793103, 4.91449814126394,
    4.74893617021277, 3.43609022556391, 4.66326530612245, 3.9047619047619,
    4.13636363636364, 4.75623325777869), .Dim = c(20L, 1L), statistic = "Average", .Dimnames = list(
        c("15-18", "19 to 24", "25 to 29", "30 to 34", "35 to 39",
        "40 to 44", "45 to 49", "NET", "Male", "Female", "NET", "North",
        "North East", "East", "South East", "South", "South West",
        "West", "North West", "NET"), "# Burger Occasions Capped at 50"), name = "table.BANNER1.by.Burger.Occasions.Capped.at.50", questions = c("BANNER1",
    "# Burger Occasions Capped at 50"))), Z1 = NULL, Z2 = NULL, groups = NULL,
        labels = NULL)
    pd.no.net <- PrepareData("Scatter", input.data.raw = b.raw, row.names.to.remove = "NET")
    expect_equal(pd.no.net$data,
                structure(list(` ` = c("Age", "Age", "Age", "Age", "Age", "Age",
                    "Age", "Gender", "Gender", "Location", "Location", "Location",
                    "Location", "Location", "Location", "Location", "Location"),
                        `# Burger Occasions Capped at 50` = c(5.29313929313929, 5.57701421800948,
                        5.45131086142322, 4.69718309859155, 4.47361647361647, 4.22584541062802,
                        3.84094256259205, 4.9765984120351, 4.54186991869919, 4.57254901960784,
                        5.05172413793103, 4.91449814126394, 4.74893617021277, 3.43609022556391,
                        4.66326530612245, 3.9047619047619, 4.13636363636364)), row.names = c("15-18",
                    "19 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                    "Male", "Female", "North", "North East", "East", "South East",
                    "South", "South West", "West", "North West"), scatter.variable.indices = c(x = 1,
                    y = 2, sizes = NA, colors = NA, groups = NA), class = "data.frame"))
    expect_equal(colnames(pd.no.net$data)[2], "# Burger Occasions Capped at 50")
    expect_equal(pd.no.net$scatter.variable.indices, c(1,2,NA,NA,NA), check.attributes = FALSE)

    pd.with.net <- PrepareData("Scatter", input.data.raw = b.raw, row.names.to.remove = "")
    expect_equal(pd.with.net$data,
                structure(list(` ` = c("Age", "Age", "Age", "Age", "Age", "Age",
                    "Age", "Age", "Gender", "Gender", "Gender", "Location", "Location",
                    "Location", "Location", "Location", "Location", "Location", "Location",
                    "Location"), `# Burger Occasions Capped at 50` = c(5.29313929313929,
                    5.57701421800948, 5.45131086142322, 4.69718309859155, 4.47361647361647,
                    4.22584541062802, 3.84094256259205, 4.75623325777869, 4.9765984120351,
                    4.54186991869919, 4.75623325777869, 4.57254901960784, 5.05172413793103,
                    4.91449814126394, 4.74893617021277, 3.43609022556391, 4.66326530612245,
                    3.9047619047619, 4.13636363636364, 4.75623325777869)), row.names = c("15-18",
                    "19 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                    "NET", "Male", "Female", "NET ", "North", "North East", "East",
                    "South East", "South", "South West", "West", "North West", "NET  "
                ), scatter.variable.indices = c(x = 1, y = 2, sizes = NA, colors = NA,
                groups = NA), class = "data.frame"))

    raw2 <- list(X = structure(c(`Arnold's` = 64.9907273851226, Mexican = 52.3593653410262,
    `Pret'a'pane` = 42.8394807335669, `Southern Fried Chicken` = 42.5509993818257,
    Asian = 40.9849577580878, `Burger Chef` = 38.9655882958994, `Lucky's Pizza` = 30.0638780135998,
    `Nero's Pizza` = 23.9027405728415, `Pizza Heaven` = 13.1052956933855,
    `Burger Shack` = 11.6216773130023, `Ma's burgers` = 11.456830826293,
    `Nuovo Burger` = 6.75870595507933, `Bread Basket` = 5.17205852050278
    ), .Dim = 13L, .Dimnames = list(c("Arnold's", "Mexican", "Pret'a'pane",
    "Southern Fried Chicken", "Asian", "Burger Chef", "Lucky's Pizza",
    "Nero's Pizza", "Pizza Heaven", "Burger Shack", "Ma's burgers",
    "Nuovo Burger", "Bread Basket")), statistic = "%", name = "table.Q2.Eaten.bought.last.month.4", questions = c("Q2 Eaten / bought last month",
    "SUMMARY")), Y = list(tbB = structure(c(`Burger Shack` = 1.87211367673179,
    `Burger Chef` = 2.44315177154944, `Nuovo Burger` = 1.74390243902439,
    `Lucky's Pizza` = 1.96433470507545, `Pizza Heaven` = 1.50708661417323,
    `Southern Fried Chicken` = 2.20145278450363, `Arnold's` = 2.80507936507937,
    `Nero's Pizza` = 1.88514680483592, `Pret'a'pane` = 2.47619047619048,
    `Ma's burgers` = 1.63243243243243, `Bread Basket` = 1.876, Asian = 2.51309164149043,
    Mexican = 2.18614718614719, `Other fast food` = 2.71685393258427,
    SUM = 51), .Dim = 15L, .Dimnames = list(c("Burger Shack", "Burger Chef",
    "Nuovo Burger", "Lucky's Pizza", "Pizza Heaven", "Southern Fried Chicken",
    "Arnold's", "Nero's Pizza", "Pret'a'pane", "Ma's burgers", "Bread Basket",
    "Asian", "Mexican", "Other fast food", "SUM")), statistic = "Average", name = "table.Q5a.Number.of.times.ordered.in.last.month.All.excluding.0s.3", questions = c("Q5a. Number of times ordered in last month All (excluding 0s) 2",
    "SUMMARY"))), Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL)

    expect_warning(pd <- PrepareData("Scatter", input.data.raw = raw2), "discarded")
    expect_equal(rownames(pd$data), rownames(raw2[[1]]))
    expect_equal(ncol(pd$data), 2)
    expect_equal(pd$scatter.variable.indices, c(1,2,NA,NA,NA), check.attributes = FALSE)

    y.color.only <- list(X = NULL, Y = list(`Q3. Age` = structure(c(`Less than 18` = 0,
        `18 to 24` = 13.4556574923547, `25 to 29` = 11.9266055045872,
        `30 to 34` = 10.0917431192661, `35 to 39` = 11.0091743119266,
        `40 to 44` = 10.7033639143731, `45 to 49` = 8.25688073394496,
        `50 to 54` = 12.2324159021407, `55 to 64` = 15.5963302752294,
        `65 or more` = 6.72782874617737, NET = 100), statistic = "%", .Dim = 11L, .Dimnames = list(
            c("Less than 18", "18 to 24", "25 to 29", "30 to 34", "35 to 39",
            "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
            "NET")), name = "Q3. Age", questions = c("Q3. Age", "SUMMARY"
        ))), Z1 = NULL, Z2 = structure(c(`Less than 18` = 0, `18 to 24` = 13.4556574923547,
        `25 to 29` = 11.9266055045872, `30 to 34` = 10.0917431192661,
        `35 to 39` = 11.0091743119266, `40 to 44` = 10.7033639143731,
        `45 to 49` = 8.25688073394496, `50 to 54` = 12.2324159021407,
        `55 to 64` = 15.5963302752294, `65 or more` = 6.72782874617737,
        NET = 100), statistic = "%", .Dim = 11L, .Dimnames = list(c("Less than 18",
        "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
        "50 to 54", "55 to 64", "65 or more", "NET")), name = "Q3. Age.2", questions = c("Q3. Age",
        "SUMMARY")), groups = NULL, labels = NULL)
    pd <- PrepareData("Scatter", input.data.raw = y.color.only)
    expect_equal(dim(pd$data), c(9,2))
    expect_equal(pd$scatter.variable.indices, c(NA, 1, NA, 2, NA), check.attributes = FALSE)

    x.only <- list(X = structure(c(`Less than 18` = 0, `18 to 24` = 13.4556574923547,
        `25 to 29` = 11.9266055045872, `30 to 34` = 10.0917431192661,
        `35 to 39` = 11.0091743119266, `40 to 44` = 10.7033639143731,
        `45 to 49` = 8.25688073394496, `50 to 54` = 12.2324159021407,
        `55 to 64` = 15.5963302752294, `65 or more` = 6.72782874617737,
        NET = 100), statistic = "%", .Dim = 11L, .Dimnames = list(c("Less than 18",
        "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
        "50 to 54", "55 to 64", "65 or more", "NET")), name = "Q3. Age", questions = c("Q3. Age",
        "SUMMARY")), Y = NULL, Z1 = NULL, Z2 = c(`Less than 18` = "Dog",
        `18 to 24` = "Dog", `25 to 29` = "Dog", `30 to 34` = "Cat", `35 to 39` = "Cat",
        `40 to 44` = "Lizard", `45 to 49` = "Lizard", `50 to 54` = "Spider",
        `55 to 64` = "Spider", `65 or more` = "Spider", NET = "Spider"
        ), groups = NULL, labels = NULL)
    pd <- PrepareData("Scatter", input.data.raw = x.only)
    expect_equal(dim(pd$data), c(10,2))
    expect_equal(pd$scatter.variable.indices, c(1, NA, NA, 2, NA), check.attributes = FALSE)

    raw.2multicolumn.tables <- list(X = structure(c(`Less than 18` = 0, `18 to 24` = 13.4556574923547,
        `25 to 29` = 11.9266055045872, `30 to 34` = 10.0917431192661,
        `35 to 39` = 11.0091743119266, `40 to 44` = 10.7033639143731,
        `45 to 49` = 8.25688073394496, `50 to 54` = 12.2324159021407,
        `55 to 64` = 15.5963302752294, `65 or more` = 6.72782874617737,
        NET = 100), statistic = "%", .Dim = 11L, .Dimnames = list(c("Less than 18",
        "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
        "50 to 54", "55 to 64", "65 or more", "NET")), name = "Q3. Age", questions = c("Q3. Age",
        "SUMMARY")), Y = list(`Q3. Age by Q2. Gender` = structure(c(0,
        13.75, 11.25, 9.375, 10, 11.875, 8.125, 11.25, 16.875, 7.5, 100,
        0, 13.1736526946108, 12.5748502994012, 10.7784431137725, 11.9760479041916,
        9.58083832335329, 8.38323353293413, 13.1736526946108, 14.3712574850299,
        5.98802395209581, 100, 0, 13.4556574923547, 11.9266055045872,
        10.0917431192661, 11.0091743119266, 10.7033639143731, 8.25688073394496,
        12.2324159021407, 15.5963302752294, 6.72782874617737, 100), statistic = "Column %", .Dim = c(11L,
        3L), .Dimnames = list(c("Less than 18", "18 to 24", "25 to 29",
        "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
        "65 or more", "NET"), c("Male", "Female", "NET")), name = "Q3. Age by Q2. Gender", questions = c("Q3. Age",
        "Q2. Gender")), `Q3. Age by Q12. How  often do you drink cola with alcohol` = structure(c(0,
        22.2222222222222, 22.2222222222222, 22.2222222222222, 0, 11.1111111111111,
        11.1111111111111, 11.1111111111111, 0, 0, 100, 0, 37.5, 12.5,
        12.5, 25, 0, 0, 0, 12.5, 0, 100, 0, 14.2857142857143, 19.047619047619,
        14.2857142857143, 4.76190476190476, 0, 19.047619047619, 0, 14.2857142857143,
        14.2857142857143, 100, 0, 17.9487179487179, 5.12820512820513,
        12.8205128205128, 15.3846153846154, 10.2564102564103, 5.12820512820513,
        25.6410256410256, 5.12820512820513, 2.56410256410256, 100, 0,
        14.6341463414634, 9.75609756097561, 12.1951219512195, 7.31707317073171,
        12.1951219512195, 14.6341463414634, 12.1951219512195, 12.1951219512195,
        4.8780487804878, 100, 0, 17.6470588235294, 17.6470588235294,
        5.88235294117647, 17.6470588235294, 11.7647058823529, 2.94117647058824,
        8.82352941176471, 8.82352941176471, 8.82352941176471, 100, 0,
        28.5714285714286, 21.4285714285714, 10.7142857142857, 3.57142857142857,
        10.7142857142857, 0, 14.2857142857143, 7.14285714285714, 3.57142857142857,
        100, 0, 7.54716981132075, 13.2075471698113, 7.54716981132075,
        5.66037735849057, 16.9811320754717, 5.66037735849057, 16.9811320754717,
        22.6415094339623, 3.77358490566038, 100, 0, 5.31914893617021,
        7.4468085106383, 8.51063829787234, 14.8936170212766, 9.57446808510638,
        10.6382978723404, 8.51063829787234, 24.468085106383, 10.6382978723404,
        100, 0, 13.4556574923547, 11.9266055045872, 10.0917431192661,
        11.0091743119266, 10.7033639143731, 8.25688073394496, 12.2324159021407,
        15.5963302752294, 6.72782874617737, 100), statistic = "Column %", .Dim = 11:10,
            .Dimnames = list(
            c("Less than 18", "18 to 24", "25 to 29", "30 to 34", "35 to 39",
            "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
            "NET"), c("Every or nearly every day", "4 to 5 days a week",
            "2 to 3 days a week", "Once a week", "Once every 2 weeks",
            "Once a month", "Once every 3 months", "Once or twice a year",
            "Never", "NET")), name = "Q3. Age by Q12. How  often do you drink cola with alcohol",
            questions = c("Q3. Age", "Q12. How  often do you drink cola with alcohol"))), Z1 = NULL,
            Z2 = NULL, groups = NULL, labels = NULL)

})

test_that("Transpose and other manipulations",
{
    tbN <- structure(c(63.4365501223676, 2.00077834514021, 11.1791324728914,
                4.13965895300275, 0, 16.7210490802315, 2.52283102636661, 0, 100,
                42.5197971574505, 4.22727556753244, 25.0123774800325, 4.08048930875767,
                0.982154142179239, 23.1779063440476, 0, 0, 100, 52.4503712968885,
                9.77459575829483, 10.6244420114616, 11.5915084726472, 0, 15.5590824607079,
                0, 0, 100, 52.456406960261, 22.0141474669304, 20.8339999181812,
                4.69544565462738, 0, 0, 0, 0, 100, 24.0226036760892, 21.8589614062009,
                18.9559755942159, 21.062337769682, 3.24014758412314, 10.8599739696888,
                0, 0, 100, 26.2960259237324, 3.92969691122051, 24.9111134595971,
                22.5545493183266, 1.43040159008451, 10.7787743948585, 0, 10.0994384021804,
                100, 39.6054027045992, 5.68840595387842, 23.0555996594492, 5.46337112399567,
                4.34321478359179, 17.6942357562947, 0, 4.14977001819105, 100,
                33.7530227639688, 9.52654012058933, 15.5759026131695, 6.62585765353863,
                5.94572111968839, 26.789854644367, 1.78310108467838, 0, 100,
                33.4860745060014, 20.4742014777578, 13.7001658596211, 4.96477942044809,
                9.27921312531725, 18.0955656108543, 0, 0, 100, 41.4119342504652,
                10.8389607946131, 18.2426130327666, 8.91794674225999, 2.72724561899096,
                15.9664506667062, 0.589885702396956, 1.30496319180105, 100, 74,
                74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
                74, 58, 58, 58, 58, 58, 58, 58, 58, 58, 73, 73, 73, 73, 73, 73,
                73, 73, 73, 68, 68, 68, 68, 68, 68, 68, 68, 68, 48, 48, 48, 48,
                48, 48, 48, 48, 48, 70, 70, 70, 70, 70, 70, 70, 70, 70, 94, 94,
                94, 94, 94, 94, 94, 94, 94, 41, 41, 41, 41, 41, 41, 41, 41, 41,
                600, 600, 600, 600, 600, 600, 600, 600, 600), .Dim = c(9L, 10L,
                2L), .Dimnames = list(c("Coca-Cola", "Diet Coke", "Coke Zero",
                "Pepsi ", "Diet Pepsi", "Pepsi Max", "Dislike all cola", "Don't care",
                "NET"), c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
                "45 to 49", "50 to 54", "55 to 64", "65+", "NET"), c("Column %",
                "Column n")), name = "Q3 - Preferred cola by D1 - Age", questions = c("Q3 - Preferred cola",
                "D1 - Age"), weight.name = "weight_", weight.label = "weighting")

    expect_error(res0 <- PrepareData("Table", input.data.table = tbN, transpose = TRUE, tidy = FALSE), NA)
    expect_warning(res <- PrepareData("Column", input.data.table = tbN, transpose = TRUE, tidy = TRUE))
    expect_equal(res$categories.title, "D1 - Age")
    expect_warning(res <- PrepareData("Column", input.data.table = tbN, transpose = TRUE,
            column.names.to.remove = "Don't care, NET", hide.rows.threshold = 50),
            "Rows 6,9 have sample size less than 50 and have been removed")
    expect_equal(dim(res$data), c(dim(res0$data)[1:2] - c(2,1)))
})

test_that("Remove unnecessary warnings",
{
    dat <- list(X = list(Country = structure(c(1L, 3L, 2L), class = "factor", .Label = c("Australia", "Denmark",
            "France"), questiontype = "PickOne", name = "Country", label = "Country", question = "Country")),
            Y = structure(c(1L, 2L, NA), class = "factor", .Label = c("1",
            "2"), questiontype = "PickOne", name = "A", label = "A", question = "A"),
            Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL)
    filt <- structure(c(TRUE, TRUE, FALSE), name = "bQXIXC", label = "Filter")
    filt2 <- structure(c(TRUE, FALSE, FALSE), name = "Another filter", label = "Filter")

    expect_error(pd <- PrepareData("Column", input.data.raw = dat, subset = filt), NA)
    expect_equal(dimnames(pd$data), list(Country = c("Australia", "France"), A = c("1", "2")))

    expect_error(pd2 <- PrepareData("Column", input.data.raw = dat, subset = filt2, tidy = FALSE), NA)
    expect_equal(dimnames(pd2$data), list(Country = "Australia", A = "Filter"))

    # When there is only value in the grouping value, the column names uses the
    # filter label instead of the grouping value
    expect_error(pd3 <- PrepareData("Column", input.data.raw = dat, subset = filt2, tidy = TRUE), NA)
    expect_equal(dimnames(pd3$data), list("Australia", "Filter"))

    # No warning "Input data is always aggregated when 'Groups' variable provided"
    dat <- list(X = list(Country = structure(c(1L, 4L, 2L, 3L), class = "factor", .Label = c("Australia",
"Denmark", "Fiji", "France"), questiontype = "PickOne", name = "Country", label = "Country", question = "Country")),
    Y = structure(c(1L, 2L, 3L, NA), class = "factor", .Label = c("1",
    "2", "3"), questiontype = "PickOne", name = "A", label = "A", question = "A"),
    Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL)
    expect_error(PrepareData("Column", input.data.raw = dat, first.aggregate = FALSE), NA)

})


#dat <- list(structure(c("STATE", "Texas", "California", "Rhode Island",
#"TOTAL", "12", "13", "12"), .Dim = c(4L, 2L)), FALSE, NULL, NULL)

test_that("Remove first column if appropriate",
{
    p1 <- list(structure(c("Ant", "1", "Bee", "2", "Cockroach", "3"), .Dim = 2:3),
        FALSE, NULL, NULL)
    res1 <- PrepareData("Column", input.data.pasted = p1)
    expect_equal(length(res1$data), 3)

    p2 <- list(structure(c("", "Insect score", "Ant", "1", "Bee", "2",
    "Cockroach", "3"), .Dim = c(2L, 4L)), FALSE, NULL, NULL)
    res2 <- PrepareData("Column", input.data.pasted = p2)
    expect_equal(length(res2$data), 3)

    p3 <- list(structure(c("Year", "2001", "2002", "2003", "Ant", "1",
    "3", "6", "Bee", "2", "2", "4", "Cockroach", "3", "6", "3"), .Dim = c(4L,
    4L)), FALSE, NULL, NULL)
    res3 <- PrepareData("Column", input.data.pasted = p3)
    expect_equal(dim(res3$data), c(3, 3))

    date.table <- list(structure(c("2015", "2016", "2017", "2018", "50%", "45%",
        "60%", "35%"), .Dim = c(4L, 2L)), FALSE, NULL, NULL)
    res4 <- PrepareData("Column", input.data.pasted = date.table)
    expect_equal(names(res4$data), c("2015", "2016", "2017", "2018"))
    expect_equal(attr(res4$data, "statistic"), "%")

    #tb <- list(structure(c("a", "b", "c", "d", "50%", "45%",
    #    "60%", "35%"), .Dim = c(4L, 2L)), FALSE, NULL, NULL)
})

tb <- structure(c(0, 13.75, 11.25, 9.375, 10, 11.875, 8.125, 11.25,
    16.875, 7.5, 100, 0, 13.1736526946108, 12.5748502994012, 10.7784431137725,
    11.9760479041916, 9.58083832335329, 8.38323353293413, 13.1736526946108,
    14.3712574850299, 5.98802395209581, 100, 0, 13.4556574923547,
    11.9266055045872, 10.0917431192661, 11.0091743119266, 10.7033639143731,
    8.25688073394496, 12.2324159021407, 15.5963302752294, 6.72782874617737,
    100), .Dim = c(11L, 3L), statistic = "Column %", .Dimnames = list(
        c("Less than 18", "18 to 24", "25 to 29", "30 to 34", "35 to 39",
        "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
        "NET"), c("Male", "Female", "NET")), name = "Q3. Age by Q2. Gender", questions = c("Q3. Age",
    "Q2. Gender"))

vdat <- list(X = list(`Q3. Age` = structure(c(3L, 7L, 3L, 3L, 9L, 9L,
    8L, 5L, 10L, 7L, 7L, 9L, 9L, 4L, 10L, 3L, 4L, 8L, 5L, 2L, 8L,
    7L, 9L, 8L, 4L, 3L, 5L, 6L, 3L, 9L, 8L, 9L, 4L, 2L, 9L, 4L, 9L,
    7L, 2L, 6L, 9L, 7L, 9L, 6L, 7L, 3L, 5L, 6L, 6L, 7L, 2L, 9L, 5L,
    3L, 6L, 4L, 9L, 4L, 10L, 2L, 5L, 6L, 2L, 7L, 2L, 4L, 10L, 5L,
    3L, 5L, 5L, 2L, 4L, 6L, 7L, 8L, 6L, 9L, 9L, 10L, 8L, 4L, 5L,
    2L, 3L, 2L, 8L, 9L, 4L, 2L, 2L, 10L, 7L, 4L, 2L, 8L, 9L, 9L,
    5L, 9L, 2L, 2L, 7L, 5L, 2L, 4L, 2L, 2L, 4L, 10L, 8L, 7L, 5L,
    6L, 6L, 5L, 2L, 6L, 9L, 8L, 8L, 5L, 3L, 6L, 3L, 5L, 4L, 10L,
    3L, 2L, 2L, 10L, 4L, 2L, 8L, 6L, 9L, 8L, 9L, 9L, 4L, 9L, 2L,
    2L, 4L, 10L, 6L, 2L, 6L, 2L, 2L, 10L, 5L, 7L, 5L, 2L, 8L, 6L,
    2L, 2L, 4L, 3L, 3L, 3L, 3L, 4L, 4L, 7L, 6L, 5L, 8L, 9L, 8L, 8L,
    8L, 9L, 6L, 5L, 3L, 3L, 6L, 2L, 5L, 9L, 6L, 5L, 6L, 3L, 3L, 3L,
    9L, 3L, 9L, 3L, 2L, 2L, 7L, 4L, 6L, 9L, 2L, 10L, 3L, 8L, 9L,
    4L, 7L, 8L, 4L, 9L, 9L, 9L, 2L, 3L, 6L, 8L, 10L, 7L, 3L, 3L,
    4L, 5L, 3L, 10L, 10L, 6L, 6L, 10L, 2L, 10L, 2L, 8L, 6L, 9L, 2L,
    9L, 9L, 8L, 9L, 5L, 9L, 3L, 9L, 2L, 5L, 3L, 10L, 6L, 7L, 8L,
    9L, 5L, 2L, 3L, 6L, 8L, 6L, 5L, 6L, 8L, 9L, 5L, 2L, 9L, 3L, 5L,
    8L, 10L, 3L, 7L, 7L, 8L, 6L, 9L, 7L, 7L, 5L, 8L, 7L, 8L, 9L,
    2L, 3L, 10L, 7L, 8L, 4L, 10L, 9L, 10L, 3L, 4L, 9L, 4L, 4L, 9L,
    9L, 8L, 6L, 5L, 7L, 9L, 5L, 6L, 5L, 3L, 8L, 6L, 7L, 5L, 8L, 2L,
    3L, 9L, 5L, 8L, 8L, 8L, 5L, 3L, 4L, 4L, 8L, 4L, 2L, 4L, 8L), class = "factor", .Label = c("Less than 18",
    "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
    "50 to 54", "55 to 64", "65 or more"), questiontype = "PickOne", name = "Q3", label = "Q3. Age", question = "Q3. Age")),
        Y = structure(c(2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L,
        1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 2L,
        2L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L,
        2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L,
        2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L,
        2L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L,
        1L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 2L,
        2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L,
        2L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 2L,
        1L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 1L,
        1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L,
        2L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 2L,
        1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L,
        1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L,
        2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L,
        2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 1L,
        1L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 2L,
        2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L,
        2L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L,
        2L), class = "factor", .Label = c("Male", "Female"), questiontype = "PickOne", name = "Q2",
        label = "Q2. Gender", question = "Q2. Gender"),
        Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL)

summary.table <- structure(c(-8.03443400756784, -0.362599159486435, 0.209925829176357,
        -2.42368911867248, 9.82834563871125, 0.782450817839146, -2.89209226937453,
        1.82510677193538, -2.04973529771204, 3.17287792659536, 0.140392828610414,
        -0.196549960054579, -1.03211191919445, 0.516055959597225, -1.80619585859029,
        2.64478679293578, -1.61267487374133, 1.29013989899307, 0.763605271365248,
        0.424225150758472, -0.254535090455084, 2.9695760553093, -2.96957605530931,
        -0.933295331668639, 1.51380943091758, 0.216258490131084, 0.216258490131084,
        0.865033960524334, -2.595101881573, -0.216258490131082, 6.24340711873983,
        -2.49736284749593, 0.832454282498644, NaN, -3.53793070061923,
        -1.0405678531233, 5.80611561772338, -1.16122312354468, 1.08630550267083,
        -2.06023457403088, -2.95924602451708, -0.711717398301574, 2.32058206788793,
        2.64438421689554, -0.269835124173015, -3.18405446524157, -2.21264801821872,
        0.701571322849838, 4.42421810285147, -1.17708555029993, 2.96300845420327,
        -3.36890002327222, -2.88183014038949, 0.0405891569068948, NaN,
        NaN, NaN, NaN, NaN, NaN), .Dim = c(6L, 10L), statistic = "z-Statistic", .Dimnames = list(
            c("Coca Cola", "Diet Coke", "Coke Zero", "Pepsi", "Pepsi Light",
            "Pepsi Max"), c("Never", "Once or twice a year", "Once every 3 months",
            "Once a month", "Once every 2 weeks", "Once a week", "2 to 3 days a week",
            "4 to 5 days a week", "Every or nearly every day", "NET")), name = "Q9. Cola drinking frequency",
            questions = c("Q9. Cola drinking frequency", "SUMMARY"))

test_that("Heatmap axis titles",
{
    res <- PrepareData("Heat", input.data.table = tb)
    expect_equal(colnames(res$data), c("Male", "Female"))
    expect_equal(res$categories.title, "Q2. Gender")
    expect_equal(res$values.title, "Q3. Age")

    expect_error(res <- PrepareData("Heat", input.data.raw = vdat, hide.empty.rows.and.columns = FALSE), NA)
    expect_equal(colnames(res$data), c("Male", "Female"))
    expect_equal(rownames(res$data), c("Less than 18", "18 to 24", "25 to 29", "30 to 34", "35 to 39",
                                       "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more"))
    expect_equal(res$categories.title, "Q2. Gender")
    expect_equal(res$values.title, "Q3. Age")

    res <- PrepareData("Heat", input.data.raw = vdat, as.percentages = TRUE)
    expect_equal(sum(res$data), 100)
    expect_equal(res$categories.title, "Q2. Gender")
    expect_equal(res$values.title, "Q3. Age")

    res <- PrepareData("Heat", input.data.table = summary.table)
    expect_equal(dim(res$data), c(6, 9))
    expect_equal(res$categories.title, "")
    expect_equal(res$values.title, "")

    res <- PrepareData("Heat", input.data.table = summary.table, transpose = TRUE)
    expect_equal(dim(res$data), c(9, 6))
    expect_equal(res$values.title, "")
    expect_equal(res$categories.title, "")
})

test_that("Row labels",
{
    res <- PrepareData("Column", input.data.table = tb, row.labels = "Under 18")
    expect_equal(rownames(res$data), c("Under 18", "25 to 29", "30 to 34", "35 to 39",
        "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more"))
})

test_that("Calculate percentages",
{
    xx <- structure(c(4, 5, 6, 3, 2, 13, 5, 6, 1, 7, 3, 2), .Dim = 4:3,
        .Dimnames = list(c("Dog", "Cat", "Lizard", "Beetle"),
        c("Alpha", "Beta", "Gamma")), statistic = "%")

    expect_equal(PrepareData("Bar", input.data.table = xx, as.percentages = T)$data,
        structure(c(57.1428571428571, 20, 42.8571428571429, 27.2727272727273,
        28.5714285714286, 52, 35.7142857142857, 54.5454545454545, 14.2857142857143,
        28, 21.4285714285714, 18.1818181818182), .Dim = 4:3, statistic = "Row %",
        assigned.rownames = TRUE, .Dimnames = list(c("Dog", "Cat", "Lizard", "Beetle"),
        c("Alpha", "Beta", "Gamma"))))

    expect_equal(PrepareData("Pie", input.data.table = xx, as.percentages = T)$data,
        structure(c(7.01754385964912, 8.7719298245614, 10.5263157894737,
        5.26315789473684, 3.50877192982456, 22.8070175438596, 8.7719298245614,
        10.5263157894737, 1.75438596491228, 12.280701754386, 5.26315789473684,
        3.50877192982456), statistic = "%", assigned.rownames = TRUE, .Dim = 4:3,
        .Dimnames = list(c("Dog", "Cat", "Lizard", "Beetle"),
        c("Alpha", "Beta", "Gamma"))))

    expect_equal(PrepareData("Pie", input.data.table = 1:5, as.percentages = T)$data,
        structure(c(`1` = 6.66666666666667, `2` = 13.3333333333333, `3` = 20,
        `4` = 26.6666666666667, `5` = 33.3333333333333), statistic = "%"))
})

tb.with.rowspan <- structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.7037037037037, 0,
0, 0, 0, 5.55555555555556, 3.7037037037037, 0, 0, 12.962962962963,
0, 0, 0, 0, 0, 0, 3.7037037037037, 0, 0, 3.7037037037037, 0,
0, 5.55555555555556, 5.55555555555556, 0, 12.962962962963, 0,
0, 0, 24.0740740740741, 5.55555555555556, 0, 0, 0, 11.1111111111111,
0, 3.7037037037037, 3.7037037037037, 0, 24.0740740740741, 3.7037037037037,
7.40740740740741, 0, 0, 0, 0, 1.85185185185185, 5.55555555555556,
0, 18.5185185185185, 0, 0, 7.40740740740741, 0, 1.85185185185185,
0, 0, 0, 0, 9.25925925925926, 0, 0, 0, 0, 0, 0, 0, 7.40740740740741,
0, 7.40740740740741, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12.962962962963,
7.40740740740741, 12.962962962963, 5.55555555555556, 12.962962962963,
18.5185185185185, 12.962962962963, 16.6666666666667, 0, 100,
0.806451612903226, 0, 0, 0.537634408602151, 0, 0, 0, 1.0752688172043,
0, 2.41935483870968, 2.41935483870968, 0, 0, 1.61290322580645,
1.34408602150538, 0, 1.0752688172043, 3.49462365591398, 2.1505376344086,
12.0967741935484, 1.88172043010753, 0, 1.0752688172043, 0, 0,
0, 1.34408602150538, 2.95698924731183, 1.88172043010753, 9.13978494623656,
3.49462365591398, 2.41935483870968, 2.41935483870968, 0, 2.68817204301075,
2.41935483870968, 2.95698924731183, 1.0752688172043, 5.37634408602151,
22.8494623655914, 1.61290322580645, 1.61290322580645, 1.61290322580645,
2.41935483870968, 2.95698924731183, 2.1505376344086, 3.2258064516129,
5.10752688172043, 0.537634408602151, 21.2365591397849, 0, 2.95698924731183,
2.68817204301075, 1.61290322580645, 1.0752688172043, 0.806451612903226,
2.41935483870968, 1.0752688172043, 0, 12.6344086021505, 1.0752688172043,
0.806451612903226, 0.268817204301075, 0, 0.537634408602151, 2.1505376344086,
1.0752688172043, 0.806451612903226, 0, 6.72043010752688, 0, 0.806451612903226,
0, 2.41935483870968, 0.806451612903226, 0.537634408602151, 0,
0.537634408602151, 0, 5.10752688172043, 0.537634408602151, 1.88172043010753,
1.0752688172043, 0.806451612903226, 0.806451612903226, 0, 1.0752688172043,
1.61290322580645, 0, 7.79569892473118, 11.8279569892473, 10.4838709677419,
9.13978494623656, 9.40860215053763, 10.2150537634409, 8.06451612903226,
13.1720430107527, 17.741935483871, 9.94623655913978, 100, 1.77935943060498,
0, 0, 0, 0, 0, 0, 2.49110320284698, 0.711743772241993, 4.98220640569395,
1.42348754448399, 1.77935943060498, 0, 0, 2.84697508896797, 0,
0.355871886120996, 1.06761565836299, 1.42348754448399, 8.89679715302491,
1.77935943060498, 1.42348754448399, 0.711743772241993, 2.49110320284698,
1.06761565836299, 2.13523131672598, 0.711743772241993, 2.84697508896797,
0, 13.1672597864769, 1.77935943060498, 0, 3.20284697508897, 3.55871886120996,
0, 0, 1.06761565836299, 0.711743772241993, 0.711743772241993,
11.0320284697509, 1.42348754448399, 4.27046263345196, 2.84697508896797,
1.42348754448399, 5.33807829181495, 2.13523131672598, 4.27046263345196,
1.77935943060498, 0, 23.4875444839858, 2.13523131672598, 4.62633451957295,
0.711743772241993, 4.62633451957295, 1.06761565836299, 0, 2.13523131672598,
1.06761565836299, 0, 16.3701067615658, 0, 1.06761565836299, 1.77935943060498,
1.42348754448399, 1.06761565836299, 0, 0.711743772241993, 4.27046263345196,
0, 10.3202846975089, 0.711743772241993, 0.711743772241993, 1.77935943060498,
1.42348754448399, 0, 0.711743772241993, 0.711743772241993, 1.42348754448399,
0, 7.47330960854092, 1.77935943060498, 0, 0.711743772241993,
0, 1.06761565836299, 0, 0.711743772241993, 0, 0, 4.27046263345196,
12.8113879003559, 13.8790035587189, 11.7437722419929, 14.9466192170818,
12.4555160142349, 4.98220640569395, 10.6761565836299, 15.6583629893238,
2.84697508896797, 100, 1.13154172560113, 0, 0, 0.282885431400283,
0, 0, 0, 1.55586987270156, 0.282885431400283, 3.25318246110325,
2.12164073550212, 0.707213578500707, 0, 0.848656294200849, 1.83875530410184,
0.424328147100424, 0.99009900990099, 2.26308345120226, 1.6973125884017,
10.8910891089109, 1.6973125884017, 0.565770862800566, 0.848656294200849,
0.99009900990099, 0.424328147100424, 0.848656294200849, 1.27298444130127,
2.68741159830269, 0.99009900990099, 10.3253182461103, 2.54596888260255,
1.27298444130127, 2.97029702970297, 1.83875530410184, 1.41442715700141,
2.26308345120226, 1.98019801980198, 0.848656294200849, 3.11173974540311,
18.2461103253182, 1.83875530410184, 2.54596888260255, 1.98019801980198,
1.83875530410184, 4.52616690240453, 1.98019801980198, 3.67751060820368,
3.67751060820368, 0.282885431400283, 22.3479490806223, 1.13154172560113,
3.96039603960396, 1.6973125884017, 2.68741159830269, 0.99009900990099,
0.424328147100424, 2.26308345120226, 1.41442715700141, 0, 14.5685997171146,
0.565770862800566, 0.848656294200849, 1.41442715700141, 0.565770862800566,
0.848656294200849, 1.13154172560113, 0.848656294200849, 2.12164073550212,
0, 8.34512022630834, 0.282885431400283, 0.707213578500707, 0.707213578500707,
1.83875530410184, 0.424328147100424, 0.565770862800566, 0.282885431400283,
1.41442715700141, 0, 6.22347949080622, 0.99009900990099, 0.99009900990099,
0.848656294200849, 0.424328147100424, 0.848656294200849, 0, 0.848656294200849,
0.848656294200849, 0, 5.7991513437058, 12.3055162659123, 11.5983026874116,
10.4667609618105, 11.3154172560113, 11.3154172560113, 7.63790664780764,
12.1640735502122, 16.8316831683168, 6.36492220650637, 100, -0.818005835383006,
NA, NA, -0.407258760842396, NA, NA, NA, -0.961261877265368, -0.407258760842396,
-1.40212219261521, 0.839449352030927, -0.64530709823366, NA,
-0.707402535962592, -1.04650651674915, 6.03592334052768, 2.09564613984881,
-1.16351225994327, -1.00472733244928, 0.508530590078004, -1.00472733244928,
-0.576769556871553, -0.707402535962592, -0.764627645620512, -0.499142205857035,
-0.707402535962592, 1.65788822315176, -1.27066941777087, -0.764627645620512,
-1.66390009661028, -1.2358809492673, -0.86824749617646, 1.16439862466676,
2.11537428385619, -0.915869522361235, 5.50109673861541, -1.08679441194398,
-0.707402535962592, -1.37030124035533, 1.15379187758881, 2.11537428385619,
-1.2358809492673, -1.08679441194398, -1.04650651674915, 2.42211430223235,
-1.08679441194398, 0.0106413323285038, 0.0106413323285038, -0.407258760842396,
0.316830465365897, 1.85944844987526, 1.35144452383663, -1.00472733244928,
-1.27066941777087, -0.764627645620512, -0.499142205857035, -0.211425260198025,
2.68146243491317, NA, 0.856092113426796, -0.576769556871553,
-0.707402535962592, 3.88057308733801, -0.576769556871553, 0.836219664424915,
-0.818005835383006, -0.707402535962592, -1.12575161447856, NA,
0.25273660920047, -0.407258760842396, -0.64530709823366, -0.64530709823366,
-1.04650651674915, -0.499142205857035, -0.576769556871553, -0.407258760842396,
3.88057308733801, NA, 0.374724051262546, -0.764627645620512,
-0.764627645620512, -0.707402535962592, -0.499142205857035, -0.707402535962592,
NA, -0.707402535962592, -0.707402535962592, NA, -1.89716371573842,
0.153029263038684, -1.00075849226536, 0.623492770478364, -1.39028136556685,
0.397674842629171, 3.13234260759348, 0.186879236014467, -0.0337236553369541,
-1.99355057707142, NA, -0.861188787568536, NA, NA, 1.34394371243885,
NA, NA, NA, -1.08808279952162, -1.49237928664851, -1.31692940908525,
0.578867168864829, -2.36469547012116, NA, 2.33441065430667, -1.03167217780249,
-1.82908155879146, 0.241027064661906, 2.3202125226349, 0.983125732445773,
1.08441730193036, 0.400013406238423, -2.11354309005927, 0.692193407943669,
-2.80193962669465, -1.82908155879146, -2.59224108478233, 0.177708611644131,
0.467079760694444, 2.52325208317932, -1.09165619905813, 1.68748732669535,
2.86519499522489, -0.909311144217653, -3.83487060096288, 3.02234648911111,
0.294414123742805, 1.96448764842209, 0.692193407943669, 3.65438160691507,
3.33959345690242, -0.471032493170391, -1.65975707403222, -0.738690178425746,
1.2108865607259, -2.11506357852688, 0.342580952313389, -0.672468652306163,
2.12891801458205, 1.34394371243885, -0.747532248936583, -2.99754132374733,
-1.44158853884063, 2.14935038486047, -1.86173117591326, 0.241027064661906,
1.64715678009446, 0.294414123742805, -0.804756139787195, NA,
-1.5361742797067, 1.90332509454262, -0.128915215237831, -2.71830745423635,
-2.11354309005927, -0.950023838419332, 2.69939877272944, 0.692193407943669,
-2.55721779962126, NA, -1.64602041572687, -1.49237928664851,
0.331820170807324, -2.36469547012116, 1.2108865607259, 1.64715678009446,
-0.105108997758324, -1.49237928664851, -2.08045701608663, NA,
-1.29431754001046, -1.28045628101638, 2.52325208317932, 0.692193407943669,
1.64715678009446, -0.128915215237831, NA, 0.692193407943669,
2.33441065430667, NA, 2.39347452148193, -0.407333380787331, -0.97517984567655,
-1.21457278469, -1.68658411986448, -0.973275063387398, 0.450044252601396,
0.864033409169612, 0.681675736087523, 4.11041222638239, NA, 1.32265541086291,
NA, NA, -1.15021362196459, NA, NA, NA, 1.63191109336649, 1.74374022404596,
2.10468216035288, -1.04623569187586, 2.76298032662301, NA, -1.99790430878255,
1.62062033992801, -1.40971838957294, -1.3833250851892, -1.73586659764644,
-0.457791391513575, -1.38245365574253, 0.137169111795428, 2.46952643631722,
-0.322319081843924, 3.27386936828111, 2.13715314575827, 3.02885137203333,
-1.08112932812731, 0.213078676224892, -2.15952416076759, 2.01691019950198,
-1.05100743237474, -2.45217584828159, 0.295816243477475, 2.76468282652721,
-2.58667388365837, -3.28608683074463, -1.41455199760725, -0.322319081843924,
-2.98491087292331, -4.03366449617797, -0.667504633270399, 2.36424915516472,
1.34355131819067, -0.667504633270399, 0.843449799268729, 0.240309991871501,
0.680356922336595, -2.17795072506901, -1.15021362196459, 0.590763227109503,
2.04924233819786, 0.737390755699498, -1.64771239813158, 2.58920839374854,
0.169073065967569, -1.40971838957294, -0.185646364548293, -0.634241389978866,
NA, 1.1027482166646, -1.62895992630314, 0.515473531625392, 0.667380272474133,
2.46952643631722, 0.515473531625392, -2.31027922581183, -0.322319081843924,
3.22017315689842, NA, 1.5422942425363, 1.74374022404596, 0.011674564760379,
2.76298032662301, -0.667504633270399, -1.40971838957294, 0.420283255007039,
1.74374022404596, 0.0165694412476335, NA, 1.11723714027112, 1.72147121712434,
-2.15952416076759, -0.322319081843924, -1.40971838957294, 0.515473531625392,
NA, -0.322319081843924, -1.99790430878255, NA, -1.4124318309085,
0.332554124159279, 1.53815065483209, 0.900854183550146, 2.47542128811821,
0.777215028775678, -2.15925067343849, -0.98301727829989, -0.677223277389709,
-3.11194230235585, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), .Dim = c(100L, 4L, 2L
), .Dimnames = list(c("18 to 24", "25 to 29", "30 to 34", "35 to 39",
"40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
"NET", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
"45 to 49", "50 to 54", "55 to 64", "65 or more", "NET", "18 to 24",
"25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
"55 to 64", "65 or more", "NET", "18 to 24", "25 to 29", "30 to 34",
"35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
"NET", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
"45 to 49", "50 to 54", "55 to 64", "65 or more", "NET", "18 to 24",
"25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
"55 to 64", "65 or more", "NET", "18 to 24", "25 to 29", "30 to 34",
"35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
"NET", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
"45 to 49", "50 to 54", "55 to 64", "65 or more", "NET", "18 to 24",
"25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
"55 to 64", "65 or more", "NET", "18 to 24", "25 to 29", "30 to 34",
"35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
"NET"), c("I am on a diet, so I tend to watch what I eat and drink",
"I tend watch what I eat and drink, but don’t consider myself",
"I typically eat and drink whatever I feel like", "NET"), c("Column %",
"z-Statistic")), basedescriptiontext = "sample size = 707; total sample size = 800; 93 missing", basedescription = list(
    Minimum = 707L, Maximum = 707L, Range = FALSE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 707L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = c("PickAny", "PickOne"
), span = list(rows = structure(list(c("Income", "Income", "Income",
"Income", "Income", "Income", "Income", "Income", "Income", "Income",
"Income", "Income", "Income", "Income", "Income", "Income", "Income",
"Income", "Income", "Income", "Income", "Income", "Income", "Income",
"Income", "Income", "Income", "Income", "Income", "Income", "Income",
"Income", "Income", "Income", "Income", "Income", "Income", "Income",
"Income", "Income", "Income", "Income", "Income", "Income", "Income",
"Income", "Income", "Income", "Income", "Income", "Income", "Income",
"Income", "Income", "Income", "Income", "Income", "Income", "Income",
"Income", "Income", "Income", "Income", "Income", "Income", "Income",
"Income", "Income", "Income", "Income", "Income", "Income", "Income",
"Income", "Income", "Income", "Income", "Income", "Income", "Income",
"Income", "Income", "Income", "Income", "Income", "Income", "Income",
"Income", "Income", "Income", "Income", "Income", "Income", "Income",
"Income", "Income", "Income", "Income", "Income", "Income"),
    c("Less than $15,000", "Less than $15,000", "Less than $15,000",
    "Less than $15,000", "Less than $15,000", "Less than $15,000",
    "Less than $15,000", "Less than $15,000", "Less than $15,000",
    "Less than $15,000", "$15,001 to $30,000", "$15,001 to $30,000",
    "$15,001 to $30,000", "$15,001 to $30,000", "$15,001 to $30,000",
    "$15,001 to $30,000", "$15,001 to $30,000", "$15,001 to $30,000",
    "$15,001 to $30,000", "$15,001 to $30,000", "$30,001 to $45,000",
    "$30,001 to $45,000", "$30,001 to $45,000", "$30,001 to $45,000",
    "$30,001 to $45,000", "$30,001 to $45,000", "$30,001 to $45,000",
    "$30,001 to $45,000", "$30,001 to $45,000", "$30,001 to $45,000",
    "$45,001 to $60,000", "$45,001 to $60,000", "$45,001 to $60,000",
    "$45,001 to $60,000", "$45,001 to $60,000", "$45,001 to $60,000",
    "$45,001 to $60,000", "$45,001 to $60,000", "$45,001 to $60,000",
    "$45,001 to $60,000", "$60,001 to $90,000", "$60,001 to $90,000",
    "$60,001 to $90,000", "$60,001 to $90,000", "$60,001 to $90,000",
    "$60,001 to $90,000", "$60,001 to $90,000", "$60,001 to $90,000",
    "$60,001 to $90,000", "$60,001 to $90,000", "$90,001 to $120,000",
    "$90,001 to $120,000", "$90,001 to $120,000", "$90,001 to $120,000",
    "$90,001 to $120,000", "$90,001 to $120,000", "$90,001 to $120,000",
    "$90,001 to $120,000", "$90,001 to $120,000", "$90,001 to $120,000",
    "$120,001 to $150,000", "$120,001 to $150,000", "$120,001 to $150,000",
    "$120,001 to $150,000", "$120,001 to $150,000", "$120,001 to $150,000",
    "$120,001 to $150,000", "$120,001 to $150,000", "$120,001 to $150,000",
    "$120,001 to $150,000", "$150,001 to $200,000", "$150,001 to $200,000",
    "$150,001 to $200,000", "$150,001 to $200,000", "$150,001 to $200,000",
    "$150,001 to $200,000", "$150,001 to $200,000", "$150,001 to $200,000",
    "$150,001 to $200,000", "$150,001 to $200,000", "$200,001 or more",
    "$200,001 or more", "$200,001 or more", "$200,001 or more",
    "$200,001 or more", "$200,001 or more", "$200,001 or more",
    "$200,001 or more", "$200,001 or more", "$200,001 or more",
    "NET", "NET", "NET", "NET", "NET", "NET", "NET", "NET", "NET",
    "NET"), c("Age", "Age", "Age", "Age", "Age", "Age", "Age",
    "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age",
    "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age",
    "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age",
    "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age",
    "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age",
    "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age",
    "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age",
    "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age",
    "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age",
    "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age",
    "Age", "Age", "Age"), c("18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET", "18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET", "18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET", "18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET", "18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET", "18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET", "18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET", "18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET", "18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET", "18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET")), class = "data.frame", .Names = c("",
"", "", ""), row.names = c(NA, 100L)), columns = structure(list(
    c("I am on a diet, so I tend to watch what I eat and drink",
    "I tend watch what I eat and drink, but don’t consider myself",
    "I typically eat and drink whatever I feel like", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
4L))), name = "table.BANNER.by.Weight.consciousness", questions = c("BANNER",
"Weight-consciousness"))

tb.with.gridq <- structure(c(41.1764705882353, 58.8235294117647, 100, 58.0357142857143,
41.9642857142857, 100, 59.5959595959596, 40.4040404040404, 100,
38.2352941176471, 61.7647058823529, 100, 50, 50, 100, 38.2978723404255,
61.7021276595745, 100, 41.8181818181818, 58.1818181818182, 100,
57.563025210084, 42.436974789916, 100, 49.0322580645161, 50.9677419354839,
100, 39.5833333333333, 60.4166666666667, 100, 53, 47, 100, 54.4117647058823,
45.5882352941176, 100, 43.5897435897436, 56.4102564102564, 100,
50.5813953488372, 49.4186046511628, 100, 53.6945812807882, 46.3054187192118,
100, 48.1617647058824, 51.8382352941177, 100, 52.1604938271605,
47.8395061728395, 100, 57.1428571428571, 42.8571428571429, 100,
46.0076045627376, 53.9923954372624, 100, 30.3370786516854, 69.6629213483146,
100, 29.1970802919708, 70.8029197080292, 100, 44.4444444444444,
55.5555555555556, 100, 41.025641025641, 58.974358974359, 100,
46.9230769230769, 53.0769230769231, 100, 56.1933534743202, 43.8066465256798,
100, 41.7989417989418, 58.2010582010582, 100, 53.8834951456311,
46.1165048543689, 100, 57.1428571428571, 42.8571428571429, 100,
36.1904761904762, 63.8095238095238, 100, 43.0769230769231, 56.9230769230769,
100, 49.375, 50.625, 100, 49.375, 50.625, 100, 49.375, 50.625,
100, 49.375, 50.625, 100, 49.375, 50.625, 100, 49.375, 50.625,
100, -0.977169614149642, 0.977169614149642, NA, 1.97686687766082,
-1.97686687766082, NA, 2.17299727089404, -2.17299727089404, NA,
-1.92079740961063, 1.92079740961063, NA, 0.157176521383107, -0.157176521383107,
NA, -2.2866392726549, 2.2866392726549, NA, -1.1615852872974,
1.1615852872974, NA, 3.0144522782866, -3.0144522782866, NA, -0.0950521966256294,
0.0950521966256294, NA, -2.04557227405644, 2.04557227405644,
NA, 1.18401254693903, -1.18401254693903, NA, 1.28957319903202,
-1.28957319903202, NA, -1.35460948532479, 1.35460948532479, NA,
0.357176581849717, -0.357176581849717, NA, 1.42498871465585,
-1.42498871465585, NA, -0.492630671346596, 0.492630671346596,
NA, 1.30011015397555, -1.30011015397555, NA, 2.91975695510422,
-2.91975695510422, NA, -1.33319529269262, 1.33319529269262, NA,
-3.81055866641894, 3.81055866641894, NA, -5.18906164437283, 5.18906164437283,
NA, -0.815532566820322, 0.815532566820322, NA, -1.06930606427569,
1.06930606427569, NA, -0.611012418318008, 0.611012418318008,
NA, 3.24052971793344, -3.24052971793344, NA, -2.38375995854075,
2.38375995854075, NA, 1.50203798324233, -1.50203798324233, NA,
3.4130537361886, -3.4130537361886, NA, -2.89918291800969, 2.89918291800969,
NA, -2.02281655880286, 2.02281655880286, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), .Dim = c(3L,
6L, 6L, 2L), .Dimnames = list(c("Male", "Female", "NET"), c("Coca-Cola",
"Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max"),
    c("Hate", "Dislike", "Neither like nor dislike", "Love",
    "Like", "NET"), c("Column %", "z-Statistic")), basedescriptiontext = "sample size = 800", basedescription = list(
    Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
    Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = c("PickOne", "PickOneMulti"
), span = list(rows = structure(list(c("Male", "Female", "NET"
)), class = "data.frame", .Names = "", row.names = c(NA, 3L)),
    columns = structure(list(c("Hate", "Dislike", "Neither like nor dislike",
    "Love", "Like", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    6L))), name = "table.Gender.by.Brand.attitude", questions = c("Gender",
"Brand attitude"))

test_that("Higher dimension tables",
{
    res <- PrepareData("Bar", input.data.table = tb.with.gridq, tidy = FALSE)$data
    expect_equal(dimnames(res)[[3]], c("Column %", "z-Statistic"))
    expect_equal(as.numeric(suppressWarnings(verbs::FlattenTableAndDropStatisticsIfNecessary(tb.with.gridq))[,-6]),
        as.numeric(res[,,1]))

    res <- PrepareData("Bar", input.data.table = tb.with.rowspan, tidy = FALSE,
                       row.names.to.remove = NULL, column.names.to.remove = NULL)$data
    expect_equal(dimnames(res)[[3]], c("Column %", "z-Statistic"))
})

tb.with.footer <- structure(c(42.625, 11.125, 17.875, 9, 2.5, 14.875, 0.75, 1.25,
100, 341, 89, 143, 72, 20, 119, 6, 10, 800), dim = c(9L, 2L), dimnames = list(
    c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
    "Pepsi Max", "Dislike all cola", "Don't care", "NET"), c("%",
    "Count")), dimnets = list(8L, NULL), dimduplicates = list(
    8L, NULL), span = list(rows = structure(list(c("Coca-Cola",
"Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi", "Pepsi Max",
"Dislike all cola", "Don't care", "NET")), class = "data.frame", names = "", row.names = c(NA,
9L)), columns = structure(list(c("%", "Count")), class = "data.frame", names = "", row.names = 1:2)), basedescriptiontext = "sample size = 800", basedescription = list(
    Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
    Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), QStatisticsTestingInfo = structure(list(
    significancearrowratio = structure(c(1, 0, 0.879177377892031,
    0.465295629820051, 1, 0.246786632390746, 1, 1, 1), dim = 9L),
    significancedirection = structure(c("Up", "None", "Up", "Down",
    "Down", "Up", "Down", "Down", "Up"), dim = 9L), significancefontsizemultiplier = structure(c(4.89,
    1, 4.42, 0.355871886120996, 0.204498977505112, 1.96, 0.204498977505112,
    0.204498977505112, 4.89), dim = 9L), significanceissignificant = structure(c(TRUE,
    FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), dim = 9L),
    zstatistic = structure(c(25.7639837203577, -1.17594946441467,
    4.5968933608937, -2.99332590941915, -8.55235974119758, 2.03118543853442,
    -10.0490226959072, -9.62140470884728, 74.8331477354788), dim = 9L),
    pcorrected = structure(c(0, 0.239615070917728, 0.00000428836959875945,
    0.0027595489353029, 1.20597222859613e-17, 0.0422361869593673,
    9.27829879438357e-24, 6.49381993537603e-22, 0), dim = 9L)), class = "data.frame", row.names = c(NA,
9L)), questiontypes = "PickOne", footerhtml = "&lt;div data-editable=\"true\" style=\"font-family:'Open Sans', sans-serif;font-size:8pt;font-weight:normal;font-style:normal;text-decoration:none;color:#505050;text-align:center;\"&gt;Preferred cola SUMMARY&lt;br /&gt;sample size = 800; 95% confidence level&lt;/div&gt;", name = "table.Preferred.cola", questions = c("Preferred cola",
"SUMMARY"))
test_that("Footer attribute is retained",
{
    res <- PrepareData("Bar", input.data.table = tb.with.footer, tidy = FALSE)
    expect_equal(attr(res$data, "footerhtml"), attr(tb.with.footer, "footerhtml"))
})


test_that("DS-3842 - QTable attribute interferes with structure of data",
{
    vals <- c(94, 99.5714285714286, 111.142857142857, 135.285714285714, 139.285714285714, 115.857142857143)
    x <- structure(vals,
                   statistic = "Average",
                   dim = c(1L, 6L),
                   dimnames = list("circumference", c("3", "1", "5", "2", "4", "NET")),
                   class = c("matrix", "array", "qTable"),
                   dimnets = list(integer(0), 5L),
                   dimduplicates = list(integer(0), 5L),
                   span = list(rows = data.frame("circumference", fix.empty.names = FALSE),
                               columns = data.frame(c("3", "1", "5", "2", "4", "NET"),
                                                    fix.empty.names = FALSE)),
                   basedescriptiontext = "base n = 35",
                   basedescription = list(Minimum = 35L, Maximum = 35L, Range = FALSE, Total = 35L,
                                          Missing = 0L, EffectiveSampleSize = 35L,
                                          EffectiveSampleSizeProportion = 100, FilteredProportion = 0),
                   QStatisticsTestingInfo = data.frame(
                       significancearrowratio = rep(0L, 6L),
                       significancedirection = rep("None", 6L),
                       significancefontsizemultiplier = rep(1L, 6L),
                       significanceissignificant = rep(FALSE, 6L),
                       zstatistic = c(-1.11009576147117, -0.823621961066177, -0.237278175921897,
                                      0.984767511884234, 1.19161763118602, NaN),
                       pcorrected = c(1, 1, 1, 1, 1, NaN)),
                   questiontypes = c("Number", "PickOne"),
                   footerhtml = paste0("Total sample; Unweighted; base n = 35; ",
                                       "Multiple comparison correction: False Discovery Rate (FDR) (p = 0.05)"),
                   name = "table.circumference.by.Tree",
                   questions = c("circumference", "Tree [orange]"))
    expected.names <- colnames(x)[!colnames(x) %in% "NET"]
    expected.vals <- vals[!colnames(x) %in% "NET"]
    for (ct in c("Box", "Bar")) {
        pd <- PrepareData(ct,
                          input.data.table = x,
                          column.names.to.remove = "NET, SUM, Total",
                          row.names.to.remove = "NET, Total, SUM",
                          tidy.labels = TRUE,
                          tidy = TRUE,
                          first.aggregate = FALSE,
                          hide.empty.rows.and.columns = FALSE,
                          select.rows = "",
                          select.columns = "")
        if (ct == "Box") {
            expected <- array(expected.vals, dim = 5L, dimnames = list(expected.names))
            class(expected) <- c("qTable", class(expected))
        } else # In PrepareData the attributes are lost for Bar
            expected <- setNames(expected.vals, expected.names)
        expect_equivalent(pd$data, expected)
    }

    tb.with.gridq.qTable <- tb.with.gridq
    class(tb.with.gridq.qTable) <- c(class(tb.with.gridq.qTable), "qTable")
    summary.table.QTable <- summary.table
    class(summary.table.QTable) <- c(class(summary.table.qTable), "qTable")
    for (ct in c("Box", "Bar", "Scatter")) {
        wn <- if (ct == "Scatter") "only the first" else NA
        expect_warning(pd1 <- PrepareData(ct,
                                          input.data.table = tb.with.gridq.QTable,
                                          tidy = FALSE,
                                          select.rows = "",
                                          select.columns = "")[["data"]],
                       wn)
        expect_warning(pd2 <- PrepareData(ct,
                                          input.data.table = tb.with.gridq,
                                          tidy = FALSE,
                                          select.rows = "",
                                          select.columns = "")[["data"]],
                       wn)
        expect_equivalent(pd1, pd2)
        expect_equivalent(PrepareData(ct,
                                      input.data.table = summary.table.QTable,
                                      tidy = FALSE,
                                      select.rows = "", select.columns = "")[["data"]],
                          PrepareData(ct,
                                      input.data.table = summary.table,
                                      tidy = FALSE,
                                      select.rows = "", select.columns = "")[["data"]])
    }
}
)