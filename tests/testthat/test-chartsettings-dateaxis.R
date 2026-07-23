context("ChartSettings date axis")

# A native PowerPoint date axis is requested by attaching "category.dates" (numeric date serials) and
# "category.date.format" to the data; PrepareData does this when the category labels are dates. The
# ChartSettings$PrimaryAxis then reports AxisType = "Date" so Q can export a c:dateAx. See preparedata.R
# (transformTable) and cchart.R (getPPTSettings).

test_that("category.dates makes PrimaryAxis a date axis for categorical charts",
{
    dat <- matrix(1:10, ncol = 2, dimnames = list(LETTERS[1:5], c("A", "B")))
    attr(dat, "category.dates") <- as.numeric(as.Date("2020-01-01") + 0:4)
    attr(dat, "category.date.format") <- "mmm dd yyyy"

    for (chart.type in c("Column", "Area", "Line", "Bar"))
    {
        res <- suppressWarnings(CChart(chart.type, dat, append.data = TRUE))
        settings <- attr(res, "ChartSettings")
        expect_equal(settings$PrimaryAxis$AxisType, "Date", info = chart.type)
        expect_equal(settings$PrimaryAxis$NumberFormat, "mmm dd yyyy", info = chart.type)
    }
})

test_that("Absence of category.dates leaves a normal category axis",
{
    dat <- matrix(1:10, ncol = 2, dimnames = list(LETTERS[1:5], c("A", "B")))
    res <- suppressWarnings(CChart("Column", dat, append.data = TRUE))
    expect_null(attr(res, "ChartSettings")$PrimaryAxis$AxisType)
})

test_that("Pie charts never get a category date axis",
{
    dat <- structure(1:5, .Names = LETTERS[1:5])
    attr(dat, "category.dates") <- as.numeric(as.Date("2020-01-01") + 0:4)
    res <- suppressWarnings(CChart("Pie", dat, append.data = TRUE))
    expect_null(attr(res, "ChartSettings")$PrimaryAxis$AxisType)
})

test_that("Date row labels flow through PrepareData to a native date axis (end to end)",
{
    serials <- as.numeric(as.Date("2020-01-01") + 0:4)
    tbl <- matrix(1:10, ncol = 2,
                  dimnames = list(as.character(as.Date("2020-01-01") + 0:4), c("A", "B")))

    pd <- suppressWarnings(PrepareData("Column", input.data.table = tbl))
    expect_equal(attr(pd$data, "category.dates"), serials)

    res <- suppressWarnings(CChart("Column", pd$data, append.data = TRUE))
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$AxisType, "Date")
    expect_equal(attr(attr(res, "ChartData"), "category.dates"), serials)
})

test_that("Date variable (raw data) flows through to a native date axis (end to end)",
{
    serials <- as.numeric(as.Date("2020-01-01") + 0:4)
    input <- list(X = list(Date = as.Date("2020-01-01") + 0:4, Score = 1:5))

    pd <- suppressWarnings(PrepareData("Column", input.data.raw = input))
    expect_equal(attr(pd$data, "category.dates"), serials)

    res <- suppressWarnings(CChart("Column", pd$data, append.data = TRUE))
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$AxisType, "Date")
    expect_equal(attr(attr(res, "ChartData"), "category.dates"), serials)
})

test_that("Non-date row labels do not trigger a date axis",
{
    tbl <- matrix(1:10, ncol = 2, dimnames = list(LETTERS[1:5], c("A", "B")))
    pd <- suppressWarnings(PrepareData("Column", input.data.table = tbl))
    expect_null(attr(pd$data, "category.dates"))
})
