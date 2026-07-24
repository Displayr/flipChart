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

test_that("convertToPPTDateFormat maps d3 date formats and rejects non-date formats",
{
    expect_equal(convertToPPTDateFormat("%Y"), "yyyy")
    expect_equal(convertToPPTDateFormat("%d %b %Y"), "dd mmm yyyy")
    expect_equal(convertToPPTDateFormat("%m %d %y"), "mm dd yy")
    expect_equal(convertToPPTDateFormat("%B %d %Y"), "mmmm dd yyyy")
    expect_equal(convertToPPTDateFormat("%H:%M"), "hh:mm")
    expect_null(convertToPPTDateFormat(""))      # Automatic / no format
    expect_null(convertToPPTDateFormat(".0%"))   # percentage, not a date
    expect_null(convertToPPTDateFormat(",.0f"))  # number, not a date
})

test_that("A user-set date categories.tick.format is preserved on the date axis, else falls back",
{
    dat <- matrix(1:10, ncol = 2, dimnames = list(LETTERS[1:5], c("A", "B")))
    attr(dat, "category.dates") <- as.numeric(as.Date("2020-01-01") + 0:4)
    attr(dat, "category.date.format") <- "mmm dd yyyy" # PrepareData's fallback

    res <- suppressWarnings(CChart("Column", dat, append.data = TRUE, categories.tick.format = "%Y"))
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$NumberFormat, "yyyy")

    res <- suppressWarnings(CChart("Column", dat, append.data = TRUE, categories.tick.format = "%d %b %Y"))
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$NumberFormat, "dd mmm yyyy")

    # No user-set format -> the fallback PrepareData chose from the labels.
    res <- suppressWarnings(CChart("Column", dat, append.data = TRUE))
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$NumberFormat, "mmm dd yyyy")
})

test_that("LabelsRotation is only sent to Q versions that can parse it (28.08+)",
{
    dat <- matrix(1:10, ncol = 2, dimnames = list(LETTERS[1:5], c("A", "B")))
    on.exit(if (exists("QFileFormatVersion", envir = .GlobalEnv)) rm("QFileFormatVersion", envir = .GlobalEnv))

    # New enough Q + a non-horizontal angle -> sent.
    assign("QFileFormatVersion", 28.08, envir = .GlobalEnv)
    res <- suppressWarnings(CChart("Column", dat, append.data = TRUE, categories.tick.angle = 90))
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$LabelsRotation, 90L)

    # Older Q -> not sent, so it can't error the export.
    assign("QFileFormatVersion", 28.06, envir = .GlobalEnv)
    res <- suppressWarnings(CChart("Column", dat, append.data = TRUE, categories.tick.angle = 90))
    expect_null(attr(res, "ChartSettings")$PrimaryAxis$LabelsRotation)

    # New Q but horizontal/default angle -> not sent (nothing to rotate).
    assign("QFileFormatVersion", 28.08, envir = .GlobalEnv)
    res <- suppressWarnings(CChart("Column", dat, append.data = TRUE, categories.tick.angle = 0))
    expect_null(attr(res, "ChartSettings")$PrimaryAxis$LabelsRotation)

    # No version info at all -> not sent.
    rm("QFileFormatVersion", envir = .GlobalEnv)
    res <- suppressWarnings(CChart("Column", dat, append.data = TRUE, categories.tick.angle = 90))
    expect_null(attr(res, "ChartSettings")$PrimaryAxis$LabelsRotation)
})
