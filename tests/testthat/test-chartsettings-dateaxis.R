context("ChartSettings date axis")

# A native PowerPoint date axis is requested by attaching "category.dates" (numeric date serials) and
# "category.date.format" to the data; PrepareData does this when the category labels are dates. The
# ChartSettings$PrimaryAxis then reports AxisType = "Date" so Q can export a c:dateAx. See preparedata.R
# (transformTable) and cchart.R (getPPTSettings).

test_that("category.dates makes PrimaryAxis a date axis for categorical charts",
{
    assign("QFileFormatVersion", 28.08, envir = .GlobalEnv)
    on.exit(suppressWarnings(rm("QFileFormatVersion", envir = .GlobalEnv)))

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
    assign("QFileFormatVersion", 28.08, envir = .GlobalEnv)
    on.exit(suppressWarnings(rm("QFileFormatVersion", envir = .GlobalEnv)))

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
    assign("QFileFormatVersion", 28.08, envir = .GlobalEnv)
    on.exit(suppressWarnings(rm("QFileFormatVersion", envir = .GlobalEnv)))

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

test_that("Date labels with an appended sample size are not treated as dates",
{
    # A sample-size rule appends "n = ..." to each label. AsDate is lenient enough to read the quarter
    # digit as a month and the sample size as a day ("2025 Q2 n = 11" -> 2025-02-11), which exported a
    # native date axis and replaced the labels with dates. Sample sizes above 31 only escaped by accident.
    labels <- c("2025 Q2 n = 11", "2025 Q3 n = 16", "2025 Q4 n = 18", "2026 Q1 n = 13")
    tbl <- matrix(1:8, ncol = 2, dimnames = list(labels, c("A", "B")))
    pd <- suppressWarnings(PrepareData("Column", input.data.table = tbl))
    expect_null(attr(pd$data, "category.dates"))
})

test_that("A sample size too large to be a day is still not treated as a date",
{
    # "Jan 2025 n = 1212" parses as 2025-12-12: the month name is ignored and the sample size read as mmdd.
    labels <- c("Jan 2025 n = 1212", "Feb 2025 n = 1007", "Mar 2025 n = 1103")
    tbl <- matrix(1:6, ncol = 2, dimnames = list(labels, c("A", "B")))
    pd <- suppressWarnings(PrepareData("Column", input.data.table = tbl))
    expect_null(attr(pd$data, "category.dates"))
})

test_that("Labels carrying any text beyond the date are not treated as dates",
{
    # The week labels are from the bug report and reach the same fault by a different route: no month
    # name, just digits the parser reads across the whole label ("W1'19 n-1212 W1" -> 2012-01-19).
    # The bracketed and dash-separated sample sizes carry no letters at all, and the parser discards
    # the month name to read them as month and day ("Jan 2025 (1212)" -> 2025-12-12).
    for (labels in list(c("Jan 2025 (1212)", "Feb 2025 (1007)"),
                        c("Jan 2025 - 1212", "Feb 2025 - 1007"),
                        c("Feb 25 2025\nn = 10", "Mar 25 2025\nn = 12"),
                        c("Feb 25 2025 (n = 10)", "Mar 25 2025 (n = 12)"),
                        c("Jan 2025 respondents", "Feb 2025 respondents"),
                        c("W1'19 n-1212 W1", "W2'19 n-1105 W2"),
                        c("W1'19\nn-1212", "W2'19\nn-1105"),
                        c("W1 2019 n = 1212", "W2 2019 n = 1105")))
    {
        tbl <- matrix(1:4, ncol = 2, dimnames = list(labels, c("A", "B")))
        pd <- suppressWarnings(PrepareData("Column", input.data.table = tbl))
        expect_null(attr(pd$data, "category.dates"), info = labels[1])
    }
})

test_that("Date range labels keep their date axis",
{
    # Q writes period labels for quarterly and weekly aggregation. Every separator flipTime parses a
    # period with is punctuation - comma, slash, or any Unicode dash - and it returns the start of the
    # range, so these are real dates and must keep their native date axis.
    for (labels in list(c("Apr-Jun 08", "Jul-Sep 08", "Oct-Dec 08"),
                        c("Jan-Mar 2025", "Apr-Jun 2025", "Jul-Sep 2025"),
                        c("jun/sep 10", "oct/dec 10", "jan/mar 11"),
                        paste0(c("Jan", "Apr", "Jul"), " 2025 ", intToUtf8(8211), " ",
                               c("Mar", "Jun", "Sep"), " 2025"), # en dash
                        c("10/16/2016-2/10/2017", "2/11/2017-5/10/2017", "5/11/2017-8/10/2017")))
    {
        tbl <- matrix(1:6, ncol = 2, dimnames = list(labels, c("A", "B")))
        pd <- suppressWarnings(PrepareData("Column", input.data.table = tbl))
        expect_equal(length(attr(pd$data, "category.dates")), 3L, info = labels[1])
    }
})

test_that("Ranges separated by a word stay on a category axis",
{
    # flipTime has no word separator, so "Jan 2025 to Mar 2025" is not recognised as a period at all. It
    # falls through to the same lenient token matching behind this bug and yields 2025-01-20 - "20" read
    # as the day and "25" as the year - so exporting it would put a wrong date on the axis.
    for (labels in list(c("Jan 2025 to Mar 2025", "Apr 2025 to Jun 2025"),
                        c("Jan 2025 and Mar 2025", "Apr 2025 and Jun 2025"),
                        c("Jan 2025 through Mar 2025", "Apr 2025 through Jun 2025")))
    {
        tbl <- matrix(1:4, ncol = 2, dimnames = list(labels, c("A", "B")))
        pd <- suppressWarnings(PrepareData("Column", input.data.table = tbl))
        expect_null(attr(pd$data, "category.dates"), info = labels[1])
    }
})

test_that("Date-only labels in the other formats the parser accepts keep their date axis",
{
    # Ordinal suffixes, CJK year/month/day markers and a timezone name qualifying a time all parse and
    # carry no content beyond the date, so the strictness above must not reject them. Built with
    # intToUtf8 to keep this file ASCII.
    jp <- function(m) paste0("2016", intToUtf8(0x5E74), m, intToUtf8(0x6708), "2", intToUtf8(0x65E5))
    kr <- function(m) paste0("2016", intToUtf8(0xB144), " ", m, intToUtf8(0xC6D4), " 2", intToUtf8(0xC77C))
    for (labels in list(c("Wednesday, 3rd February, 2010", "Thursday, 4th March, 2010",
                          "Friday, 5th April, 2010"),
                        c("1st Feb 2010", "2nd Mar 2010", "3rd Apr 2010"),
                        c("Feb 1st, 2010", "Mar 2nd, 2010", "Apr 3rd, 2010"),
                        vapply(1:3, jp, character(1)),
                        vapply(1:3, kr, character(1)),
                        c("2020-01-01 10:00:00 UTC", "2020-01-02 10:00:00 UTC",
                          "2020-01-03 10:00:00 UTC"),
                        c("2020-01-01 10:00:00 AEST", "2020-01-02 10:00:00 AEST",
                          "2020-01-03 10:00:00 AEST")))
    {
        tbl <- matrix(1:6, ncol = 2, dimnames = list(labels, c("A", "B")))
        pd <- suppressWarnings(PrepareData("Column", input.data.table = tbl))
        expect_equal(length(attr(pd$data, "category.dates")), 3L, info = labels[1])
    }
})

test_that("Labels that are dates and nothing else still get a date axis",
{
    # Guards the strictness above against over-rejecting: every single-date format PrepareData or Q can
    # put on a category label.
    for (labels in list(c("2020-01-01", "2020-01-02", "2020-01-03"),
                        c("Feb 25 2025", "Mar 25 2025", "Apr 25 2025"),
                        c("25 Feb 2025", "25 Mar 2025", "25 Apr 2025"),
                        c("Jan 2025", "Feb 2025", "Mar 2025")))
    {
        tbl <- matrix(1:6, ncol = 2, dimnames = list(labels, c("A", "B")))
        pd <- suppressWarnings(PrepareData("Column", input.data.table = tbl))
        expect_equal(length(attr(pd$data, "category.dates")), 3L, info = labels[1])
    }
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
    # Unmapped strftime tokens leave a stray "%" (which Excel reads as x100), so bail out to the fallback.
    expect_null(convertToPPTDateFormat("%e %b %Y"))  # %e (space-padded day) not mapped
    expect_null(convertToPPTDateFormat("%-d %b %Y")) # %-d (no-pad day) not mapped
    expect_null(convertToPPTDateFormat("%j"))        # %j (day of year) not mapped
})

test_that("A user-set date categories.tick.format is preserved on the date axis, else falls back",
{
    assign("QFileFormatVersion", 28.08, envir = .GlobalEnv)
    on.exit(suppressWarnings(rm("QFileFormatVersion", envir = .GlobalEnv)))

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

    # New enough Q + a non-horizontal angle -> sent as a double, so fractional angles survive.
    assign("QFileFormatVersion", 28.08, envir = .GlobalEnv)
    res <- suppressWarnings(CChart("Column", dat, append.data = TRUE, categories.tick.angle = 90))
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$LabelsRotation, 90)
    res <- suppressWarnings(CChart("Column", dat, append.data = TRUE, categories.tick.angle = 45.5))
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$LabelsRotation, 45.5)

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

test_that("categories.axis.number.type = 'Category' exports date labels as plain categories",
{
    assign("QFileFormatVersion", 28.08, envir = .GlobalEnv)
    on.exit(suppressWarnings(rm("QFileFormatVersion", envir = .GlobalEnv)))

    dat <- matrix(1:10, ncol = 2, dimnames = list(LETTERS[1:5], c("A", "B")))
    attr(dat, "category.dates") <- as.numeric(as.Date("2020-01-01") + 0:4)
    attr(dat, "category.date.format") <- "mmm dd yyyy"

    # Default -> date axis.
    res <- suppressWarnings(CChart("Column", dat, append.data = TRUE))
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$AxisType, "Date")

    # Explicit "Automatic" -> date axis.
    res <- suppressWarnings(CChart("Column", dat, append.data = TRUE, categories.axis.number.type = "Automatic"))
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$AxisType, "Date")

    # "Category" -> no date axis (plain string categories).
    res <- suppressWarnings(CChart("Column", dat, append.data = TRUE, categories.axis.number.type = "Category"))
    expect_null(attr(res, "ChartSettings")$PrimaryAxis$AxisType)
})

test_that("Date-axis ChartSettings are withheld from Q versions that cannot parse them",
{
    dat <- matrix(1:10, ncol = 2, dimnames = list(LETTERS[1:5], c("A", "B")))
    attr(dat, "category.dates") <- as.numeric(as.Date("2020-01-01") + 0:4)
    attr(dat, "category.date.format") <- "mmm dd yyyy"
    on.exit(if (exists("QFileFormatVersion", envir = .GlobalEnv)) rm("QFileFormatVersion", envir = .GlobalEnv))

    # Old Q, and internal builds that predate the change and report 28.07 (28.06 + 0.01), can't parse
    # AxisType = "Date" and would error the whole export - so it must not be sent.
    for (v in c(28.06, 28.07)) {
        assign("QFileFormatVersion", v, envir = .GlobalEnv)
        res <- suppressWarnings(CChart("Column", dat, append.data = TRUE))
        expect_null(attr(res, "ChartSettings")$PrimaryAxis$AxisType, info = v)
    }

    # No version info at all -> also withheld.
    rm("QFileFormatVersion", envir = .GlobalEnv)
    res <- suppressWarnings(CChart("Column", dat, append.data = TRUE))
    expect_null(attr(res, "ChartSettings")$PrimaryAxis$AxisType)
})
