context("Per-point markers in the PPT export")

# flipStandardCharts reports which points of a Line chart show a marker, in the CustomPoints
# attribute. These check that the export turns that into per-point PptMarkerSettings rather
# than putting a marker on every point.

set.seed(12345)
dat.2d <- matrix(rnorm(5 * 2), 5, 2, dimnames = list(letters[1:5], c("A", "B")))
col.2d <- c("#5C9AD3", "#ED7D31")

seriesOf <- function(...)
    attr(CChart("Line", dat.2d, append.data = TRUE, colors = col.2d, ...),
         "ChartSettings")$TemplateSeries

indicesOf <- function(s) vapply(s$CustomPoints, function(p) p$Index, numeric(1))
stylesOf <- function(s) vapply(s$CustomPoints, function(p) p$Marker$Style, character(1))

test_that("Markers everywhere stay a series-level setting", {
    ts <- seriesOf(marker.show = TRUE, marker.size = 8)
    expect_equal(ts[[1]]$Marker$Style, "Circle")
    expect_equal(ts[[1]]$Marker$Size, 8)
    expect_length(ts[[1]]$CustomPoints, 0)
    expect_length(ts[[2]]$CustomPoints, 0)
})

test_that("A Line chart with no markers turns them off at series level", {
    ts <- seriesOf(marker.show = FALSE)
    expect_equal(ts[[1]]$Marker$Style, "None")
    expect_equal(ts[[2]]$Marker$Style, "None")
    expect_length(ts[[1]]$CustomPoints, 0)
})

test_that("Markers at ends are off by default and switched on at the end points", {
    ts <- seriesOf(marker.show.at.ends = TRUE, marker.size = 8)
    expect_equal(ts[[1]]$Marker$Style, "None")
    expect_equal(indicesOf(ts[[1]]), c(0, 4))
    expect_equal(stylesOf(ts[[1]]), c("Circle", "Circle"))
    expect_equal(ts[[1]]$CustomPoints[[1]]$Marker$Size, 8)

    # Numbered within the series, so series 2 repeats 0 and 4 rather than continuing
    expect_equal(indicesOf(ts[[2]]), c(0, 4))
})

test_that("Markers at the last end switch on a single point per series", {
    ts <- seriesOf(marker.show.at.last.end = TRUE, marker.size = 8)
    expect_equal(ts[[1]]$Marker$Style, "None")
    expect_equal(indicesOf(ts[[1]]), 4)
    expect_equal(indicesOf(ts[[2]]), 4)
})

test_that("Each series keeps its own marker symbol on its custom points", {
    ts <- seriesOf(marker.show.at.ends = TRUE, marker.symbols = "circle,square")
    expect_equal(stylesOf(ts[[1]]), c("Circle", "Circle"))
    expect_equal(stylesOf(ts[[2]]), c("Square", "Square"))
})

test_that("A chart from an older flipStandardCharts keeps markers on every point", {
    # No CustomPoints attribute at all is how a build without this feature presents, and it
    # must not be read as "no markers anywhere"
    settings <- list(TemplateSeries = list(
        list(Marker = list(Style = "Circle", Size = 6), BackgroundColor = "#5C9AD3")))
    out <- updateChartSettingsWithLabels(settings, NULL, NULL)
    expect_equal(out$TemplateSeries[[1]]$Marker$Style, "Circle")
})

test_that("Globally numbered custom points are left alone", {
    # CombinedScatter numbers its points across the chart and has no per-point marker
    # visibility; only a list marked as series-numbered drives markers off at series level.
    settings <- list(TemplateSeries = list(
        list(Marker = list(Style = "Circle", Size = 6), BackgroundColor = "#5C9AD3")))
    scatter.points <- list(list(list(Index = 2, OutlineColor = "#FF0000", OutlineWidth = 2)))
    out <- updateChartSettingsWithLabels(settings, NULL, scatter.points)
    expect_equal(out$TemplateSeries[[1]]$Marker$Style, "Circle")
})
