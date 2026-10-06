context("AppendExportAttributes")

dat <- matrix(c(1:5, 5:1), 5, 2, dimnames = list(letters[1:5], c("A", "B")))
colors <- c("#FF0000", "#008000")

test_that("A chart drawn without CChart gets the same export attributes as CChart gives it",
{
    via.cchart <- CChart("Line", dat, append.data = TRUE, colors = colors, line.type = "Dot, Solid",
                         values.title = "Proportion", categories.title = "Price")
    drawn <- flipStandardCharts::Line(dat, colors = colors, line.type = "Dot, Solid",
                                      y.title = "Proportion", x.title = "Price")
    direct <- AppendExportAttributes(drawn, "Line",
        list(colors = colors, line.type = "Dot, Solid", values.title = "Proportion", categories.title = "Price"),
        dat)

    expect_equal(attr(direct, "ChartLabels")$ValueAxisTitle, "Proportion")
    for (attribute in c("ChartData", "ChartSettings", "ChartLabels"))
        expect_equal(attr(direct, attribute), attr(via.cchart, attribute), info = attribute)
    expect_false(inherits(direct, "visualization-selector"))
})

test_that("Existing ChartData is kept and existing series labels are added to",
{
    drawn <- flipStandardCharts::Line(dat, colors = colors)
    attr(drawn, "ChartData") <- dat[, 1, drop = FALSE]
    attr(drawn, "ChartLabels") <- list(SeriesLabels = list(
        list(CustomPoints = list(list(Index = 2, Segments = list(list(Text = "Peak")))))))
    result <- AppendExportAttributes(drawn, "Line", list(colors = colors, categories.title = "Price"), dat)

    expect_equal(attr(result, "ChartData"), dat[, 1, drop = FALSE])
    expect_equal(attr(result, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[1]]$Segments[[1]]$Text, "Peak")
    expect_equal(attr(result, "ChartLabels")$PrimaryAxisTitle, "Price")
    expect_false(attr(result, "ChartSettings")$TemplateSeries[[1]]$ShowDataLabels)
})
