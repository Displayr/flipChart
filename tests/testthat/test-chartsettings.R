context("ChartSettings")

set.seed(12345)
dat.1d <- structure(1:10, .Names = LETTERS[1:10])
col.1d.multicolor <- PrepareColors(dat.1d, "Pyramid", palette = "Strong colors")[[1]]
dat.2d <- matrix(rnorm(13*3), 13, 3, dimnames=list(letters[1:13], LETTERS[1:3]))
col.2d <- PrepareColors(dat.2d, "Column", palette = "Default colors")[[1]]
col.2d.gradient <- PrepareColors(dat.2d, "Column", palette = "Greens")[[1]]


test_that("Chart settings",
{
    res <- CChart("Area", dat.2d, append.data = TRUE, colors = col.2d,
                  title = "Meaningless area chart", categories.title = "Letters")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor, "#5C9AD366")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$BackgroundColor, "#ED7D3166")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$BackgroundColor, "#A5A5A566")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$OutlineStyle, "None")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$ShowDataLabels, FALSE)
    expect_equal(attr(res, "ChartSettings")$ShowChartTitle, TRUE)
    expect_equal(attr(res, "ChartSettings")$ValueAxis$Maximum, NULL)
    expect_equal(attr(res, "ChartLabels")$ChartTitle, "Meaningless area chart")
    expect_equal(attr(res, "ChartLabels")$PrimaryAxisTitle, "Letters")
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$ShowTitle, TRUE)
    expect_true(is.null(attr(res, "ChartWarning")))

    res <- CChart("Area", abs(dat.2d), append.data = TRUE, colors = col.2d.gradient,
            type = "Stacked", font.units = "pt", global.font.color = "#2C2C2C",
            values.bounds.maximum = 6, values.bounds.minimum = -3,
            data.label.font.family = "Arial", data.label.font.size = 10,
            data.label.show = TRUE, data.label.font.autocolor = TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor, "#BAE4B3FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$BackgroundColor, "#74C476FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$BackgroundColor, "#238B45FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$DataLabelsPosition, "BestFit")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$ShowDataLabels, TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$OutlineStyle, "None")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$DataLabelsFont,
            list(family = "Arial", size = 9.7502437560939, color = "#2C2C2C"))
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$DataLabelsFont$color,"#FFFFFF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$DataLabelsFont$color,"#2C2C2C")
    expect_equal(attr(res, "ChartSettings")$ShowChartTitle, FALSE)
    expect_equal(attr(res, "ChartSettings")$ValueAxis$Maximum, 6)
    expect_equal(attr(res, "ChartSettings")$ValueAxis$Minimum, -3)

    res <- CChart("Bar", dat.1d, append.data = TRUE, colors = "#FF0000",
            values.grid.width = 1, categories.grid.width = 0,
            values.grid.color = "#CCCCCC", categories.grid.color = "#BBBBBB",
            values.line.color = "#000000", categories.line.color = "#222222",
            values.line.width = 2, categories.line.width = 2,
            data.label.show = TRUE, bar.gap = 0.0,
            marker.border.color = "#000000", marker.border.width = 2)
    expect_equal(length(attr(res, "ChartSettings")$TemplateSeries), 1)
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis, list(
            LabelsFont = list(color = NULL, family = NULL, size = numeric(0)),
            ShowTitle = FALSE,
            TitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            AxisLine = list(Color = "#222222", Width = 1.50003750093752,
            Style = "Solid"), MajorGridLine = list(Color = "#BBBBBB",
            Width = 0, Style = "None"), RotateLabels = FALSE, LabelPosition = "Low"))
    expect_equal(attr(res, "ChartSettings")$ValueAxis, list(
            LabelsFont = list(color = NULL, family = NULL, size = numeric(0)),
            ShowTitle = FALSE,
            TitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            NumberFormat = "General",
            AxisLine = list(Color = "#000000", Width = 1.50003750093752,
            Style = "Solid"), MajorGridLine = list(Color = "#CCCCCC",
            Width = 0.750018750468762, Style = "Solid"), Crosses = "AutoZero"))
    expect_equal(attr(res, "ChartSettings")$GapWidth, 0)

    res <- CChart("Column", abs(dat.2d), append.data = TRUE, colors = col.2d.gradient,
            values.grid.width = 1, categories.grid.width = 0,
            values.grid.color = "#CCCCCC", categories.grid.color = "#BBBBBB",
            values.line.color = "#000000", categories.line.color = "#222222",
            values.line.width = 2, categories.line.width = 2, categories.tick.angle = 90,
            type = "Stacked", data.label.show = TRUE, data.label.centered = FALSE,
            bar.gap = 0.3, data.label.font.autocolor = TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor, "#BAE4B3FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$BackgroundColor, "#74C476FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$BackgroundColor, "#238B45FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$ShowDataLabels, TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$DataLabelsPosition, "InsideEnd")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$DataLabelsFont$color, "#2C2C2C")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$DataLabelsFont$color, "#2C2C2C")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$DataLabelsFont$color, "#FFFFFF")
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis, list(
            LabelsFont = list(color = NULL, family = NULL, size = numeric(0)),
            ShowTitle = FALSE,
            TitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            AxisLine = list(Color = "#222222", Width = 1.50003750093752,
            Style = "Solid"), MajorGridLine = list(Color = "#BBBBBB",
            Width = 0, Style = "None"), RotateLabels = TRUE, LabelPosition = "Low"))
    expect_equal(attr(res, "ChartSettings")$ValueAxis, list(
            LabelsFont = list(color = NULL, family = NULL, size = numeric(0)),
            ShowTitle = FALSE,
            TitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            NumberFormat = "General",
            AxisLine = list(Color = "#000000", Width = 1.50003750093752,
            Style = "Solid"), MajorGridLine = list(Color = "#CCCCCC",
            Width = 0.750018750468762, Style = "Solid"), Crosses = "AutoZero"))
    expect_equal(attr(res, "ChartSettings")$GapWidth, 30)

    res <- CChart("Line", dat.2d, append.data = TRUE, colors = col.2d,
            shape = "Curved", line.type = "Dot", line.thickness = "1,2,3",
            marker.show = TRUE, marker.size = 10,
            data.label.show = TRUE, data.label.font.autocolor = TRUE)
    expect_equal(attr(res, "ChartType"), "Line Markers")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor, "#5C9AD3FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$BackgroundColor, "#ED7D31FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$BackgroundColor, "#A5A5A5FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$DataLabelsFont$color, "#5C9AD3")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$DataLabelsFont$color, "#ED7D31")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$DataLabelsFont$color, "#A5A5A5")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$ShowDataLabels, TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$OutlineStyle, "Dot")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$OutlineColor, "#A5A5A5")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineWidth, 0.750018750468762)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$OutlineWidth, 1.50003750093752)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$OutlineWidth, 2.25005625140629)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$Marker,
           list(Size = 10, OutlineStyle = "None", BackgroundColor = "#ED7D31FF"))

    res <- CChart("Radar", dat.2d, append.data = TRUE, colors = col.2d, line.thickness = 2)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor, "#5C9AD366")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$BackgroundColor, "#ED7D3166")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$BackgroundColor, "#A5A5A566")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineColor, "#5C9AD3")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$OutlineColor, "#ED7D31")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineStyle, "Solid")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineWidth, 1.50003750093752)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$ShowCategoryNames, FALSE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$ShowDataLabels, FALSE)

    res <- CChart("Palm", abs(dat.2d), append.data = TRUE, colors = col.2d)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor, "#5C9AD366")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineStyle, "Solid")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$OutlineColor, "#ED7D31")

    res <- CChart("Donut", dat.1d[1:4], append.data = TRUE, colors = col.1d.multicolor[1:4],
            pie.inner.radius = 40, pie.border.color = "#FFFFFF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor, "#E41A1CFF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$BackgroundColor, "#4A72A6FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$BackgroundColor, "#48A462FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[4]]$BackgroundColor, "#7E6E85FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[4]]$OutlineStyle, "Solid")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[4]]$OutlineColor, "#FFFFFF")
    expect_equal(attr(res, "ChartSettings")$HoleSize, 40)
    expect_equal(attr(res, "ChartSettings")$FirstSliceAngle, 270)

    res <- CChart("Pie", dat.1d[5:9], append.data = TRUE, pie.border.color = "#333333",
            colors = col.1d.multicolor[5:9])
    expect_equal(length(attr(res, "ChartSettings")$TemplateSeries), 5)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor, "#D16948FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[5]]$BackgroundColor, "#EC83BAFF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[4]]$OutlineStyle, "Solid")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[4]]$OutlineColor, "#333333")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$ShowDataLabels, TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$ShowCategoryNames, TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[4]]$DataLabelsPosition, "OutsideEnd")
    expect_equal(attr(res, "ChartSettings")$FirstSliceAngle, 270)

    res <- CChart("Pie", abs(dat.2d), append.data = TRUE)
    expect_equal(attr(res, "ChartType"), "Sunburst")
    expect_true(grepl("This visualization is a 2-dimensional Pie chart which cannot be exported to PowerPoint.",
                      attr(res, "ChartWarning")))

    res <- CChart("Bar", dat.2d, small.multiples = TRUE, append.data = TRUE)
    expect_true(grepl("This visualization is a small multiple which is not supported by PowerPoint",
                      attr(res, "ChartWarning")))


    res <- CChart("ColumnMultiColor", dat.1d, append.data = TRUE, bar.gap = 0.4,
            colors = col.1d.multicolor, opacity = 0.7, marker.border.opacity = 1,
            marker.border.width = 1, marker.border.color = "#FF0000")
    expect_equal(length(attr(res, "ChartSettings")$TemplateSeries), 1)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$CustomPoints,
            list(list(BackgroundColor = "#E41A1CB2", Index = 0),
            list(BackgroundColor = "#4A72A6B2",
            Index = 1), list(BackgroundColor = "#48A462B2", Index = 2),
            list(BackgroundColor = "#7E6E85B2", Index = 3), list(BackgroundColor = "#D16948B2",
            Index = 4), list(BackgroundColor = "#FFB716B2", Index = 5),
            list(BackgroundColor = "#E1C62FB2", Index = 6), list(BackgroundColor = "#B75F49B2",
            Index = 7), list(BackgroundColor = "#EC83BAB2", Index = 8),
            list(BackgroundColor = "#999999B2", Index = 9)))
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineStyle, "Solid")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineColor, "#FF0000")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineWidth, 0.750018750468762)
    expect_equal(attr(res, "ChartSettings")$GapWidth, 40)
    expect_equal(attr(res, "ChartSettings")$ShowLegend, FALSE)

    res <- CChart("Pyramid", dat.1d[1:4], append.data = TRUE, bar.gap = 0.6,
            colors = col.1d.multicolor[1:4], opacity = 0.3)
    expect_equal(length(attr(res, "ChartSettings")$TemplateSeries), 1)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$CustomPoints,
            list(list(BackgroundColor = "#E41A1C4C", Index = 0),
            list(BackgroundColor = "#4A72A64C", Index = 1), list(BackgroundColor = "#48A4624C",
            Index = 2), list(BackgroundColor = "#7E6E854C", Index = 3)))
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineStyle, "None")
    expect_equal(attr(res, "ChartSettings")$GapWidth, 60)
    expect_equal(attr(res, "ChartSettings")$ShowLegend, FALSE)
    expect_true(grepl("This visualization is of type 'Pyramid' which is not supported by PowerPoint.",
                      attr(res, "ChartWarning")))


    res <- CChart("Histogram", list(x=1:10, y=rnorm(20)), density.color = "#FF0000", append.data = T,
            title = "Histogram Chart", footer = "This chart is for testing",
            background.fill.color = "#0000FF", background.fill.opacity = 0.2)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor,
                 "#FF0000")
    expect_equal(attr(res, "ChartSettings")$BackgroundColor, "#0000FF33")
    expect_equal(attr(res, "ChartLabels")$ChartTitle, "Histogram Chart")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$ShowDataLabels, FALSE)
    expect_equal(attr(res, "ChartSettings")$ShowChartTitle, TRUE)
    expect_true(grepl("This visualization is a Histogram chart which cannot be exported to PowerPoint", attr(res, "ChartWarning")))
})
