context("ChartSettings")

set.seed(12345)
dat.1d <- structure(1:10, .Names = LETTERS[1:10])
col.1d.multicolor <- PrepareColors(dat.1d, "Pyramid", palette = "Strong colors")[[1]]
dat.2d <- matrix(rnorm(13*3), 13, 3, dimnames=list(letters[1:13], LETTERS[1:3]))
col.2d <- PrepareColors(dat.2d, "Column", palette = "Default colors")[[1]]
col.2d.gradient <- PrepareColors(dat.2d, "Column", palette = "Greens")[[1]]


test_that("Chart settings",
{
    res <- CChart("Area", dat.2d, append.data = TRUE, colors = col.2d)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries,
            list(list(BackgroundColor = "#5C9AD366", Marker = list(BackgroundColor = "#5C9AD3"),
            ShowDataLabels = FALSE, DataLabelsFont = list(family = NULL,
            size = numeric(0), color = NA_character_), DataLabelPosition = "BestFit",
            OutlineColor = "#5C9AD3", OutlineWidth = 0.750018750468762, OutlineStyle = "None"),
            list(BackgroundColor = "#ED7D3166",
            Marker = list(BackgroundColor = "#ED7D31"), ShowDataLabels = FALSE,
            DataLabelsFont = list(family = NULL, size = numeric(0), color = NA_character_),
            DataLabelPosition = "BestFit", OutlineColor = "#ED7D31", OutlineWidth = 0.750018750468762,
            OutlineStyle = "None"), list(BackgroundColor = "#A5A5A566",
            Marker = list(BackgroundColor = "#A5A5A5"), ShowDataLabels = FALSE,
            DataLabelsFont = list(family = NULL, size = numeric(0), color = NA_character_),
            DataLabelPosition = "BestFit", OutlineColor = "#A5A5A5", OutlineWidth = 0.750018750468762,
            OutlineStyle = "None")))

    res <- CChart("Area", abs(dat.2d), append.data = TRUE, colors = col.2d.gradient,
            type = "Stacked", font.units = "pt", global.font.color = "#2C2C2C",
            data.label.font.family = "Arial", data.label.font.size = 10,
            data.label.show = TRUE, data.label.font.autocolor = TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries,
            list(list(BackgroundColor = "#BAE4B3FF", Marker = list(BackgroundColor = "#BAE4B3"),
            ShowDataLabels = TRUE, DataLabelsFont = list(family = "Arial",
            size = 9.75243810952738, color = "#2C2C2C"), DataLabelPosition = "BestFit",
            OutlineColor = "#BAE4B3", OutlineWidth = 0.750018750468762, OutlineStyle = "None"),
            list(BackgroundColor = "#74C476FF",
            Marker = list(BackgroundColor = "#74C476"), ShowDataLabels = TRUE,
            DataLabelsFont = list(family = "Arial", size = 9.75243810952738,
            color = "#FFFFFF"), DataLabelPosition = "BestFit", OutlineColor = "#74C476",
            OutlineWidth = 0.750018750468762, OutlineStyle = "None"),
            list(BackgroundColor = "#238B45FF",
            Marker = list(BackgroundColor = "#238B45"), ShowDataLabels = TRUE,
            DataLabelsFont = list(family = "Arial", size = 9.75243810952738,
            color = "#2C2C2C"), DataLabelPosition = "BestFit", OutlineColor = "#238B45",
            OutlineWidth = 0.750018750468762, OutlineStyle = "None")))

    res <- CChart("Bar", dat.1d, append.data = TRUE, colors = "#FF0000",
            values.grid.width = 1, categories.grid.width = 0,
            values.grid.color = "#CCCCCC", categories.grid.color = "#BBBBBB",
            values.line.color = "#000000", categories.line.color = "#222222",
            values.line.width = 2, categories.line.width = 2,
            data.label.show = TRUE, bar.gap = 0.0,
            marker.border.color = "#000000", marker.border.width = 2)
    expect_equal(attr(res, "ChartSettings"),
            list(TemplateSeries = list(list(BackgroundColor = "#FF0000FF",
            Marker = list(BackgroundColor = "#FF0000"), ShowDataLabels = TRUE,
            DataLabelsFont = list(family = NULL, size = numeric(0), color = NA_character_),
            DataLabelPosition = "BestFit", OutlineColor = "#FF0000", OutlineWidth = 0.750018750468762,
            OutlineStyle = "None")), Legend = list(Font = list(color = NULL,
            family = NULL, size = numeric(0))), ChartTitleFont = list(
            color = NULL, family = NULL, size = numeric(0)), PrimaryAxis = list(
            LabelsFont = list(color = NULL, family = NULL, size = numeric(0)),
            TitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            AxisLine = list(Color = "#222222", Width = 1.50003750093752,
            Style = "Solid"), MajorGridLine = list(Color = "#BBBBBB",
            Width = 0, Style = "None"), RotateLabels = FALSE), ValueAxis = list(
            LabelsFont = list(color = NULL, family = NULL, size = numeric(0)),
            TitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            AxisLine = list(Color = "#000000", Width = 1.50003750093752,
            Style = "Solid"), MajorGridLine = list(Color = "#CCCCCC",
            Width = 0.750018750468762, Style = "Solid")), GapWidth = 0))

    res <- CChart("Column", abs(dat.2d), append.data = TRUE, colors = col.2d.gradient,
            values.grid.width = 1, categories.grid.width = 0,
            values.grid.color = "#CCCCCC", categories.grid.color = "#BBBBBB",
            values.line.color = "#000000", categories.line.color = "#222222",
            values.line.width = 2, categories.line.width = 2, categories.tick.angle = 90,
            type = "Stacked", data.label.show = TRUE, data.label.centered = FALSE,
            bar.gap = 0.3, data.label.font.autocolor = TRUE)
    expect_equal(attr(res, "ChartSettings"),
            list(TemplateSeries = list(list(BackgroundColor = "#BAE4B3FF",
            Marker = list(BackgroundColor = "#BAE4B3"), ShowDataLabels = TRUE,
            DataLabelsFont = list(family = NULL, size = numeric(0), color = "#2C2C2C"),
            DataLabelPosition = "InsideEnd", OutlineColor = "#BAE4B3", OutlineWidth = 0.750018750468762,
            OutlineStyle = "None"), list(BackgroundColor = "#74C476FF",
            Marker = list(BackgroundColor = "#74C476"), ShowDataLabels = TRUE,
            DataLabelsFont = list(family = NULL, size = numeric(0), color = "#2C2C2C"),
            DataLabelPosition = "InsideEnd", OutlineColor = "#74C476", OutlineWidth = 0.750018750468762,
            OutlineStyle = "None"), list(BackgroundColor = "#238B45FF",
            Marker = list(BackgroundColor = "#238B45"), ShowDataLabels = TRUE,
            DataLabelsFont = list(family = NULL, size = numeric(0), color = "#FFFFFF"),
            DataLabelPosition = "InsideEnd", OutlineColor = "#238B45", OutlineWidth = 0.750018750468762,
            OutlineStyle = "None")), Legend = list(Font = list(color = NULL,
            family = NULL, size = numeric(0))), ChartTitleFont = list(
            color = NULL, family = NULL, size = numeric(0)), PrimaryAxis = list(
            LabelsFont = list(color = NULL, family = NULL, size = numeric(0)),
            TitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            AxisLine = list(Color = "#222222", Width = 1.50003750093752,
            Style = "Solid"), MajorGridLine = list(Color = "#BBBBBB",
            Width = 0, Style = "None"), RotateLabels = TRUE), ValueAxis = list(
            LabelsFont = list(color = NULL, family = NULL, size = numeric(0)),
            TitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            AxisLine = list(Color = "#000000", Width = 1.50003750093752,
            Style = "Solid"), MajorGridLine = list(Color = "#CCCCCC",
            Width = 0.750018750468762, Style = "Solid")), GapWidth = 30))

    res <- CChart("Line", dat.2d, append.data = TRUE, colors = col.2d,
            shape = "Curved", line.type = "Dot", line.thickness = "1,2,3",
            marker.show = TRUE, marker.size = 10,
            data.label.show = TRUE, data.label.font.autocolor = TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries,
            list(list(BackgroundColor = "#5C9AD3FF", Marker = list(BackgroundColor = "#5C9AD3"),
            ShowDataLabels = TRUE, DataLabelsFont = list(family = NULL,
            size = numeric(0), color = "#5C9AD3"), DataLabelPosition = "BestFit",
            OutlineColor = "#5C9AD3", OutlineWidth = 0.750018750468762, OutlineStyle = "Dot"),
            list(BackgroundColor = "#ED7D31FF", Marker = list(BackgroundColor = "#ED7D31"),
            ShowDataLabels = TRUE, DataLabelsFont = list(family = NULL,
            size = numeric(0), color = "#ED7D31"), DataLabelPosition = "BestFit",
            OutlineColor = "#ED7D31", OutlineWidth = 1.50003750093752, OutlineStyle = "Dot"),
            list(BackgroundColor = "#A5A5A5FF", Marker = list(BackgroundColor = "#A5A5A5"),
            ShowDataLabels = TRUE, DataLabelsFont = list(family = NULL,
            size = numeric(0), color = "#A5A5A5"), DataLabelPosition = "BestFit",
            OutlineColor = "#A5A5A5", OutlineWidth = 2.25005625140629, OutlineStyle = "Dot")))

    res <- CChart("Radar", dat.2d, append.data = TRUE, colors = col.2d, line.thickness = 2)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries,
            list(list(BackgroundColor = "#5C9AD366", Marker = list(BackgroundColor = "#5C9AD3"),
            ShowDataLabels = FALSE, DataLabelsFont = list(family = NULL,
            size = numeric(0), color = NA_character_), DataLabelPosition = "BestFit",
            OutlineColor = "#5C9AD3", OutlineWidth = 1.50003750093752, OutlineStyle = "Solid"),
            list(BackgroundColor = "#ED7D3166", Marker = list(BackgroundColor = "#ED7D31"),
            ShowDataLabels = FALSE, DataLabelsFont = list(family = NULL,
            size = numeric(0), color = NA_character_), DataLabelPosition = "BestFit",
            OutlineColor = "#ED7D31", OutlineWidth = 1.50003750093752, OutlineStyle = "Solid"),
            list(BackgroundColor = "#A5A5A566", Marker = list(BackgroundColor = "#A5A5A5"),
            ShowDataLabels = FALSE, DataLabelsFont = list(family = NULL,
            size = numeric(0), color = NA_character_), DataLabelPosition = "BestFit",
            OutlineColor = "#A5A5A5", OutlineWidth = 1.50003750093752, OutlineStyle = "Solid")))

    res <- CChart("Donut", dat.1d[1:4], append.data = TRUE, colors = col.1d.multicolor[1:4],
            pie.inner.radius = 40, pie.border.color = "#FFFFFF")
    expect_equal(attr(res, "ChartSettings"),
            list(TemplateSeries = list(list(BackgroundColor = "#E41A1CFF",
            Marker = list(BackgroundColor = "#E41A1C"), ShowDataLabels = FALSE,
            DataLabelsFont = list(family = NULL, size = numeric(0), color = NA_character_),
            DataLabelPosition = "BestFit", OutlineColor = "#FFFFFF",
            OutlineWidth = 0.750018750468762, OutlineStyle = "None"),
            list(BackgroundColor = "#4A72A6FF", Marker = list(BackgroundColor = "#4A72A6"),
            ShowDataLabels = FALSE, DataLabelsFont = list(family = NULL,
            size = numeric(0), color = NA_character_), DataLabelPosition = "BestFit",
            OutlineColor = "#FFFFFF", OutlineWidth = 0.750018750468762,
            OutlineStyle = "None"), list(BackgroundColor = "#48A462FF",
            Marker = list(BackgroundColor = "#48A462"), ShowDataLabels = FALSE,
            DataLabelsFont = list(family = NULL, size = numeric(0),
            color = NA_character_), DataLabelPosition = "BestFit",
            OutlineColor = "#FFFFFF", OutlineWidth = 0.750018750468762,
            OutlineStyle = "None"), list(BackgroundColor = "#7E6E85FF",
            Marker = list(BackgroundColor = "#7E6E85"), ShowDataLabels = FALSE,
            DataLabelsFont = list(family = NULL, size = numeric(0),
            color = NA_character_), DataLabelPosition = "BestFit",
            OutlineColor = "#FFFFFF", OutlineWidth = 0.750018750468762,
            OutlineStyle = "None")), Legend = list(Font = list(color = NULL,
            family = NULL, size = numeric(0))), ChartTitleFont = list(
            color = NULL, family = NULL, size = numeric(0)), HoleSize = 40))

    res <- CChart("Pie", dat.1d[5:9], append.data = TRUE, pie.border.color = "#333333",
            colors = col.1d.multicolor[5:9])
    expect_equal(attr(res, "ChartSettings"),
            list(TemplateSeries = list(list(BackgroundColor = "#D16948FF",
            Marker = list(BackgroundColor = "#D16948"), ShowDataLabels = FALSE,
            DataLabelsFont = list(family = NULL, size = numeric(0), color = NA_character_),
            DataLabelPosition = "BestFit", OutlineColor = "#333333",
            OutlineWidth = 0.750018750468762, OutlineStyle = "None"),
            list(BackgroundColor = "#FFB716FF", Marker = list(BackgroundColor = "#FFB716"),
            ShowDataLabels = FALSE, DataLabelsFont = list(family = NULL,
            size = numeric(0), color = NA_character_), DataLabelPosition = "BestFit",
            OutlineColor = "#333333", OutlineWidth = 0.750018750468762,
            OutlineStyle = "None"), list(BackgroundColor = "#E1C62FFF",
            Marker = list(BackgroundColor = "#E1C62F"), ShowDataLabels = FALSE,
            DataLabelsFont = list(family = NULL, size = numeric(0),
            color = NA_character_), DataLabelPosition = "BestFit",
            OutlineColor = "#333333", OutlineWidth = 0.750018750468762,
            OutlineStyle = "None"), list(BackgroundColor = "#B75F49FF",
            Marker = list(BackgroundColor = "#B75F49"), ShowDataLabels = FALSE,
            DataLabelsFont = list(family = NULL, size = numeric(0),
            color = NA_character_), DataLabelPosition = "BestFit",
            OutlineColor = "#333333", OutlineWidth = 0.750018750468762,
            OutlineStyle = "None"), list(BackgroundColor = "#EC83BAFF",
            Marker = list(BackgroundColor = "#EC83BA"), ShowDataLabels = FALSE,
            DataLabelsFont = list(family = NULL, size = numeric(0),
            color = NA_character_), DataLabelPosition = "BestFit",
            OutlineColor = "#333333", OutlineWidth = 0.750018750468762,
            OutlineStyle = "None")), Legend = list(Font = list(color = NULL,
            family = NULL, size = numeric(0))), ChartTitleFont = list(
            color = NULL, family = NULL, size = numeric(0))))

    res <- CChart("ColumnMultiColor", dat.1d, append.data = TRUE, bar.gap = 0.4,
            colors = col.1d.multicolor, opacity = 0.7, marker.border.opacity = 1,
            marker.border.width = 1, marker.border.color = "#FF0000")
    expect_equal(attr(res, "ChartSettings"),
            list(TemplateSeries = list(CustomPoints = list(list(BackgroundColor = "#E41A1CB2",
            Index = 0), list(BackgroundColor = "#4A72A6B2", Index = 1),
            list(BackgroundColor = "#48A462B2", Index = 2), list(BackgroundColor = "#7E6E85B2",
            Index = 3), list(BackgroundColor = "#D16948B2", Index = 4),
            list(BackgroundColor = "#FFB716B2", Index = 5), list(BackgroundColor = "#E1C62FB2",
            Index = 6), list(BackgroundColor = "#B75F49B2", Index = 7),
            list(BackgroundColor = "#EC83BAB2", Index = 8), list(BackgroundColor = "#999999B2",
            Index = 9)), ShowDataLabels = FALSE, DataLabelsFont = list(
            family = NULL, size = numeric(0), color = NA_character_),
            DataLabelPosition = "BestFit", OutlineColor = "#FF0000",
            OutlineWidth = 0.750018750468762, OutlineStyle = "Solid"),
            Legend = list(Font = list(color = NULL, family = NULL, size = numeric(0))),
            ChartTitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            PrimaryAxis = list(LabelsFont = list(color = NULL, family = NULL,
            size = numeric(0)), TitleFont = list(color = NULL, family = NULL,
            size = numeric(0)), AxisLine = list(Color = NULL, Width = numeric(0),
            Style = "Solid"), MajorGridLine = list(Color = NULL,
            Width = numeric(0), Style = "Solid"), RotateLabels = FALSE),
            ValueAxis = list(LabelsFont = list(color = NULL, family = NULL,
            size = numeric(0)), TitleFont = list(color = NULL, family = NULL,
            size = numeric(0)), AxisLine = list(Color = NULL, Width = numeric(0),
            Style = "Solid"), MajorGridLine = list(Color = NULL,
            Width = numeric(0), Style = "Solid")), GapWidth = 40,
            ShowLegend = FALSE))

    res <- CChart("Pyramid", dat.1d[1:4], append.data = TRUE, bar.gap = 0.6,
            colors = col.1d.multicolor[1:4], opacity = 0.3)
    expect_equal(attr(res, "ChartSettings"),
            list(TemplateSeries = list(CustomPoints = list(list(BackgroundColor = "#E41A1C4C",
            Index = 0), list(BackgroundColor = "#4A72A64C", Index = 1),
            list(BackgroundColor = "#48A4624C", Index = 2), list(BackgroundColor = "#7E6E854C",
            Index = 3)), ShowDataLabels = FALSE, DataLabelsFont = list(
            family = NULL, size = numeric(0), color = NA_character_),
            DataLabelPosition = "BestFit", OutlineColor = "#E41A1C",
            OutlineWidth = 0.750018750468762, OutlineStyle = "None"),
            Legend = list(Font = list(color = NULL, family = NULL, size = numeric(0))),
            ChartTitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            PrimaryAxis = list(LabelsFont = list(color = NULL, family = NULL,
            size = numeric(0)), TitleFont = list(color = NULL, family = NULL,
            size = numeric(0)), AxisLine = list(Color = NULL, Width = numeric(0),
            Style = "Solid"), MajorGridLine = list(Color = NULL,
            Width = numeric(0), Style = "Solid"), RotateLabels = FALSE),
            ValueAxis = list(LabelsFont = list(color = NULL, family = NULL,
            size = numeric(0)), TitleFont = list(color = NULL, family = NULL,
            size = numeric(0)), AxisLine = list(Color = NULL, Width = numeric(0),
            Style = "Solid"), MajorGridLine = list(Color = NULL,
            Width = numeric(0), Style = "Solid")), GapWidth = 60,
            ShowLegend = FALSE))
})
