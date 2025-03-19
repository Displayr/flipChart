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
            categories.title = "Letters",
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
    expect_equal(attr(res, "ChartLabels")$PrimaryAxisTitle, "Letters")
    expect_equal(attr(res, "ChartSettings")$ValueAxis$Maximum, 6)
    expect_equal(attr(res, "ChartSettings")$ValueAxis$Minimum, -3)

    res <- CChart("Bar", dat.1d, append.data = TRUE, colors = "#FF0000",
            values.grid.width = 1, categories.grid.width = 0,
            values.grid.color = "#CCCCCC", categories.grid.color = "#BBBBBB",
            values.line.color = "#000000", categories.line.color = "#222222",
            values.line.width = 2, categories.line.width = 2,
            values.zero.line.width = 2, values.zero.line.color = "#0000FF",
            data.label.show = TRUE, bar.gap = 0.0,
            marker.border.color = "#000000", marker.border.width = 2)
    expect_equal(length(attr(res, "ChartSettings")$TemplateSeries), 1)
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis, list(
            LabelsFont = list(color = NULL, family = NULL, size = numeric(0)),
            ShowTitle = FALSE,
            TitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            NumberFormat = "General",
            AxisLine = list(Color = "#0000FF", Width = 1.50003750093752,
            Style = "Solid"), Crosses = "AutoZero", MajorGridLine = list(Color = "#BBBBBB",
            Width = 0, Style = "None"), RotateLabels = FALSE, LabelPosition = "Low"))
    expect_equal(attr(res, "ChartSettings")$ValueAxis, list(
            LabelsFont = list(color = NULL, family = NULL, size = numeric(0)),
            ShowTitle = FALSE,
            TitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            NumberFormat = "General",
            AxisLine = list(Color = "#000000", Width = 1.50003750093752,
            Style = "Solid"), MajorGridLine = list(Color = "#CCCCCC",
            Width = 0.750018750468762, Style = "Solid")))
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
    #expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$ShowDataLabels, TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$DataLabelsPosition, "InsideEnd")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$DataLabelsFont$color, "#2C2C2C")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$DataLabelsFont$color, "#2C2C2C")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$DataLabelsFont$color, "#FFFFFF")
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis, list(
            LabelsFont = list(color = NULL, family = NULL, size = numeric(0)),
            ShowTitle = FALSE,
            TitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            NumberFormat = "General",
            AxisLine = list(Color = "#222222", Width = 1.50003750093752,
            Style = "Solid"), Crosses = "Minimum", MajorGridLine = list(Color = "#BBBBBB",
            Width = 0, Style = "None"), RotateLabels = TRUE, LabelPosition = "Low"))
    expect_equal(attr(res, "ChartSettings")$ValueAxis, list(
            LabelsFont = list(color = NULL, family = NULL, size = numeric(0)),
            ShowTitle = FALSE,
            TitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            NumberFormat = "General",
            AxisLine = list(Color = "#000000", Width = 1.50003750093752,
            Style = "Solid"), Crosses = "Minimum", MajorGridLine = list(Color = "#CCCCCC",
            Width = 0.750018750468762, Style = "Solid")))
    expect_equal(attr(res, "ChartSettings")$GapWidth, 42.85714, tolerance = 1e-3)

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
    #expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$ShowDataLabels, TRUE)
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
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[4]]$DataLabelsPosition, "OutsideEnd")
    expect_equal(attr(res, "ChartSettings")$FirstSliceAngle, 270)
    expect_equal(attr(res, "ChartLabels")$SeriesLabels[[1]]$ShowValue, TRUE)

    res <- CChart("Pie", dat.1d[5:9]/100, append.data = TRUE, pie.border.color = "#333333",
            colors = col.1d.multicolor[5:9], data.label.format = "%")
    expect_equal(attr(res, "ChartLabels")$SeriesLabels[[1]],
        list(CustomPoints = list(list(Index = 0, Segments = list(list(
        Field = "CategoryName"), list(Text = ": "), list(Field = "Value")))),
        NumberingFormat = "0.#%"))

    res <- CChart("Pie", abs(dat.2d), append.data = TRUE)
    expect_equal(attr(res, "ChartType"), "Sunburst")
    expect_true(grepl("This visualization is a 2-dimensional Pie chart which cannot be exported to PowerPoint.",
                      attr(res, "ChartWarning")))

    res <- CChart("Bar", dat.2d, small.multiples = TRUE, append.data = TRUE)
    expect_true(grepl("Small multiples are not supported by PowerPoint",
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
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineColor, "#FF0000FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineWidth, 0.750018750468762)
    expect_equal(attr(res, "ChartSettings")$GapWidth, 66.66667, tolerance = 1e-3)
    expect_equal(attr(res, "ChartSettings")$ShowLegend, FALSE)

    res <- CChart("Pyramid", dat.1d[1:4], append.data = TRUE, bar.gap = 0.6,
            colors = col.1d.multicolor[1:4], opacity = 0.3)
    expect_equal(length(attr(res, "ChartSettings")$TemplateSeries), 1)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$CustomPoints,
            list(list(BackgroundColor = "#E41A1C4C", Index = 0),
            list(BackgroundColor = "#4A72A64C", Index = 1), list(BackgroundColor = "#48A4624C",
            Index = 2), list(BackgroundColor = "#7E6E854C", Index = 3)))
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineStyle, "None")
    expect_equal(attr(res, "ChartSettings")$GapWidth, 150)
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

    res <- CChart("Bar Pictograph", dat.1d, append.data = TRUE)
    expect_equal(length(attr(res, "ChartSettings")$TemplateSeries), 1)
    expect_equal(length(attr(res, "ChartSettings")$TemplateSeries[[1]]$CustomPoints), 10)
    expect_true(!is.null(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineColor))
    expect_true(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineWidth < 1)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineStyle, "None")
    expect_true(!is.null(attr(res, "ChartWarning")))

    res <- CChart("CombinedScatter", dat.2d, values.zero.line.width = 2,
        values.zero.line.color = "#FF0000", values.zero.line.dash = "dot",
        categories.zero.line.width = 1.5, categories.zero.line.color = "#008000",
        append.data = TRUE)
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$Crosses, "AutoZero")
    expect_equal(attr(res, "ChartSettings")$ValueAxis$Crosses, "AutoZero")
})

test_that("Scatter axes bounds",
{
    dat1 <- structure(list(` ` = c(16.5292618516667, 0.479370604963302, 19.8251578509455,
        2.52774200787021, 12.691554218326, 7.10203021588027, 5.52494604558057,
        10.535784457086, 3.24015111335468, 10.9507195992616, 2.69311285214555,
        4.19706622209025, 2.64466606664772, 1.05843689418161), table.Dimensions.2 = c(79,
        76, 69, 82, 77, 74, 83, 75, 85, 80, 74, 78, 83, 76)), row.names = c("Hearing Aids",
        "Apps &amp; Digital Solutions", "Pricing", "Training", "Business Support",
        "Marketing", "Sales Representative", "Complaints Response", "Support",
        "Accessibility", "Repairs/Remakes", "Orders", "Invoices", "Shipping Packaging"
        ), scatter.variable.indices = c(x = 1, y = 2, sizes = NA, colors = NA,
        groups = NA), class = "data.frame")
    expect_error(res1 <- CChart("Scatter", dat1, append.data = TRUE), NA)
    expect_equal(attr(res1, "ChartSettings")$ValueAxis$Minimum, 60)
    expect_equal(attr(res1, "ChartSettings")$PrimaryAxis$Minimum, -10)

    dat2 <- structure(c(11.6216773130023, 38.9655882958994, 6.75870595507933,
        30.0638780135998, 13.1052956933855, 42.5509993818257, 64.9907273851226,
        23.9027405728415, 42.8394807335669, 11.456830826293, 5.17205852050278,
        40.9849577580878, 52.3593653410262, 9.56109622913662, 0.88604986606223,
        49.9484854729034, 88.7492272820935, 22.5221512466516, 81.6814341644344,
        69.0912837420152, 89.4498248506079, 94.5600659385947, 83.9892849783639,
        91.0570780960231, 62.930146301257, 19.7403667834329, 81.104471460952,
        89.0171028229961, 34.3292808572017, NA, 26.3548320626417, 43.6018957345972,
        13.434988666804, 36.8637955903565, 29.157222336699, 48.4648670925201,
        53.8429837214094, 32.4953636925613, 59.9629095404904, 22.9548732742633,
        12.239851638162, 58.2320214300433, 54.9969091283742, 12.6725736657737,
        0.638780135998352), .Dim = c(15L, 3L), .Dimnames = list(c("Burger Shack",
        "Burger Chef", "Nuovo Burger", "Lucky's Pizza", "Pizza Heaven",
        "Southern Fried Chicken", "Arnold's", "Nero's Pizza", "Pret'a'pane",
        "Ma's burgers", "Bread Basket", "Asian", "Mexican", "Other fast food",
        "None of these"), c("table.Q2.Eaten.bought.last.month", "table.Q3.Ever.Eaten.3",
        "table.Q4.Consider.3")), assigned.rownames = TRUE,
        scatter.variable.indices = c(x = 1, y = 2, sizes = 3, colors = NA, groups = 3))
    expect_warning(res2 <- CChart("Scatter", dat2, append.data = TRUE), "missing values")
    expect_equal(attr(res2, "ChartSettings")$ValueAxis$Minimum, 10)
    expect_equal(attr(res2, "ChartSettings")$PrimaryAxis$Minimum, -10)

    dat3 <- structure(c(6.33333333333333, 56, 21.8333333333333, 9.16666666666667,
        60.5, 9.16666666666667, 9.66666666666667, 1.83333333333333, 58.3333333333333,
        53.6666666666667, 2.5, 57.5, 31.3333333333333, 17.8333333333333
        ), .Dim = c(7L, 2L), .Dimnames = list(c("Coke", "Diet Coke",
        "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max", "None of these"
        ), c("Feminine", "Health-conscious")), statistic = "%", basedescriptiontext = "sample size = 600", basedescription = list(
            Minimum = 600L, Maximum = 600L, Range = FALSE, Total = 600L,
            Missing = 0L, EffectiveSampleSize = 600L, EffectiveSampleSizeProportion = 100,
            FilteredProportion = 0), questiontypes = "PickAnyGrid", span = list(
            rows = structure(list(c("Coke", "Diet Coke", "Coke Zero",
            "Pepsi", "Diet Pepsi", "Pepsi Max", "None of these", "NET"
            )), class = "data.frame", .Names = "", row.names = c(NA,
            8L)), columns = structure(list(c("Feminine", "Health-conscious",
            "NET")), class = "data.frame", .Names = "", row.names = c(NA,
            3L))), name = "table.q5.2", questions = c("q5 2", "SUMMARY"
        ), assigned.rownames = TRUE, scatter.variable.indices = c(x = 1,
        y = 2, sizes = 3, colors = NA, groups = 3))
    expect_error(res3 <- CChart("Scatter", dat3, append.data = TRUE), NA)
    expect_equal(attr(res3, "ChartSettings")$ValueAxis$Minimum, -0.1)
    expect_equal(attr(res3, "ChartSettings")$PrimaryAxis$Minimum, 0)
})

test_that("Legend position",
{
    viz <- CChart("Column", dat.2d, signif.show = FALSE, append.data = TRUE,
              legend.orientation = "Vertical", legend.x.position = 1.0,
              legend.y.position = 1.2, data.label.show = FALSE)
    expect_equal(attr(viz, "ChartSettings")$Legend$Position, "Right")

    viz <- CChart("Column", dat.2d, signif.show = FALSE, append.data = TRUE,
              legend.orientation = "Horizontal", legend.x.position = 0.5,
              legend.y.position = -0.2, data.label.show = FALSE)
    expect_equal(attr(viz, "ChartSettings")$Legend$Position, "Bottom")

    viz <- CChart("Column", dat.2d, legend.show = FALSE, append.data = TRUE)
    expect_false(attr(viz, "ChartSettings")$ShowLegend)
    expect_equal(attr(viz, "ChartSettings")$Legend$Position, "Right")
})

test_that("Color opacity",
{
    viz <- CChart("Bar", dat.2d, append.data = TRUE,
        colors=c("#FF000080", "#00FF00", "blue"), opacity = 0.05,
        marker.border.width = 2, marker.border.color = "#222222", marker.border.opacity = 0.5)
    expect_equal(attr(viz, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor, "#FF000006")
    expect_equal(attr(viz, "ChartSettings")$TemplateSeries[[2]]$BackgroundColor, "#00FF000D")
    expect_equal(attr(viz, "ChartSettings")$TemplateSeries[[3]]$BackgroundColor, "#0000FF0D")
    expect_equal(attr(viz, "ChartSettings")$TemplateSeries[[3]]$OutlineColor, "#22222280")
    expect_equal(attr(viz, "ChartSettings")$TemplateSeries[[3]]$OutlineWidth, 1.500, tol = 1e-3)
})
