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
            categories.title = "Letters", categories.title.font.size = 11,
            values.bounds.maximum = 6, values.bounds.minimum = -3,
            values.tick.font.size = 9,
            data.label.font.family = "Arial", data.label.font.size = 10,
            data.label.show = TRUE, data.label.font.autocolor = TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor, "#BAE4B3FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$BackgroundColor, "#74C476FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$BackgroundColor, "#238B45FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$DataLabelsPosition, "BestFit")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$ShowDataLabels, TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$OutlineStyle, "None")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$DataLabelsFont,
            list(family = "Arial", size = 10, color = "#2C2C2C"))
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$DataLabelsFont$color,"#FFFFFF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$DataLabelsFont$color,"#2C2C2C")
    expect_equal(attr(res, "ChartSettings")$ShowChartTitle, FALSE)
    expect_equal(attr(res, "ChartLabels")$PrimaryAxisTitle, "Letters")
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$TitleFont$size, 11)
    expect_equal(attr(res, "ChartSettings")$ValueAxis$Maximum, 6)
    expect_equal(attr(res, "ChartSettings")$ValueAxis$Minimum, -3)
    expect_equal(attr(res, "ChartSettings")$ValueAxis$LabelsFont$size, 9)

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
            AxisLine = list(Color = "#0000FF", Width = 1.5,
            Style = "Solid"), Crosses = "AutoZero", MajorGridLine = list(Color = "#BBBBBB",
            Width = 0, Style = "None"), RotateLabels = FALSE, LabelPosition = "Low"))
    expect_equal(attr(res, "ChartSettings")$ValueAxis, list(
            LabelsFont = list(color = NULL, family = NULL, size = numeric(0)),
            ShowTitle = FALSE,
            TitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            NumberFormat = "General",
            AxisLine = list(Color = "#000000", Width = 1.5,
            Style = "Solid"), MajorGridLine = list(Color = "#CCCCCC",
            Width = 0.75, Style = "Solid")))
    expect_equal(attr(res, "ChartSettings")$GapWidth, 0)
    expect_true(is.null(attr(res, "ChartSettings")$ValueAxis$Crosses))

    res <- CChart("BarMultiColor", dat.1d, append.data = TRUE, colors = col.1d.multicolor)
    expect_true(is.null(attr(res, "ChartSettings")$ValueAxis$Crosses))

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
            AxisLine = list(Color = "#222222", Width = 1.5,
            Style = "Solid"), Crosses = "AutoZero", MajorGridLine = list(Color = "#BBBBBB",
            Width = 0, Style = "None"), RotateLabels = TRUE, LabelPosition = "Low"))
    expect_equal(attr(res, "ChartSettings")$ValueAxis, list(
            LabelsFont = list(color = NULL, family = NULL, size = numeric(0)),
            ShowTitle = FALSE,
            TitleFont = list(color = NULL, family = NULL, size = numeric(0)),
            NumberFormat = "General",
            AxisLine = list(Color = "#000000", Width = 1.5,
            Style = "Solid"), Crosses = "Minimum", MajorGridLine = list(Color = "#CCCCCC",
            Width = 0.75, Style = "Solid")))
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
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineWidth, 0.75)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$OutlineWidth, 1.5)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$OutlineWidth, 2.25)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$Marker,
           list(Size = 10, OutlineStyle = "None", BackgroundColor = "#ED7D31FF",
                Style = "Circle"))

    res <- CChart("Radar", dat.2d, append.data = TRUE, colors = col.2d, line.thickness = 2)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor, "#5C9AD366")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$BackgroundColor, "#ED7D3166")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$BackgroundColor, "#A5A5A566")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineColor, "#5C9AD3")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$OutlineColor, "#ED7D31")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineStyle, "Solid")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineWidth, 1.5)
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
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineWidth, 0.75)
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

    res <- CChart("CombinedScatter", abs(dat.2d) + 10,
        values.line.width = 2, categories.line.width = 2,
        append.data = TRUE)
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$Crosses, "Minimum")
    expect_equal(attr(res, "ChartSettings")$ValueAxis$Crosses, "Minimum")
})

test_that("FS2-4532: Line PPT settings are per-series", {
    res <- CChart("Line", dat.2d, append.data = TRUE, colors = col.2d,
            line.type = "Solid,Dot", marker.show = TRUE,
            marker.symbols = "circle,square", marker.size = "6,10,14")
    ts <- attr(res, "ChartSettings")$TemplateSeries
    expect_equal(ts[[1]]$OutlineStyle, "Solid")
    expect_equal(ts[[2]]$OutlineStyle, "Dot")
    expect_equal(ts[[3]]$OutlineStyle, "Solid")   # recycled
    expect_equal(ts[[1]]$Marker$Style, "Circle")
    expect_equal(ts[[2]]$Marker$Style, "Square")
    expect_equal(ts[[3]]$Marker$Style, "Circle")  # recycled
    expect_equal(ts[[1]]$Marker$Size, 6)
    expect_equal(ts[[2]]$Marker$Size, 10)
    expect_equal(ts[[3]]$Marker$Size, 14)
})

test_that("Radar PPT settings take a line type per series", {
    # Radar reaches the OutlineStyle loop the same way Line does, so a comma-separated
    # line type has to be split for it too rather than reaching PowerPoint as one string
    res <- CChart("Radar", dat.2d, append.data = TRUE, colors = col.2d,
                  line.type = "Solid,Dot")
    ts <- attr(res, "ChartSettings")$TemplateSeries
    expect_equal(ts[[1]]$OutlineStyle, "Solid")
    expect_equal(ts[[2]]$OutlineStyle, "Dot")
    expect_equal(ts[[3]]$OutlineStyle, "Solid")   # recycled
})

test_that("Radar line type reaches the chart as well as the export", {
    # It used to be read for PowerPoint but dropped on the way to the chart, so a dotted
    # radar exported dotted and rendered solid
    expect_warning(CChart("Radar", dat.2d, append.data = TRUE, colors = col.2d,
                          line.type = "Dot"), NA)
})

test_that("FS2-4532: scalar inputs still broadcast (old Plugins back-compat)", {
    res <- CChart("Line", dat.2d, append.data = TRUE, colors = col.2d,
            line.type = "Dash", marker.show = TRUE, marker.size = 8)
    ts <- attr(res, "ChartSettings")$TemplateSeries
    expect_equal(ts[[1]]$OutlineStyle, "Dash")
    expect_equal(ts[[3]]$OutlineStyle, "Dash")
    expect_equal(ts[[1]]$Marker$Size, 8)
    expect_equal(ts[[3]]$Marker$Size, 8)
})

test_that("FS2-4532: line type is only split per-series for the charts that support it", {
    # Line and Radar both take a line type per series, so a comma-separated value is split
    # across them. Time Series takes one line type for the whole chart, and splitting it
    # there would turn a single setting into a per-series one.
    dat <- matrix(1:6, 3, 2, dimnames = list(letters[1:3], c("A", "B")))
    args <- list(colors = c("#FF0000", "#00AA00"), line.type = "Solid,Dot")
    stylesFor <- function(chart.type)
        vapply(getPPTSettings(chart.type, args, dat)$TemplateSeries,
               function(s) s$OutlineStyle, character(1))

    expect_equal(stylesFor("Line"), c("Solid", "Dot"))
    expect_equal(stylesFor("Radar"), c("Solid", "Dot"))
    expect_equal(stylesFor("Time Series"), c("Solid,Dot", "Solid,Dot"))
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

test_that("Grid line type is exported to PowerPoint settings (RS-22447)",
{
    res <- suppressWarnings(CChart("Column", dat.1d, append.data = TRUE,
            values.grid.width = 1, values.grid.dash = "Dot",
            categories.grid.width = 1, categories.grid.dash = "Dash"))
    expect_equal(attr(res, "ChartSettings")$ValueAxis$MajorGridLine$Style, "Dot")
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$MajorGridLine$Style, "Dash")

    # A zero-width grid is still "None" regardless of the dash setting.
    res0 <- suppressWarnings(CChart("Column", dat.1d, append.data = TRUE,
            values.grid.width = 0, values.grid.dash = "Dot"))
    expect_equal(attr(res0, "ChartSettings")$ValueAxis$MajorGridLine$Style, "None")

    # Backwards compatible: no dash supplied still exports as "Solid".
    res1 <- suppressWarnings(CChart("Column", dat.1d, append.data = TRUE,
            values.grid.width = 1))
    expect_equal(attr(res1, "ChartSettings")$ValueAxis$MajorGridLine$Style, "Solid")

    # Backwards compatible: grid.width omitted entirely (NULL in the raw user
    # args seen by getPPTSettings) must keep the previous "Solid" default, not
    # collapse to "None".
    res2 <- suppressWarnings(CChart("Column", dat.1d, append.data = TRUE))
    expect_equal(attr(res2, "ChartSettings")$ValueAxis$MajorGridLine$Style, "Solid")
})

test_that("getGridLineStyle handles missing/NA widths (RS-22447)",
{
    # Only an explicit width of 0 hides the grid.
    expect_equal(getGridLineStyle(0, "Dot"), "None")
    expect_equal(getGridLineStyle(0, NULL), "None")
    # A visible grid uses the dash if given, otherwise "Solid".
    expect_equal(getGridLineStyle(1, "Dash"), "Dash")
    expect_equal(getGridLineStyle(1, NULL), "Solid")
    # Missing/NA width must not hide the grid or error - it keeps "Solid"
    # (the previous default) unless an explicit dash is supplied.
    expect_equal(getGridLineStyle(NULL, NULL), "Solid")
    expect_equal(getGridLineStyle(NA, NULL), "Solid")
    expect_equal(getGridLineStyle(NULL, "Dot"), "Dot")
})

test_that("Smooth follows the first series' shape, whatever form the shape arrives in", {
    dat <- matrix(1:6, 3, 2, dimnames = list(letters[1:3], c("A", "B")))
    smoothFor <- function(shape) {
        args <- list(colors = c("#FF0000", "#00AA00"))
        if (!is.null(shape)) args$shape <- shape
        getPPTSettings("Line", args, dat)$Smooth
    }

    # PowerPoint takes one setting for the whole chart, so a per-series shape has to pick
    # one; the first series is the same series other chart-wide settings are taken from.
    expect_true(smoothFor("Curved"))
    expect_false(smoothFor("Straight"))

    # The per-series forms: comma-separated as the Plugins send it, or a vector
    expect_true(smoothFor("Curved,Curved"))
    expect_true(smoothFor("Curved, Straight"))
    expect_false(smoothFor("Straight,Curved"))
    expect_true(smoothFor(c("Curved", "Straight")))
    expect_false(smoothFor(c("Straight", "Curved")))

    # Unset stays unsmoothed, and case does not matter
    expect_false(smoothFor(NULL))
    expect_true(smoothFor("curved"))

    # Curved and Straight are what the controls send, but the chart also takes plotly's own
    # names, and a chart drawn curved has to export curved whichever name asked for it
    expect_true(smoothFor("spline"))
    expect_true(smoothFor("Spline"))
    expect_false(smoothFor("linear"))
    expect_true(smoothFor("spline,linear"))
    expect_false(smoothFor("linear,spline"))
})

test_that("Numeric series settings export from every form a chart may have been saved with", {
    # The controls used to be a text box taking "6, 10, 14", and are now numeric ones, so a
    # deck exported today may come from either. Both have to keep working.
    dat <- matrix(1:9, 3, 3, dimnames = list(letters[1:3], c("A", "B", "C")))
    sizesFor <- function(v)
        vapply(getPPTSettings("Line", list(colors = c("#F00", "#0A0", "#00F"),
                                           marker.size = v), dat)$TemplateSeries,
               function(s) s$Marker$Size, numeric(1))

    expect_equal(sizesFor("6,10,14"), c(6, 10, 14))       # old text box, per series
    expect_equal(sizesFor(c(6, 10, 14)), c(6, 10, 14))    # new numeric controls, per series
    expect_equal(sizesFor(10), c(10, 10, 10))             # new numeric control, chart wide
    expect_equal(sizesFor("10"), c(10, 10, 10))           # old text box, chart wide
})

test_that("Marker symbols map to PowerPoint styles through their plotly variants", {
    expect_equal(markerSymbolToPPTStyle(c("circle", "square", "diamond")),
                 c("Circle", "Square", "Diamond"))

    # The six the control offers
    expect_equal(markerSymbolToPPTStyle(c("circle-open", "square-open", "diamond-open")),
                 c("Circle", "Square", "Diamond"))

    # Plotly appends -open and -dot to the family name, in either combination, and a caller
    # reaching past the control can send any of them
    expect_equal(markerSymbolToPPTStyle(c("square-open-dot", "diamond-open-dot", "circle-dot")),
                 c("Square", "Diamond", "Circle"))

    # Anything the lookup does not name falls back to a circle rather than an invalid style
    expect_equal(markerSymbolToPPTStyle(c("hexagram", "", NA)), rep("Circle", 3))
})

test_that("Every plotly family PowerPoint has a style for is mapped", {
    expect_equal(markerSymbolToPPTStyle(c("triangle-up", "triangle-down", "triangle-left")),
                 rep("Triangle", 3))
    expect_equal(markerSymbolToPPTStyle(c("x", "x-thin")), c("X", "X"))
    # plotly's cross is the upright +, which PowerPoint calls Plus; its x is the diagonal one
    expect_equal(markerSymbolToPPTStyle(c("cross", "cross-thin")), c("Plus", "Plus"))
    expect_equal(markerSymbolToPPTStyle(c("star", "star-triangle-up", "star-square")),
                 rep("Star", 3))

    # The variants of the new families go through the family rule like the rest
    expect_equal(markerSymbolToPPTStyle(c("triangle-up-open-dot", "x-thin-open", "star-open")),
                 c("Triangle", "X", "Star"))

    # A shape overlaid with a cross or an x is still that shape
    expect_equal(markerSymbolToPPTStyle(c("circle-cross", "square-x", "diamond-tall")),
                 c("Circle", "Square", "Diamond"))

    # The families PowerPoint has nothing for still fall back
    expect_equal(markerSymbolToPPTStyle(c("pentagon", "hexagon", "hexagram", "bowtie",
                                          "y-up", "line-ew", "arrow-up")),
                 rep("Circle", 7))
})
