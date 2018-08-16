context("Prepare Colors")
xx <- 1:5
x2d <- cbind(A = 1:5, B = 2:6, C = 3:7)

test_that("Warnings", {
    expect_warning(CChart("Pie", xx, colors = PrepareColors(xx, "Pie", palette = "Custom color", palette.custom.color = "red")$series.colors), "Only a single color specified")
    # Pie chart may potentially (but not in this case) contain missing values so less colors are allowed
    expect_silent(PrepareColors(xx, "Donut", palette = "Custom palette", palette.custom.palette = "red,green"))

    expect_silent(CChart("Bar Pictograph", xx, colors = PrepareColors(xx, "Bar Pictograph", palette = "Custom color", palette.custom.color = "red")$series.colors))
    expect_silent(CChart("Bar Pictograph", xx, colors = PrepareColors(xx, "Bar Pictograph", palette = "Strong colors")$series.colors))
    expect_warning(PrepareColors(xx, "Bar Pictograph", palette = "Custom palette", palette.custom.palette = "red,blue"), "Custom palette does not have the number of colors required")

    expect_error(CChart("Column", xx, colors = PrepareColors(xx, "Column", palette = "Custom color", palette.custom.color = "red")$series.colors), NA)
    expect_warning(CChart("Column", x2d, colors = PrepareColors(x2d, "Column", palette = "Custom color", palette.custom.color = "red")$series.colors), "Only a single color specified for multiple series.")
    expect_error(CChart("Column", x2d, small.multiples = TRUE, colors = PrepareColors(x2d, "Column", small.multiples = TRUE, palette = "Custom color", palette.custom.color = "red")$series.colors), NA)
    expect_silent(PrepareColors(x2d, "Column", small.multiples = TRUE, palette = "Custom color", palette.custom.color = "red", fit.palette = "Custom color",fit.palette.custom.color = "red"))
    expect_silent(CChart("Column", x2d, colors = PrepareColors(x2d, "Column", palette = "Strong colors")$series.colors))

    expect_silent(PrepareColors(x2d, "Time Series", palette = "Strong colors"))
    expect_silent(PrepareColors(x2d, "Time Series", palette = "Custom palette", palette.custom.palette = "red,blue,green"))
    expect_warning(PrepareColors(x2d, "Time Series", palette = "Custom palette", palette.custom.palette = "red,blue"), "Custom palette does not have the number of colors required")
    # Chart may be created showing rangebar (i.e. only 1 series)
    expect_silent(PrepareColors(x2d, "Time Series", palette = "Custom color", palette.custom.color = "red"))
})

test_that("Pie chart", {
    expect_error(PrepareColors(table(1:10), "Pie", palette = "Greys, light to dark", subslice.palette = "Greys, light to dark"), NA)
})
