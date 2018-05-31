context("Prepare Colors")
xx <- 1:5
x2d <- cbind(A = 1:5, B = 2:6)

test_that("Warnings", {
    expect_warning(CChart("Pie", xx, colors = PrepareColors(xx, "Pie", palette = "Custom color", palette.custom.color = "red")$series.colors))
    expect_error(CChart("Bar Pictograph", xx, colors = PrepareColors(xx, "Bar Pictograph", palette = "Custom color", palette.custom.color = "red")$series.colors), NA)
    expect_error(CChart("Bar Pictograph", xx, colors = PrepareColors(xx, "Bar Pictograph", palette = "Strong colors")$series.colors), NA)
    expect_error(CChart("Column", xx, colors = PrepareColors(xx, "Column", palette = "Custom color", palette.custom.color = "red")$series.colors), NA)
    expect_warning(CChart("Column", x2d, colors = PrepareColors(x2d, "Column", palette = "Custom color", palette.custom.color = "red")$series.colors))
    expect_error(CChart("Column", x2d, small.multiples = TRUE, colors = PrepareColors(x2d, "Column", small.multiples = TRUE, palette = "Custom color", palette.custom.color = "red")$series.colors), NA)
    expect_error(CChart("Column", x2d, colors = PrepareColors(x2d, "Column", palette = "Strong colors")$series.colors), NA)
})

test_that("Pie chart", {
    expect_error(PrepareColors(table(1:10), "Pie", palette = "Greys, light to dark", subslice.palette = "Greys, light to dark"), NA)
})
