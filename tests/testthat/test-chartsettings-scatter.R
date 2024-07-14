context("ChartSettings - Scatter")

dat <- LifeCycleSavings

test_that("ChartSettings - Scatter",
{
    v.ind <-  c(x = 1, y = 2, sizes = 3, colors = NA, groups = 5)
    attr(dat, "scatter.variable.indices") <- v.ind
    res <- CChart("Scatter", dat, scatter.labels.as.hovertext = FALSE,
            scatter.colors.column = NA, data.label.font.autocolor = TRUE,
            scatter.sizes.as.diameter = FALSE, scatter.colors.as.categorical = TRUE,
            values.grid.width = 0, categories.grid.width = 0, # defaults for unused controls
            grid.show = TRUE,
            colors = "#0000FF", marker.size = 6, opacity = 0.3, append.data = TRUE)
    expect_equal(attr(res, "ChartSettings")$BubbleScale, 60)
    expect_equal(attr(res, "ChartSettings")$BubbleSizeType, "Area")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$ShowDataLabels, TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$DataLabelsFont$color, "#0000FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor, "#0000FF4C")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$Marker,
            list(Size = 6, OutlineStyle = "None", BackgroundColor = "#0000FF4C"))
    expect_equal(attr(res, "ChartSettings")$PrimaryAxis$MajorGridLine,
            list(Style = "Solid", Width = 1, Color = "#E1E1E1"))
    expect_equal(attr(res, "ChartSettings")$ValueAxis$MajorGridLine,
            list(Style = "Solid", Width = 1, Color = "#E1E1E1"))
    expect_equal(attr(res, "ChartLabels"), list(PrimaryAxisTitle = "sr", ValueAxisTitle = "pop15"))
    expect_true(attr(res, "ChartSettings")$PrimaryAxis$ShowTitle)
    expect_true(attr(res, "ChartSettings")$ValueAxis$ShowTitle)

    v.ind <-  c(x = 1, y = 2, sizes = 3, colors = 4, groups = 5)
    attr(dat, "scatter.variable.indices") <- v.ind
    res <- CChart("Scatter", dat, scatter.labels.as.hovertext = TRUE,
            scatter.colors.column = NA, marker.size = 10,
            scatter.sizes.as.diameter = TRUE, scatter.colors.as.categorical = TRUE,
            colors = "#0000FF", append.data = TRUE)
    expect_equal(attr(res, "ChartSettings")$BubbleScale, 100)
    expect_equal(attr(res, "ChartSettings")$BubbleSizeType, "Width")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$ShowDataLabels, FALSE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor, "#0000FF66")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$Marker,
            list(Size = 10, OutlineStyle = "None", BackgroundColor = "#0000FF66"))

    v.ind <-  c(x = 1, y = 2, sizes = NA, colors = 4, groups = 5)
    attr(dat, "scatter.variable.indices") <- v.ind
    res <- CChart("Scatter", dat[1:3,], scatter.labels.as.hovertext = TRUE,
            scatter.sizes.column = NA, scatter.colors.as.categorical = TRUE,
            colors = c("#FF0000", "#00FF00", "#0000FF"), marker.size = 6,
            append.data = TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[2]]$BackgroundColor, "#00FF00FF")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[3]]$Marker,
            list(Size = 6, OutlineStyle = "None", BackgroundColor = "#0000FFFF"))

    v.ind <-  c(x = 1, y = 2, sizes = 3, colors = 4, groups = 5)
    attr(dat, "scatter.variable.indices") <- v.ind
    res <- CChart("Scatter", dat, scatter.colors.as.categorical = FALSE,
          colors = c("red", "white", "blue"), marker.size = 10, append.data = TRUE)
    expect_equal(attr(res, "ChartType"), "Bubble")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineStyle, "None")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$CustomPoints[[1]],
        list(Index = 0, Marker = list(BackgroundColor = "#D9D9FF66",
        Style = "Circle", Size = 10)))
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$CustomPoints[[16]],
		list(Index = 15, Marker = list(BackgroundColor = "#FF656566",
        Style = "Circle", Size = 10)))
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$CustomPoints[[44]],
        list(Index = 43, Marker = list(BackgroundColor = "#0000FF66",
        Style = "Circle", Size = 10)))


    pasted <- list(structure(c("", "a", "b", "c", "d", "e", "f", "g", "x",
            "1", "2", "4", "2", "4", "6", "3", "y", "6", "7", "5", "5", "2",
            "8", "9", "z", "1", "2", "3", "4", "5", "6", "7"), .Dim = c(8L,
            4L)), FALSE, NULL, NULL)
    pd <- PrepareData("Scatter", input.data.pasted = pasted)
    res <- CChart("Scatter", pd$data, colors = "#FF0000", opacity = NULL,
                  append.data = TRUE, scatter.colors.as.categorical = TRUE)
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$BackgroundColor, "#FF000066")

})


dat.char.colors <- structure(list(`X-coord` = c(2, 6, 7, 3, 4), `Y-coord` = c(3,
5, 7, 3, 8), Size = c(1, 2, 3, 4, 5), Group = c("A", "A", "B",
"B", "C")), row.names = c("alpha", "beta", "gamma", "delta",
"epsilon"), scatter.variable.indices = c(x = 1, y = 2, sizes = 3,
colors = 4, groups = 4), class = "data.frame")

dat.char.coords <- structure(list(Day = c("Monday", "Tuesday", "Monday", "Wednesay",
"Friday", "Monday", "Tuesday", "Thursday", "Wednesday"), Time = c("Lunch",
"Dinner", "Breakfast", "Breakfast", "Lunch", "Lunch", "Dinner",
"Lunch", "Lunch")), row.names = c("Amy", "Ben", "Jim", "Tim",
"Ian", "Sara", "Rod", "Bob", "Liz"), scatter.variable.indices = c(x = 1,
y = 2, sizes = NA, colors = NA, groups = 2), class = "data.frame")

dat.factor.coords <- structure(list(Species = structure(c(2L, 2L, 2L, 1L, 3L, 2L,
3L, 3L, 2L, 2L, 2L, 2L, 3L, 2L, 2L, 3L, 3L, 3L, 1L, 1L), .Label = c("setosa",
"versicolor", "virginica"), class = "factor"), `Grid: Petal.Length` = c(4.7,
4.4, 4.1, 1.7, 5.1, 3.7, 4.8, 6.4, 4.4, 4.7, 4.1, 4.5, 5.1, 4.5,
4.1, 6.7, 5.4, 5.7, 1.6, 1.4)), scatter.variable.indices = c(x = 1,
y = 2, sizes = NA, colors = NA, groups = NA), row.names = c(64L,
91L, 68L, 24L, 111L, 82L, 139L, 132L, 88L, 51L, 89L, 69L, 134L,
85L, 100L, 123L, 140L, 125L, 26L, 18L), class = "data.frame")

test_that("Scatterplot ChartData conversion",
{
    res <- convertChartDataToNumeric(dat.char.colors)
    expect_equal(unlist(lapply(res, class)), c(`X-coord` = "numeric",
            `Y-coord` = "numeric", Size = "numeric", Group = "factor"))
    expect_equal(dim(res), dim(dat.char.colors))
    expect_equal(attr(res, "scatter.variable.indices"),
            attr(dat.char.colors, "scatter.variable.indices"))

    res <- convertChartDataToNumeric(dat.char.coords)
    expect_equal(unlist(lapply(res, class)), c(Day = "integer", Time = "integer"))
    expect_equal(dim(res), dim(dat.char.coords))
    expect_equal(attr(res, "scatter.variable.indices"),
            attr(dat.char.coords, "scatter.variable.indices"))

    res <- convertChartDataToNumeric(dat.factor.coords)
    expect_equal(unlist(lapply(res, class)),
            c(Species = "integer", `Grid: Petal.Length` = "numeric"))
    expect_equal(dim(res), dim(dat.factor.coords))
    expect_equal(attr(res, "scatter.variable.indices"),
            attr(dat.factor.coords, "scatter.variable.indices"))
})

dat <- structure(list(multi1 = structure(c(6L, 5L, 3L, 3L, 3L, 2L, 4L,
2L, 9L, 2L, 6L, 4L, 7L, 6L, 10L, 7L, 6L, 2L, 3L, 5L), .Dim = c(10L,
2L), .Dimnames = list(c("A", "B", "C", "D", "E", "F", "G", "H",
"I", "J"), c("Col 1", "Col 2"))), multi2 = structure(c(7, 6,
4, 4, 4, 3, 5, 3, 10, 3, 6, 4, 7, 6, 10, 7, 6, 2, 3, 5), .Dim = c(10L,
2L), .Dimnames = list(c("A", "B", "C", "D", "E", "F", "G", "H",
"I", "J"), c("Col 1", "Col 2"))), multi3 = structure(c(7, 6,
4, 4, 4, 3, 5, 3, 10, 3, 7, 5, 8, 7, 11, 8, 7, 3, 4, 6), .Dim = c(10L,
2L), .Dimnames = list(c("A", "B", "C", "D", "E", "F", "G", "H",
"I", "J"), c("Col 1", "Col 2")))), scatter.variable.indices = c(x = 1,
y = 2, sizes = 3, colors = 4))
test_that("Scatter with multiple tables",
{
    expect_warning(CChart("Scatter", dat, append.data = TRUE))
})
