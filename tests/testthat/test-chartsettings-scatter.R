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
          colors = c("red", "white", "blue"), append.data = TRUE)
    expect_equal(attr(res, "ChartType"), "Bubble")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$OutlineStyle, "None")
    expect_equal(attr(res, "ChartSettings")$TemplateSeries[[1]]$CustomPoints,
            list(list(Index = 0, BackgroundColor = "#D9D9FF66", Marker = list(
            BackgroundColor = "#D9D9FF66")), list(Index = 1, BackgroundColor = "#FFB8B866",
            Marker = list(BackgroundColor = "#FFB8B866")), list(Index = 2,
            BackgroundColor = "#F6F6FF66", Marker = list(BackgroundColor = "#F6F6FF66")),
            list(Index = 3, BackgroundColor = "#FF0D0D66", Marker = list(
            BackgroundColor = "#FF0D0D66")), list(Index = 4, BackgroundColor = "#FF535366",
            Marker = list(BackgroundColor = "#FF535366")), list(Index = 5,
            BackgroundColor = "#8484FF66", Marker = list(BackgroundColor = "#8484FF66")),
            list(Index = 6, BackgroundColor = "#FF4A4A66", Marker = list(
            BackgroundColor = "#FF4A4A66")), list(Index = 7, BackgroundColor = "#FF1A1A66",
            Marker = list(BackgroundColor = "#FF1A1A66")), list(Index = 8,
            BackgroundColor = "#FF181866", Marker = list(BackgroundColor = "#FF181866")),
            list(Index = 9, BackgroundColor = "#FF313166", Marker = list(
            BackgroundColor = "#FF313166")), list(Index = 10, BackgroundColor = "#C4C4FF66",
            Marker = list(BackgroundColor = "#C4C4FF66")), list(Index = 11,
            BackgroundColor = "#FF191966", Marker = list(BackgroundColor = "#FF191966")),
            list(Index = 12, BackgroundColor = "#FFCFCF66", Marker = list(
            BackgroundColor = "#FFCFCF66")), list(Index = 13, BackgroundColor = "#E9E9FF66",
            Marker = list(BackgroundColor = "#E9E9FF66")), list(Index = 14,
            BackgroundColor = "#C9C9FF66", Marker = list(BackgroundColor = "#C9C9FF66")),
            list(Index = 15, BackgroundColor = "#FF656566", Marker = list(
            BackgroundColor = "#FF656566")), list(Index = 16, BackgroundColor = "#FF1A1A66",
            Marker = list(BackgroundColor = "#FF1A1A66")), list(Index = 17,
            BackgroundColor = "#FF121266", Marker = list(BackgroundColor = "#FF121266")),
            list(Index = 18, BackgroundColor = "#FFECEC66", Marker = list(
            BackgroundColor = "#FFECEC66")), list(Index = 19, BackgroundColor = "#FF000066",
            Marker = list(BackgroundColor = "#FF000066")), list(Index = 20,
            BackgroundColor = "#FF888866", Marker = list(BackgroundColor = "#FF888866")),
            list(Index = 21, BackgroundColor = "#FFA9A966", Marker = list(
            BackgroundColor = "#FFA9A966")), list(Index = 22, BackgroundColor = "#FF989866",
            Marker = list(BackgroundColor = "#FF989866")), list(Index = 23,
            BackgroundColor = "#FF0F0F66", Marker = list(BackgroundColor = "#FF0F0F66")),
            list(Index = 24, BackgroundColor = "#CACAFF66", Marker = list(
            BackgroundColor = "#CACAFF66")), list(Index = 25, BackgroundColor = "#FF424266",
            Marker = list(BackgroundColor = "#FF424266")), list(Index = 26,
            BackgroundColor = "#E6E6FF66", Marker = list(BackgroundColor = "#E6E6FF66")),
            list(Index = 27, BackgroundColor = "#FFD7D766", Marker = list(
            BackgroundColor = "#FFD7D766")), list(Index = 28, BackgroundColor = "#FFB6B666",
            Marker = list(BackgroundColor = "#FFB6B666")), list(Index = 29,
            BackgroundColor = "#FF1E1E66", Marker = list(BackgroundColor = "#FF1E1E66")),
            list(Index = 30, BackgroundColor = "#FF3E3E66", Marker = list(
            BackgroundColor = "#FF3E3E66")), list(Index = 31, BackgroundColor = "#FF111166",
            Marker = list(BackgroundColor = "#FF111166")), list(Index = 32,
            BackgroundColor = "#FF282866", Marker = list(BackgroundColor = "#FF282866")),
            list(Index = 33, BackgroundColor = "#FF080866", Marker = list(
            BackgroundColor = "#FF080866")), list(Index = 34, BackgroundColor = "#FF3F3F66",
            Marker = list(BackgroundColor = "#FF3F3F66")), list(Index = 35,
            BackgroundColor = "#FF494966", Marker = list(BackgroundColor = "#FF494966")),
            list(Index = 36, BackgroundColor = "#FF151566", Marker = list(
            BackgroundColor = "#FF151566")), list(Index = 37, BackgroundColor = "#FF585866",
            Marker = list(BackgroundColor = "#FF585866")), list(Index = 38,
            BackgroundColor = "#5B5BFF66", Marker = list(BackgroundColor = "#5B5BFF66")),
            list(Index = 39, BackgroundColor = "#B2B2FF66", Marker = list(
            BackgroundColor = "#B2B2FF66")), list(Index = 40, BackgroundColor = "#FF272766",
            Marker = list(BackgroundColor = "#FF272766")), list(Index = 41,
            BackgroundColor = "#FF141466", Marker = list(BackgroundColor = "#FF141466")),
            list(Index = 42, BackgroundColor = "#FFE0E066", Marker = list(
            BackgroundColor = "#FFE0E066")), list(Index = 43, BackgroundColor = "#0000FF66",
            Marker = list(BackgroundColor = "#0000FF66")), list(Index = 44,
            BackgroundColor = "#FF5E5E66", Marker = list(BackgroundColor = "#FF5E5E66")),
            list(Index = 45, BackgroundColor = "#FF060666", Marker = list(
            BackgroundColor = "#FF060666")), list(Index = 46, BackgroundColor = "#FF252566",
            Marker = list(BackgroundColor = "#FF252566")), list(Index = 47,
            BackgroundColor = "#FF585866", Marker = list(BackgroundColor = "#FF585866")),
            list(Index = 48, BackgroundColor = "#FF040466", Marker = list(
            BackgroundColor = "#FF040466")), list(Index = 49, BackgroundColor = "#FF141466",
            Marker = list(BackgroundColor = "#FF141466"))))
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
