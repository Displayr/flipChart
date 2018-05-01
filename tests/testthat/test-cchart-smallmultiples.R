context("CChart - small multiples")

xx <- structure(list(`1 - 6CvrwXdVdJ` = 1:10, `2 - 9UwNndFTfm` = 2:11,
    `3 - W71N36nz3i` = 3:12), .Names = c("1 - 6CvrwXdVdJ", "2 - 9UwNndFTfm",
"3 - W71N36nz3i"), row.names = c(NA, -10L), class = "data.frame")

test_that("Small multiples",
{
    expect_error(CChart("Column", xx, small.multiples = TRUE,
                         title = "Main title", nrows = 3), NA)
    expect_warning(CChart("Column", xx, small.multiples = FALSE,
                         title = "Main title", nrows = 3),
                   "The following arguments have been ignored: nrows")
    expect_warning(CChart("Radar", xx, small.multiples = TRUE,
                         title = "Main title", nrows = 2, average.show = TRUE,
                         x.title.font.family = "X axis"),
                   "The following arguments have been ignored: x.title.font.family")
})
