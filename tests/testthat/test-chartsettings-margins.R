context("ChartSettings plot area margins")

# PowerPoint sizes the plot area around the text it has to fit, so an exported chart does not match
# the one drawn in the document when the user fixed their own margins (RS-23684). ChartSettings
# carries the margins so Q can position the plot area explicitly, but only when the user asked for
# exactly those margins: "Customize margins" ticked (which is the only way any margin.* argument
# reaches the chart) and "Automatically expand margins" unticked.

dat <- matrix(1:10, ncol = 2, dimnames = list(LETTERS[1:5], c("A", "B")))

marginsFor <- function(...)
    attr(suppressWarnings(CChart("Bar", dat, append.data = TRUE, ...)),
         "ChartSettings")$PlotAreaMargins

test_that("Fixed margins are exported",
{
    expect_equal(marginsFor(margin.top = 30, margin.left = 80, margin.bottom = 50,
                            margin.right = 40, margin.autoexpand = FALSE),
                 list(Top = 30, Left = 80, Bottom = 50, Right = 40))
})

test_that("Auto-expanding margins are not exported",
{
    # There is no fixed plot area to reproduce, so PowerPoint's own layout is the closer match.
    expect_null(marginsFor(margin.top = 30, margin.left = 80, margin.bottom = 50,
                           margin.right = 40, margin.autoexpand = TRUE))
})

test_that("Margins are not exported when the user did not customize them",
{
    # Unticking "Customize margins" leaves the margin.* arguments unset, whatever autoexpand says.
    expect_null(marginsFor(margin.autoexpand = FALSE))
    expect_null(marginsFor(margin.top = 30, margin.left = 80, margin.autoexpand = FALSE))
})

test_that("Margins are not exported by charts that predate the autoexpand control",
{
    # Such charts send no margin.autoexpand at all and their margins are auto-expanded, so an
    # unset value must not be read as "do not expand".
    expect_null(marginsFor(margin.top = 30, margin.left = 80, margin.bottom = 50,
                           margin.right = 40))
})

test_that("Non-numeric margins are not exported",
{
    expect_null(marginsFor(margin.top = 30, margin.left = "80", margin.bottom = 50,
                           margin.right = 40, margin.autoexpand = FALSE))
    expect_null(marginsFor(margin.top = 30, margin.left = NA, margin.bottom = 50,
                           margin.right = 40, margin.autoexpand = FALSE))
})
