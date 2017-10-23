context("CChart: miscellaneous tests")


test_that("flipStandardCharts::Chart chart functions",{
    library("flipStandardCharts")
    # Data
    pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
    names(pie.sales) <- c("Blueberry", "Cherry", "Apple", "Boston Cream", "Other", "Vanilla Cream")
    # Chart
    #print(Chart(pie.sales, title = "My pie sales", title.font.size = 20, title.font.color = "red", x.title = "Pie type", x.title.font.color = "brown"))
    # CChart
    print(CChart("Pie Chart", pie.sales, title = "My pie sales", title.font.size = 20, title.font.color = "red", x.title = "Pie type", x.title.font.color = "brown", warn.if.no.match = F))
    # Capitalization
    print(CChart("Pie Chart", pie.sales, title = "My pie sales", title.font.Size = 20, title.Font.color = "red", x.title = "Pie type", x.title.font.color = "brown", warn.if.no.match = F))
    # Re-ordering
    print(CChart("Pie Chart", pie.sales, title = "My pie sales", title.font.Size = 20, color.title.Font = "red", x.title = "Pie type", x.title.font.color = "brown", warn.if.no.match = F))
    # Substitution of parameter name
    print(CChart("Pie Chart", pie.sales, main = "My pie sales", title.font.Size = 20, color.title.Font = "red", x.title = "Pie type", x.title.font.color = "brown", warn.if.no.match = F))
    # Substitution of part of a parameter name (within . as a delimiter)
    print(CChart("Pie Chart", pie.sales, main = "My pie sales", font.Size.main = 20, main.font.color = "red", xlab = "Pie type", x.title.font.color = "brown", warn.if.no.match = F))
    # Non-supported arugment
    expect_warning(print(CChart("Pie Chart", pie.sales, sfdsmain = "My pie sales", font.Size.main = 20, main.font.color = "red", xlab = "Pie type", x.title.font.color = "brown")))
    expect_warning(print(CChart("Pie Chart", pie.sales, sfdsmain = "My pie sales")))
    expect_warning(print(CChart("Pie Chart", pie.sales, sfdsmain = "My pie sales", warn.if.no.match = FALSE)), NA)
})

test_that("Comparing parameters",{
    # Exactly equal (after re-arranging)
    expect_true(flipChart:::parametersEqual("x.axis.title", "x.axis.title"))
    expect_true(flipChart:::parametersEqual("x.axis.title", "axis.x.title"))
    expect_false(flipChart:::parametersEqual("x.axis.title", "xlab"))
    expect_false(flipChart:::parametersEqual("xlab", "x.axis.title"))
    expect_false(flipChart:::parametersEqual("x.axis.title", "x.title"))
    expect_false(flipChart:::parametersEqual("x.axis.title", "axis.title"))
    expect_false(flipChart:::parametersEqual("colors", "main"))
 })

test_that("selecting chart functions",{

    ####    Reproducing pie charts, with focus on classic pie charts
    requireNamespace("grDevices")
    # Original
    pie(rep(1, 24), col = rainbow(24), radius = 0.9)
    # Called via CChart
    CChart("pie", rep(1, 24))
    # Data pased in as a variable
    x = rep(1, 24)
    CChart("pie", x)

    # Original
    pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
    names(pie.sales) <- c("Blueberry", "Cherry", "Apple", "Boston Cream", "Other", "Vanilla Cream")
    pie(pie.sales, col = c("purple", "violetred1", "green3", "cornsilk", "cyan", "white"))
    # CChart
    CChart("pie", pie.sales, col = c("purple", "violetred1", "green3", "cornsilk", "cyan", "white"))

    # Various parameters of pie
    CChart("pie", pie.sales, col = rainbow(24), radius = 0.4)
    colr = gray(seq(0.4, 1.0, length = 6))
    CChart("pie", pie.sales, col = colr)

    CChart("pie", pie.sales, clockwise = TRUE, main = "pie(*, clockwise = TRUE)")
    segments(0, 0, 0, 1, col = "red", lwd = 2)
    text(0, 1, "init.angle = 90", col = "red")

    n <- 200
    CChart("pie", rep(1, n), labels = "", col = rainbow(n), border = NA,
        main = "pie(*, labels=\"\", col=rainbow(n), border=NA,..")

    ## Another case showing pie() is rather fun than science:
    ## (original by FinalBackwardsGlance on http://imgur.com/gallery/wWrpU4X)
    dt = c(Sky = 78, "Sunny side of pyramid" = 17, "Shady side of pyramid" = 5)
    cols = c("deepskyblue", "yellow", "yellow3")
    CChart("pie", dt, init.angle = 315, col = cols, border = FALSE)

})

test_that("Appending data",{

    ####    Reproducing pie charts, with focus on classic pie charts
    requireNamespace("grDevices")
    z = CChart("Pie", rep(1, 24), append.data = TRUE, warn.if.no.match = FALSE)
    expect_true(!is.null(attr(z, "ChartData")))
    z = CChart("Pie", rep(1, 24), append.data = FALSE, warn.if.no.match = FALSE)
    expect_true(is.null(attr(z, "ChartData")))
    expect_error(CChart("pie", rep(1, 24), append.data = TRUE, warn.if.no.match = FALSE))
    z = CChart("pie", rep(1, 24), append.data = FALSE, warn.if.no.match = FALSE)
    expect_true(is.null(attr(z, "ChartData")))
})






