context("cchart")


test_that("flipStandardCharts::Chart chart functions",{
    library("flipStandardCharts")
    # Data
    pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
    names(pie.sales) <- c("Blueberry", "Cherry", "Apple", "Boston Cream", "Other", "Vanilla Cream")
    # Example 1
    print(Chart(pie.sales, title = "My pie sales", title.font.size = 20, title.font.color = "red", x.title = "Pie type", x.title.font.color = "brown"))
    print(CChart("Chart", pie.sales, title = "My pie sales", title.font.size = 20, title.font.color = "red", x.title = "Pie type", x.title.font.color = "brown"))
    # print(CChart("Chart", pie.sales, title = "My pie sales", font.title.size = 20, title.color.font = "red", title.x = "Pie type", font.color.x.title = "brown"))
    #
    #
    #     print(CChart("Chart", pie.sales, title.size.font = 20, font.title.color = "red"))
    #
    #     print(Chart(pie.sales, title = "My pie sales", title.size.font = 20, font.title.color = "red"))
    #
    #     print(CChart("Chart", pie.sales, main = "My pie sales", font.size.main = 20, main.font.colour = "red"))
    #
    #
    #
    #     print(CChart("Chart", pie.sales, title = "My pie sales", title.font.size = 20, title.font.color = "red"))
    # print(CChart("Chart", pie.sales, main = "My pie sales"))
    # print(CChart("Chart", pie.sales, main = "My pie sales", color = "red"))
    #
    #
    #
    # Chart(pie.sales, title = "My pie sales", title.font.size = 20, title.font.color = "red")


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
    # Exactly equal with substitution
    expect_true(flipChart:::parametersEqualAfterSubstitution("x.axis.title", "x.axis.title"))
#expect_true(flipChart:::parametersEqualAfterSubstitution("x.axis.title", "axis.x.title"))
    expect_true(flipChart:::parametersEqualAfterSubstitution("x.axis.title", "xlab"))
    expect_true(flipChart:::parametersEqualAfterSubstitution("xlab", "x.axis.title"))
    expect_true(flipChart:::parametersEqualAfterSubstitution("x.axis.title", "x.title"))
#expect_true(flipChart:::parametersEqualAfterSubstitution("x.axis.title", "axis.title"))
    expect_false(flipChart:::parametersEqualAfterSubstitution("colors", "main"))
    # # Subordinate
    # expect_true(flipChart:::recipientIsSubordinateOrEqual("x.axis.title", "x.axis.title"))
    # expect_true(flipChart:::recipientIsSubordinateOrEqual("x.axis.title", "axis.x.title"))
    # expect_false(flipChart:::recipientIsSubordinateOrEqual("x.axis.title", "xlab"))
    # expect_false(flipChart:::recipientIsSubordinateOrEqual("xlab", "x.axis.title"))
    # expect_true(flipChart:::recipientIsSubordinateOrEqual("x.axis.title", "x.title"))
    # expect_true(flipChart:::recipientIsSubordinateOrEqual("x.axis.title", "axis.title"))
    # expect_false(flipChart:::recipientIsSubordinateOrEqual("colors", "main"))
    # # Subordinate with substitution
    # expect_true(flipChart:::recipientIsSubordinateOrEqualAfterSubstitution("x.axis.title", "x.axis.title"))
    # expect_true(flipChart:::recipientIsSubordinateOrEqualAfterSubstitution("x.axis.title", "axis.x.title"))
    # expect_true(flipChart:::recipientIsSubordinateOrEqualAfterSubstitution("x.axis.title", "xlab"))
    # expect_true(flipChart:::recipientIsSubordinateOrEqualAfterSubstitution("xlab", "x.axis.title"))
    # expect_true(flipChart:::recipientIsSubordinateOrEqualAfterSubstitution("x.axis.title", "x.title"))
    # expect_true(flipChart:::recipientIsSubordinateOrEqualAfterSubstitution("x.axis.title", "axis.title"))
    # expect_false(flipChart:::recipientIsSubordinateOrEqualAfterSubstitution("colors", "main"))
})

#
# test_that("Substituting parameter names",{
#     p = list("x.axis.title" = 1)
#     a = list("x.axis.title" = 1)
#     expect_equal(flipChart:::substituteArgumentNames(names(p), a), p)
#     p = list("x.axis.title" = 1)
#     a = list("axis.x.title" = 1)
#     expect_equal(flipChart:::substituteArgumentNames(names(p), a), p)
#     p = list("xlab" = 1)
#     a = list("axis.x.title" = 1)
#     expect_equal(flipChart:::substituteArgumentNames(names(p), a), p)
#     p = list("x.axis.title" = 1)
#     a = list("xlab" = 1)
#     expect_equal(flipChart:::substituteArgumentNames(names(p), a), p)
#     p = list("x.axis.title" = 1)
#     a = list("axis.x.title" = 1)
#     expect_equal(flipChart:::substituteArgumentNames(names(p), a), p)
#     p = list("x.axis.title" = 1)
#     a = list("axis.x.title" = 1)
#     expect_equal(flipChart:::substituteArgumentNames(names(p), a), p)
#     p = list("x.axis.title" = 1)
#     a = list("x.title" = 1)
#     expect_equal(flipChart:::substituteArgumentNames(names(p), a), p)
#     p = list("x.axis.title" = 1)
#     a = list("axis.title" = 1)
#     expect_equal(flipChart:::substituteArgumentNames(names(p), a), p)
#     p = list("x.title" = 1)
#     a = list("x.axis.title" = 1)
#     expect_equal(flipChart:::substituteArgumentNames(names(p), a), p)
#     p = list("axis.title" = 1)
#     a = list("x.axis.title" = 1)
#     expect_equal(flipChart:::substituteArgumentNames(names(p), a), a)
# })


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

#    CChart("Chart", type = "Pie", dt, colors = cols)
#    CChart("Chart", type = "Column", dt)
#    CChart("Chart", type = "Bar", dt, data.label.show = FALSE)

 #   recipient <- "y.axis.title"
 #   donor <- "axis.title.y"






})




