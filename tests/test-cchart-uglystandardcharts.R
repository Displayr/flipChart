# The ones that don't depend on Distribution are in the testthat directory
library(flipStandardCharts)


#test_that("Violin", {
    set.seed(1223)
    z = list(Normal = rnorm(1000), "Poisson with unit lamda" = rpois(1000, 1), Exponential = rexp(1000))
    Violin(z, title = "Comparing distributions",
           values.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red")
    CChart("Violin", z, title = "Comparing distributions",
           values.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red")
    CChart("Violin", z, title = "Comparing distributions",
           y.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red")
    CChart("Histogram", z, title = "Comparing distributions",
           y.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red")

#})
