context("PrepareData: table manipulation")

# This is QTable with the Base n statistic
# Note that 'Statistics - Right' and 'Statistics - Below' are never passed
# to the R output

tabWithN <- structure(c(12.2448979591837, 6.12244897959184, 4.08163265306122,
6.12244897959184, 4.08163265306122, 8.16326530612245, 22.4489795918367,
19.3877551020408, 17.3469387755102, 100, 32.2033898305085, 13.5593220338983,
5.08474576271187, 10.1694915254237, 5.08474576271187, 0, 5.08474576271187,
13.5593220338983, 15.2542372881356, 100, 11.8811881188119, 12.8712871287129,
12.8712871287129, 13.3663366336634, 8.41584158415842, 13.3663366336634,
13.3663366336634, 8.91089108910891, 4.95049504950495, 100, 10.8843537414966,
17.687074829932, 11.5646258503401, 7.48299319727891, 16.3265306122449,
3.40136054421769, 6.12244897959184, 22.4489795918367, 4.08163265306122,
100, 12.5, 6.25, 18.75, 6.25, 9.375, 12.5, 18.75, 15.625, 0,
100, 3.57142857142857, 19.6428571428571, 8.92857142857143, 16.0714285714286,
26.7857142857143, 5.35714285714286, 10.7142857142857, 8.92857142857143,
0, 100, 13.0769230769231, 2.30769230769231, 12.3076923076923,
20, 13.8461538461538, 6.92307692307692, 11.5384615384615, 12.3076923076923,
7.69230769230769, 100, 6.57894736842105, 15.7894736842105, 7.89473684210526,
5.26315789473684, 11.8421052631579, 9.21052631578947, 9.21052631578947,
28.9473684210526, 5.26315789473684, 100, 98, 98, 98, 98, 98,
98, 98, 98, 98, 98, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 202,
202, 202, 202, 202, 202, 202, 202, 202, 202, 147, 147, 147, 147,
147, 147, 147, 147, 147, 147, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 130, 130, 130,
130, 130, 130, 130, 130, 130, 130, 76, 76, 76, 76, 76, 76, 76,
76, 76, 76, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800), .Dim = c(10L, 8L, 3L), .Dimnames = list(
    c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
    "45 to 49", "50 to 54", "55 to 64", "65 or more", "NET"),
    c("Every or nearly every day", "4 to 5 days a week", "2 to 3 days a week",
    "Once a week", "Once every 2 weeks", "Once a month", "Less than once a month",
    "Never"), c("Column %", "Column n", "Base n")), name = "Age by Exercise frequency",         questions = c("Age", "Exercise frequency"))

x2d <- tabWithN[,,1]

# vector (with no dimensions)
x1d <- structure(1:10, .Names = c("a", "b", "c", "d", "e", "f", "g",
"h", "i", "j"))

# array (with dimension of 1-d)
array1d <- table(rpois(20, 4))
test_that("Select Rows",
{
    expect_warning(res <- PrepareData("Column", input.data.table = tabWithN, tidy = TRUE),
                   "Multiple statistics detected")

    expect_silent(res <- PrepareData("Table", input.data.table = tabWithN, tidy = FALSE))
    expect_equal(dim(res$data), c(9,8,3))

    expect_warning(res <- PrepareData("Column", input.data.table = tabWithN, select.rows = "30 to 34, 35 to 39"), "Multiple statistics detected")
    expect_equal(rownames(res$data), c("30 to 34", "35 to 39"))

    res <- PrepareData("Table", input.data.table = LifeCycleSavings, first.k.rows = 10,
                       sort.rows = TRUE, reverse.rows = TRUE, reverse.columns = TRUE)
    expect_equal(colnames(res$data), rev(colnames(LifeCycleSavings)))
    expect_equal(rownames(res$data), c("China", "Brazil", "Austria", "Belgium", "Colombia",
                                       "Australia", "Costa Rica", "Chile", "Canada", "Bolivia"))

})

test_that("Sorting rows",
{
    expect_silent(res0 <- PrepareData("Table", input.data.table = tabWithN, tidy = FALSE, sort.rows = TRUE))
    expect_equal(dim(res0$data), c(9,8,3))

    expect_warning(res1 <- PrepareData("Column", input.data.table = tabWithN, tidy = TRUE, sort.rows = TRUE),
                   "Multiple statistics detected")

    # Not working currently because res0 uses column '2 to 3 days a week', but res1 uses column 'Never'
    #expect_equal(rownames(res0$data), rownames(res1$data))
})




