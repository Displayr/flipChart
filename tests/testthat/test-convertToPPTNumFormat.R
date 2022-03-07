context("convertToPPTNumFormat")

test_that("convertToPPTNumFormat",
{
    expect_true(is.null(convertToPPTNumFormat("")))
    #expect_equal(convertToPPTNumFormat(",.2f"), "#,##.00")
    #expect_equal(convertToPPTNumFormat(",.0f"), "#,##")
    expect_equal(convertToPPTNumFormat(".1%"), "0.0%")
    expect_equal(convertToPPTNumFormat(".2"), "0.00")
})
