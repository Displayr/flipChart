# context("Test 'nice' standard charts via CChart")
# # The ones that depend on Distribution, which does not test well, are in the test directory.
# library(flipStandardCharts)
#
# test_that("Column",
#           {
#
#             Column(-1:5, y.zero = FALSE, y.zero.line.width = 1,  grid.show = FALSE, y.title = "Dog")
#             CChart("Column", -1:5, y.zero = FALSE, y.zero.line.width = 1,  grid.show = FALSE, y.title = "Dog")
# })
#
# test_that("Venn",
#           {
#                 ### R Output as an input
#                 # Simple example
#                 r.output <- list(
#                     list("sets"= list(0), "label"= "Like", "size"= 100),
#                     list("sets"= list(1), "label"= "Love", "size"= 50),
#                     list("sets"= list(2), "label"= "Dislike", "size"= 100),
#                     list("sets"= list(3), "label"= "Hate", "size"= 50),
#                     list("sets"= list(0, 1), "size"= 50),
#                     list("sets"= list(0, 2), "size"= 0),
#                     list("sets"= list(2, 3), "size"= 50))
#                 Venn(r.output)
#                 CChart("Venn", r.output)
#
#           })
#
