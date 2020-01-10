context("PrepareData: Scatterplot inputs for Regression")

# Code to produce regression outputs
library(flipRegression)
data("stacked.cola.associations")
smaller.linear <- Regression(NumericAttitude ~ Beautiful + Carefree + Charming + Confident,
                             data = stacked.cola.associations,
                             type = "Linear", output = "Summary")
linear.summary <- Regression(NumericAttitude ~ Beautiful + Carefree + Charming + Confident + DownToEarth,
                             data = stacked.cola.associations,
                             type = "Linear", output = "Summary")
linear.shapley <- Regression(NumericAttitude ~ Beautiful + Carefree + Charming + Confident + DownToEarth,
                             data = stacked.cola.associations,
                             type = "Linear", output = "Shapley regression")
linear.importance <- Regression(NumericAttitude ~ Beautiful + Carefree + Charming + Confident + DownToEarth,
                                data = stacked.cola.associations,
                                type = "Linear", output = "Relative Importance Analysis")
binary.summary <- Regression(BinaryAttitude ~ Beautiful + Carefree + Charming + Confident + DownToEarth,
                             data = stacked.cola.associations,
                             type = "Binary Logit", output = "Summary")
binary.importance <- Regression(BinaryAttitude ~ Beautiful + Carefree + Charming + Confident + DownToEarth,
                                data = stacked.cola.associations,
                                type = "Binary Logit", output = "Relative Importance Analysis")
ordered.summary <- Regression(Attitude ~ Beautiful + Carefree + Charming + Confident + DownToEarth,
                              data = stacked.cola.associations,
                              type = "Ordered Logit", output = "Summary")
ordered.importance <- Regression(Attitude ~ Beautiful + Carefree + Charming + Confident + DownToEarth,
                                 data = stacked.cola.associations,
                                 type = "Ordered Logit", output = "Relative Importance Analysis")
nbd.summary <- suppressWarnings(Regression(NumericAttitude ~ Beautiful + Carefree + Charming + Confident + DownToEarth,
                                           data = stacked.cola.associations,
                                           type = "NBD", output = "Summary"))
nbd.importance <- suppressWarnings(Regression(NumericAttitude ~ Beautiful + Carefree + Charming + Confident + DownToEarth,
                                              data = stacked.cola.associations,
                                              type = "NBD", output = "Relative Importance Analysis"))
poisson.summary <- Regression(NumericAttitude ~ Beautiful + Carefree + Charming + Confident + DownToEarth,
                              data = stacked.cola.associations,
                              type = "Poisson", output = "Summary")
poisson.importance <- Regression(NumericAttitude ~ Beautiful + Carefree + Charming + Confident + DownToEarth,
                                 data = stacked.cola.associations,
                                 type = "Poisson", output = "Relative Importance Analysis")
quasi.poisson.summary <- Regression(NumericAttitude ~ Beautiful + Carefree + Charming + Confident + DownToEarth,
                                    data = stacked.cola.associations,
                                    type = "Quasi-Poisson", output = "Summary")
quasi.poisson.importance <- Regression(NumericAttitude ~ Beautiful + Carefree + Charming + Confident + DownToEarth,
                                       data = stacked.cola.associations,
                                       type = "Quasi-Poisson", output = "Relative Importance Analysis")
multinomial.summary <- Regression(Attitude ~ Beautiful + Carefree + Charming + Confident + DownToEarth,
                                  data = stacked.cola.associations,
                                  type = "Ordered Logit", output = "Summary")

# table of regressor to add to regression
large.performance.table <- list(
    table.Performance = structure(c(Beautiful = 21.71254, Carefree = 29.30683,
                                     Charming = 20.38736, Confident = 31.39653, DownToEarth = 26.35066,
                                     Feminine = 27.5739, Fun = 33.89399, `Health-conscious` = 34.40367,
                                     Hip = 26.75841, Honest = 24.66871, Humorous = 19.62283, Imaginative = 22.88481,
                                     Individualistic = 23.49643, Innocent = 17.5841, Intelligent = 23.34353,
                                     Masculine = 27.01325, Older = 24.15902, `Open to new experiences` = 27.67584,
                                     Outdoorsy = 29.76555, Rebellious = 21.4577, Reckless = 20.94801,
                                     Reliable = 28.95005, Sexy = 21.81448, Sleepy = 15.39246, Tough = 23.49643,
                                     Traditional = 28.5423, `Trying to be cool` = 29.45973, Unconventional = 19.62283,
                                     `Up-to-date` = 30.27523, `Upper-class` = 19.57187, Urban = 28.3894,
                                     `Weight-conscious` = 43.11927, Wholesome = 19.06218, Youthful = 29.61264,
                                     NET = 97.45158),
                                   .Dim = 35L,
                                   .Dimnames = list(c("Beautiful",
                                                      "Carefree", "Charming", "Confident", "DownToEarth", "Feminine",
                                                      "Fun", "Health-conscious", "Hip", "Honest", "Humorous", "Imaginative",
                                                      "Individualistic", "Innocent", "Intelligent", "Masculine", "Older",
                                                      "Open to new experiences", "Outdoorsy", "Rebellious", "Reckless",
                                                      "Reliable", "Sexy", "Sleepy", "Tough", "Traditional", "Trying to be cool",
                                                      "Unconventional", "Up-to-date", "Upper-class", "Urban", "Weight-conscious",
                                                      "Wholesome", "Youthful", "NET")),
                                   statistic = "%", name = "table.Performance", questions = c("Performance",
                                                                                              "SUMMARY")))
performance.table <- list(
    table.Performance = structure(c(Beautiful = 21.71254, Carefree = 29.30683,
                                    Charming = 20.38736, Confident = 31.39653, DownToEarth = 26.35066),
                                  .Dim = 5L,
                                  .Dimnames = list(c("Beautiful",  "Carefree", "Charming", "Confident", "DownToEarth")),
                                  statistic = "%", name = "table.Performance", questions = c("Performance", "SUMMARY")))

small.performance.table <- list(
    table.Performance = structure(c(Beautiful = 21.71254, Carefree = 29.30683,
                                    Charming = 20.38736, Confident = 31.39653),
                                  .Dim = 4L,
                                  .Dimnames = list(c("Beautiful",  "Carefree", "Charming", "Confident")),
                                  statistic = "%", name = "table.Performance", questions = c("Performance", "SUMMARY")))


regression.types <- c("binary", "linear", "nbd", "ordered", "poisson", "quasi.poisson")
importance.regression.types <- c("linear.shapley",
                                 paste0(regression.types, ".importance"))
standard.regression.types <- c(paste0(regression.types, ".summary"),
                               "multinomial.summary")
all.regression.types <- c(importance.regression.types, standard.regression.types)

#######################################################
### Test Regression input in Output in pages         ##
#######################################################

for (regression in all.regression.types)
    test_that(paste0("Test regression inputs to scatter: ", regression), {
        regression.to.input <- suppressWarnings(get(regression))
        expect_error(PrepareData(chart.type = "Scatter", input.data.table = regression.to.input), NA)
    })

#######################################################
### Test Regression in X position against table in Y ##
#######################################################

# Test against perfect table (only relevant entries)
# Expect no errors or warnings
for (regression in importance.regression.types)
    test_that(paste0("Test regression input X against table input Y: ", regression), {
        regression.to.input <- suppressWarnings(get(regression))
        expect_error(pd <- PrepareData(chart.type = "Scatter",
                                       input.data.raw = list(X = regression.to.input,
                                                             Y = performance.table)), NA)
    })

# Expect warning about intercepts in table output
for (regression in standard.regression.types)
    test_that(paste0("Test regression input X against table input Y: ", regression), {
        regression.to.input <- suppressWarnings(get(regression))
        if(grepl("^(ordered|multinomial)", regression, perl = TRUE))
            warning.suffix <- "Don t Know"
        else
            warning.suffix <- "\\(Intercept\\)"
        expected.warning <- paste0("Rows that did not occur in all of the input tables were discarded: ",
                                   warning.suffix)
        expect_warning(PrepareData(chart.type = "Scatter",
                                   input.data.raw = list(X = regression.to.input,
                                                         Y = performance.table)), expected.warning)
    })

# Test against larger table (more than relevant entries)
# Expect warning to include names of table elements not in regression output
for (regression in importance.regression.types)
    test_that(paste0("Test regression input X against table input Y: ", regression), {
        regression.to.input <- suppressWarnings(get(regression))
        expected.warning <- paste0("Rows that did not occur in all of the input tables were discarded: ",
                                   "Feminine, Fun, Health-conscious, Hip, Honest, Humorous, Imaginativ")
        expect_warning(PrepareData(chart.type = "Scatter",
                                   input.data.raw = list(X = regression.to.input,
                                                         Y = large.performance.table)),
                                    expected.warning)
    })


for (regression in standard.regression.types)
    test_that(paste0("Test regression input X against table input Y: ", regression), {
        regression.to.input <- suppressWarnings(get(regression))
        if(grepl("^(ordered|multinomial)", regression, perl = TRUE))
            warning.suffix <- "Don t Know"
        else
            warning.suffix <- "\\(Intercept\\), Feminine"
        expected.warning <- paste0("Rows that did not occur in all of the input tables were discarded: ",
                                   warning.suffix)
        expect_warning(PrepareData(chart.type = "Scatter",
                                   input.data.raw = list(X = regression.to.input,
                                                         Y = large.performance.table)), expected.warning)
    })


# Test against smaller table (some entries missing compared to regression)
# Expect warning to include names of table elements not in regression output
# In this case, DownToEarth is missing
for (regression in importance.regression.types)
    test_that(paste0("Test regression input X against table input Y: ", regression), {
        regression.to.input <- suppressWarnings(get(regression))
        expected.warning <- paste0("Rows that did not occur in all of the input tables were discarded: ",
                                   "DownToEarth")
        expect_warning(PrepareData(chart.type = "Scatter",
                                   input.data.raw = list(X = regression.to.input,
                                                         Y = small.performance.table)),
                       expected.warning)
    })


for (regression in standard.regression.types)
    test_that(paste0("Test regression input X against table input Y: ", regression), {
        regression.to.input <- suppressWarnings(get(regression))
        if(grepl("^(ordered|multinomial)", regression, perl = TRUE))
            warning.suffix <- "DownToEarth, Don t Know"
        else
            warning.suffix <- "\\(Intercept\\), DownToEarth"
        expected.warning <- paste0("Rows that did not occur in all of the input tables were discarded: ",
                                   warning.suffix)
        expect_warning(PrepareData(chart.type = "Scatter",
                                   input.data.raw = list(X = regression.to.input,
                                                         Y = small.performance.table)), expected.warning)
    })

#######################################################
### Test Regression in X position against table in Y ##
#######################################################


# Test against perfect table (only relevant entries)
# Expect no errors or warnings
for (regression in importance.regression.types)
    test_that(paste0("Test table input X against regression input Y: ", regression), {
        regression.to.input <- suppressWarnings(get(regression))
        expect_error(pd <- PrepareData(chart.type = "Scatter",
                                       input.data.raw = list(X = performance.table,
                                                             Y = list(model = regression.to.input))), NA)
    })

# Expect warning about intercepts in table output
for (regression in standard.regression.types)
    test_that(paste0("Test table input X against regression input Y: ", regression), {
        regression.to.input <- suppressWarnings(get(regression))
        if(grepl("^(ordered|multinomial)", regression, perl = TRUE))
            warning.suffix <- "Don t Know"
        else
            warning.suffix <- "\\(Intercept\\)"
        expected.warning <- paste0("Rows that did not occur in all of the input tables were discarded: ",
                                   warning.suffix)
        expect_warning(PrepareData(chart.type = "Scatter",
                                   input.data.raw = list(X = performance.table,
                                                         Y = list(model = regression.to.input))), expected.warning)
    })

# Test against larger table (more than relevant entries)
# Expect warning to include names of table elements not in regression output
for (regression in importance.regression.types)
    test_that(paste0("Test table input X against regression input Y: ", regression), {
        regression.to.input <- suppressWarnings(get(regression))
        expected.warning <- paste0("Rows that did not occur in all of the input tables were discarded: ",
                                   "Feminine, Fun, Health-conscious, Hip, Honest, Humorous, Imaginativ")
        expect_warning(PrepareData(chart.type = "Scatter",
                                   input.data.raw = list(X = large.performance.table,
                                                         Y = list(model = regression.to.input))),
                       expected.warning)
    })

large.warning.suffix <- paste0("Feminine, Fun, Health-conscious, Hip, Honest, Humorous, Imaginative, ",
                               "Individualistic, Innocent, Intelligent, Masculine, Older, ",
                               "Open to new experiences, Outdoorsy, Rebellious, Reckless, Reliable, Sexy, ",
                               "Sleepy, Tough, Traditional, Trying to be cool, Unconventional, Up-to-date, ",
                               "Upper-class, Urban, Weight-conscious, Wholesome, Youthful, NET, ")
for (regression in standard.regression.types)
    test_that(paste0("Test table input X against regression input Y: ", regression), {
        regression.to.input <- suppressWarnings(get(regression))
        if(grepl("^(ordered|multinomial)", regression, perl = TRUE))
            warning.suffix <- paste0(large.warning.suffix, "Don t Know")
        else
            warning.suffix <- paste0(large.warning.suffix, "\\(Intercept\\)")
        expected.warning <- paste0("Rows that did not occur in all of the input tables were discarded: ",
                                   warning.suffix)
        expect_warning(PrepareData(chart.type = "Scatter",
                                   input.data.raw = list(X = large.performance.table,
                                                         Y = list(model = regression.to.input))), expected.warning)
    })
