context("streamgraphs")

test_that("Streamgraph", #devtools::install_github("hrbrmstr/streamgraph")
          {
              set.seed(1223)
              x <- t(apply(matrix(runif(200), nrow = 4), 1, cumsum))
              rownames(x) <- c('Aardvark', 'Three toed sloth', 'Camel', 'Dog')
              colnames(x) <- as.character(seq(as.Date("1910/1/1"), by = "month", length.out = ncol(x)))

              # Testing combinations of inputs.
              Streamgraph(x)
              Streamgraph(x, x.tick.interval = 2)
              Streamgraph(x, x.tick.interval = 6)
              Streamgraph(x, x.tick.interval = 6, y.axis.show = FALSE)
              Streamgraph(x, x.tick.interval = 6, y.number.ticks = 3)
              Streamgraph(x, x.tick.interval = 6, y.number.ticks = 10)
              Streamgraph(x, x.tick.interval = 6, y.number.ticks = 10,  hover.decimals = 0)
              # Testing better example
              library(dplyr)

              ggplot2movies::movies %>%
                  select(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short)  %>%
                  group_by(year) %>% as.data.frame  -> dat
              dat <- aggregate.data.frame(dat, list(dat$year), sum)
              rownames(dat) <- dat[,1]
              dat <- dat[, -1:-2]
              # Date
              Streamgraph(t(dat), x.tick.interval = 20, x.number.format = "%y")
              # Number
              Streamgraph(t(dat), x.tick.interval = 20, x.number.format = "Number")
          })

