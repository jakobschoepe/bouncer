context(desc = "Resampling and parameter estimation")

param <- c(0.0,
           0.3, 0.1,
           0.0, 0.2, 0.5)
margins <- c("norm", "binom", "binom", "norm")
paramMargins <- list(list(mean = 0, sd = 1), list(size = 1, prob = 0.3), list(size = 1, prob = .1), list(mean = 20, sd = 4))
betas <- c(-3.10, 0.00, -0.45, 0.22, -0.16)
myData <- dg(param = param, dim = 4L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3+ V4, betas = betas, link = "logit")
f <- function(data) {
           null <- glm(formula = y ~ 1, family = binomial, data = data)
           full <- glm(formula = y ~ ., family = binomial, data = data)
           coef(step(object = null, scope = list(upper = full), direction = "both", trace = 0, k = 2))
}

test_that(desc = "peims throws an error if arguments are misspecified", 
          code = {ncpus <- as.integer(x = parallel::detectCores())
                  expect_error(object = peims(f = NULL, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus),
                               regexp = "\"f\" must be a function")
                  expect_error(object = peims(f = function(){}, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus),
                               regexp = "\"f\" must contain the following argument: \"data\"")
                  expect_error(object = peims(f = f, data = NULL, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus),
                               regexp = "\"data\" must be a data frame or data table")
                  expect_error(object = peims(f = f, data = myData, size = NULL, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus),
                               regexp = "\"size\" must be a positive integer")
                  expect_error(object = peims(f = f, data = myData, size = rep(x = 100L, times = 2), replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus),
                               regexp = "single positive integer for \"size\" expected")
                  expect_error(object = peims(f = f, data = myData, size = 0L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus),
                               regexp = "\"size\" must be a positive integer")
                  expect_error(object = peims(f = f, data = myData, size = 101L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus),
                               regexp = "\"size\" exceeds the number of available observations in \"data\"")
                  expect_error(object = peims(f = f, data = myData, size = 100L, replace = NULL, k = 100L, seed = 123L, ncpus = ncpus),
                               regexp = "\"replace\" must be a logical value")
                  expect_error(object = peims(f = f, data = myData, size = 100L, replace = c(FALSE, TRUE), k = 100L, seed = 123L, ncpus = ncpus),
                               regexp = "single logical value for \"replace\" expected")
                  expect_error(object = peims(f = f, data = myData, size = 100L, replace = TRUE, k = NULL, seed = 123L, ncpus = ncpus),
                               regexp = "\"k\" must be a positive integer equal to or greater than 2")
                  expect_error(object = peims(f = f, data = myData, size = 100L, replace = TRUE, k = rep(x = 100L, times = 2), seed = 123L, ncpus = ncpus),
                               regexp = "single positive integer for \"k\" expected")
                  expect_error(object = peims(f = f, data = myData, size = 100L, replace = TRUE, k = 1L, seed = 123L, ncpus = ncpus),
                               regexp = "\"k\" must be a positive integer equal to or greater than 2")
                  expect_error(object = peims(f = f, data = myData, size = 100L, replace = TRUE, k = 100L, seed = NULL, ncpus = ncpus),
                               regexp = "\"seed\" must be an integer")
                  expect_error(object = peims(f = f, data = myData, size = 100L, replace = TRUE, k = 100L, seed = rep(x = 123L, times = 2), ncpus = ncpus),
                               regexp = "single integer for \"seed\" expected")
                  expect_error(object = peims(f = f, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = NULL),
                               regexp = "\"ncpus\" must be a positive integer")
                  expect_error(object = peims(f = f, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = rep(x = ncpus, times = 2)),
                               regexp = "single positive integer for \"ncpus\" expected")
                  expect_error(object = peims(f = f, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = 0L),
                               regexp = "\"ncpus\" must be a positive integer")
                  expect_error(object = peims(f = f, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus + 1L),
                               regexp = "\"ncpus\" exceeds the number of detected cores")
          }
)

RNGkind(kind = "L'Ecuyer-CMRG")
set.seed(123)

myList <- lapply(X = 1:100, function(i) {
  seed <- .Random.seed
  tmp1 <- sample(1:100, size = 100, replace = TRUE)
  .Random.seed <<- nextRNGStream(seed = seed)
  return(tmp1)
})

myMatrix <- as.matrix(x = data.table::rbindlist(l = lapply(X = 1:100, function(i) {as.list(x = table(myList[[i]]))}), fill = TRUE))
myMatrix <- myMatrix[, order(as.integer(x = colnames(x = myMatrix)))]
myMatrix[is.na(x = myMatrix)] <- 0

test_that(desc = "peims resamples as expected", 
          code = {ncpus <- as.integer(x = parallel::detectCores())
                  expect_identical(object = peims(f = f, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus), 
                                   expected = {
                                   })
          }
)

test_that(desc = "peims returns estimated model parameters accurately",
          code = {ncpus <- as.integer(x = parallel::detectCores())
                  expect_identical(object = peims(f = f, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus)@betaij,
                                   expected = {
                                   })
          }
)
