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
           full <- glm(formula = mpg ~ ., family = binomial, data = data)
           coef(step(object = null, scope = list(upper = full), direction = "both", trace = 0, k = 2))
}

test_that(desc = "peims throws an error if arguments are misspecified", 
          code = {expect_error(object = peims::peims(f = NULL, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = parallel::detectCores()),
                               regexp = "\"f\" must be a function")
                  expect_error(object = peims::peims(f = function(){}, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = parallel::detectCores()),
                               regexp = "\"f\" must contain the following argument: \"data\"")
                  expect_error(object = peims::peims(f = f, data = NULL, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = parallel::detectCores()),
                               regexp = "\"data\" must be a data frame or data table")
                  expect_error(object = peims::peims(f = f, data = myData, size = NULL, replace = TRUE, k = 100L, seed = 123L, ncpus = parallel::detectCores()),
                               regexp = "\"size\" must be a positive integer")
                  expect_error(object = peims::peims(f = f, data = myData, size = c(100L, 100L), replace = TRUE, k = 100L, seed = 123L, ncpus = parallel::detectCores()),
                               regexp = "single positive integer for \"size\" expected")
                  expect_error(object = peims::peims(f = f, data = myData, size = 0L, replace = TRUE, k = 100L, seed = 123L, ncpus = parallel::detectCores()),
                               regexp = "\"size\" must be a positive integer")
                  expect_error(object = peims::peims(f = f, data = myData, size = 101L, replace = TRUE, k = 100L, seed = 123L, ncpus = parallel::detectCores()),
                               regexp = "\"size\" exceeds the number of available observations in \"data\"")
                  expect_error(object = peims::peims(f = f, data = myData, size = 100L, replace = NULL, k = 100L, seed = 123L, ncpus = parallel::detectCores()),
                               regexp = "\"replace\" must be a logical value")
                  expect_error(object = peims::peims(f = f, data = myData, size = 100L, replace = c(FALSE, TRUE), k = 100L, seed = 123L, ncpus = parallel::detectCores()),
                               regexp = "single logical value for \"replace\" expected")
                  expect_error(object = peims::peims(f = f, data = myData, size = 100L, replace = TRUE, k = NULL, seed = 123L, ncpus = parallel::detectCores()),
                               regexp = "\"k\" must be a positive integer equal to or greater than 2")
                  expect_error(object = peims::peims(f = f, data = myData, size = 100L, replace = TRUE, k = c(100L, 1000L), seed = 123L, ncpus = parallel::detectCores()),
                               regexp = "single positive integer for \"k\" expected")
                  expect_error(object = peims::peims(f = f, data = myData, size = 100L, replace = TRUE, k = 1L, seed = 123L, ncpus = parallel::detectCores()),
                               regexp = "\"k\" must be a positive integer equal to or greater than 2")
                  expect_error(object = peims::peims(f = f, data = myData, size = 100L, replace = TRUE, k = 100L, seed = NULL, ncpus = parallel::detectCores()),
                               regexp = "\"seed\" must be an integer")
                  expect_error(object = peims::peims(f = f, data = myData, size = 100L, replace = TRUE, k = 100L, seed = c(123L, 1234L), ncpus = parallel::detectCores()),
                               regexp = "single integer for \"seed\" expected")
                  expect_error(object = peims::peims(f = f, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = NULL),
                               regexp = "\"ncpus\" must be a positive integer")
                  expect_error(object = peims::peims(f = f, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = rep(as.integer(parallel::detectCores()), times = 2)),
                               regexp = "single positive integer for \"ncpus\" expected")
                  expect_error(object = peims::peims(f = f, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = 0L),
                               regexp = "\"ncpus\" must be a positive integer")
                  expect_error(object = peims::peims(f = f, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = parallel::detectCores() + 1),
                               regexp = "\"ncpus\" exceeds the number of detected cores")
          }
)
  
test_that(desc = "peims resamples as expected", 
          code = {expect_identical(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = parallel::detectCores()), 
                                   expected = {
                                   }
          }
)

test_that(desc = "peims returns estimated model parameters accurately",
          code = {expect_identical(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = parallel::detectCores()),
                                   expected = {
                                   }
          }
)
