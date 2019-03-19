context(desc = "Resampling and parameter estimation")

test_that(desc = "peims throws an error if arguments are misspecified", 
          code = {expect_error(object = peims::peims(f = NULL, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = 2L),
                               regexp = "\"f\" must be a function")
                  expect_error(object = peims::peims(f = function(){}, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = 2L),
                               regexp = "\"f\" must contain the following argument: \"data\"")
                  expect_error(object = peims::peims(f = f, data = NULL, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = 2L),
                               regexp = "\"data\" must be a data frame or data table")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = NULL, replace = TRUE, k = 100L, seed = 123L, ncpus = 2L),
                               regexp = "\"size\" must be a positive integer")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = c(32L, 31L), replace = TRUE, k = 100L, seed = 123L, ncpus = 2L),
                               regexp = "single positive integer for \"size\" expected")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 0L, replace = TRUE, k = 100L, seed = 123L, ncpus = 2L),
                               regexp = "\"size\" must be a positive integer")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 33L, replace = TRUE, k = 100L, seed = 123L, ncpus = 2L),
                               regexp = "\"size\" exceeds the number of available observations in \"data\"")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = NULL, k = 100L, seed = 123L, ncpus = 2L),
                               regexp = "\"replace\" must be a logical value")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = c(TRUE, FALSE), k = 100L, seed = 123L, ncpus = 2L),
                               regexp = "single logical value for \"replace\" expected")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = NULL, seed = 123L, ncpus = 2L),
                               regexp = "\"k\" must be a positive integer equal to or greater than 2")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = c(100L, 1000L), seed = 123L, ncpus = 2L),
                               regexp = "single positive integer for \"k\" expected")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 1L, seed = 123L, ncpus = 2L),
                               regexp = "\"k\" must be a positive integer equal to or greater than 2")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = NULL, ncpus = 2L),
                               regexp = "\"seed\" must be an integer")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = c(123L, 1234L), ncpus = 2L),
                               regexp = "single integer for \"seed\" expected")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = NULL),
                               regexp = "\"ncpus\" must be a positive integer")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = rep(as.integer(parallel::detectCores()), times = 2)),
                               regexp = "single positive integer for \"ncpus\" expected")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = 0L),
                               regexp = "\"ncpus\" must be a positive integer")
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = parallel::detectCores() + 1),
                               regexp = "\"ncpus\" exceeds the number of detected cores")
          }
)
  
test_that(desc = "parallelized pseudo-random resampling works as expected", 
          code = {expect_identical(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = parallel::detectCores()), 
                                   expected = {
                                   }
          }
)
