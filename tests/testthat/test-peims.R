context(desc = "Resampling and parameter estimation")

test_that(desc = "peims throws an error if arguments are misspecified", 
          code = {expect_error(object = peims::peims(f = NULL, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = 2L))
                  expect_error(object = peims::peims(f = function(){}, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = 2L))
                  expect_error(object = peims::peims(f = f, data = NULL, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = 2L))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = NULL, replace = TRUE, k = 100L, seed = 123L, ncpus = 2L))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = c(32L, 31L), replace = TRUE, k = 100L, seed = 123L, ncpus = 2L))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 0L, replace = TRUE, k = 100L, seed = 123L, ncpus = 2L))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 33L, replace = TRUE, k = 100L, seed = 123L, ncpus = 2L))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = NULL, k = 100L, seed = 123L, ncpus = 2L))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = c(TRUE, FALSE), k = 100L, seed = 123L, ncpus = 2L))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = NULL, seed = 123L, ncpus = 2L))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = c(100L, 1000L), seed = 123L, ncpus = 2L))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 1L, seed = 123L, ncpus = 2L))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = NULL, ncpus = 2L))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = c(123L, 1234L), ncpus = 2L))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = NULL))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = rep(as.integer(parallel::detectCores()), times = 2)))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = 0L))
                  expect_error(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = parallel::detectCores() + 1))
          }
)
  
test_that(desc = "parallelized pseudo-random resampling works as expected", 
          code = {expect_identical(object = peims::peims(f = f, data = mtcars, size = 32L, replace = TRUE, k = 100L, seed = 123L, ncpus = parallel::detectCores()), 
                                   expected = {
                                   }
          }
)
