test_that(desc = "peims throws an error if arguments are misspecified", 
          code = {ncpus <- as.integer(x = parallel::detectCores())
                  expect_error(object = peims(f = NULL, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus),
                               regexp = "\"f\" must be a function")
                               
                  expect_error(object = peims(f = function(){}, data = myData, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus),
                               regexp = "\"f\" must contain the following argument: \"data\"")
                               
                  expect_error(object = peims(f = f, data = NULL, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus),
                               regexp = "\"data\" must be a data frame")
                               
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
