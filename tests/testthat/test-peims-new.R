testthat::test_that(desc = "bouncer throws an error if arguments are misspecified", 
                    code = {ncpus <- as.integer(x = parallel::detectCores())
                            testthat::expect_error(object = bouncer(f = NULL, data = data, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"f\" must be a function")
                               
                            testthat::expect_error(object = bouncer(f = function(){}, data = data, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"f\" must contain the following argument: \"data\"")
                               
                            testthat::expect_error(object = bouncer(f = f, data = NULL, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"data\" must be a data frame")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = NULL, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"size\" must be a positive integer")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = rep(x = 100L, times = 2), replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "single positive integer for \"size\" expected")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 0L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"size\" must be a positive integer")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 101L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"size\" exceeds the number of available observations in \"data\"")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 100L, replace = NULL, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"replace\" must be a logical value")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 100L, replace = c(FALSE, TRUE), k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "single logical value for \"replace\" expected")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 100L, replace = TRUE, k = NULL, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"k\" must be a positive integer equal to or greater than 2")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 100L, replace = TRUE, k = rep(x = 100L, times = 2), seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "single positive integer for \"k\" expected")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 100L, replace = TRUE, k = 1L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"k\" must be a positive integer equal to or greater than 2")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 100L, replace = TRUE, k = 100L, seed = NULL, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"seed\" must be an integer")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 100L, replace = TRUE, k = 100L, seed = rep(x = 123L, times = 2), ncpus = ncpus, method = "simple"),
                                                   regexp = "single integer for \"seed\" expected")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = NULL, method = "simple"),
                                                   regexp = "\"ncpus\" must be a positive integer")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = rep(x = ncpus, times = 2), method = "simple"),
                                                   regexp = "single positive integer for \"ncpus\" expected")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = 0L, method = "simple"),
                                                   regexp = "\"ncpus\" must be a positive integer")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus + 1L, method = "simple"),
                                                   regexp = "\"ncpus\" exceeds the number of detected cores")
                  
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = NULL),
                                                   regexp = "\"method\" must be a character string")
                  
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = c("simple", "block")),
                                                   regexp = "single character string for \"method\" expected")
                  
                            testthat::expect_error(object = bouncer(f = f, data = data, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "NULL"),
                                                   regexp = "\"method\" is misspecified")
          }
)
