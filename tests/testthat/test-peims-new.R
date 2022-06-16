set.seed(12345)
X1 <- rnorm(100)
X2 <- rbinom(100, 1, 0.3)
X3 <- rnorm(100)
X4 <- rbinom(100, 1, 0.5)
b <- model.matrix(~ X1 + X2 + X3 + X4) %*% c(-2.10, 0.00, -0.45, 0.22, -0.16)
Y <- rbinom(100, 1, 1 / (1 + exp(-b)))
data01 <- data.frame(X1, X2, X3, X4, Y)
data02 <- data.frame(X1, X2, X3, X4, Y = rep(0:1, each = 50), id = rep(1:50, times = 2))
f01 <- function(data, seed) {
  out <- tryCatch(expr = {
                    y <- data$Y
                    X <- as.matrix(subset(data, select = -c(Y, id)))
                    tmp <- glmnet::cv.glmnet(x = X, y = y, family = "binomial", alpha = 1, type.measure = "class", penalty.factor = c(0,1,1,1), standardize = FALSE)
                    coef(tmp)[,1]
                  },
                  error = function(cond) {message(".Random.seed: c(", sapply(seq_len(7), function(i) {paste0(seed[i], ifelse(i < 7, "L, ", "L"))}), ")\n", cond)},
                  warning = function(cond) {message(".Random.seed: c(", sapply(seq_len(7), function(i) {paste0(seed[i], ifelse(i < 7, "L, ", "L"))}), ")\n", cond)})
  return(out)
}

f02 <- function(data, seed) {
  out <- tryCatch(expr = {
                    y <- glmnet::stratifySurv(y = Surv(start = rep(0, times = length(y)), stop = rep(1, times = length(y)), event = data$Y), strata = data$id)
                    X <- as.matrix(subset(data, select = -c(Y, id)))
                    tmp <- glmnet::cv.glmnet(x = X, y = y, family = "cox", alpha = 1, type.measure = "class", penalty.factor = c(0,1,1,1), standardize = FALSE)
                    coef(tmp)[,1]
                  },
                  error = function(cond) {message(".Random.seed: c(", sapply(seq_len(7), function(i) {paste0(seed[i], ifelse(i < 7, "L, ", "L"))}), ")\n", cond)},
                  warning = function(cond) {message(".Random.seed: c(", sapply(seq_len(7), function(i) {paste0(seed[i], ifelse(i < 7, "L, ", "L"))}), ")\n", cond)})
  return(out)                                                 
}

ncpus <- as.integer(x = parallel::detectCores())

testthat::test_that(desc = "bouncer throws an error if arguments are misspecified", 
                    code = {testthat::expect_error(object = bouncer(f = NULL, data = data01, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"f\" must be a function")
                               
                            testthat::expect_error(object = bouncer(f = function(){}, data = data01, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"f\" must contain the following argument: \"data\"")
                               
                            testthat::expect_error(object = bouncer(f = f, data = NULL, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"data\" must be a data frame")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = NULL, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"size\" must be a positive integer")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = rep(x = 100L, times = 2), replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "single positive integer for \"size\" expected")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 0L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"size\" must be a positive integer")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 101L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"size\" exceeds the number of available observations in \"data\"")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 100L, replace = NULL, k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"replace\" must be a logical value")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 100L, replace = c(FALSE, TRUE), k = 100L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "single logical value for \"replace\" expected")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 100L, replace = TRUE, k = NULL, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"k\" must be a positive integer equal to or greater than 2")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 100L, replace = TRUE, k = rep(x = 100L, times = 2), seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "single positive integer for \"k\" expected")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 100L, replace = TRUE, k = 1L, seed = 123L, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"k\" must be a positive integer equal to or greater than 2")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 100L, replace = TRUE, k = 100L, seed = NULL, ncpus = ncpus, method = "simple"),
                                                   regexp = "\"seed\" must be an integer")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 100L, replace = TRUE, k = 100L, seed = rep(x = 123L, times = 2), ncpus = ncpus, method = "simple"),
                                                   regexp = "single integer for \"seed\" expected")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = NULL, method = "simple"),
                                                   regexp = "\"ncpus\" must be a positive integer")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = rep(x = ncpus, times = 2), method = "simple"),
                                                   regexp = "single positive integer for \"ncpus\" expected")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = 0L, method = "simple"),
                                                   regexp = "\"ncpus\" must be a positive integer")
                               
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus + 1L, method = "simple"),
                                                   regexp = "\"ncpus\" exceeds the number of detected cores")
                  
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = NULL),
                                                   regexp = "\"method\" must be a character string")
                  
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = c("simple", "block")),
                                                   regexp = "single character string for \"method\" expected")
                  
                            testthat::expect_error(object = bouncer(f = f, data = data01, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = ncpus, method = "NULL"),
                                                   regexp = "\"method\" is misspecified")
          }
)

set.seed(123, kind = "L'Ecuyer-CMRG")
tmp01 <- lapply(seq_len(2), function(i) {
  tmp01 <- .Random.seed
  tmp02 <- lapply(seq_len(50), function(i) {
    tmp03 <- .Random.seed
    tmp04 <- sample(seq_len(100), size = 100, replace = TRUE)
    return(tmp04)
  })
  assign(".Random.seed", parallel::nextRNGStream(tmp01), envir = .GlobalEnv)
  return(tmp02)
})
tmp02 <- do.call(c, Map(list, tmp01[[1]], tmp01[[2]]))
tmp03 <- as.matrix(data.table::rbindlist(lapply(seq_len(100), function(i) {as.list(table(tmp02[[i]]))}), fill = TRUE))
tmp03 <- tmp03[, order(as.integer(colnames(tmp03)))]
tmp03[is.na(tmp03)] <- 0
tmp04 <- bouncer(f = f, data = data, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = 2L, method = "simple")
tmp05 <- tmp04@oir

set.seed(456, kind = "L'Ecuyer-CMRG")
tmp06 <- lapply(seq_len(2), function(i) {
  tmp01 <- .Random.seed
  tmp02 <- lapply(seq_len(50), function(i) {
    tmp03 <- .Random.seed
    tmp04 <- sample(seq_len(50), size = 50, replace = TRUE)
    return(tmp04)
  })
  assign(".Random.seed", parallel::nextRNGStream(tmp01), envir = .GlobalEnv)
  return(tmp02)
})
tmp07 <- do.call(c, Map(list, tmp06[[1]], tmp06[[2]]))
tmp08 <- as.matrix(data.table::rbindlist(lapply(seq_len(100), function(i) {as.list(table(tmp07[[i]]))}), fill = TRUE))
tmp08 <- tmp08[, order(as.integer(colnames(tmp08)))]
tmp08[is.na(tmp08)] <- 0
tmp09 <- bouncer(f = f, data = data, size = 50L, replace = TRUE, k = 100L, seed = 456L, ncpus = 2L, method = "block")
tmp10 <- tmp09@oir

testthat::test_that(desc = "bouncer resamples as expected",
                    code = {testthat::expect_identical(object = tmp05,
                                                       expected = tmp03)
                            
                            testthat::expect_identical(object = tmp10,
                                                       expected = tmp08)
          }
)
