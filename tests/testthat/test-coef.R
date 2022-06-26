set.seed(12345)
X1 <- rnorm(100)
X2 <- rbinom(100, 1, 0.3)
X3 <- rnorm(100)
X4 <- rbinom(100, 1, 0.5)
b <- model.matrix(~ X1 + X2 + X3 + X4) %*% c(-2.10, 0.00, -0.45, 0.22, -0.16)
Y <- rbinom(100, 1, 1 / (1 + exp(-b)))
data01 <- data.frame(X1, X2, X3, X4, Y)
f01 <- function(data, seed) {
  out <- tryCatch(expr = {
                    y <- data$Y
                    X <- as.matrix(subset(data, select = -c(Y)))
                    tmp <- glmnet::cv.glmnet(x = X, y = y, family = "binomial", alpha = 1, type.measure = "class", penalty.factor = c(0,1,1,1), standardize = FALSE)
                    coef(tmp)[,1]
                  },
                  error = function(cond) {message(".Random.seed: c(", sapply(seq_len(7), function(i) {paste0(seed[i], ifelse(i < 7, "L, ", "L"))}), ")\n", cond)},
                  warning = function(cond) {message(".Random.seed: c(", sapply(seq_len(7), function(i) {paste0(seed[i], ifelse(i < 7, "L, ", "L"))}), ")\n", cond)})
  return(out)
}

tmp01 <- bouncer(f = f01, data = data01, size = 100L, replace = TRUE, k = 100L, seed = 123L, ncpus = 2L, method = "simple")
tmp02 <- coef(tmp01)
tmp03 <- c(-2.0479605615, 0.2467211155, 0, 0, 0)
names(tmp03) <- c("(Intercept)", "X1", "X2", "X3", "X4")

testthat::test_that(desc = "Smoothed coefficients are estimated as expected",
                    code = {
                      testthat::expect_equal(object = tmp02, expected = tmp03)
                    }
)
