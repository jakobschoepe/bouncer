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
