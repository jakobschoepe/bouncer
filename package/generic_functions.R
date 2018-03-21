# File: generic_functions.R
# Package: peims
# Version: 0.2.0
# Author: Jakob Schöpe
# Date: March 20, 2018
#
# Dependencies: data.table
#
# Copyright (C) 2018 Jakob Schöpe
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

setMethod(f = "coef",
          signature = "peims",
          definition = function(object) {
                    return(colMeans(x = object@betaij, na.rm = TRUE))
          }
)

setMethod(f = "confint",
          signature = "peims",
          definition = function(object, level = .95, method = "bcsi") {
                    # Store 
                    betaij <- object@betaij
                    betaj <- colMeans(x = betaij, na.rm = TRUE)
                    oir <- object@oir
                    or <- colMeans(x = oir, na.rm = TRUE)
                    alpha <- (1 - level) / 2
                    j <- ncol(x = betaij)
                    k <- nrow(x = betaij)
                    p <- c(alpha, 1 - alpha)
                    q <- qt(p = p, df = k - j)
                    
                    ci <- array(data = NA, dim = c(j, 2L), dimnames = list(names(x = betaj), paste(x = format(x = 100 * p, digits = 3, scientific = FALSE, trim = TRUE), "%", sep = "")))
                    
                    if (method == "bcsi") {
                              o1 <- oir - rep(x = or, each = k)
                              o2 <- oir - 1
                              b1 <- betaij - rep(x = betaj, each = k)
                              ci[] <- t(x = betaj + rep(x = q, each = j) * sapply(X = 1:j, FUN = function(i) {sqrt(x = sum(x = colSums(x = (o1 * b1[, i]) / k, na.rm = TRUE)^2) - 1/(k)^2 * sum(x = colSums(x = ((o2 * b1[, i]) - colMeans(x = o2 * b1[, i], na.rm = TRUE))^2, na.rm = TRUE)))}))
                              }
                    
                    else if (method == "pcti") {
                              ci[] <- t(x = sapply(X = 1:j, FUN = function(i) {quantile(x = betaij[, i], prob = p, na.rm = TRUE)}))
                              }
                                        
                    else if (method == "smoi") {
                              o1 <- oir - rep(x = or, each = k)
                              b1 <- betaij - rep(x = betaj, each = k)
                              ci[] <- t(x = betaj + rep(x = q, each = j) * sapply(X = 1:j, FUN = function(i) {sqrt(x = sum(x = colSums(x = (o1 * b1[, i]) / k, na.rm = TRUE)^2))}))
                              }
                    
                    else if (method == "stdi") {
                              ci[] <- t(x = betaj + rep(x = q, each = j) * sapply(X= 1:j, FUN = function(i) {sd(x = betaij[, i], na.rm = TRUE)}))
                              }
            
                    return(ci)
                    }
)

setMethod(f = "show",
          signature = c("summary", "peims"),
          definition = function(object) {
                    cat("\nSummary of the resampling process (k = ", 
                        nrow(x = object@betaij), 
                        " with n = ", 
                        ncol(x = object@oir), 
                        ")\n\nNumber of unique resampling replicates: ", 
                        nrow(x = unique(x = object@oir)), 
                        "\nNumber of unique models: ", 
                        nrow(x = object@frqM), 
                        "\n\nEstimates from bagging with correponding bias-corrected smoothed confidence intervals\n", 
                        sep = ""
                    )
                    
                    print(x = object@estm)
                    
                    cat("\nInclusion frequency of variables:\n", 
                        sep = ""
                    )
                    print(x = object$frqV)
          }
)

setMethod(f = "subset",
          signature = "peims",
          definition = function(x, model = 1) {
                    betaij <- data.table::as.data.table(x = x@betaij)
                    oir <- data.table::as.data.table(x = x@oir)
                    tmpVar <- data.table::as.data.table(x = !is.na(x = betaij))
                    tmpVar <- tmpVar[, n := .N, by = names(x = tmpVar)]
                    tmpVar <- tmpVar[, id := 1:nrow(x = tmpVar)]
                    tmpVar <- tmpVar[order(n, decreasing = TRUE)]
                    tmpVar <- tmpVar[, m := .GRP, by = eval(names(x = betaij))]
                    i <- tmpVar[m == model, id]
                    betaij <- as.matrix(set(x = betaij[i], j = unique(x = which(x = is.na(x = betaij[i]), arr.ind = TRUE)[, 2]), value = NULL))
                    oir <- as.matrix(oir[i])
                    
                    return(new(Class = "peims", oir = oir, betaij = betaij)) 
          }
)

setMethod(f = "summary", 
          signature = "peims", 
          definition = function(object) {
                    betaij <- object@betaij
                    oir <- object@oir
                    # Check if parameter estimates from preceding model fitting are missing to 
                    # subsequently compute frequencies of unique models and included variables 
                    # to indicate instability in model selection
                    tmpVar <- !is.na(x = betaij)
                    
                    # 'k' indicates the sets of resampling replicates for further computations
                    k <- nrow(x = tmpVar)
                    
                    # 'n' indicates the number of observations in each set of resampling 
                    # replicates
                    n <- ncol(x = object@oir)
                    
                    estm <- cbind("Point estimate" = coef(object), confint(object))
                    
                    # Cast matrix into data table to compute frequencies of unique models more
                    # efficient
                    frqM <- data.table::as.data.table(x = tmpVar)
                    
                    # Compute absolute frequencies of unique models to indicate instability in 
                    # model selection
                    frqM <- frqM[, .N, by = names(x = frqM)]
                    
                    # Compute relative frequencies of unique models to indicate instability in 
                    # model selection
                    frqM <- frqM[, Pr := N / k]
                    
                    # Arrange data table to create a rank order of frequencies of unique models
                    frqM <- frqM[order(N, decreasing = TRUE)]
                    
                    # Compute relative frequencies of included variables to indicate instability 
                    # in model selection
                    frqV <- colSums(x = obj) / k
                    
                    return(list(estm = estim, frqM = frqM[], frqV = frqV, oir = oir, betaij = betaij))
          }
)
