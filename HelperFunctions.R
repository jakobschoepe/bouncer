# File: HelperFunctions.R
# Version: 0.1.0
# Author: Jakob Schöpe
# Date: March 14, 2018
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
          signature = "bouncR",
          definition = function(object, model, na.action) {
                    obj <- object@betas
                    if(!missing(x = na.action)) {
                              obj[which(x = is.na(x = obj))] <- na.action
                    }
                    
                    if(!missing(x = model)) {
                              
                    }
                    
                    betas <- colMeans(x = obj, na.rm = TRUE)
                    return(thetas)
          }
)


setMethod(f = "confint",
          signature = "bouncR",
          definition = function(object, level, model = 1, ...) {
            obs <- selectModel(object = object, model = model)[[1]]
            betas <- selectModel(object = object, model = model)[[2]]
            thetas <- coef(object = object, model = model)
            pnames <- names(x = thetas)
            parm <- 1:length(x = thetas)
            alpha <- (1 - level) / 2
            p <- c(alpha, 1 - alpha)
            pct <- paste(format(100 * p, trim = TRUE, scientific = FALSE, digits = 3), "%", sep = "")
            n <- nrow(x = betas)
            df <- n - length(x = thetas)
            q <- qt(p = p, df = df)
            
            dobs <- do.call(what = cbind, args = lapply(X = 1:ncol(obs), FUN = function(i) {obs[,i] - mean(obs[,i])}))
            dbetas <- do.call(what = cbind, args = lapply(X = 1:ncol(betas), FUN = function(i) {betas[,i] - mean(betas[,i])}))
            
            sd <- sapply(X = 1:ncol(dbetas), FUN = function(i) {sqrt(x = sum(x = sapply(X = 1:ncol(x = dobs), FUN = function(ii) {sum(x = sapply(X = 1:nrow(x = dbetas), FUN = function(iii) {(dbetas[iii,i] * dobs[iii,ii]) / nrow(x = dbetas)}))^2})))})
            ci <- array(data = NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
            
            ci[] <- t(x = sapply(X = 1:length(x = parm), FUN = function(i) {thetas[parm[i]] + q * sd[i]}))
            
            return(ci)
          }
)


setMethod(f = "show",
          signature = "bouncR",
          definition = function(object) {
                    obj <- summary(object)
                    cat("\nSummary of the resampling process (k = ", nrow(x = object@obs), " with n = ", ncol(x = object@obs), ")\n\nNumber of unique resampling replicates: ", nrow(x = unique(x = object@obs)), "\nNumber of unique models: ", nrow(x = obj$frqM), "\n\n", sep = "")
                    print(x = obj$frqM[1:3])
                    cat("\nInclusion frequency of variables:\n", sep = "")
                    print(x = obj$frqV)
          }
)


setMethod(f = "summary", 
          signature = "bouncR", 
          definition = function(object) {
                    obj <- object@betas
                    k <- nrow(x = obj)
                    frqM <- data.table::as.data.table(x = obj)
                    frqM <- frqM[, .N, by = names(x = frqM)]
                    frqM <- frqM[, Pr := N / k]
                    frqM <- frqM[order(-N)]
                    frqV <- colSums(x = obj) / k
                    
                    return(list(frqM = frqM[], frqV = frqV))
          }
)


selectModel <- function(object, model = 1) {
                  # Identify all unique models
                  unqMod <- summary(object)
                  
                  # Select model
                  selMod <- unqMod[model, 1:(ncol(unqMod)-2)]
                  
                  # Create an index
                  i <- which(x = apply(X = !is.na(object@betas), MARGIN = 1, FUN = function(x) {all(x == selMod)}))
                  
                  # Create a matrix which contains model parameters
                  betas <- object@betas[i,]
                  
                  # Identify excluded covariates
                  na <- unique(x = which(x = is.na(betas), arr.ind = TRUE)[,2])
                  
                  # Delete excluded covariates/columns from "betas"
                  if(length(x = na) > 0) {
                    betas <- as.matrix(x = betas[,-na])
                  }
                  
                  obs <- object@obs[i,]
                  
                  return(list(obs, betas, i))
               }
)
