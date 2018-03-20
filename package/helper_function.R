# File: helper_function.R
# Package: peims
# Version: 0.2.0
# Author: Jakob Schöpe
# Date: March 20, 2018
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
