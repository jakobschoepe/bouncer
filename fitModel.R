# File: fitModel.R
# Version: 0.1.0
# Author: Jakob Schöpe
# Date: February 19, 2018
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

setClass(Class = "bouncR", representation = representation(obs = "matrix", betas = "matrix"))

fitModel <- function(f, data, size, replace, k, seed, ncpus, pkgs, ...) {
              if(!is.function(x = f)) {
                stop("\"f\" must be a function")
              }
  
              else if(any(!is.element(el = c("i", "data", "size", "replace"), set = names(x = formals(fun = f))))) {
                stop("\"f\" must contain the following arguments: \"i\", \"data\", \"size\" and \"replace\"")
              }
  
              else if(!is.data.frame(x = data)) {
                stop("\"data\" must be a data frame")
              }
                
              else if(!is.integer(x = size)) {
                stop("\"size\" must be a positive integer")
              }
              
              else if(size < 1) {
                stop("\"size\" must be a positive integer")
              }
                
              else if(length(x = size) > 1) {
                stop("single positive integer for \"size\" expected")
              }
                
              else if(size > nrow(x = data)) {
                stop("\"size\" exceeds available number of observations")
              }
              
              else if(!is.logical(x = replace)) {
                stop("\"replace\" must be a logical value")
              }
                
              else if(length(x = replace) > 1) {
                stop("single logical value for \"replace\" expected")
              }
              
              else if(!is.integer(x = k)) {
                stop("\"k\" must be a positive integer equal to or greater 2")
              }
                
              else if(k < 2) {
                stop("\"k\" must be a positive integer equal to or greater 2")
              }
              
              else if(length(x = k) > 1) {
                stop("single positive integer for \"k\" expected")
              }
              
              else if(isTRUE(x = replace) & k > choose(n = nrow(x = data) + size - 1, k = size)) {
                stop("\"size\" is to large considering ", k, " resampling replicates with replacement")
              }
              
              else if(!isTRUE(x = replace) & k > choose(n = nrow(x = data), k = size)) {
                stop("\"size\" is to large considering ", k, " resampling replicates without replacement")
              }
                               
              else if(!is.integer(x = seed)) {
                stop("\"seed\" must be an integer")
              }
              
              else if(length(x = seed) > 1) {
                stop("single integer for \"seed\" expected")
              }
              
              else if(!is.integer(x = ncpus)) {
                stop("\"ncpus\" must be a positive integer")
              }
                
              else if(ncpus < 1) {
                stop("\"ncpus\" must be a positive integer")
              }
                
              else if(length(x = ncpus) > 1) {
                stop("single positive integer for \"ncpus\" expected")
              }
              
              else if(ncpus > parallel::detectCores()) {
                stop("number of cores exceeds number of detected cores")
              }
                                          
              else {
                # Add an identifier to each observation
                data$id <- 1:nrow(x = data)
                
                # Set up a cluster for parallel processing
                cluster <- parallel::makePSOCKcluster(names = ncpus)
                
                # Load required packages on each node
                if(!missing(x = pkgs)) {
                  if(!is.character(x = pkgs)) {
                    parallel::stopCluster(cl = cluster)
                    stop("\"pkgs\" must be a character vector")
                  }
                  else {
                    parallel::clusterCall(cl = cluster, fun = lapply, X = pkgs, FUN = require, character.only = TRUE)
                  }
                }
                
                # Export required objects to each node
                parallel::clusterExport(cl = cluster, varlist = c('data', 'f'))
                
                # Set seed for L'Ecuyer's pseudorandom number generator
                parallel::clusterSetRNGStream(cl = cluster, iseed = seed)
                
                # Generate sets of pseudorandom resampling replicates and run 'f'
                models <- pbapply::pblapply(cl = cluster, X = 1:k, FUN = f, data = data, size = size, replace = replace, ...)
              
                # Shut down the cluster
                parallel::stopCluster(cl = cluster)
                
                # Create a matrix which contains the counts of drawn realizations in each resampling replicate
                obs <- as.matrix(x = data.table::rbindlist(l = lapply(X = 1:k, function(i) {as.list(x = table(match(x = models[[i]][[1]], table = 1:nrow(data))))}), fill = TRUE))
                
                # Sort columns of "obs"
                obs <- obs[, order(as.integer(x = colnames(x = obs)))]
                
                # Replace NAs in "obs" with zeros (?)
                obs[is.na(x = obs)] <- 0
                
                # Create a matrix which contains the model parameters from each resampling replicate
                betas <- as.matrix(x = data.table::rbindlist(l = lapply(X = 1:k, function(i) {as.list(x = models[[i]][[2]])}), fill = TRUE))
                
                # Sort columns of "betas"
                betas <- betas[, order(colnames(x = betas))]
                
                # Create a new S4 "bouncR" object
                OUTPUT <- new(Class = "bouncR", obs = obs, betas = betas)
                
                # Return the S4 "bouncR" object
                return(OUTPUT)
              }
}
