# import necessary libraries
library(readxl)

library(NlcOptim)

library(RColorBrewer)

# read data
funds <- read_excel(path = "Data_Exercise 7.xlsx", sheet = "FundData")

indices <- read_excel(path = "Data_Exercise 7.xlsx", sheet = "IndexData")

# prepare data
funds_new <- as.matrix(funds[87: 1, - 1])

indices_new <- as.matrix(indices[87: 1, - 1])

# define style analysis function
style_func <- function(rets, ind, method, dt){
  windows <- nrow(rets) - dt + 1
  
  nrets <- ncol(rets)
  
  nind <- ncol(ind)
  
  expos_arr <- array(data = NA, dim = c(windows, nind, nrets))
  
  error_arr <- array(data = NA, dim = c(windows, 1, nrets))
  
  err_var_arr <- array(data = NA, dim = c(1, 1, nrets))
  
  r_sq_arr <- array(data = NA, dim = c(windows, 1, nrets))
  
  switch (method,
    "unc" = {
      for(j in 1: nrets){
        for(i in 1: windows){
          unc_reg <- lm(formula = rets[i: (dt + i - 1), j] ~
                          ind[i: (dt + i - 1),])
          
          expos_arr[i,, j] <- unc_reg$coefficients[-1]
          
          error_arr[i,, j] <- rets[i, j] - ind[i,] %*% expos_arr[i,, j]
            
          err_var_arr[,,j] <- var(error_arr[,,j])
          
          r_sq_arr[i,, j] <- summary(unc_reg)$r.squared
        }
      }
    },
    
    "con" = {
      for(j in 1: nrets){
        for(i in 1: windows){
          con_reg <- solnl(X = rep(x = 1 / nind, times = nind),
                           objfun = function(b) var(rets[i: (dt + i - 1), j] -
                                                      ind[i: (dt + i - 1),] %*%
                                                      b),
                           Aeq = matrix(data = 1, nrow = 1, ncol = nind),
                           Beq = 1)
          
          expos_arr[i,, j] <- con_reg$par
          
          error_arr[i,, j] <- rets[i, j] - ind[i,] %*% expos_arr[i,, j]
          
          err_var_arr[,,j] <- var(error_arr[,,j])
          
          r_sq_arr[i,, j] <- 1 - con_reg$fn / var(rets[i: (dt + i - 1), j])
        }
      }
    },
    
    "quad" = {
      for(j in 1: nrets){
        for(i in 1: windows){
          quad_prog <- solnl(X = rep(x = 1 / nind, times = nind),
                             objfun = function(b) var(rets[i: (dt + i - 1), j] -
                                                        ind[i: (dt + i - 1),]
                                                      %*% b),
                             Aeq = matrix(data = 1, nrow = 1, ncol = nind),
                             Beq = 1, lb = rep(x = 0, times = nind))
          
          expos_arr[i,, j] <- quad_prog$par
          
          error_arr[i,, j] <- rets[i, j] - ind[i,] %*% expos_arr[i,, j]
          
          err_var_arr[,,j] <- var(error_arr[,,j])
          
          r_sq_arr[i,, j] <- 1 - quad_prog$fn / var(rets[i: (dt + i - 1), j])
        }
        
        fig_col <- brewer.pal(n = nind, name = "Dark2")
        
        par(xpd = T, mar = par()$mar + c(0, 0, 0, 8))
        
        figure <- barplot(t(expos_arr[,,j]), main = colnames(rets)[j],
                          col = fig_col)
        
        lines(x = figure, y = t(r_sq_arr[,,j]), col = "red")
        
        legend(x = "right" , fill = fig_col, legend = colnames(ind),
               bty = "n", bg = NA, cex = .7, pt.cex = .7, xjust = 1,
               inset = c(-.3, 0))
        
        par(mar = c(5, 4, 4, 2) + .1)
      }
    },
    
    stop("Please enter 'unc', 'con', or 'quad' as method.")
  )
  
  lst <- list("Exposures" = expos_arr, "Tracking errors" = error_arr,
              "Tracking error variance" = err_var_arr,
              "R squared" = r_sq_arr)
  
  return(lst)
}

# perform style function for each type of algorithm
style_unc <- style_func(rets = funds_new, ind = indices_new, method = "unc",
                        dt = 36)

style_con <- style_func(rets = funds_new, ind = indices_new, method = "con",
                        dt = 36)

style_quad <- style_func(rets = funds_new, ind = indices_new, method = "quad",
                         dt = 36)
# replacing matrices with multidimensional arrays improves consistency and
# allows to better tackle the nested for loop construction; possible error
# involves tracking errors, the solution shows its size should be equal to dt,
# whereas mine is equal to windows; this implies that the residuals from each
# data point should be stored, originating the same problem i had in the
# previous attempt, namely that i should update dt values at a time, in a non
# overlapping fashion, which led me to try different kinds of indexing before
# giving up because i couldn't come up with a solution