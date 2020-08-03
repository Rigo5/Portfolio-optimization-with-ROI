library(quantmod)
library(stringr)
library(ROI)
library(ROI.plugin.quadprog)
#function for download data 
#I gonna use the getSymbols function


get_symbol = function(ticker, from = '2017-01-04'){
  error_handler = function(e){
    warning(paste0(ticker, ' is not present', sep = ''))
    #return NA value 
    return(NA)
  }
  price = tryCatch(getSymbols(ticker, from = from, auto.assign = FALSE),
                   error = error_handler)
  
  return(price)
}

#return a data frame with the different prices from a list 
merge_prices = function(list_prices){
  
  prices = list_prices[[1]]
  #stop here if there is just one ticker 
  if(length(list_prices) == 1){return(prices)}
  
  for(i in 2:length(list_prices)){
    #join only if the element is not na 
    if(!is.na(list_prices[[i]])){
      prices = inner_join(prices, list_prices[[i]], by = 'Index')
    }
  }
  
  return(prices)
}

#get prices of ticker vector
get_prices = function(ticker_vec, from = '2017-01-04'){
  
  list_prices = list()
  c = 1 
  for(i in ticker_vec){
    
    price = get_symbol(ticker = i, from = from)
    price = fortify.zoo(price)
    #select adj prices ans the Index
    price = price[,c(1,7)]
    #change col names 
    colnames(price) = c('Index', i)
    
    list_prices[[c]] = price
    c = c + 1 
  }
  return(merge_prices(list_prices))
}


test = get_prices(c('AAPL', 'GE', 'T'))


#return dor just one vector data
ret = function(vect){
  vect = as.numeric(unlist(vect))
  M = length(vect)
  i = 2
    
  results = c()
  results[1] = NA
  while(i <= M){
    results[i] = (vect[i] - vect[i-1])/vect[i-1]
    i = i +1
  }
  return(results)
}

#complete function wich use the previus one 
get_returns = function(prices){
  N = ncol(prices)
  for(i in 2:N){
    prices[, i] = ret_single(prices[, i]) 
    }
    
  return(prices[-1, ])
}

####Porfolio Classic Markowitz#####

min_var_portfolio = function(r_mat, beta = 0.5, short = FALSE){
  r_mat = na.omit(r_mat)
  
  N = ncol(r_mat)
  asset_names = colnames(r_mat)
  mu = colMeans(r_mat)
  
  
  obj = Q_objective(
    Q = beta * 2 * cov(r_mat),
    L = -mu
  )
  #The short is such that by selling I finance other positions
  #Is basic model without any type of cost of borrowing
  if(short == FALSE){
    Amat = rep(1,N)
    constr = L_constraint(
      Amat,
      dir = c('=='),
      rhs = c(100)
    )
  }else{
    Amat = rep(1,N)
    bound = V_bound(li = 1:N,
                    lb = rep(-100, N))
    constr = L_constraint(
      Amat,
      dir = c('=='),
      rhs = c(100)
    )
    #costructing the optimization problem 
    portfolio = OP(objective = obj,
                   constraints = constr,
                   bounds = bound)
    #return the problem solution objective
    return(ROI_solve(test_p, solver = 'quadprog'))
  }
  
  portfolio = OP(objective = obj,
                 constraints = constr)
  
  return(list(objective = obj, constraint =  constr))
}


















