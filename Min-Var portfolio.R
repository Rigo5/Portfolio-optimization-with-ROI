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

#function for cumulated returns
get_cumulated = function(mat, price = TRUE ){
  N = ncol(mat)
  if(price == TRUE){
    ret_mat = get_returns(mat)
    
    
    #Index base = 100
    ret_mat[, 2:N] = ret_mat[, 2:N] +1
    ret_mat[1, 2:N] = 100 
    #cumulated returns now
    if(ncol(ret_mat) > 2){
      ret_mat[, 2:N] = apply(ret_mat[, 2:N], 2, cumprod)
    }else{
      ret_mat[, 2] = cumprod(ret_mat[, 2])
    }
    
    
    return(ret_mat)
  }else{
    #Index base = 100
    mat[, 2:N] = mat[, 2:N] +1
    mat[1, 2:N] = 100 
    #cumulated returns now
    if(ncol(mat) > 2){
      mat[, 2:N] = apply(mat[, 2:N], 2, cumprod)
    }else{
      mat[, 2] = cumprod(mat[, 2])
    }
    return(mat)
  }
  
}

####Porfolio Classic Markowitz#####

min_var_portfolio = function(r_mat, beta = 0.5, short = FALSE){
  r_mat = na.omit(r_mat)
  #erase the Index
  r_mat = r_mat[, -1]
  
  N = ncol(r_mat)
  asset_names = colnames(r_mat)
  mu = colMeans(r_mat)
  
  
  obj = Q_objective(
    Q = beta * 2 * cov(r_mat),
    L = -(1-beta)*mu
  )
  #The short is such that by selling I finance other positions
  #Is basic model without any type of cost of borrowing
  if(short == FALSE){
    Amat = rep(1,N)
    constr = L_constraint(
      Amat,
      dir = c('=='),
      rhs = c(1)
    )
  }else{
    Amat = rep(1,N)
    bound = V_bound(li = 1:N,
                    lb = rep(-100, N))
    constr = L_constraint(
      Amat,
      dir = c('=='),
      rhs = c(1)
    )
    #costructing the optimization problem 
    portfolio = OP(objective = obj,
                   constraints = constr,
                   bounds = bound)
    #return the problem solution objective
    return(ROI_solve(portfolio, solver = 'quadprog'))
  }
  
  portfolio = OP(objective = obj,
                 constraints = constr)
  
  return(ROI_solve(portfolio, solver = 'quadprog'))
}


#I gonna scrape the tickers of the SP500 

scrape_sp500 = function(){
  require(rvest)
  
  tabl = read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies') %>% html_table(header = TRUE, fill = TRUE ) %>%
    .[[1]] %>% as.data.frame() %>% .[, c(1,2,4)]
  
  tick = as.list(tabl[, 1])
  names(tick) = tabl[, 2]
  
  return(tick)
}


#function for backtesting 

backtest = function(sample, ret_mat, short = FALSE, beta = 0.5){
  #beginning and end of sample period 
  start_p = sample +1  
  end_p = start_p - sample
  
  N = nrow(ret_mat)
  Index = ret_mat[, 1]
  solution = list()
  
  c = 1
  while(start_p <= N){
    data_p = ret_mat[end_p:start_p, -1]
    solution_list = min_var_portfolio(data_p, beta = beta, short = short)
    solution[[c]] = round(solution_list$solution, 3)
    
    start_p = start_p + 1
    end_p = end_p + 1
    c = c +1 
  }
  weight_data = do.call(rbind, solution)
  
  #now I derive the portfolio returns 
  port_ret = round(weight_data * ret_mat[(sample +1):N, -1], 3)
  port_ret = apply(port_ret, 1, sum)
  
  #arrange the returns with the Date index 
  return(data.frame(Index = Index[(sample +1):N], Port_ret = port_ret))
  
}

