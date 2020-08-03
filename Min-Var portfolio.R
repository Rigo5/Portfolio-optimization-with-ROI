library(quantmod)
library(stringr)

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



