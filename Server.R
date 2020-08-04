#Obtain the tickers 
ticker_list = scrape_sp500()


#Server part of the shiny app 

server = function(input, output){
  prices = eventReactive(input$go, { 
    tickers = c()
    for(i in 1:length(input$Port_stocks)){
      tickers[i] = ticker_list[input$Port_stocks[i]]
    }
    print(tickers)
    return(get_prices(tickers, input$Start_date))
  })
  
  returns = reactive({
    return(get_returns(prices()))
  })
  
  cumulated = reactive({
    return(get_cumulated(prices()))
  })
  
  output$Cumulated_returns = renderPlotly({
    ggplotly(cumulated() %>%
             gather(key = 'Asset', value = 'Cumulated', -Index) %>%
      ggplot(mapping = aes(x = Index, y = Cumulated, colour = Asset))+
             geom_line()+
             theme_classic())
  })
  
  portfolio = reactive({
    N = nrow(returns())
    ret_mat = returns()[(N - input$Sample):N, ]
    
    if(input$Type_port == 'Short'){
      return(min_var_portfolio(ret_mat, beta = input$Beta, short = TRUE))
    }else{
      return(min_var_portfolio(ret_mat, beta = input$Beta))
    }
  })
  
  output$Weights = renderPlotly({
    data = data.frame(Asset = colnames(returns()[, -1]), Weights = round(portfolio()$solution, 3))
    
    ggplotly(ggplot(data = data, mapping = aes(x = Asset, y = Weights, fill = as.factor(Asset)))+
               geom_bar( stat="identity")+
               theme(legend.position = 'none',
                     panel.background = element_rect(fill = 'white')))
  })
  
}



shinyApp(ui, server)

