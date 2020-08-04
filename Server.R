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
}



shinyApp(ui, server)

