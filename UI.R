#####Shiny dash of the portfolio#####
#user interface script 
library(shiny)
library(plotly)
library(ggplot2)


ui = fluidPage(
  title = 'Markowitz Portfolio with ROI S&P 500',
  fluidRow(
    column(
      4,
      selectInput(
        'Port_stocks',
        'Stocks for Portfolio',
        multiple = TRUE,
        choices = names(ticker_list),
        selected = names(ticker_list)[1:10]
      )
    ),
    column(
      4,
      dateInput(
        'Start_date',
        'From',
        value = '2017-01-04'
      )
    ),
    column(
      4,
      actionButton("go", "Go")
    )
  ),
  fluidRow(
    plotlyOutput('Cumulated_returns')
  ),
  fluidRow(
    column(
      4,
      numericInput(
        'Sample',
        'Sample dimension',
        value = 300,
        min = 300,
        max = 600
      )
    ),
    column(
      4,
      selectInput(
        'Type_port',
        'Strategy',
        choices = c('Long Only' = 'Long Only', 'Short' = 'Short')
      )
    ),
    column(
      4,
      numericInput(
        'Beta',
        'Risk aversion',
        value = 0.5,
        min = 0.000001,
        max = 1
      )
    )
  ),
  fluidRow(
    plotlyOutput('Weights')
  ),
  fluidRow(
    plotlyOutput('Backtest')
  )
)
