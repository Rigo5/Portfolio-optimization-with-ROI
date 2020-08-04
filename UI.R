#####Shiny dash of the portfolio#####
#user interface script 
library(shiny)
library(plotly)
library(ggplot2)


ui = fluidPage(
  title = 'Markowitz Portfolio with ROI S&P 500',
  fluidRow(
    column(
      6,
      selectInput(
        'Port_stocks',
        'Stocks for Portfolio',
        multiple = TRUE,
        choices = ticker,
        selected = ticker[1:10]
      )
    ),
    column(
      6,
      dateInput(
        'Start_date',
        'From',
        value = '2017-01-04'
      )
    )
  ),
  fluidRow(
    plotlyOutput('Cumulated_returns')
  ),
  fluidRow(
    numericInput(
      'Sample',
      'Sample dimension',
      value = 300,
      min = 300,
      max = 600
    )
  ),
  fluidRow(
    plotlyOutput('Weights')
  ),
  fluidRow(
    plotlyOutput('Backtest')
  )
)