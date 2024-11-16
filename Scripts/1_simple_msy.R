library(shiny)
library(deSolve)

# Define UI
ui <- fluidPage(
  titlePanel("Maximum Sustainable Yield (MSY) Model with Two Fish Species"),
  fluidRow(
    column(4,
           sliderInput("harvestRate", "Harvest Rate:", min = 0, max = 1, value = 0, step = 0.1),
           # numericInput("harvestRateInput", "Harvest Rate Input:", value = 0, min = 0, max = 1, step = 0.1),
           sliderInput("SARpenalty", "Species at risk penalty:", min = 0, max = 1000, value = 0, step = 100),
           # numericInput("SARpenaltyInput", "Species at risk penalty Input:", value = 0, min = 0, max = 1000, step = 100),
           sliderInput("selectiveNets", "Selective nets:", min = 0, max = 100, value = 0, step = 10),
           # numericInput("selectiveNetsInput", "Selective nets Input:", value = 0, min = 0, max = 100, step = 10)
    ),
    column(8,
           plotOutput("populationPlot", height = "300px"),
           plotOutput("profitPlot", height = "300px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observe({
    updateSliderInput(session, "harvestRate", value = input$harvestRateInput)
    updateSliderInput(session, "SARpenalty", value = input$SARpenaltyInput)
    updateSliderInput(session, "selectiveNets", value = input$selectiveNetsInput)
  })

  # observe({
  #   updateNumericInput(session, "harvestRateInput", value = input$harvestRate)
  #   updateNumericInput(session, "SARpenaltyInput", value = input$SARpenalty)
  #   updateNumericInput(session, "selectiveNetsInput", value = input$selectiveNets)
  # })

  output$populationPlot <- renderPlot({
    # Parameters
    r1 <- 0.5  # Intrinsic growth rate for Commercial Fish
    K1 <- 1000 # Carrying capacity for Commercial Fish
    r2 <- 0.05  # Intrinsic growth rate for Species at Risk Fish
    K2 <- 200  # Carrying capacity for Species at Risk Fish
    H <- input$harvestRate # Harvest rate
    P <- input$SARpenalty # SAR penalty
    C <- input$selectiveNets # Selective nets

    # Model function
    fish_model <- function(time, state, parameters) {
      with(as.list(c(state, parameters)), {
        dN1 <- r1 * N1 * (1 - N1 / K1) - H * N1
        dN2 <- r2 * N2 * (1 - N2 / K2) - H * N2 * (1 - C / 100)
        list(c(dN1, dN2))
      })
    }

    # Initial state
    state <- c(N1 = 500, N2 = 20)

    # Parameters
    parameters <- c(r1 = r1, K1 = K1, r2 = r2, K2 = K2, H = H, C = C)

    # Time steps
    times <- seq(0, 50, by = 1)

    # Solve the model
    out <- ode(y = state, times = times, func = fish_model, parms = parameters)

    # Plot results
    plot(out[, "time"], out[, "N1"], type = "l", col = "blue", lwd = 2,
         xlab = "Time", ylab = "Population Size",
         main = "Population Dynamics under Different Harvest Rates", ylim = c(0, 1000))
    lines(out[, "time"], out[, "N2"], col = "red", lwd = 2)
    abline(h = 0, lwd = 2, lty = 2)
    legend("topright", legend = c("Commercial Fish", "Species at Risk Fish"),
           col = c("blue", "red"), lwd = 2)
  })

  output$profitPlot <- renderPlot({
    # Parameters
    r1 <- 0.5  # Intrinsic growth rate for Commercial Fish
    K1 <- 1000 # Carrying capacity for Commercial Fish
    r2 <- 0.05  # Intrinsic growth rate for Species at Risk Fish
    K2 <- 200  # Carrying capacity for Species at Risk Fish
    H <- input$harvestRate # Harvest rate
    P <- input$SARpenalty # SAR penalty
    C <- input$selectiveNets # Selective nets

    # Model function
    fish_model <- function(time, state, parameters) {
      with(as.list(c(state, parameters)), {
        dN1 <- r1 * N1 * (1 - N1 / K1) - H * N1
        dN2 <- r2 * N2 * (1 - N2 / K2) - H * N2 * (1 - C / 100)
        list(c(dN1, dN2))
      })
    }

    # Initial state
    state <- c(N1 = 500, N2 = 20)

    # Parameters
    parameters <- c(r1 = r1, K1 = K1, r2 = r2, K2 = K2, H = H, C = C)

    # Time steps
    times <- seq(0, 50, by = 1)

    # Solve the model
    out <- ode(y = state, times = times, func = fish_model, parms = parameters)

    # Calculate profit
    profit <- H * 10 * out[, "N1"] - H * (1 - C / 100) * P * out[, "N2"] - C * 0.0001 * H * out[, "N1"]

    # Plot results
    plot(out[, "time"], profit, type = "l", col = "green", lwd = 2,
         xlab = "Time", ylab = "Net Profit",
         main = "Annual Net Profit from Fishing ($)", ylim = c(min(profit), max(profit)))

    calculate_npv <- function(net_profits, discount_rate = 0.03) {
      npv <- sum(net_profits / (1 + discount_rate)^(0:(length(net_profits) - 1)))
      return(npv)
    }

    npv = round(calculate_npv(net_profits = profit), 0)

    legend("topright", legend = paste0("Net present value= $", npv), bty = "n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
