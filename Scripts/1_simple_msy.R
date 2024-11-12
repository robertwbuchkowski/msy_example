# Load necessary libraries
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Maximum Sustainable Yield (MSY) Model with Two Fish Species"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("harvestRate", "Harvest Rate:", min = 0, max = 1, value = 0.5, step = 0.01),
      sliderInput("SARpenalty", "Species at risk penalty:", min = 0, max = 1000, value = 0, step = 10)
    ),
    mainPanel(
      plotOutput("populationPlot"),
      plotOutput("profitPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$populationPlot <- renderPlot({
    # Parameters
    r1 <- 0.5  # Intrinsic growth rate for Commercial Fish
    K1 <- 1000 # Carrying capacity for Commercial Fish
    r2 <- 0.2  # Intrinsic growth rate for Species at Risk Fish
    K2 <- 200  # Carrying capacity for Species at Risk Fish
    H <- input$harvestRate # Harvest rate
    P <- input$SARpenalty # SAR penalty

    # Time steps
    time <- seq(0, 50, by = 1)

    # Initialize populations
    N1 <- numeric(length(time))
    N2 <- numeric(length(time))
    N1[1] <- 500 # Initial population for Commercial Fish
    N2[1] <- 20  # Initial population for Species at Risk Fish

    # Simulate population dynamics
    for (t in 2:length(time)) {
      N1[t] <- N1[t-1] + r1 * N1[t-1] * (1 - N1[t-1] / K1) - H * N1[t-1]
      N2[t] <- N2[t-1] + r2 * N2[t-1] * (1 - N2[t-1] / K2) - H * N2[t-1]
      if (N1[t] < 0) N1[t] <- 0 # Population cannot be negative
      if (N2[t] < 0) N2[t] <- 0 # Population cannot be negative
    }

    # Plot results
    plot(time, N1, type = "l", col = "blue", lwd = 2,
         xlab = "Time", ylab = "Population Size",
         main = "Population Dynamics under Different Harvest Rates", ylim = c(0, 1000))
    lines(time, N2, col = "red", lwd = 2)
    abline(h = 0, lwd = 2, lty = 2)
    legend("topright", legend = c("Commercial Fish", "Species at Risk Fish"),
           col = c("blue", "red"), lwd = 2)
  })

  output$profitPlot <- renderPlot({
    # Parameters
    r1 <- 0.5  # Intrinsic growth rate for Commercial Fish
    K1 <- 1000 # Carrying capacity for Commercial Fish
    r2 <- 0.2  # Intrinsic growth rate for Species at Risk Fish
    K2 <- 200  # Carrying capacity for Species at Risk Fish
    H <- input$harvestRate # Harvest rate
    P <- input$SARpenalty # SAR penalty

    # Time steps
    time <- seq(0, 50, by = 1)

    # Initialize populations and profit
    N1 <- numeric(length(time))
    N2 <- numeric(length(time))
    profit <- numeric(length(time))
    N1[1] <- 500 # Initial population for Commercial Fish
    N2[1] <- 20  # Initial population for Species at Risk Fish

    # Simulate population dynamics and calculate profit
    for (t in 2:length(time)) {
      N1[t] <- N1[t-1] + r1 * N1[t-1] * (1 - N1[t-1] / K1) - H * N1[t-1]
      N2[t] <- N2[t-1] + r2 * N2[t-1] * (1 - N2[t-1] / K2) - H * N2[t-1]
      if (N1[t] < 0) N1[t] <- 0 # Population cannot be negative
      if (N2[t] < 0) N2[t] <- 0 # Population cannot be negative
      profit[t] <- H * (10 * N1[t] - P * N2[t])
    }

    # Plot results
    plot(time, profit, type = "l", col = "green", lwd = 2,
         xlab = "Time", ylab = "Net Profit",
         main = "Annual Net Profit from Fishing ($)", ylim = c(0,2000))

    calculate_npv <- function(net_profits, discount_rate = 0.03) {
      npv <- sum(net_profits / (1 + discount_rate)^(0:(length(net_profits) - 1)))
      return(npv)
    }

    npv = round(calculate_npv(net_profits = profit), 0)

    legend("topright", legend = paste0("Net present value= $",npv), bty = "n")


  })
}

# Run the application
shinyApp(ui = ui, server = server)
