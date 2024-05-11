library(shiny)
library(httr)
library(ggplot2)
library(dplyr)
library(cubature)
library(reactlog)

# reactlog_enable()

download <- function(projectURL) {
  urlPath = paste0(projectURL, ".json")
  data = httr::GET(urlPath)
  return(jsonlite::fromJSON(httr::content(data, "text")))
}

ui <- fluidPage(
  
  title = "NeuroStride",
  titlePanel("NeuroStride"),
  
  fluidRow(
    column(12,
           htmlOutput("warning")
           # span( textOutput("warning"), style="color: red, font-style: bold")
      )
  ),
  
  fluidRow(
    column(3, style="margin-left: 20px; margin-right: 20px; border-right: 1px lightgray solid",
           h3("Graph Options"),
           h5("Select all data to be shown in the graphs below. Graphs refresh live when sensor is on."),
           checkboxInput("avg_values", "Show average value lines", value=TRUE),
           actionButton("clear", "Clear all data")
          ),
    
    column(2, style="margin-right: 20px; border-right: 1px lightgray solid",
           h3("Position"),
           checkboxGroupInput("pos_opts", "Select data to be shown", c("Pitch", "Roll", "Yaw"), selected = c("Pitch", "Roll", "Yaw"))
    ),
    
    column(2, style="margin-right: 20px; border-right: 1px lightgray solid",
           h3("Acceleration"),
           checkboxGroupInput("accel_opts", "Select data to be shown", c("x", "y", "z"), selected = c("x", "y", "z"))
    ),
    
    column(2, style="margin-right: 20px; border-right: 1px lightgray solid",
           h3("Gyroscope"),
           checkboxGroupInput("gyro_opts", "Select data to be shown", c("x", "y", "z"), selected = c("x", "y", "z"))
    ),
    
    column(2,
           h3("Magnetism"),
           checkboxGroupInput("mag_opts", "Select data to be shown", c("x", "y", "z"), selected = c("x", "y", "z"))
    ),
  ),
  
  hr(),
  
  fluidRow(style="margin-bottom: 20px;",
    column(6, style="border-right: 1px lightgray solid;",
           h4("Position Graph", style="margin-left: 40%;"),
           plotOutput("posPlot")
    ),
    
    column(6,
           h4("Acceleration Graph", style="margin-left: 40%;"),
           plotOutput("accelPlot")
    )
  ),
  
  hr(),
  
  fluidRow(style="margin-bottom: 70px;",
    column(6, style="border-right: 1px lightgray solid;",
           h4("Gyroscope Graph", style="margin-left: 40%;"),
           plotOutput("gyroPlot")
    ),
    
    column(6,
           h4("Magnetism Graph", style="margin-left: 40%;"),
           plotOutput("magPlot")
    )
  )
  
)

server <- function(input, output, session) {
  
  clear_db <- function() {
    httr::DELETE( 'https://neurostride-80ede-default-rtdb.firebaseio.com/.json' )
  }
  
  observeEvent(input$clear,{
    clear_db()
  })
  
  df <- reactivePoll(100, session,
                     
                     checkFunc = function() {
                       data = download('https://neurostride-80ede-default-rtdb.firebaseio.com/')
                       data <- bind_rows(lapply(data, as.data.frame))
                       rownames(data) <- seq_len(nrow(data)) - 1

                       length(data)
                     },
                     
                     valueFunc = function() {
                       print("refresh")
                       data = download('https://neurostride-80ede-default-rtdb.firebaseio.com/')
                       data <- bind_rows(lapply(data, as.data.frame))
                       rownames(data) <- seq_len(nrow(data)) - 1
                       
                       return(data)
                     }
  )
  
  output$warning <- renderText({
    df <- df()
    if (length(df$pitch) > 5000) {
      return( paste("<font color=\"#FF0000\"><b>", "Live refresh disabled after 5000 records.", "</b></font>") )
    }
  })
  
  update <- function(curr) { reactive({
    values <- c()
    values$vars <- c()
    values$groups <- c()
    values$avgs <- c()
    check <- input$avg_values
    data <- df()
    
    if (curr == "pos") {
      cols <- tolower( input$pos_opts )
    }
    if (curr == "accel") {
      cols <- paste("accel_", input$accel_opts, sep = "")
    }
    if (curr == "gyro") {
      cols <- paste("gyro_", input$gyro_opts, sep = "")
    }
    if (curr == "mag") {
      cols <- paste("mag_", input$mag_opts, sep = "")
    }
    
    for (i in 1:length(cols)) {
      values$vars <- c( values$vars, c( data[[ cols[i] ]] ) )
      values$groups <- c( values$groups, c( rep(cols[i], nrow(data)) ) )
      
      if (check) {
        x <- 1:length( data[[ cols[i] ]] )
        f <- splinefun(x, data[[ cols[i] ]])
        
        if (any(!is.finite(f(x)))) {
          next
        }
        else {
          integral <- adaptIntegrate(f, min(x), max(x))
          avg_value <- integral$integral / (x[length(x)] - x[1])
          values$avgs <- c(values$avgs, avg_value)
        }
      }
      
    }
    
    return(values)
  }) }
  
  output$posPlot <- renderPlot({
    
    if( length(input$pos_opts) == 0 ) {
      return( ggplot() )
    }
    
    df <- df()
    
    if( length(df) == 0 ) {
      return( ggplot() )
    }
    
    lengths <- length(df$pitch) - 1
    pos_args <- update("pos")()
    df_pos <- data.frame(x = 0:lengths, y = pos_args$vars, group = pos_args$groups)
    plot <- ggplot(df_pos, aes(x, y, col = group)) +
              geom_line() + facet_grid(group ~ ., scales = "free_y")
    
    colors <- c("red", "green", "blue")
    if (length(pos_args$avgs) != 0) {
      for (i in 1:length(pos_args$avgs)) {
        plot <- plot + geom_hline(yintercept=pos_args$avgs[i], linetype="dashed", color=colors[i])
      }
    }
    
    return(plot)
      
  })
  
  output$accelPlot <- renderPlot({
    
    if( length(input$accel_opts) == 0 ) {
      return( ggplot() )
    }
    
    df <- df()
    
    if( length(df) == 0 ) {
      return( ggplot() )
    }
    
    lengths <- length(df$accel_x) - 1
    pos_args <- update("accel")()
    
    df_accel <- data.frame(x = 0:lengths, y = pos_args$vars, group = pos_args$groups)
    plot <- ggplot(df_accel, aes(x, y, col = group)) + 
              geom_line() + facet_grid(group ~ ., scales = "free_y")
    
    colors <- c("red", "green", "blue")
    if (length(pos_args$avgs) != 0) {
      for (i in 1:length(pos_args$avgs)) {
        plot <- plot + geom_hline(yintercept=pos_args$avgs[i], linetype="dashed", color=colors[i])
      }
    }
    
    return(plot)
    
  })
  
  output$gyroPlot <- renderPlot({
    
    if( length(input$gyro_opts) == 0 ) {
      return( ggplot() )
    }
    
    df <- df()
    
    if( length(df) == 0 ) {
      return( ggplot() )
    }
    
    lengths <- length(df$gyro_x) - 1
    pos_args <- update("gyro")()
    
    df_gyro <- data.frame(x = 0:lengths, y = pos_args$vars, group = pos_args$groups)
    plot <- ggplot(df_gyro, aes(x, y, col = group)) + 
              geom_line() + facet_grid(group ~ ., scales = "free_y")
    
    colors <- c("red", "green", "blue")
    if (length(pos_args$avgs) != 0) {
      for (i in 1:length(pos_args$avgs)) {
        plot <- plot + geom_hline(yintercept=pos_args$avgs[i], linetype="dashed", color=colors[i])
      }
    }
    
    return(plot)
    
  })
  
  output$magPlot <- renderPlot({
    
    if( length(input$mag_opts) == 0 ) {
      return( ggplot() )
    }
    
    df <- df()
    
    if( length(df) == 0 ) {
      return( ggplot() )
    }
    
    lengths <- length(df$mag_x) - 1
    pos_args <- update("mag")()
    
    df_mag <- data.frame(x = 0:lengths, y = pos_args$vars, group = pos_args$groups)
    plot <- ggplot(df_mag, aes(x, y, col = group)) + 
              geom_line() + facet_grid(group ~ ., scales = "free_y")
    
    colors <- c("red", "green", "blue")
    if (length(pos_args$avgs) != 0) {
      for (i in 1:length(pos_args$avgs)) {
        plot <- plot + geom_hline(yintercept=pos_args$avgs[i], linetype="dashed", color=colors[i])
      }
    }
    
    return(plot)
    
  })
  
}

shinyApp(ui = ui, server = server)
runApp("NeuroStride")

# shiny::reactlogShow()