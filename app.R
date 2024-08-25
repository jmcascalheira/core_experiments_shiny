library(shiny)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Lithic Core Platform Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("Use the tabs to work through each step:"),
      p("1. Set the number of platforms."),
      p("2. Generate a random platform to strike."),
      p("3. Record the presence/absence of platforms after striking."),
      hr(),
      downloadButton("download_data", "Download Data as CSV", style = "font-size: 18px; padding: 10px 20px;")
      , width = 3),  # Adjust the width here to make it narrower (default is 4)
    
    mainPanel(
      tabsetPanel(id = "inTabset",
                  tabPanel("Platform Setup", 
                           h4("Set the number of platforms available:"),
                           actionButton("minus_btn", "-", style = "font-size: 24px; padding: 10px 20px;"),
                           actionButton("plus_btn", "+", style = "font-size: 24px; padding: 10px 20px;"),
                           br(), br(),
                           uiOutput("platform_count_ui"),
                           br(), br(),
                           div(
                             actionButton("go_to_random_btn", "Go to Random Platform", style = "font-size: 18px; padding: 10px 20px;"),
                             style = "position: absolute; right: 10px; bottom: 10px;"
                           )
                  ),
                  tabPanel("Random Platform", 
                           h4("Generate a Random Platform:"),
                           actionButton("generate_btn", "Generate Random Platform", style = "font-size: 24px; padding: 10px 20px;"),
                           br(), br(),
                           uiOutput("random_platform_square"),
                           br(), br(),
                           div(
                             actionButton("go_to_record_btn", "Go to Record Presence/Absence", style = "font-size: 18px; padding: 10px 20px;"),
                             style = "position: absolute; right: 10px; bottom: 10px;"
                           )
                  ),
                  tabPanel("Record Presence/Absence", 
                           h4("Record the presence/absence of platforms:"),
                           uiOutput("platform_toggle_squares"),
                           actionButton("record_changes_btn", "Record Changes", style = "font-size: 24px; padding: 10px 20px;"),
                           tableOutput("results_table"),
                           br(), br(),
                           div(
                             actionButton("go_to_setup_btn", "Go to Platform Setup", style = "font-size: 18px; padding: 10px 20px;"),
                             style = "position: absolute; right: 10px; bottom: 10px;"
                           )
                  )
      )
      , width = 9)  # Adjust the main panel width to compensate (default is 8)
  )
)


server <- function(input, output, session) {
  
  # Reactive value to store the current platforms (including their status)
  platforms <- reactiveVal(data.frame(Number = 1, Status = TRUE))
  
  # Reactive value to store the selected random platform
  random_platform <- reactiveVal(NULL)
  
  # Reactive value to store the platform presence/absence data as an R object
  platform_data <- reactiveVal(data.frame())
  
  # Reactive value to store the strike count
  strike_count <- reactiveVal(0)
  
  # Increase the number of platforms
  observeEvent(input$plus_btn, {
    current_platforms <- platforms()
    max_number <- ifelse(nrow(current_platforms) == 0, 0, max(current_platforms$Number))
    new_platform <- data.frame(Number = max_number + 1, Status = TRUE)
    platforms(rbind(current_platforms, new_platform))
  })
  
  # Decrease the number of platforms
  observeEvent(input$minus_btn, {
    current_platforms <- platforms()
    if (nrow(current_platforms) > 1) {
      platforms(current_platforms[-nrow(current_platforms),])
    }
  })
  
  # Display the current platforms as large squares in Tab 1
  output$platform_count_ui <- renderUI({
    available_platforms <- platforms() %>% filter(Status == TRUE)
    platform_numbers <- available_platforms$Number
    
    if (length(platform_numbers) > 0) {
      div(
        lapply(platform_numbers, function(num) {
          div(
            style = "display: inline-block; width: 100px; height: 100px; margin: 5px; background-color: #D3D3D3; text-align: center; line-height: 100px; border-radius: 10px; font-size: 36px;",
            num
          )
        })
      )
    } else {
      h4("No platforms available.")
    }
  })
  
  # Clear the random platform when navigating to Tab 2
  observeEvent(input$go_to_random_btn, {
    random_platform(NULL)
    updateTabsetPanel(session, "inTabset", selected = "Random Platform")
  })
  
  # Display the randomly selected platform as a big shaded square in Tab 2
  output$random_platform_square <- renderUI({
    if (!is.null(random_platform())) {
      div(
        style = "display: inline-block; width: 100px; height: 100px; margin: 5px; background-color: #D3D3D3; text-align: center; line-height: 100px; border-radius: 10px; font-size: 36px;",
        random_platform()
      )
    } else {
      h4("No platform selected.")
    }
  })
  
  # Generate a random platform number based on available platforms
  observeEvent(input$generate_btn, {
    available_platforms <- platforms() %>% filter(Status == TRUE)
    if (nrow(available_platforms) > 0) {
      random_platform(sample(available_platforms$Number, 1))
    } else {
      random_platform(NULL)
    }
  })
  
  # Generate UI elements for toggling presence/absence with large squares in Tab 3
  output$platform_toggle_squares <- renderUI({
    current_platforms <- platforms()
    platform_numbers <- current_platforms$Number
    
    if (length(platform_numbers) > 0) {
      div(
        lapply(1:nrow(current_platforms), function(i) {
          num <- current_platforms$Number[i]
          status <- current_platforms$Status[i]
          square_style <- if (status) {
            "display: inline-block; width: 100px; height: 100px; margin: 5px; background-color: #D3D3D3; text-align: center; line-height: 100px; border-radius: 10px; font-size: 36px; cursor: pointer;"
          } else {
            "display: inline-block; width: 100px; height: 100px; margin: 5px; background-color: #FFFFFF; text-align: center; line-height: 100px; border-radius: 10px; font-size: 36px; border: 2px solid #D3D3D3; cursor: pointer;"
          }
          
          div(id = paste0("platform_toggle_", num), 
              style = square_style,
              num,
              onclick = paste0("Shiny.setInputValue('toggle_platform_", num, "', Math.random())")
          )
        })
      )
    } else {
      h4("No platforms available.")
    }
  })
  
  # Toggle platform presence/absence on click in Tab 3
  observe({
    current_platforms <- platforms()
    lapply(1:nrow(current_platforms), function(i) {
      num <- current_platforms$Number[i]
      observeEvent(input[[paste0("toggle_platform_", num)]], {
        current_status <- current_platforms$Status[current_platforms$Number == num]
        current_platforms$Status[current_platforms$Number == num] <- !current_status
        platforms(current_platforms)
      }, ignoreInit = TRUE)
    })
  })
  
  # Record the changes after toggling presence/absence
  observeEvent(input$record_changes_btn, {
    current_platforms <- platforms()
    platforms(current_platforms)  # Update the reactive value with the new statuses
    
    # Update the platform data stored in session
    new_data <- as.data.frame(t(current_platforms$Status))
    colnames(new_data) <- paste0("Platform_", current_platforms$Number)
    
    existing_data <- platform_data()
    
    if (nrow(existing_data) == 0) {
      platform_data(new_data)
    } else {
      all_columns <- union(colnames(existing_data), colnames(new_data))
      
      for (col in setdiff(all_columns, colnames(existing_data))) {
        existing_data[[col]] <- NA
      }
      
      for (col in setdiff(all_columns, colnames(new_data))) {
        new_data[[col]] <- NA
      }
      
      existing_data <- existing_data[, all_columns, drop = FALSE]
      new_data <- new_data[, all_columns, drop = FALSE]
      
      platform_data(rbind(existing_data, new_data))
    }
    
    # Update the strike count
    strike_count(strike_count() + 1)
    
    updateTabsetPanel(session, "inTabset", selected = "Record Presence/Absence")
  })
  
  # Display the results table
  output$results_table <- renderTable({
    platform_data()
  })
  
  # Provide a button to download the data as a CSV file with a sequential strike number
  output$download_data <- downloadHandler(
    filename = function() {
      paste("platform_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- platform_data()
      if (nrow(data) > 0) {
        data <- cbind(Strike = 1:nrow(data), data)
      }
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Navigate to the "Random Platform" tab
  observeEvent(input$go_to_random_btn, {
    random_platform(NULL)  # Clear the previous random platform
    updateTabsetPanel(session, "inTabset", selected = "Random Platform")
  })
  
  # Navigate to the "Record Presence/Absence" tab
  observeEvent(input$go_to_record_btn, {
    updateTabsetPanel(session, "inTabset", selected = "Record Presence/Absence")
  })
  
  # Navigate back to the "Platform Setup" tab
  observeEvent(input$go_to_setup_btn, {
    updateTabsetPanel(session, "inTabset", selected = "Platform Setup")
  })
}

shinyApp(ui = ui, server = server)