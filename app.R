library(shiny)
library(dplyr)

ui <- fluidPage(
  titlePanel("Randomize Flaking Experiments"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("Use the tabs to work through each step:"),
      p("1. Enter experiment details."),
      p("2. Set the number of platforms."),
      p("3. Generate a random platform to strike."),
      p("4. Record the presence/absence of platforms after striking."),
      hr(),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(id = "inTabset",
                  tabPanel("Experiment Details", 
                           h4("Enter Experiment Details:"),
                           textInput("experiment_number", "Experiment Number:"),
                           dateInput("date", "Date:", value = Sys.Date()),
                           textInput("knapper", "Knapper:"),
                           textInput("material", "Material:"),
                           selectInput("stone_quality", "Stone Quality:", choices = c("Excellent", "Good", "Fair", "Poor")),
                           textInput("cobble_type", "Cobble Type:"),
                           textInput("site_collected", "Site Collected:"),
                           numericInput("length", "Length (mm):", value = NULL, min = 0),
                           numericInput("width", "Width (mm):", value = NULL, min = 0),
                           numericInput("thickness", "Thickness (mm):", value = NULL, min = 0),
                           numericInput("weight", "Weight (g):", value = NULL, min = 0),
                           textAreaInput("notes", "Notes:", ""),
                           br(),
                           downloadButton("download_experiment", label = "Download Experiment and Start", style = "font-size: 18px; padding: 10px 20px;")
                  ),
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
                           selectInput("platform_quality", "Platform Quality", choices = c("", 1:5), selected = ""),  # Platform Quality with empty default
                           selectInput("platform_location", "Platform Location",
                                       choices = c("", 
                                                   "Position 1 (proximal end to 30% of the scar's length)" = "Position 1",
                                                   "Position 2 (30% to 80% of the scar's length)" = "Position 2",
                                                   "Position 3 (distal end, beyond 80% of the scar's length)" = "Position 3",
                                                   "Position 4 (on cortical or specific surfaces)" = "Position 4",
                                                   "Position 5 ('flawline cortex' surface)" = "Position 5",
                                                   "Position 6 (small core scar remnants)" = "Position 6"
                                       ), selected = ""),  # Platform Location with empty default
                           selectInput("platform_type", "Platform Type", choices = c("", "Cortical", "Plain", "Dihedral"), selected = ""),  # Platform Type with empty default
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
                             actionButton("go_to_setup_btn", "Start New Strike", style = "font-size: 18px; padding: 10px 20px;"),  # Updated button label
                             style = "position: absolute; right: 10px; bottom: 10px;"
                           )
                  )
      )
      , width = 9)  # Main panel wider to accommodate more content
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store the current platforms, experiment details, and random platform
  platforms <- reactiveVal(data.frame(Number = 1, Status = TRUE))
  random_platform <- reactiveVal(NULL)
  platform_data <- reactiveVal(data.frame())
  strike_count <- reactiveVal(0)
  max_platform_number <- reactiveVal(1)  # Track the maximum platform number used
  
  # Download handler for the experiment details CSV
  output$download_experiment <- downloadHandler(
    filename = function() {
      paste0("experiment_details_", Sys.Date(), ".csv")
    },
    content = function(file) {
      experiment_info <- data.frame(
        Experiment_Number = input$experiment_number,
        Date = input$date,
        Knapper = input$knapper,
        Material = input$material,
        Stone_Quality = input$stone_quality,
        Cobble_Type = input$cobble_type,
        Site_Collected = input$site_collected,
        Length_mm = input$length,
        Width_mm = input$width,
        Thickness_mm = input$thickness,
        Weight_g = input$weight,
        Notes = input$notes,
        stringsAsFactors = FALSE
      )
      write.csv(experiment_info, file, row.names = FALSE)
      
      # After downloading, move to Platform Setup tab
      updateTabsetPanel(session, "inTabset", selected = "Platform Setup")
    }
  )
  
  # Increase the number of platforms
  observeEvent(input$plus_btn, {
    current_platforms <- platforms()
    new_number <- max_platform_number() + 1  # Use the next available platform number
    new_platform <- data.frame(Number = new_number, Status = TRUE)
    platforms(rbind(current_platforms, new_platform))
    max_platform_number(new_number)  # Update the max platform number used
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
  
  # Clear the random platform when navigating to Tab 2 and reset dropdowns
  observeEvent(input$go_to_random_btn, {
    random_platform(NULL)
    
    # Reset the dropdowns to empty when going to the Random Platform tab
    updateSelectInput(session, "platform_quality", selected = "")
    updateSelectInput(session, "platform_location", selected = "")
    updateSelectInput(session, "platform_type", selected = "")
    
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
      selected_platform <- sample(available_platforms$Number, 1)
      random_platform(selected_platform)
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
    
    # Collect platform statuses in a named vector
    platform_statuses <- as.list(setNames(current_platforms$Status, paste0("Platform_", current_platforms$Number)))
    
    # Add the new data row with experiment number, strike number, randomly selected platform, and platform statuses
    new_data <- data.frame(
      Experiment = input$experiment_number,
      Strike = strike_count() + 1,
      RandomPlatform = random_platform(),
      Platform_Quality = input$platform_quality,  # Include Platform Quality in data
      Platform_Location = input$platform_location,  # Include Platform Location in data
      Platform_Type = input$platform_type,  # Include Platform Type in data
      platform_statuses,
      stringsAsFactors = FALSE
    )
    
    # Ensure the column structure matches by including all potential platform columns
    existing_data <- platform_data()
    all_columns <- union(names(existing_data), names(new_data))
    
    # Ensure both existing and new data have all the columns
    if (nrow(existing_data) > 0) {
      for (col in setdiff(all_columns, names(existing_data))) {
        existing_data[[col]] <- NA
      }
    }
    for (col in setdiff(all_columns, names(new_data))) {
      new_data[[col]] <- NA
    }
    
    # Remove platforms marked as absent from further consideration
    current_platforms <- current_platforms %>% filter(Status == TRUE)
    platforms(current_platforms)  # Update the reactive value with the filtered statuses
    
    # Bind the new data to the existing data
    if (nrow(existing_data) > 0) {
      platform_data(rbind(existing_data[, all_columns], new_data[, all_columns]))
    } else {
      platform_data(new_data[, all_columns])
    }
    
    # Update the strike count
    strike_count(strike_count() + 1)
    
    updateTabsetPanel(session, "inTabset", selected = "Record Presence/Absence")
  })
  
  # Display the results table with all relevant data
  output$results_table <- renderTable({
    platform_data()
  })
  
  # Provide a button to download the data as a CSV file with only Experiment and Random Platform
  output$download_data <- downloadHandler(
    filename = function() {
      paste("platform_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- platform_data()
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
  
  # Navigate back to the "Platform Setup" tab (renamed button)
  observeEvent(input$go_to_setup_btn, {
    updateTabsetPanel(session, "inTabset", selected = "Platform Setup")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
