library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Dataset Filter and Plot"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("datasetSelect"), # Dropdown for selecting dataset
      uiOutput("filters") # UI for filters will be dynamically generated
    ),
    mainPanel(
      plotOutput("plot"), # The ID here is "plot"
      tableOutput("dataPreview") # Add this line for data preview
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamically list datasets in the folder
  output$datasetSelect <- renderUI({
    files <- list.files(path = "data/Survivors vs Twins comparison files_absolute/cleaned/", full.names = TRUE)
    selectInput("selectedDataset", "Choose a dataset", files)
  })
  
  # Generate UI for filters based on selected dataset
  output$filters <- renderUI({
    req(input$selectedDataset)
    df <- read.csv(input$selectedDataset, stringsAsFactors = FALSE)
    chrCols <- sapply(df, is.character)
    filterUI <- list()
    if (any(chrCols)) {
      for (col in names(df)[chrCols]) {
        filterUI[[col]] <- selectInput(inputId = paste0("filter_", col), 
                                       label = paste("Filter", col), 
                                       choices = unique(df[[col]]), 
                                       selected = unique(df[[col]])[1])
        # Add an option to facet if the number of unique values is 9 or less
        if (length(unique(df[[col]])) <= 9) {
          filterUI[[paste0("facet_", col)]] <- checkboxInput(inputId = paste0("facet_", col), 
                                                             label = paste("Facet by", col), 
                                                             value = FALSE)
        }
      }
    }
    do.call(tagList, filterUI)
  })
  
  # Reactive expression to filter data
  filteredData <- reactive({
    req(input$selectedDataset)
    df <- read_csv(input$selectedDataset)
    chrCols <- names(sapply(df, is.character))
    
    for (col in chrCols) {
      filterInput <- input[[paste0("filter_", col)]]
      if (!is.null(filterInput) && filterInput != "") {
        df <- df[df[[col]] == filterInput, ]
      }
    }
    df
  })
  
  output$dataPreview <- renderTable({
    df <- filteredData()
    if(nrow(df) > 0) {
      head(df) # Show the first few rows of the dataframe
    }
  })
  
  # Define chrCols as a reactive value
  chrCols <- reactive({
    req(input$selectedDataset)
    df <- read.csv(input$selectedDataset, stringsAsFactors = FALSE)
    names(sapply(df, is.character))
  })
  
  # Plot
  # Plot with optional faceting
  output$plot <- renderPlot({
    req(filteredData()) # Ensure the filtered data is available
    df <- filteredData() 
    if(nrow(df) == 0) {
      return(ggplot() + labs(title = "No data available with the current filters"))
    }
    
    p <- ggplot(df, aes(x = Property, y = Mean)) +
      geom_line() +
      geom_ribbon(aes(ymin = LowerBound, ymax = UpperBound), fill = "blue", alpha = 0.2) +
      theme_minimal()
    
    # Use chrCols() here
    for (col in chrCols()) {
      facetInput <- input[[paste0("facet_", col)]]
      if (!is.null(facetInput) && facetInput) {
        p <- p + facet_wrap(as.formula(paste0("~", col)))
      }
    }
    
    p
  })
  
}

shinyApp(ui = ui, server = server)
