#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(shiny.maxRequestSize = 30*1024^2)
library(shiny)
library(shinythemes)
library(dplyr) # Package for data manipulations
library(magrittr) # Package for pipe operator
library(ggplot2) # Package for creating graphs

course_data <- europe %>% # Load the course data
  mutate(AvgTemperatureC = round((AvgTemperatureF - 32)*5/9, 1)) # Create a new column with Avg Temperature in Celsius

country_df <- course_data %>%
  filter(Year >= 1900) %>% # Subset the rows to keep data more than or equal to a year
  filter(Country == "Switzerland") # Subset the rows to keep a specific country

city_df <- country_df %>% 
  filter(City == "Zurich") %>% # Subset the rows for specific City
  filter(Year == 2005) # Subset the rows for specific Year

year_df <- country_df %>% 
  filter(City == "Zurich") %>% # Subset the rows for specific City
  filter(Year == 2005) %>%  # Subset the rows for specific Year
  group_by(Country, City, Year, Month) %>% 
  summarise(MinTempF = min(AvgTemperatureF),
            MeanTempF = round(mean(AvgTemperatureF), 1),
            MaxTempF = max(AvgTemperatureF),
            MinTempC = min(AvgTemperatureC),
            MeanTempC = round(mean(AvgTemperatureC), 1),
            MaxTempC = max(AvgTemperatureC)) %>% 
  ungroup()

# Define UI for application
ui <- fluidPage(
  # Specify the selected theme to the 'theme' argument
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("COURSE SHINY APP"),
  
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      "This is the sidebar panel",
      
      # Input: A simple slider ----
      sliderInput(inputId = "year", label = "Year",
                  min = 2000,
                  max = 2019,
                  step = 1, 
                  value = 2000,
                  sep = ''),
      
      # Input: A simple drop down list  ----
      selectInput(inputId = "country", label = "Country:",
                  choices = NULL),
      
      # Input: A simple drop down list  ----
      selectInput(inputId = "city", label = "City:",
                  choices = NULL),
      
      # Input: A simple text input  ----
      textInput(inputId = "text_input", label = "Input text here:"),
      
      # Input: A simple radio button input  ----
      radioButtons(inputId = "temp_scale", label = "Temperature scale:",
                   choices = list("Fahrenheit" = "fahrenheit",
                                  "Celsius" = "celsius"),
                   selected = "fahrenheit"),
      
      actionButton(inputId = "button",label = "Find"),
      
      br(),br(),
      
      fileInput(inputId = "file", label = "Upload RDS file",
                multiple = FALSE,
                accept = c(".rds")),
      
      downloadButton(outputId = "download", label = "Download")
      
    ),
    
    # Main panel
    mainPanel(
      "This is the main panel",
      
      textOutput(outputId = "text_output"),
      
      # Layout: Tabset with info, data, and plots tabs ----
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Info",
                           h3("App description"),
                           p("This is the course shiny app.", br(),
                             "It is created during the course exercises using the europe.rds data:", br(),
                             strong("Average daily temperatures"), "(in Fahrenheit) from cities around
                                           Europe from 2000 to 2019"),
                           hr(),
                           verbatimTextOutput("data_summary")
                  ),
                  tabPanel(title = "Data",
                           
                           dataTableOutput("data_table")
                  ),
                  tabPanel(title = "Plots",
                           fluidRow(
                             column(width = 12, plotOutput("lineplot"))
                           ),
                           fluidRow(
                             column(width = 6, plotOutput("boxplot")),
                             column(width = 6, plotOutput("lineplotTemp"))
                           )
                  )
      )
    )
  )
)

# Define server side logic
server <- function(input, output, session) {
  country_df <- reactive({
    course_data %>%
      filter(Year >= input$year) %>% # Subset the rows to keep data more than or equal to a year
      filter(Country == input$country) # Subset the rows to keep a specific country
  })
  
  city_df <- eventReactive(input$button, {
    country_df() %>% 
      filter(City == input$city) %>% # Subset the rows for specific City
      filter(Year == input$year) # Subset the rows for specific Year
  })
  
  year_df <- reactive({
    country_df() %>% 
      filter(City == input$city) %>% # Subset the rows for specific City
      filter(Year == input$year) %>%  # Subset the rows for specific Year
      group_by(Country, City, Year, Month) %>% 
      summarise(MinTempF = min(AvgTemperatureF),
                MeanTempF = round(mean(AvgTemperatureF), 1),
                MaxTempF = max(AvgTemperatureF),
                MinTempC = min(AvgTemperatureC),
                MeanTempC = round(mean(AvgTemperatureC), 1),
                MaxTempC = max(AvgTemperatureC)) %>% 
      ungroup()
    
  })
  
  # Output: Render a text output  ----
  output$text_output <- renderText({
    paste("Your inputs are:", input$year, input$country, input$city, input$text_input, input$temp_scale)
  })
  
  # Output: Render a print output  ----
  output$data_summary <- renderPrint({
    summary(course_data)
  })
  
  # Output: Render a (dynamic) table output  ----
  output$data_table <- renderDataTable({
    city_df()
  })
  
  # Output: Render a plot output  ----
  output$lineplot <- renderPlot({
    ggplot(data = city_df()) +
      geom_line(mapping = aes(x = Date, y = AvgTemperatureF), size = 1) +
      ylab("Average daily temperatures (in Fahrenheit)")
  })
  
  # Output: Render a plot output  ----
  output$boxplot <- renderPlot({
    ggplot(data = country_df()) +
      geom_boxplot(mapping = aes(x = Month, y = AvgTemperatureF, group = Year))
  })
  
  # Output: Render a plot output  ----
  output$lineplotTemp <- renderPlot({
    
    if(input$temp_scale == "fahrenheit"){
      res <- ggplot(data = year_df()) +
        geom_line(mapping = aes(x = Month, y = MinTempF), size = 1, colour = "red", linetype = "dotted") +
        geom_line(mapping = aes(x = Month, y = MeanTempF), size = 1, colour = "black") +
        geom_line(mapping = aes(x = Month, y = MaxTempF), size = 1, colour = "red", linetype = "dotted") +
        scale_x_discrete(name = "", limits = month.abb) +
        ylab("Average daily temperatures (in Fahrenheit)")
    }
    
    if(input$temp_scale == "celsius"){
      res <- ggplot(data = year_df()) +
        geom_line(mapping = aes(x = Month, y = MinTempC), size = 1, colour = "red", linetype = "dotted") +
        geom_line(mapping = aes(x = Month, y = MeanTempC), size = 1, colour = "black") +
        geom_line(mapping = aes(x = Month, y = MaxTempC), size = 1, colour = "red", linetype = "dotted") +
        scale_x_discrete(name = "", limits = month.abb) +
        ylab("Average daily temperatures (in Celsius)")
    }
    
    return(res)
    
  })
  
  observe({
    
    new_choices <- unique(course_data$City[course_data$Country == input$country])
    
    updateSelectInput(session, inputId = "city", choices = new_choices)
    
  })
  output$download <- downloadHandler(
    filename = "city_data.csv", content = function(file) {
      write.csv(city_df(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
