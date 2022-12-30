#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Demo explorable: Beginner Data Storytelling Case Study"),
  p("Let's say you want to show your audience how the pandemic affected the number of women enrolled in higher education institutions in Colombia. What would be the best plot to explain your point?"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("chartType", "Select chart type", choices = c("Histogram", "Line plot")),
      conditionalPanel(
        condition = "input.chartType == 'Histogram'",
        selectInput("histVar1", "Select variable", choices = c("Women enrollment by university in 2020",
                                                               "Number of programs per university in 2020"))
      ),
      conditionalPanel(
        condition = "input.chartType == 'Line plot'",
        selectInput("lineVar1", "Select variable", choices = c("Total women enrolled", "Percentage women enrolled")),
        selectInput("lineVar2", "Select time variable", choices = c("Year", "Semester"))
      ),
      actionButton("button", "Create chart!")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- read_csv("../datasets/enrollmnent-colombia.csv")
  
  plot <- eventReactive(input$button, {
    if(input$chartType == "Histogram") {
      
      # If user selects histogram
      if (input$histVar1 == "Women enrollment by university in 2020") {
        return(
          # Histogram of Women enrollmentt by unversity
          data |> 
            filter(Year == 2020) |> 
            group_by(Id_Institution, Sex) |> 
            summarise(Enrolled = sum(Enrolled)) |>
            mutate(Total_Enrolled = sum(Enrolled),
                   Women_enrollment = Enrolled/Total_Enrolled) |>
            filter(Sex == 2) |> 
            select(Women_enrollment) |> 
            ggplot() +
            geom_histogram(aes(x = Women_enrollment)) +
            labs(
              x = "Women enrollment by university 2020"
            )
        )
      } else if (input$histVar1 == "Number of programs per university in 2020") {
        return(
          # Histogram of programs by universitry
          data |> 
            filter(Year == 2020) |> 
            group_by(Id_Institution) |> 
            distinct(Id_Program) |> 
            summarise(Number_programs = n()) |>
            ggplot() +
            geom_histogram(aes(x = Number_programs)) +
            labs(
              x = "Number of programs per university"
            )
        )
      }
    } else if (input$chartType == "Line plot") {
      
      # If user selects line plot
      if (input$lineVar1 == "Total women enrolled" & input$lineVar2 == "Year") {
        return(
          data |> 
            group_by(Year, Sex) |> 
            summarise(Enrolled = sum(Enrolled)) |> 
            mutate(Total_Enrolled = sum(Enrolled)) |> 
            filter(Sex == 2) |> 
            ggplot() +
            geom_line(aes(x = Year, y = Total_Enrolled)) +
            labs(
              x = "Year",
              y = "Total women enrolled"
            )
        )
      } else if (input$lineVar1 == "Total women enrolled" & input$lineVar2 == "Semester") {
        return(
          data |> 
            mutate(Year_semester = paste(Year, Semester, sep = "-")) |> 
            group_by(Year_semester, Sex) |> 
            summarise(Enrolled = sum(Enrolled)) |> 
            mutate(Total_Enrolled = sum(Enrolled)) |> 
            filter(Sex == 2) |> 
            bind_cols(x = c(1:12)) |> 
            ggplot() +
            geom_line(aes(x = x, y = Total_Enrolled)) +
            labs(
              x = "Semester",
              y = "Total women enrolled"
            ) +
            scale_x_continuous(
              breaks = c(1:12),
              labels = c("2015-1", "2015-2", "2016-1", "2016-2",
                         "2017-1", "2017-2", "2018-1", "2018-2",
                         "2019-1", "2019-2", "2020-1", "2020-2")
            ) +
            theme(
              axis.text.x = element_text(angle = 90, vjust = 0.5)
            )
        )
      } else if (input$lineVar1 == "Percentage women enrolled" & input$lineVar2 == "Year") {
        return(
          data |> 
            group_by(Year, Sex) |> 
            summarise(Enrolled = sum(Enrolled)) |> 
            mutate(Total_Enrolled = sum(Enrolled),
                   Women_enrollment = Enrolled/Total_Enrolled) |> 
            filter(Sex == 2) |> 
            ggplot() +
            geom_line(aes(x = Year, y = Women_enrollment)) +
            labs(
              x = "Year",
              y = "Percentage women enrolled"
            )
        )
      } else if (input$lineVar1 == "Percentage women enrolled" & input$lineVar2 == "Semester") {
        return(
          data |> 
            mutate(Year_semester = paste(Year, Semester, sep = "-")) |> 
            group_by(Year_semester, Sex) |> 
            summarise(Enrolled = sum(Enrolled)) |> 
            mutate(Total_Enrolled = sum(Enrolled),
                   Women_enrollment = Enrolled/Total_Enrolled) |> 
            filter(Sex == 2) |> 
            bind_cols(x = c(1:12)) |> 
            ggplot() +
            geom_line(aes(x = x, y = Women_enrollment)) +
            labs(
              x = "Semester",
              y = "Percentage women enrolled"
            ) +
            scale_x_continuous(
              breaks = c(1:12),
              labels = c("2015-1", "2015-2", "2016-1", "2016-2",
                         "2017-1", "2017-2", "2018-1", "2018-2",
                         "2019-1", "2019-2", "2020-1", "2020-2")
            ) +
            theme(
              axis.text.x = element_text(angle = 90, vjust = 0.5)
            )
        )
      }
    }
  })
  
  output$plot <- renderPlot({
    plot()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
