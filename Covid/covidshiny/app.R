Sys.setlocale("LC_ALL", "German")
library(shiny)
library(dplyr)
library(ggplot2)

ui <- navbarPage("My Shiny App",
                 
                 tabPanel("Relative Häufigkeiten nach Bezirk und Geschlecht", 
                          
                          selectInput("gender_p1", "Geschlecht:", choices = c("Alle", unique(data$Geschlecht))),
                          plotOutput("bar_chart", height = 600, width = 1300),
                          dataTableOutput("table")
                 ),
                 
                 tabPanel("Fälle nach Bezirk", 
                          selectInput("gender_p2", "Geschlecht:", choices = c("Alle", unique(data$Geschlecht))),
                          selectInput("age_group", "Alter:", choices = c("Alle", unique(data$Altersgruppe))),
                          plotOutput("bar_chart_age", height = 600, width = 1300)
                 ),
                 
                 tabPanel("Dashboard",
                          fluidRow(
                            column(width = 4,
                                   wellPanel(
                                     icon("virus", lib = "font-awesome"),
                                     "Gesamt Fälle: ",
                                     verbatimTextOutput("total_cases")
                                   )
                            ),
                            column(width = 4,
                                   wellPanel(
                                     icon("smile", lib = "font-awesome"),
                                     "Gesamt Genesen: ",
                                     verbatimTextOutput("total_recoveries")
                                   )
                            ),
                            column(width = 4,
                                   wellPanel(
                                     icon("skull-crossbones", lib = "font-awesome"),
                                     "Gesamt Tote: ",
                                     verbatimTextOutput("total_deaths")
                                   )
                            )
                          )
                 )
                 
)
server <- function(input, output, session) {
  
  #Output for page 1
  filtered_data <- reactive({
    data %>% 
      filter(Geschlecht == input$gender_p1 | input$gender_p1 == "Alle") %>% 
      group_by(Landkreis) %>% 
      summarise(count = n())
  })
  
  output$bar_chart <- renderPlot({
    ggplot(data = filtered_data(), aes(x = Landkreis, y = count, fill = Landkreis)) + 
      geom_bar(stat = "identity")+
      geom_text(aes(label = count), vjust = -0.3, color = "black", size = 5)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      ggtitle("Fälle nach Bezirk")
  })
  
  relative_data <- reactive({
    if(input$gender_p1 == "Alle"){
      filtered_data() %>%
        left_join(einwohner, by = "Landkreis") %>%
        mutate(relative_häufigkeit = count / Gesamt)
    } else if(input$gender_p1 == "M") {
      filtered_data() %>%
        left_join(einwohner, by = "Landkreis") %>%
        mutate(relative_häufigkeit  = count / Männlich)
    } else {
      filtered_data() %>%
        left_join(einwohner, by = "Landkreis") %>%
        mutate(relative_häufigkeit = count / Weiblich)
    }
  })
  output$table <- renderDataTable({
    relative_data()
  })
  
  # Output for page 2
  filtered_data_age <- reactive({
    data %>% 
      filter(
        (Altersgruppe == input$age_group | input$age_group == "Alle") & 
          (Geschlecht == input$gender_p2 | input$gender_p2 == "Alle")
      ) %>% 
      group_by(Landkreis) %>% 
      summarise(count = n())
  })
  output$bar_chart_age <- renderPlot({
    ggplot(data = filtered_data_age(), aes(x = Landkreis, y = count, fill = Landkreis)) + 
      geom_bar(stat = "identity")+
      geom_text(aes(label = count), vjust = -0.3, color = "black", size = 5)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      ggtitle("Fälle nach Bezirk und Alter")
  })

# Output for Page 3
  # Output for total cases
  output$total_cases <- renderText({
    sum(data$AnzahlFall)
  })
  
  # Output for total recoveries
  output$total_recoveries <- renderText({
    sum(data$AnzahlGenesen)
  })
  
  # Output for total deaths
  output$total_deaths <- renderText({
    sum(data$AnzahlTodesfall)
  })
  }
shinyApp(ui,server)
  
