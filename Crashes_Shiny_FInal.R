library(shiny)
library(shinythemes)

result <- function(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) {
  tree <- predict(object = model_nb, rbind(trainSet[,c(1:13)], 
                                             data.frame(state_name = v1, month_of_crash = v2, day_of_week = v3, 
                                                        hour_of_crash = v4,
                                                        land_use_name = v5, functional_system_name =  v6, 
                                                        route_signing_name = v7, 
                                                        relation_to_junction_specific_location_name = v8, 
                                                        type_of_intersection = v9,
                                                        relation_to_trafficway_name = v10, light_condition_name = v11, 
                                                        atmospheric_conditions_name = v12, 
                                                        national_highway_system = v13))[nrow(trainSet)+1,])
  
  if (tree == "class1") {
    print("We predict that the EMT will reach in under 2 minutes.")
  } else if (tree == "class2") {
    print("We predict that the EMT will reach between 2 - 5 minutes.")
  } else if (tree == "class3") {
    print("We predict that the EMT will reach between 5 - 10 minutes.")
  } else if (tree == "class4") {
    print("We predict that the EMT will reach between 10 - 15 minutes.")
  } else if (tree == "class5") {
    print("We predict that the EMT will reach between 15 - 30 minutes.")
  } else if (tree == "class6") {
    print("We predict that the EMT will reach between 30 - 60 minutes.")
  } else if (tree == "class7") {
    print("We predict that the EMT will reach in more than 1 hour.")
  }
}
  
ui <- 
  fluidPage(theme = shinytheme("darkly"),
            pageWithSidebar(
  headerPanel('Predicting EMT Arrival Time'),
  sidebarPanel(
    selectInput('a', 'State', sort(unique(testSet$state_name)),
                selected = "Iowa"),
    
    selectInput('b', 'Month', sort(unique(testSet$month_of_crash)),
                selected = 5),
    
    selectInput('c', 'Day of Week', sort(unique(testSet$day_of_week)),
                selected = 2),
    
    selectInput('d', 'Hour', sort(unique(testSet$hour_of_crash)),
                selected = 16),
    
    selectInput('e', 'Area Type', sort(unique(testSet$land_use_name)),
                selected = "Rural"),
    
    selectInput('f', 'Road Type', sort(unique(testSet$functional_system_name)),
                selected = "Major Collector"),
    
    selectInput('g', 'Route Sign', sort(unique(testSet$route_signing_name)),
                selected = "County Road"),
    
    selectInput('h', 'Road Location Type', sort(unique(testSet$relation_to_junction_specific_location_name)),
                selected = "Intersection Related"),
    
    selectInput('i', 'Intersection Type', sort(unique(testSet$type_of_intersection)),
                selected = "Four-Way Intersection"),
    
    selectInput('j', 'Relation to Trafficway', sort(unique(testSet$relation_to_trafficway_name)),
                selected = "On Roadside"),
    
    selectInput('k', 'Light Condition', sort(unique(testSet$light_condition_name)),
                selected = "Daylight"),
    
    selectInput('l', 'Atmospheric Condition', sort(unique(testSet$atmospheric_conditions_name)),
                selected = "Rain"),
    
    selectInput('m', 'National Highway System?', unique(testSet$national_highway_system),
                selected = 0)),
  mainPanel(
    strong(span(textOutput("selected_var"), style = "color:lightblue; font-family: 'Arial'; font-si35pt"))
    )
)

)


server <- function(input, output, session) {
  
  output$selected_var <- renderText({
  
    result(input$a, as.numeric(input$b), as.numeric(input$c), as.numeric(input$d), input$e, input$f, input$g, input$h, 
           input$i, input$j, input$k, input$l, as.numeric(input$m))
  })
}

shinyApp(ui = ui, server = server)


