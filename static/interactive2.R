library(tidyverse)
library(ggpubr)
library(modelr)
library(data.table)
library(formattable)
library(sf)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(htmltools)
library(shiny)

get(load("processed_data.RData"))

minority <- processed_data %>% filter(race4 == "All Other Races")

set.seed(1)
processed_data <- processed_data %>% drop_na()
processed_data_unShuffled <- processed_data
whole <- processed_data_unShuffled[sample(nrow(processed_data_unShuffled)),]

whole1 <- whole[1:91509,]
whole2 <- whole[91510:183018,]
whole3 <- whole[183019:274527,]

whole_Train <- rbind(whole1,whole3)
whole_Test <- whole2

attach(whole_Train)
train_model_new <- glm(serious_consideration_suicide ~ sex + weapon_carrying + safety_concerns_at_school + Threatened_at_school + total_num_methamphetamines + first_time_drinking + total_num_get_high + has_got_drug_at_school, family = binomial)
detach(whole_Train)



ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput(inputId = "year",
                            label = "Select Year",  min = 1999, max = 2019,
                            value = 1999, step = 2,animate = animationOptions(interval = 2000, loop = TRUE)),
                checkboxInput("legend", "Show legend", TRUE),
                selectInput("colors", "Color Scheme", rownames(color[c("YlOrRd","YlGn","RdPu","Oranges"),]))
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    interactive1[interactive1$year == input$year,]
  })
  
  filtershp <- reactive({subset(states, is.element(states$NAME,filteredData()$state))})
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
   leaflet()  %>% setView(-96,37.8,4)  %>% addProviderTiles(providers$Stamen.TonerLite) 
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    
    bins <- c(0.0,0.15,0.3,0.45,0.6,0.75)
    pal <- colorBin(input$colors, domain = interactive1$prob_suicide_cont_state,bins = bins)
    
    labels <- paste("<h5>", filtershp()$NAME,"</h5>", 
                    "<h6>", "predicted percent of people considered suicide is: ", round(filteredData()$prob_suicide_cont_state,digit =3), "</h6>",
                    sep ="")
    
    leafletProxy("map", data = filtershp()) %>% 
      clearShapes() %>% 
      addPolygons(
        weight = 1,
        smoothFactor = 0.5,
        color = "white",
        fillOpacity = 0.5, 
        fillColor = pal(filteredData()$prob_suicide_cont_state),
        highlight = highlightOptions(weight = 5),
        label = lapply(labels, HTML),
      )
  })
  
  #Create a legend
  observe({
    
    proxy <- leafletProxy("map", data = interactive1)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      bins <- c(0.0,0.1,0.2,0.3,0.4,0.5)
      pal <- colorBin(input$colors, domain = interactive1$prob_suicide_cont_state,bins = bins)
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~interactive1$prob_suicide_cont_state, title = "Predicted Percentage of people with suicide consideration "
      )
    }
  })
  
}

shinyApp(ui, server)

