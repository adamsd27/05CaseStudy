#libraries that we need
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

#if injuries does not exist, then read in these files (check to see if data exist in the environment)
if (!exists("injuries")) {
  injuries <- vroom::vroom("neiss/injuries.tsv.gz")
  products <- vroom::vroom("neiss/products.tsv")
  population <- vroom::vroom("neiss/population.tsv")
}

#code to get products for filtering
prod_codes <- setNames(products$prod_code, products$title)

#useful factor lumping function
#lumps together everything after first 5 injuries will clean up injuries table. 
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}
#start the UI
ui <- fluidPage(
  fluidRow(
      #user input select product
      column(8,
             selectInput("code", "Product",
                         choices = setNames(products$prod_code, products$title),
                         width = "100%"
             )
      ),
      #user input to select variable to plot, either rate or count
      column(2, selectInput("y", "Y axis", c("rate", "count")))
    ),
  fluidRow(
    #output from the server as tables
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    #output from server as plot
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
)


# server
server <- function(input, output, session) {
  #make reactive to select a product code
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  #output table for diagnosis
  output$diag <- renderTable(count_top(selected(), diag), width = "100%")
  
  #output table for body part
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  
  #output table for location
  output$location <- renderTable(count_top(selected(), location), width = "100%")
  
  #another reactive, for getting rate of injury per 10k people and raw number, have n being calculated, and then also rate (but not used originally)
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  #output a plot
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  
  #tell a story based on action button
  narrative_sample <- eventReactive( #event will react, grab 1 random story out
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1) 
  )
  output$narrative <- renderText(narrative_sample())#will output that to narrative
}


shinyApp(ui, server)