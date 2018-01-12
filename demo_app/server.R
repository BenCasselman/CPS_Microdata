library(shiny)
library(tidyverse)
library(scales)
library(zoo)
source("recession_shading.R")

load("shiny_data.RData")

shinyServer(function(input,output,session) {
  w <- reactive({
    
    ed_choice <- input$ed
    marital_choice <- input$marital
    race_choice <- input$race
    sex_choice <- input$sex
    age_choice <- input$age
    if (is.null(ed_choice)) ed_choice <- c("Less than high school", "HS diploma or GED", "Some college or associate", "BA or higher")
    if (is.null(marital_choice)) marital_choice <- c("Never married", "Married", "Previously married")
    if (is.null(race_choice)) race_choice <- c("White", "Black", "Hispanic")
    if (is.null(sex_choice)) sex_choice <- c("Men", "Women")
    if (is.null(age_choice)) age_choice <- c("Ages 16-17", "Ages 18-24", "Ages 25-34", "Ages 35-44", "Ages 45-54", "Ages 55-64", "Ages 65-plus")
    
    if (input$facet_row != "." & input$facet_col != ".") {
      
      w <- shiny_data %>%
        filter(ed %in% ed_choice,
               marital %in% marital_choice,
               race %in% race_choice,
               sex %in% sex_choice,
               age %in% age_choice,
               year(date) >= input$dates) %>% 
        rename_(facet_row = input$facet_row,
                main = input$main,
                facet_col = input$facet_col,
                numerator = input$measure)
      
      if(input$measure == "unemp") w <- w %>% select(-pop) %>%  rename(pop = lf)
      
      w <- w %>%
        group_by(date, main, facet_row, facet_col) %>%
        summarize(epop = sum(numerator)/sum(pop)) %>%
        arrange(date) %>%
        group_by(main, facet_row, facet_col) %>%
        mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>%
        filter(!is.na(roll))
      
    } else
      if (input$facet_row != "." & input$facet_col == ".") {
        
        w <- shiny_data %>%
          filter(ed %in% ed_choice,
                 marital %in% marital_choice,
                 race %in% race_choice,
                 sex %in% sex_choice,
                 age %in% age_choice,
                 year(date) >= input$dates) %>% 
          rename_(facet_row = input$facet_row,
                  main = input$main,
                  numerator = input$measure)
        
        if(input$measure == "unemp") w <- w %>% select(-pop) %>%  rename(pop = lf)
        
        w <- w %>%
          group_by(date, main, facet_row) %>%
          summarize(epop = sum(numerator)/sum(pop)) %>%
          arrange(date) %>%
          group_by(main, facet_row) %>%
          mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>%
          filter(!is.na(roll))
        
      } else {
        
        w <- shiny_data %>%
          filter(ed %in% ed_choice,
                 marital %in% marital_choice,
                 race %in% race_choice,
                 sex %in% sex_choice,
                 age %in% age_choice,
                 year(date) >= input$dates)%>% 
          rename_(main = input$main,
                  numerator = input$measure)
        
        if(input$measure == "unemp") w <- w %>% select(-pop) %>%  rename(pop = lf)
        
        w <- w %>%
          group_by(date, main) %>%
          summarize(epop = sum(numerator)/sum(pop)) %>%
          arrange(date) %>%
          group_by(main) %>%
          mutate(roll = rollmean(epop, 12, na.pad = T, align = "right")) %>%
          filter(!is.na(roll))
      }
    
  }
  )
  
  output$plot <- renderPlot({
    
    
    
    p <-  w() %>%
      ggplot(., aes(date, roll, colour = main)) + geom_line(size = 1) + 
      labs(x = NULL, y = NULL) + theme(legend.title = element_blank()) + scale_y_continuous(label = percent) 
    
    if (input$dates < 2008) {
      date <- paste0(input$dates, "-01-01")
      p <- p + recession_shade(date)}
    
    if (input$facet_row != "." & input$facet_col != ".") {
    
    facet_row <- input$facet_row
    facet_col <- input$facet_col
    
    p <-  p + facet_grid(facet_row ~ facet_col)
    } else
      if (input$facet_row != ".") {
        facet_row <- input$facet_row
        
        p <-  p + facet_wrap(~facet_row)
      }
  
    p
    
  })
  
  output$title <- reactive({
    measure <- tibble(user = c("emp", "FT", "unemp", "lf"),
                      label = c("Employment rate", "Full-time employment rate", "Unemployment rate", "Participation rate"))
    variable <- tibble(user = c("race", "sex", "age", "ed", "marital"),
                       label = c("race", "sex", "age", "education", "marital status"))
    
    paste(measure$label[measure$user == input$measure], "by", variable$label[variable$user == input$main])})
  
  
})

