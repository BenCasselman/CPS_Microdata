library(shiny)
library(ggvis)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Exploring the American labor force"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(position="left",
                sidebarPanel(h2("Choose your variables"),
                             h5("Note: You can't pick the same variable twice"),
                             radioButtons("measure", label = strong("What are we measuring?"),
                                          c("Employment rate" = "emp", "Full-time employment rate" = "FT", "Unemployment rate" = "unemp", "Participation rate" = "lf"),
                                          selected = "emp"),
                             radioButtons("main", label = strong("Select a primary variable (shown in every chart)"),
                                          c("Race" = "race", "Sex" = "sex", "Age" = "age", "Education" = "ed", "Marital status" = "marital"),
                                          selected = "race"),
                             selectInput("facet_row", label = strong("Select a secondary variable"),
                                          c("None" = ".", "Race" = "race", "Sex" = "sex", "Age" = "age", "Education" = "ed", "Marital status" = "marital")),
                             selectInput("facet_col", label = strong("Select another secondary variable (optional)"),
                                          c("None" = ".", "Race" = "race", "Sex" = "sex", "Age" = "age", "Education" = "ed", "Marital status" = "marital")),
                             h2("Filters"),
                             h5("Note: This will break if you make too many selections"),
                             sliderInput("dates",label=strong("Start date"),min=1995,max=2016,step=1,value=1995, sep = ""),
                             checkboxGroupInput("age", label = "Age groups",
                                                c("Ages 16-17", "Ages 18-24", "Ages 25-34", "Ages 35-44", "Ages 45-54", "Ages 55-64", "Ages 65-plus"),
                                                selected = c("Ages 25-34", "Ages 35-44", "Ages 45-54")),
                             checkboxGroupInput("ed", label = "Education groups",
                                                c("Less than high school", "HS diploma or GED", "Some college or associate", "BA or higher")),
                             checkboxGroupInput("marital", label = "Marital status",
                                                c("Never married", "Married", "Previously married")),
                             checkboxGroupInput("race", label = "Race",
                                                c("White", "Black", "Hispanic", "Asian", "Other"),
                                                selected = c("White", "Black")),
                             checkboxGroupInput("sex", label = "Sex",
                                                c("Men", "Women"))
                             
                             ),
                
                mainPanel(h2(textOutput("title")),
                          h5("12-month rolling average"),
                  plotOutput("plot")
                ))
  
))
