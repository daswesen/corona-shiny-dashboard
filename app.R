
library(tidyverse)
library(reshape2)

infections <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")


my_infections <- infections %>%
  select(-Lat,-Long) %>%
  select(-`Province/State`) %>%
  group_by(`Country/Region`) %>%
  summarise_each(list(sum)) %>%
  rename(Country = `Country/Region`)

my_deaths <- deaths %>%
  select(-Lat,-Long) %>%
  select(-`Province/State`) %>%
  group_by(`Country/Region`) %>%
  summarise_each(list(sum)) %>%
  rename(Country = `Country/Region`)

my_countries <- my_infections %>%
  select(Country)

num_cols <- ncol(my_infections) - 1
  
#if (interactive()) {
  ui <- fluidPage(
    titlePanel( div(
      windowTitle="Corona Dashboard")),

  sidebarLayout(
    sidebarPanel(
      
      radioButtons("variant", 
                   p("Infections / Deaths:"),
                   choices = list("Infections" = 1, "Deaths" = 2), 
                   selected = 1),
      
      radioButtons("yaxis", 
                   p("y Axis::"),
                   choices = list("Linear" = 1, "Log2" = 2), 
                   selected = 1),
      
      sliderInput("ab_wann", 
                  p("Plot after Day of case number:"), 
                  min = 0, max = num_cols, value = 1),
      
      checkboxGroupInput('test', 'Countries:', my_countries$Country, selected = c("Germany", "France", "Italy", "US")),
    ),
    mainPanel(
      h2("Corona Dashboard"),
      plotOutput("result"),

    )))
  
  server <- function(input, output) {
    


    output$result <- renderPlot({
      
      data <- reactive({
        
        if(input$variant == 1) {
          data <- my_infections
        }
        else
        {
          data <- my_deaths
        }
      })
      
      my_data <- data()
      
    
      my_countries <- my_data %>%
        select(Country)
      
      n <- my_data$Country # save the names in the first column
      my_data <- as.data.frame(t(my_data[,-1])) # use t() to transpose and remove the 1st row
      colnames(my_data) <- n # add tshe names as colum names
      my_data <- tibble::rownames_to_column(my_data, "Day") # convert the rownames to a column
      
      my_data <- my_data %>%
        mutate(Day = as.Date(Day,"%m/%d/%y"))
      
      
      d <- melt(my_data, id.vars="Day")
      
     d <- d %>%
        filter(variable == input$test)
      
      d2 <- d %>%
        filter(value >= input$ab_wann) %>%
        group_by(variable) %>%
        mutate(id = row_number())
      
      if(input$yaxis == 1) {
        ggplot(d2, aes(id,value, col=variable)) + 
          geom_line() +
          #scale_y_continuous(trans = 'log2')+
          xlab("Day") + 
          ylab("Cases")
      }
      else {
        ggplot(d2, aes(id,value, col=variable)) + 
          geom_line() +
          scale_y_continuous(trans = 'log2')+
          xlab("Day") + 
          ylab("Cases")
      }
      
    })
    
  }
  
  shinyApp(ui, server)
#}