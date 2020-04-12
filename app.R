
library(tidyverse)
library(reshape2)

infections <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
tests <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv")
population <- read_delim("population.csv",  "\t", escape_double = FALSE, trim_ws = TRUE)

population <- population %>%
    #filter(`Country` == "China" | `Country` == "France" | `Country` == "Germany" | `Country` == "Italy" | `Country` == "United States" | `Country` == "New Zealand" | `Country` == "Israel") %>%
    select(`Country`,Population) %>%
    mutate(`Country` = replace(`Country`, `Country` == "United States","US")) %>%
    rename(variable = `Country`) %>%
    rename(population = Population)


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

tests <- tests %>%
  group_by(Entity) %>%
  filter(row_number()==n())
  
#if (interactive()) {
  ui <- fluidPage(
    titlePanel("Corona Dashboard"),

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
      
      checkboxGroupInput('test', 'Countries:', my_countries$Country, selected = c("Germany", "France", "Italy", "New Zealand", "US")),
    ),
    mainPanel(
      #h2("Corona Dashboard"),
      HTML("<p>This is a demo how R and Shiny can be used to create an interactive Dashboard, in this case to look at Corona data. It started with an <a href='https://tom.alby.de/using-r-to-plot-corona-data/'>article</a> how to use R and ggplot2 to visualize the data. The code for this dashboard is on <a href='https://github.com/daswesen/corona-shiny-dashboard'>GitHub</a>.</p>"),
      strong("Please note that this is just a demo of Shiny, I am not a medical expert and I cannot guarantee that data and software code are free of errors."),
      p("Also, please note that countries differ in how data is acquired. Some countries test more than others and are thus more likely to identify more infections, but test data is also difficult to compare. Germany, for example, counts the number of tests performed whereas some countries count how many people were tested."),
      p("This first plot looks at absolute numbers:"),
      plotOutput("result"),
      p("In this plot, numbers per 100.000 people are visualized:"),
      plotOutput("result2"),

    )))
  
  server <- function(input, output) {
    
    data <- reactive({
      
      if(input$variant == 1) {
        data <- my_infections
      }
      else
      {
        data <- my_deaths
      }
    })
    
    


    output$result <- renderPlot({
      

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
          ggtitle("Corona-related Cases") + 
          xlab("Day") + 
          ylab("Cases")
        

      }
      else {
        ggplot(d2, aes(id,value, col=variable)) + 
          geom_line() +
          ggtitle("Corona-related Cases") + 
          scale_y_continuous(trans = 'log2')+
          xlab("Day") + 
          ylab("Cases")
        

      }
      
    })
    
    
    output$result2 <- renderPlot({
      
      
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
      
      cases_vs_population <- d2 %>%
        #deaths_vs_population <- d_deaths %>%
        left_join(population) %>%
        mutate(value = (value/population)*100000)
      

      if(input$yaxis == 1) {
        ggplot(cases_vs_population, aes(Day,value, col=variable)) + 
          geom_line() +
          ggtitle("Corona-related Cases per 100.000 people per Country") + 
          xlab("Day") + 
          ylab("Number of Deaths per 100.000")
      }
      else {

        
        ggplot(cases_vs_population, aes(Day,value, col=variable)) + 
          geom_line() +
          ggtitle("Corona-related Cases per 100.000 people per Country") + 
          scale_y_continuous(trans = 'log2')+
          xlab("Day") + 
          ylab("Number of Cases per 100.000")
      }
      
    })
    
  }
  
  shinyApp(ui, server)
#}