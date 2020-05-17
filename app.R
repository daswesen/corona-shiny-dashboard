
library(tidyverse, warn.conflicts = FALSE)
library(reshape2)
library(ggrepel)

infections <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
tests <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv")
population <- read_delim("population.csv",  "\t", escape_double = FALSE, trim_ws = TRUE)
grippe <- read_delim("https://raw.githubusercontent.com/daswesen/analyzing-corona-data-with-r/master/grippe.csv", ";", escape_double = FALSE, col_types = cols(date = col_date(format = "%d.%m.%y"), infected = col_integer()), trim_ws = TRUE)

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
                   choices = list("Linear" = 1, "Log2 Scale" = 2), 
                   selected = 1),
      
      sliderInput("ab_wann", 
                  p("Plot after Day of case number/after x in 100.000:"), 
                  min = 0, max = num_cols, value = 0),
      
      checkboxGroupInput('test', 'Countries:', my_countries$Country, selected = c("Germany", "France", "Italy", "New Zealand", "US")),
    ),
    mainPanel(
      #h2("Corona Dashboard"),
      HTML("<p>This is a demo how R and Shiny can be used to create an interactive dashboard, in this case to look at Corona data. It started with an <a href='https://tom.alby.de/using-r-to-plot-corona-data/'>article</a> how to use R and ggplot2 to visualize the data. The code for this dashboard is on <a href='https://github.com/daswesen/corona-shiny-dashboard'>GitHub</a>. Please contact me at tom@alby.de if you have comments or found a bug.</p>"),
      strong("Please note that this is just a demo of Shiny, I am not a medical expert and I cannot guarantee that data and software code are error-free."),
      tags$hr(),
      HTML("<p>Data comes from the <a href='https://github.com/CSSEGISandData/COVID-19'>Johns Hopkins University'</a> (infection and deaths data), <a href='https://www.worldometers.info/world-population/population-by-country/'>Worldmeter</a> (population data), <a href='https://ourworldindata.org/covid-testing'>Our World in Data</a> (test data), and the <a href='https://influenza.rki.de'>RKI</a> (influenza data for Germany).</p>"),
      h2("Absolute Numbers"),
      p("Please note that countries differ in how data is acquired. Some countries test more than others and are thus more likely to identify more infections, but test data is also difficult to compare. Germany, for example, counts the number of tests performed whereas some countries count how many people were tested."),
      plotOutput("result"),
      h2("Data per 100.000 People"),
      plotOutput("result2"),
      h2("How many tests were performed?"),
      plotOutput("result5"),
      h2("Germany: Corona Data compared to Influenza Data"),
      p("These plots are independent from what you select on the left; it uses the German RKI influenza data and compares it to the German Corona data. Influenza data is updated on a weekly basis, Corona data on a daily basis. As a consequence, the Corona line may flatten because the week is not full yet."),
      plotOutput("result3"),
      plotOutput("result4"),
      

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
        filter(variable %in% input$test)
      
      d2 <- d %>%
        filter(value > input$ab_wann-1) %>%
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
        filter(variable %in% input$test)
      

      
       d2 <- d %>%
        #deaths_vs_population <- d_deaths %>%
        left_join(population) %>%
        mutate(value = (value/population)*100000)
      
      cases_vs_population <- d2 %>%
        filter(value >= input$ab_wann) %>%
        group_by(variable) %>%
        mutate(id = row_number())
      

      if(input$yaxis == 1) {
        ggplot(cases_vs_population, aes(id,value, col=variable)) + 
          geom_line() +
          ggtitle("Corona-related Cases per 100.000 people per Country") + 
          xlab("Day") + 
          ylab("Number of Cases per 100.000")
      }
      else {

        
        ggplot(cases_vs_population, aes(id,value, col=variable)) + 
          geom_line() +
          ggtitle("Corona-related Cases per 100.000 people per Country") + 
          scale_y_continuous(trans = 'log2')+
          xlab("Day") + 
          ylab("Number of Cases per 100.000")
      }
      
    })
    
    output$result3 <- renderPlot({
    
      n <- my_infections$Country # save the names in the first column
      my_infections <- as.data.frame(t(my_infections[,-1])) # use t() to transpose and remove the 1st row
      colnames(my_infections) <- n # add tshe names as colum names
      my_infections <- tibble::rownames_to_column(my_infections, "Day") # convert the rownames to a column
      
      my_infections <- my_infections %>%
        mutate(Day = as.Date(Day,"%m/%d/%y"))
      
    my_infections %>%
      select(Day,Germany) %>%
      full_join(grippe, c("Day" = "date")) %>%
      select(-deaths) %>%
      rename(Corona = Germany) %>%
      rename(Influenza = infected) %>%
      mutate(Influenza17_18 = case_when(Day < "2018-12-31" ~ Influenza)) %>%
      mutate(Influenza19_20 = case_when(Day > "2018-12-31" ~ Influenza)) %>%
      select(-Influenza) %>%
      melt(., id.vars="Day") %>%
      filter(!is.na(value)) %>%
      mutate(week = lubridate::week((Day))) %>%
      mutate(week = if_else(week > 38,week-53,week)) %>%
      group_by(week, variable) %>%
      summarize(value = max(value)) %>%
      ggplot(aes(week,value, col=variable)) + 
      geom_line()  +
      ggtitle("Two Influenza Seasons compared to Corona") + 
      xlab("Week") + 
      ylab("Accumulated Number of reported Infections")
    
    })
    
    output$result4 <- renderPlot({
      
      n <- my_deaths$Country # save the names in the first column
      my_deaths <- as.data.frame(t(my_deaths[,-1])) # use t() to transpose and remove the 1st row
      colnames(my_deaths) <- n # add tshe names as colum names
      my_deaths <- tibble::rownames_to_column(my_deaths, "Day") # convert the rownames to a column
      
      my_deaths <- my_deaths %>%
        mutate(Day = as.Date(Day,"%m/%d/%y"))
    
    my_deaths %>%
      select(Day,Germany) %>%
      full_join(grippe, c("Day" = "date")) %>%
      select(-infected) %>%
      rename(Corona = Germany) %>%
      rename(Influenza = deaths) %>%
      mutate(Influenza17_18 = case_when(Day < "2018-12-31" ~ Influenza)) %>%
      mutate(Influenza19_20 = case_when(Day > "2018-12-31" ~ Influenza)) %>%
      select(-Influenza) %>%
      melt(., id.vars="Day") %>%
      filter(!is.na(value)) %>%
      filter(value != 0) %>%
      mutate(week = lubridate::week((Day))) %>%
      mutate(week = if_else(week > 38,week-53,week)) %>%
      group_by(week, variable) %>%
      summarize(value = max(value)) %>%
      ggplot(aes(week,value, col=variable)) + 
      geom_line()  +
      ggtitle("Deaths: Two Influenza Seasons compared to Corona in Germany") + 
      xlab("Week") + 
      ylab("Accumulated Number of reported Deaths")
    
  })
    
    output$result5 <- renderPlot({
      
      tests2 <- tests %>%
        mutate(Country = str_remove(Entity, " - .*")) %>%
        mutate(Country = ifelse(Country == "United States", "US",Country)) %>%
        filter(Country %in% input$test) 
        
    
    ggplot(tests2, aes(x = Date, y = `Cumulative total`)) +
      geom_point(aes(color = Entity,  size = `Cumulative total`)) +
      geom_text_repel(aes(label=Entity)) + # Repel takes care of putting the labels close to the data points but without leaving the grid
      #scale_y_continuous(trans = 'log2') +
      labs(title="Performed Tests on Day of Year, Size reflects tests performed per million", x ="Date", y = "Tests performed") +
      theme(legend.position = "none")
    
  })
    
  }
  
  shinyApp(ui, server)
#}