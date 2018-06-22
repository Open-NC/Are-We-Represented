#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(scales)
library(markdown)

#Import data
commissioners <- read_csv("data/cc-21june2018.csv") 
voter <- read_csv("data/nc_voter_demographics.csv") 
voter[is.na(voter)] <- 0

###----

#Tidy Commissioners Data
nc_cc_party <- commissioners %>%
  mutate(County = toupper(County)) %>%
  group_by(County, `Party-VR`) %>%
  count(County, `Party-VR`) %>%
  spread(`Party-VR`, n) %>%
  mutate(GRE = 0, LIB = 0) %>%
  select(County, DEM, GRE, LIB, REP, UNA) %>%
  gather(`Party-VR`, n, DEM:UNA, factor_key=TRUE) %>%
  ungroup() %>%
  complete(County, `Party-VR`, fill = list(n = 0))

nc_cc_race <- commissioners %>%
  mutate(County = toupper(County)) %>%
  group_by(County, Race) %>%
  count(County, Race) %>%
  ungroup() %>%
  complete(County, Race, fill = list(n = 0)) %>%
  spread(Race, n) %>%
  mutate(A = 0, O = 0) %>%
  select(County, A, B, I, M, O, U, W) %>%
  gather(Race, n, A:W, factor_key=TRUE) %>%
  arrange(County)

nc_cc_eth <- commissioners %>%
  mutate(County = toupper(County)) %>%
  group_by(County, Ethnicity) %>%
  count(County, Ethnicity) %>%
  spread(Ethnicity, n) %>%
  mutate(HL = 0) %>%
  select(County, HL, NL, UN) %>%
  gather(Ethnicity, n, HL:UN, factor_key=TRUE) %>%
  ungroup() %>%
  complete(County, Ethnicity, fill = list(n = 0))

nc_cc_gender <- commissioners %>%
  mutate(County = toupper(County)) %>%
  group_by(County, Gender) %>%
  count(County, Gender) %>%
  spread(Gender, n)  %>%
  mutate(U = 0) %>%
  gather(Gender, n, F:U, factor_key=TRUE) %>%
  ungroup() %>%
  complete(County, Gender, fill = list(n = 0))

#Palettes
party_palette <- c("DEM" = "#2E86C1", "GRE" = "#17A589", "LIB" = "#D4AC0D", "REP" = "#CB4335", "UNA" = "#884EA0" )

race_palette <- c("A" = "#E74C3C", "B" = "#27AE60", "I" = "#9B59B6", "M" = "#1ABC9C",  "O" = "#F1C40F", "U" = "#C0392B", "W" = "#E67E22")

cc_gender_palette <- c("F" = "#9B59B6", "M" = "#1ABC9C", "U" = "#F4D03F")

voter_gender_palette <- c("Female" = "#9B59B6", "Male" = "#1ABC9C", "UnDisclosedGender" = "#F4D03F")

#Labels
party_labels <- c("DEM" = "Democrats", "REP" = "Republicans", "GRE" = "Green", "LIB" = "Libertarians", "UNA" = "Unaffiliated")

race_labels <- c("A" = "Asian", "B" = "Black", "I" = "Native American", "M" = "Multiracial", "O" = "Other", "U" = "Undisclosed", "W" = "White")

eth_labels <- c("HL" = "Hispanic-Latino", "NL" = "Not Hispanic-Latino", "UN" = "Undisclosed")

cc_gender_labels <- c("F" = "Female", "M" = "Male", "U" = "Undisclosed")

voter_gender_labels <- c("Female" = "Female", "Male" = "Male", "UnDisclosedGender" = "Undisclosed")

###----

# Define UI 
ui <- fluidPage(theme = shinytheme("flatly"),
      fluidRow(
        column(12, align = "center", offset = 2,

    mainPanel(
      column(12, align = "center",
      titlePanel("Are We Represented?")
      ), #titlepanel column
      column(12, align = "center",
      br(),
      p("Does a county's board of commissioners reflect the demographics of the registered voters in the populations they serve?"),
      br(),
      selectInput("county", 
                  label = "Choose a county to display",
                  choices = c("ALAMANCE",     
                              "ALEXANDER",    
                              "ALLEGHANY",    
                              "ANSON",        
                              "ASHE",        
                              "AVERY",
                              "BEAUFORT",
                              "BERTIE",
                              "BLADEN",
                              "BRUNSWICK",   
                              "BUNCOMBE",
                              "BURKE",
                              "CABARRUS",
                              "CALDWELL",
                              "CAMDEN",
                              "CARTERET",
                              "CASWELL",
                              "CATAWBA",
                              "CHATHAM",
                              "CHEROKEE",
                              "CHOWAN",
                              "CLAY",
                              "CLEVELAND",
                              "COLUMBUS",
                              "CRAVEN",
                              "CUMBERLAND",
                              "CURRITUCK",
                              "DARE",
                              "DAVIDSON",
                              "DAVIE",
                              "DUPLIN",
                              "DURHAM",
                              "EDGECOMBE",
                              "FORSYTH",
                              "FRANKLIN",    
                              "GASTON",
                              "GATES",
                              "GRAHAM",
                              "GRANVILLE",
                              "GREENE",
                              "GUILFORD",
                              "HALIFAX",
                              "HARNETT",
                              "HAYWOOD",
                              "HENDERSON",   
                              "HERTFORD",
                              "HOKE",
                              "HYDE",
                              "IREDELL",
                              "JACKSON",     
                              "JOHNSTON",
                              "JONES",        
                              "LEE",
                              "LENOIR",       
                              "LINCOLN",     
                              "MCDOWELL",     
                              "MACON", 
                              "MADISON",      
                              "MARTIN",     
                              "MECKLENBURG", 
                              "MITCHELL", 
                              "MONTGOMERY", 
                              "MOORE", 
                              "NASH",      
                              "NEW HANOVER", 
                              "NORTHAMPTON",
                              "ONSLOW",   
                              "ORANGE",     
                              "PAMLICO",    
                              "PASQUOTANK",  
                              "PENDER", 
                              "PERQUIMANS",  
                              "PERSON", 
                              "PITT",    
                              "POLK",       
                              "RANDOLPH",  
                              "RICHMOND",  
                              "ROBESON",  
                              "ROCKINGHAM",  
                              "ROWAN",  
                              "RUTHERFORD",  
                              "SAMPSON",
                              "SCOTLAND",  
                              "STANLY",  
                              "STOKES",     
                              "SURRY",
                              "SWAIN", 
                              "TRANSYLVANIA", 
                              "TYRRELL",
                              "UNION",
                              "VANCE", 
                              "WAKE", 
                              "WARREN",    
                              "WASHINGTON", 
                              "WATAUGA",
                              "WAYNE",    
                              "WILKES",     
                              "WILSON",   
                              "YADKIN",   
                              "YANCEY"  ), 
                  selected = "ALAMANCE"
                  ) #selectinput
      ), #selectinput column
      
      column(12, align = "center",
        tabsetPanel(type = "tabs",
          tabPanel("Party", 
                   fluidRow(
                     br(),
                     br(),
                     column(6,
                        plotOutput("cc_party")), 
                     column(6, plotOutput("voter_party"))
                   )#fluidRow
          ), #tabPanel
          tabPanel("Race", 
                   fluidRow(
                     br(),
                     br(),
                     column(6, plotOutput("cc_race")), 
                     column(6, plotOutput("voter_race"))
                   )#fluidRow
          ), #tabPanel
          tabPanel("Ethnicity", 
                   fluidRow(
                     br(),
                     br(),
                     column(6, plotOutput("cc_eth")), 
                     column(6, plotOutput("voter_eth"))
                   )#fluidRow
          ), #tabPanel
          tabPanel("Gender", 
                  fluidRow(
                    br(),
                    br(),
                    column(6, plotOutput("cc_gender")), 
                    column(6, plotOutput("voter_gender"))
                  ) #fluidRow
          ) #tabPanel  
        ), #tabsetPanel
        br(),
        br()
      ), #tabsetPanel column
      
      p("In North Carolina, county commissioners are elected to provide for the safety and welfare of residents within their counties and to administer social services, public health services and education programs in conjunction with the state. They are responsible for adopting an annual budget, establishing the property tax rate, providing funding for the construction of public school buildings, and enacting local ordinances. Each board appoints a county manager to administer and oversee the activities of county departments."), 
       p("Commission boards range in member size from 5 to 9. County Commissioners are generally elected to 4 year terms. In some counties, all commissioners are elected during the same election cycle. In others, the election cycles are staggered among the members."),
      p(HTML(paste0("For information on registering to vote, ", a(href ="https://www.ncsbe.gov/Voters/Registering-to-Vote", "click here to visit the North Carolina State Board of Elections website.")))),
      p("Voter regisration data was sourced from a North Carolina State Board of Elections database last modified June 16, 2018. County Commissioner data was compiled from a  candidate listing spreadsheet received via FOIA to the NC State Board of Elections, the state voter registration database and county government websites.")
      ) #mainPanel
    )# mainPanel column
  )# mainPanel fluidRow
  ) #fluidPage
###----

# Server
server <- function(input, output) {

  #County Commissioners - Party Affiliation
  output$cc_party <- renderPlot({

    county_cc_party <- reactive({
      req(input$county)
      
      nc_cc_party %>%
        filter(County == input$county) %>%
        mutate(cc_party_pct = (n/sum(n)))
      
    })
    
    ggplot(county_cc_party(), aes(x = `Party-VR`, y=cc_party_pct, fill = factor(`Party-VR`))) + 
      geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = party_palette) +
      scale_x_discrete(labels= party_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Party", 
           x=NULL, 
           y=NULL, 
           title="County Commissioners") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
  })
  
  #Voters - Party Affiliation
  output$voter_party <- renderPlot({
    
    county_voter_party <- reactive({
      req(input$county)
      voter %>%
        filter(county_desc == input$county) %>%
        select(DEM, REP, GRE, LIB, UNA) %>%
        gather("DEM":"UNA", key = "variable", value = "value") %>%
        mutate(party_pct = (value/sum(value)))
      
    })
    
    ggplot(county_voter_party(), aes(x = variable, y=party_pct, fill = factor(variable))) + 
      geom_bar(width = 1, stat = "identity", position = position_dodge(preserve = "single")) +
      scale_fill_manual(values = party_palette) +
      scale_x_discrete(labels = party_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Party", 
           x=NULL, 
           y=NULL, 
           title="Registered Voters") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
    
  })

  #County Commissioners - Race
  output$cc_race <- renderPlot({
    
    county_cc_race <- reactive({
      req(input$county)
      
      nc_cc_race %>%
        filter(County == input$county) %>%
        mutate(cc_race_pct = (n/sum(n)))
      
    })
    
    ggplot(county_cc_race(), aes(x = Race, y=cc_race_pct, fill = factor(Race))) + 
      geom_bar(stat = "identity", position = position_dodge2(preserve = "single"), fill = "#DC7633") +
      #scale_fill_manual(values = race_palette) +
      scale_x_discrete(labels= race_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Race", 
           x=NULL, 
           y=NULL, 
           title="County Commissioners") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
  })
  
  #Voters - Race
  output$voter_race <- renderPlot({
    
    county_voter_race <- reactive({
      req(input$county)
      voter %>%
        filter(county_desc == input$county) %>%
        select(A, B, I, M, O, U, W) %>%
        gather("A":"W", key = "variable", value = "value") %>%
        mutate(race_pct = (value/sum(value)))
      
    })
    
    ggplot(county_voter_race(), aes(x = variable, y=race_pct, fill = factor(variable))) + 
      geom_bar(width = 1, stat = "identity", position = position_dodge(preserve = "single"), fill = "#DC7633") +
      scale_x_discrete(labels= race_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Race", 
           x=NULL, 
           y=NULL, 
           title="Registered Voters") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
    
  })
  
  #County Commissioners - Ethnicity
  output$cc_eth <- renderPlot({
    
    county_cc_eth <- reactive({
      req(input$county)
      
      nc_cc_eth %>%
        filter(County == input$county) %>%
        mutate(cc_eth_pct = (n/sum(n)))
      
    })
    
    ggplot(county_cc_eth(), aes(x = Ethnicity, y=cc_eth_pct, fill = factor(Ethnicity))) + 
      geom_bar(stat = "identity", position = position_dodge2(preserve = "single"), fill = "#DC7633") +
      scale_x_discrete(labels= eth_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Race", 
           x=NULL, 
           y=NULL, 
           title="County Commissioners") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
  })
  
  #Voters - Ethnicity
  output$voter_eth <- renderPlot({
    
    county_voter_eth <- reactive({
      req(input$county)
      voter %>%
        filter(county_desc == input$county) %>%
        select(HL, NL, UN) %>%
        gather("HL":"UN", key = "variable", value = "value") %>%
        mutate(eth_pct = (value/sum(value)))
      
    })
    
    ggplot(county_voter_eth(), aes(x = variable, y=eth_pct, fill = factor(variable))) + 
      geom_bar(width = 1, stat = "identity", position = position_dodge(preserve = "single"), fill = "#DC7633") +
      scale_x_discrete(labels= eth_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Race", 
           x=NULL, 
           y=NULL, 
           title="Registered Voters") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
    
  })
  
  #County Commissioners - Gender
  output$cc_gender <- renderPlot({
    
    county_cc_gender <- reactive({
      req(input$county)
      
      nc_cc_gender %>%
        filter(County == input$county) %>%
        mutate(cc_gender_pct = (n/sum(n)))
      
    })
    
    ggplot(county_cc_gender(), aes(x = Gender, y=cc_gender_pct, fill = factor(Gender))) + 
      geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = cc_gender_palette) +
      scale_x_discrete(labels= cc_gender_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Gender", 
           x=NULL, 
           y=NULL, 
           title="County Commissioners") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
  })
  
  #Voters - Gender
  output$voter_gender <- renderPlot({
    
    county_voter_gender <- reactive({
      req(input$county)
      voter %>%
        filter(county_desc == input$county) %>%
        select(Male, Female, Undisclosed) %>%
        gather("Male":"Undisclosed", key = "variable", value = "value") %>%
        mutate(gender_pct = (value/sum(value)))
      
    })
    
    ggplot(county_voter_gender(), aes(x = variable, y=gender_pct, fill = factor(variable))) + 
      geom_bar(width = 1, stat = "identity", position = position_dodge(preserve = "single")) +
      scale_fill_manual(values = voter_gender_palette) +
      scale_x_discrete(labels= voter_gender_labels) +
      scale_y_continuous(labels=percent, limits = c(0,1)) +
      labs(fill="Gender", 
           x=NULL, 
           y=NULL, 
           title="Registered Voters") +
      theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1, size = 14)) +
      guides(fill=FALSE)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
