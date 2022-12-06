library(shiny)
library("dplyr")
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
library(shinyjs)

# Load the data and rename columns

ind_data=read.csv("IHMStefanini_industrial_safety_and_health_database.csv")
ind_data <- ind_data %>% 
  rename("Date" = "Data",
         "Industry Sector" = "Industry.Sector",
         "Accident Level" = "Accident.Level",
         "Employee Gender"="Genre",
         "Employee Type"="Employee.ou.Terceiro",
         "Potential Accident Level"="Potential.Accident.Level")

variables=c("Industry Sector","Accident Level","Employee Gender","Employee Type","Potential Accident Level","Countries", "Local")



about="Founded in 1994 as a systems, instrumentation, electrical and Industrial IT integrator, ihm has been expanding its activities over the years. Becoming part of the Stefanini group in 2015, in what formed the largest LATAM operational technology company, today it operates in the entire industry vertical: from the assembly of panels and electrocentres to the use of disruptive technologies and mindset change for digital transformation, going through automation, electrical and IT projects.

With such diverse Industrial activities, some accidents are bound to happen within the facilities. Presented here is an analysis of the accidents and their various aspects. It is hoped that this report would give an overview to the various aspects of the accidents and provide an insight towards mitigating them."




single_plot<-function(data,col){
  
  ggplot(data)+aes(x=data[,col])+
    geom_bar(width=.5,aes(fill=data[,col]))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+scale_fill_brewer(palette="Set3")+
    labs(title="(a)")+xlab(col)+guides(fill="none")
  
}


double_plot<-function(data,col1,col2){
  ggplot(data)+aes(x=data[,col1])+
    geom_bar(aes(fill=data[,col2]),position=position_dodge2(preserve = "single"))+
    labs(title="(b)", fill=col2)+xlab(col1)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_fill_brewer(palette="Set3")
  
}



library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Industrial Labour Accidents",
                  titleWidth = 350),
  dashboardSidebar(
    width=350,
    sidebarMenu(
      menuItem("About", tabName="about"),
      menuItem("Univariate", tabName = "univariate" ),
      menuItem("Bivariate", tabName = "bivariate")
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS("body > div > header > nav > a {visibility: hidden}"),
    tabItems(
      #About tab
      tabItem(tabName = "about",
              fluidRow(p(),height=400),
              fluidRow(
                column(p(),width=2),
                column(p(about, style = "font-size:20px;"),width=8,align="left")),
              
              fluidRow(
                column(p(),width=2),
                column(
                h1("About the Dataset"),
                p("The data set contains records of accidents from 12 plants in 3 different countries of the ihm Stefanini coroporation. Every row in the data set represents a single incident of accident. Each row has the following attributes:",style = "font-size:20px;"),
                tags$ul(
                  tags$li("Date: Time stamp"), 
                  tags$li("Countries: Country of accident (anonymized)"), 
                  tags$li("Local: City of Accident (anonymized)"),
                  tags$li("Industry Sector: Industrial Sector of the plant"),
                  tags$li("Accident Level: (I to VI) Severity of accident (I :- Not Severe & VI :- Very Severe)"),
                  tags$li("Potential Accident Level: How bad the accident could have been (due to other factors involved in the accident)"),
                  tags$li("Gender: Gender of the employee involved with the accident (Male or Female)"),
                  
                  tags$li("Employee or Third Party: If the injured person is an employee or third party"),
                  tags$li("Critical Risk: Some description of the risk involved in the accident"),
                  
                  style = "font-size:20px;"
                ),
                
                width=8,align="left")
              ),
              
              
              fluidRow(
                column(p(),width=2),
                column(p("Data sourced from : https://www.kaggle.com/datasets/ihmstefanini/industrial-safety-and-health-analytics-database",style = "font-size:20px;"),width=8,align='left')
              )
              ),
      
      
      # First tab content
      tabItem(tabName = "univariate",
              fluidRow(
                box(plotOutput("plot1", height = 400),width=6),
                box(
                  title = "Controls",
                  selectInput("variable","Choose a variable:",
                              choices = variables), width=4),
                
              ),
              fluidRow(
                box( tableOutput("table1")
                     
                  
                )
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "bivariate",

                 fluidRow(
                   box(plotOutput("plot2",height=400)),
                   
                   box(
                     title ='Controls',
                     selectInput("variable1","Choose a variable:",
                                 choices = variables),
                     selectInput("variable2", "Choose a second variable:",
                                 choices = variables)
                      )),
              
              fluidRow(
                box( tableOutput("table2")
                     
                     
                )
              )
              )
    )
  )
)

server <- function(input, output) {

  
  output$plot1 <- renderPlot({
    data <- ind_data
   single_plot(ind_data,input$variable)
  })
  
  output$plot2 <- renderPlot({
    data <- ind_data
    double_plot(ind_data,input$variable1,input$variable2)
  })
  
  output$table1<- renderTable({
    x<-data.frame(table(ind_data[,input$variable])) %>%

  
    rename(!!input$variable := "Var1", "Accident Frequency"="Freq")
    
    x
  })
  
  output$table2<-renderTable({
  x<-ind_data %>%
      group_by_at(unique(c(input$variable1,input$variable2))) %>%
      summarise(total_count=n(), .groups="keep") %>%
    rename("Accident Frequency"="total_count")
  x
  })
  
  output$text1 <-renderText({})
}





shinyApp(ui, server)
