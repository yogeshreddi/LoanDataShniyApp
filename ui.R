# title: "HMDA Data Analysis"
# author: "Yogesh"
# date: "December 09, 2016"
# output: html_document

 
#**********************************************************************************************************
# Creation of the dashboard using the shiny-dashboard commands 

ui <- dashboardPage(
  dashboardHeader(title = "CapitalOne"),
  # Sidebar content
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Descriptive Stats",       tabName = "0",    icon = icon("dashboard")),
      menuItem("Competitor Filtering",       tabName = "1",    icon = icon("dashboard")),
      menuItem("County Wise Filtering",       tabName = "2",    icon = icon("dashboard")),
      menuItem("State Wise Filtering",       tabName = "3",    icon = icon("dashboard"))
    
      )
  ),
  
  # Body content
  dashboardBody(
  
    tabItems(
      tabItem(tabName = "0",
             h2('Descriptive Stats of the HMDA Data'),
             fluidRow(
               box(
                 # Control for Class type of variables
                 title = "Class type filter",height = 150,width = 1000,
                 selectInput("ClassType", "Class Type",
                             choices=c(sort(decreasing = T,c('All','Continuous')))
                 )
               )
             ),
             dataTableOutput('mytable')),
    tabItem(tabName = "1",
              # Creates the controls for providing input to filter data for plots
              fluidRow(
                box(
                  # Control for Respondent_Name_TS
                  title = "Competitor Filter",background = "navy",height = 150,width = 1000,
                  selectInput("Respondent", "Respondent Name",
                              choices=c(unique(hmdaData%>%
                                                 group_by(Respondent_Name_TS)%>%
                                                 summarise(n=n())%>%
                                                 arrange(desc(n))%>%
                                                 select(Respondent_Name_TS)))
                )
              )
              ),
              fluidRow(
                       box(title= "Distribution of Loan Amount",background = "navy",solidHeader = TRUE, 
                    plotOutput("firstPlot"),height = 500),
                    box(title= "Distribution of applicants Income",background = "navy",solidHeader = TRUE, 
                        plotOutput("secondPlot"),height = 500)),
              fluidRow(box(title= "Respondent Stats",background = "navy",solidHeader = TRUE, 
                           textOutput("text1"),tags$head(tags$style(
                                          "#text1{color: white;
                                                   font-size: 30px;
                                                  font-style: italic;
                                          }"
                         )
                           )
                           ,height = 200,width = 1500))
      ),
      tabItem(tabName = "2",
              # Creates the controls for providing input to filter data for plots
              fluidRow(
                box(
                  # Control for County_Name
                  title = "County Filter",background = "navy",height = 150,width = 1000,
                  selectInput("County", "County Name",
                              choices=c(sort(decreasing = T,unique(hmdaData$County_Name)))
                  )
                )
              ),
              fluidRow(
                box(title= "Distirbution of Applicants of top 10 respondents in the selected county",background = "navy",solidHeader = TRUE, 
                    plotOutput("thirdplot"),height = 500,width = 1500)),
              fluidRow(
                box(title= "County Stats",background = "navy",solidHeader = TRUE, 
                    textOutput("text2"),tags$head(tags$style(
                      "#text2{color: white;
                              font-size: 30px;
                              font-style: italic;
                      }"
                         )
                    )
                    ,height = 200,width = 1500))
              ),
      tabItem(tabName = "3",
              # Creates the controls for providing input to filter data for plots
              fluidRow(
                box(
                  # Control for State
                  title = "State Filter",background = "navy",height = 150,width = 1000,
                  selectInput("state", "State Name",
                              choices=c(unique(hmdaData%>%
                                                 group_by(State)%>%
                                                 summarise(n=n())%>%
                                                 arrange(desc(n))%>%select(State)))
                  )
                )
              ),
              fluidRow(
                box(title= "Distirbution of Applicants of top 10 respondents in the selected State",background = "navy",solidHeader = TRUE, 
                    plotOutput("fourthplot"),height = 500,width = 1500)),
              fluidRow(
                box(title= "State wise Stats",background = "navy",solidHeader = TRUE, 
                    textOutput("text3"),tags$head(tags$style(
                      "#text3{color: white;
                      font-size: 30px;
                      font-style: italic;
                      }"
                         )
                    )
                    ,height = 200,width = 1500))
                )
      )
      )
      
    )
