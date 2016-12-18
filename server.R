# title: "HMDA Data Analysis"
# author: "Yogesh"
# date: "December 09, 2016"
# output: html_document


#**********************************************************************************************************
server <- function(input, output) 
  {
  # Filter function is written inorder to capture the controls in the form of state, respondent_name_ts etc   

  # filter for to filter the data for respondent 
  filter_repondent = reactive({
    hmdaData%>%
      filter(
        Respondent_Name_TS == input$Respondent
      ) 
  })
  
  # filter for to filter the data for County
  filter_county = reactive({
    hmdaData%>%
      filter(
        County_Name == input$County
      ) 
  })
  
  # filter for to filter the data for State
  filter_state = reactive({
    hmdaData%>%
      filter(
        State == input$state
      ) 
  })
  
  
  filter_continous = reactive({
    hmdaData[,sapply(hmdaData,is.integer) | sapply(hmdaData,is.numeric)]
  })
  
  
#*******************************************************************************************************  

  
  # rendering data table containing descriptive stats 
  
  output$mytable = renderDataTable({
    if(input$ClassType == 'All'){
      descData
    }
    else{
      descForCont(filter_continous())
    }
    
  },options = list(lengthMenu = c(seq(10,20,5)), pageLength = 10))
  
  
  
  
  output$filterControls <- renderUI({
      selectInput("yearInput", "County", 
                  choices=c(unique(filter_repondent()$County_Name)))
  })


  output$firstPlot <- renderPlot({
    plot(density((filter_repondent()$Loan_Amount_000)),lwd=3,main=NA)
    abline(v = (quantile(filter_repondent()$Loan_Amount_000,0.5) ), col='red',lwd=3)
  })
  
  output$secondPlot <- renderPlot({
    plot(density((filter_repondent()$Applicant_Income_000),na.rm = T),lwd=3,main=NA)
    abline(v = (quantile(filter_repondent()$Applicant_Income_000,0.5,na.rm = T) ), col='red',lwd=3)
  })
  
  output$text1 <- renderText({ 
    paste(input$Respondent,
          " Respondent has an average of ",
          round(nrow(filter_repondent())/3,0),
          " number of loans applicants with an average of about ",
          round(mean(filter_repondent()$Loan_Amount_000,na.rm = T),2),
          " thousand $ per loan applicant")
  })
  
  output$thirdplot <- renderPlot({
    d <- filter_county()%>%
      group_by(Respondent_Name_TS)%>%
      summarise(n = n())%>%
      arrange(desc(n))
    
    ggplot( d[1:10,], aes( x= Respondent_Name_TS,y=n))+
      geom_bar(stat = "identity")
  })
  
  output$text2 <- renderText({
    paste("This county covers about ",
          nrow(filter_county())/3,
          " loan applications on an average for an year.", 
          " Also covers about ",
          round(sum(filter_county()$Loan_Amount_000,na.rm = T)*100/sum(hmdaData$Loan_Amount_000,na.rm = T),8),
          " percentage of the total loan amount of all three years!",
          " Which is about ",
          sum(filter_county()$Loan_Amount_000,na.rm = T),
          "000 $")
  })
  
  
  
### State filtering elemnets
  
  output$fourthplot <- renderPlot({
    
    d <- filter_state()%>%
      group_by(Respondent_Name_TS)%>%
      summarise(n = n())%>%
      arrange(desc(n))
    
    ggplot( d[1:10,], aes( x= Respondent_Name_TS,y=n))+
      geom_bar(stat = "identity")
  })
  
  output$text3 <- renderText({
    paste("This State covers about ",
          nrow(filter_state())/3,
          " loan applications on an average for an year.", 
          " Also covers about ",
          round(sum(filter_state()$Loan_Amount_000,na.rm = T)*100/sum(hmdaData$Loan_Amount_000,na.rm = T),8),
          " percentage of the total loan amount of all three years!",
          " Which is about ",
          sum(filter_state()$Loan_Amount_000,na.rm = T),
          "000 $")
  })
}
#******************************** The END ***************************************************************
