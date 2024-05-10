library(shinydashboard)
library(plotly)

turnover = read.csv(file="https://isarenn.github.io/irennenberg/FinalProject/Turnover.csv")
turnover <- turnover[complete.cases(turnover),]
names(turnover)[12]='extraversion'
names(turnover)[13]='agreeableness'
names(turnover)[14]='conscientiousness'
names(turnover)[15]='neuroticism'
names(turnover)[16]='openness' #Translations to English from Russian!
turnovernum <-subset(turnover, select=c(way,event,extraversion, agreeableness, conscientiousness, neuroticism, openness,age,stag))
x.namesnum = names(turnovernum)[-c(1,6)]
y.namesnum =names(turnovernum)[-1]
turnovercat <-subset(turnover, select=c(gender,industry,profession,traffic,coach,head_gender,greywage,way))
x.namescat = names(turnovercat)
y.namescat =names(turnovercat)
x.names = names(turnover)[-c(1,6)]
y.names =names(turnover)[-1]

header0 <- dashboardHeader(title = "Employee Turnover Analysis")

sider0 <- dashboardSidebar(
  sidebarMenu(
    menuItem("Numerical Variables", tabName = "num", icon = icon("sun"), badgeLabel = "Analysis 1", badgeColor ="fuchsia"),
    menuItem("Categorical Variables", icon = icon("cat"), tabName = "cat", badgeLabel = "Analysis 2",
             badgeColor = "purple"
    ),
    menuItem("Summary", tabName="sum", icon=icon("pen")),
    menuItem("Source code for app", icon = icon("heart"),
             href = "https://isarenn.github.io/irennenberg/FinalProject/app.R"
    )
  ),
  
  radioButtons("way", "Travel",
               c("bus","foot","car","all"),
               selected="all")
)

body0 <-   dashboardBody(
  tabItems(
    tabItem(tabName="num",
            fluidRow(
              tabBox(
                title = "Plots",
                id = "tabset1", height = "617px",
                br(),
                tabPanel("Scatterplot", plotlyOutput("scatter1", height=600)),
                tabPanel("Prediction Plot", plotOutput("pred1",height=600)),
                tabPanel("Diagnostics", plotOutput("diag1", height=600))
              ),
              box(title="Select X",
                  status="info", width=2, solidHeader=TRUE,
                  selectInput("xnum","X Variable",
                              choices=x.namesnum,
                              selected=x.namesnum[4])
                  ),
              box(title="Select Y",
                  status="info", width=2, solidHeader=TRUE,
                  selectInput("ynum","Y Variable",
                              choices=y.namesnum,
                              selected=y.namesnum[2])
                  ),
              box(title="Numeric Input",
                  status="info",
                  width=2, solidHeader=TRUE,
                  numericInput(inputId = "newX", 
                               label = "New Value for Prediction:", 
                               value = 7,
                               min = 0, 
                               max = 100000000, 
                               step = 0.1)
                
              ),
              tabBox(title = "Analysis",
                     id = "tabset2", height = "200px",
                     tabPanel("Regression Coef", tableOutput("reg1"))
              ),
              box(title="Big5 Personality Scale", width=4, height=300, background="purple",
                  "High Scores Correspond to the top, and low to the bottom characteristics.",
                  img(src="https://isarenn.github.io/irennenberg/FinalProject/big5.webp", width="85%"),
                  br()
              ),
              box(title="Variable Definitions", solidHeader=TRUE, width=2, height=300, background="purple",
                  "stag=experience", br(),
                  "event= 1(left), 0 (stay)", br(),
                  "traffic=the source the person was hired from", br(),
                  "coach=presence of a coach for probation period", br(),
                  "head_gender=gender of manager",br(),
                  "grey_wage=salary to the tax authorities",
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                  )
           
              
      )
    ),
    
    tabItem(tabName="cat",
            fluidRow(
              tabBox(
                title = "Plots",
                id = "tabset1", height = "565px",
                tabPanel("Boxplot", plotOutput("box1")),
                tabPanel("SxR Table (For EVENT)", tableOutput("table1")),
                tabPanel("Pie Chart", plotOutput("pie1"))
              ),
              tabBox(
                title="Tests For Significance",
                side="right",
                id="tabset3", height="100px",
                tabPanel("ANOVA", tableOutput("anova1")),
                tabPanel("Chi-Sq Test", tableOutput("chi1"))
              ),
              box(title="Select X",
                  status="info",
                  width=4, solidHeader=TRUE,
                  selectInput("xcat","X Variable",
                              choices=x.namescat,
                              selected=x.namescat[5])
              ),
              box(title="Select Y",
                  status="info",
                  width=4, solidHeader=TRUE,
                  selectInput("ycat","Y Variable",
                              choices=y.namesnum,
                              selected=y.namesnum[2])
              ),
              box(title="Pie Chart Variable",
                  status="info",
                  width=4, solidHeader=TRUE,
                  selectInput("pie","Pie Variable",
                              choices=y.namescat,
                              selected=y.namescat[5 ])
                  )
              
    )
  ),
  tabItem(tabName="sum",
          fluidRow(
            box(title="Summary", color="teal", width=12,
                "Throughout the numerical analysis, it was found that there were relationships between the Big5 personalities. If someone was more selfless and submissive, they were more likely to be withdrawn. 
                If someone was more distracted and rash, they were more likely to be detached. If someone was eccentric and flexible, they were more attention driven and 
                excitement driven. There were more females in the study than males, and the largest industry in the study was Retail. The largest profession was HR. Most people
                in the study did not have a coach given that they were on probation. There were more managers that were male as compared to female. There was a difference in extraversion
                if the person had a coach on the probation period, but not for any of the other big5 personalities. The way in which people got to work mostly affected 
                neuroticism and openness. For the SxR table, it was found that the way you get to work does affect whether or not you leave (p value of 0.0067). However, having a coach during probation
                did not affect whether or not someone would leave. There was no difference in gender on whether or not the person left their job, but there were key differences in 
                personality scores between men and women. ")
            
          )
  )
 )
)
  
ui <- dashboardPage(header = header0,
                    sidebar = sider0,
                    body = body0,
                    skin="purple")

server <- function(input, output) {
  
  workDat <- reactive({
    if (input$way == "all") {
      return(turnover)
    } else {
      return(turnover[turnover$way == input$way, ])
    }
  })
  
  
#Here is the Scatter Plot
  
  
  output$scatter1 <- renderPlotly({
    plot_ly(data = workDat(),
            x =  ~workDat()[[input$xnum]], 
            y =  ~workDat()[[input$ynum]], 
            color = ~workDat()$way, 
            hovertemplate = paste('<i><b>',input$ynum,'<b></i>: %{y}',
                                  '<br><b>',input$xnum,'</b>:  %{x}',
                                  '<br><b>',input$way,'</b>'),
            alpha  = 0.9,
            size = ~workDat()[[input$ynum]],
            type = "scatter",
            mode = "markers")   %>%
      layout(
        title = paste( input$xnum, "vs", input$ynum), 
        plot_bgcolor = "#e5ecf6",
        margin = list(l = 20, r = 20, b = 80, t = 30),
        xaxis = list(title = paste(input$xnum,'()')), 
        yaxis = list(title = paste(input$ynum,'()')), 
        legend = list(
          title=list(text='<b> Way </b>'),
          orientation = "h",   
          xanchor = "center", 
          x = 0.5)
      )
  })
  
  #This is the prediction Plot
  
  output$pred1<- renderPlot({
    print(input$xnum)
    print(input$ynum)
    print(workDat())
    par(bg="white")
    dataset = workDat()[,-1]
    
    m3 = lm(dataset[,input$ynum] ~ dataset[,input$xnum])
    
    pred.y = coef(m3)[1] + coef(m3)[2]*input$newX
    
    plot(dataset[,input$xnum], dataset[,input$ynum], 
         xlab = input$xnum,
         ylab = input$ynum,
         main = paste("Relationship between", input$ynum, "and", input$xnum)
    )
    abline(m3,
           col = "red",
           lwd = 1,
           lty=2)
    points(input$newX, pred.y, pch = 19, col = "red", cex = 2)
  })
  
  #Regression Coefficients
  
  output$reg1<-renderTable({
    br()
    br()
    dataset = workDat()[,-1]
    
    m0 = lm(dataset[,input$ynum] ~ dataset[,input$xnum])
    
    regcoef = data.frame(coef(summary(m0)))
    
    regcoef$Pvalue = regcoef[,names(regcoef)[4]]
    
    regcoef$Variable = c("Intercept", input$X)
    regcoef[,c(6, 1:3, 5)]
    
  })
  
  #Regression Diagnostics
  
  output$diag1<-renderPlot({
    par(bg="white")
    dataset = workDat()[,-1]   # define the working data set
    #####
    m1=lm(dataset[,input$ynum] ~ dataset[,input$xnum])
    par(mfrow=c(2,2))
    plot(m1)
  })   
  
  # Box Plot!
  
  output$box1<- renderPlot({
    ggplot(data=workDat(), aes(x=workDat()[[input$xcat]], y=workDat()[[input$ycat]])) +
      geom_boxplot(aes(fill=workDat()[[input$xcat]])) +  
      ylab(input$ycat) + 
      xlab(input$xcat) + 
      ggtitle(paste("Boxplot of the categorical variable: ", input$xcat )) +
      labs(fill = "Legend") +
      stat_summary(fun=mean, geom="point", shape=5, size=4)  
  })
  
  #ANOVA!
  
  anova <- reactive({
    req(input$ycat, input$xcat)
    aov(as.formula(paste(input$ycat, "~", input$xcat)), data = workDat())
  })
  
  output$anova1 <- renderPrint({
    summary(anova())
  })
 
  #sxr table
  
  output$table1<-renderTable({
    table(workDat()[[input$xcat]],turnover$event)
  })
  
  #Chi-Squared test
  
  output$chi1<-renderPrint({
    chisq.test(table(workDat()[[input$xcat]],turnover$event))
  })
   
 #Pie chart!
  
  output$pie1<-renderPlot({
    mytable<-table(workDat()[[input$pie]])
    labels<-paste(names(mytable),"\n",mytable,sep="")
    pie(mytable, main=paste("Pie Chart of the categorical variable: ", input$pie))
  })
  
  
}
  
shinyApp(ui, server)

