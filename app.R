#Name of the app goes here
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(resampledata)   #datasets to accompany Chihara and Hesterberg
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-Shelf: 24px;
    }
  ')
))
#Load the dataset dset[,c(1,3,8,9)]

#dset <- get("Alelager")
dset <- read.csv(url("https://raw.githubusercontent.com/STAT-JET-ASU/Datasets/master/Chihara/Cereals.csv"))
dset <- dset[,c(2,3,4,5)]
#Clean up the dataset if necessary


#The user interface
header <- dashboardHeader(title = "Permutation testing for Cereals",
                          titleWidth = 500)
sidebar <- dashboardSidebar(width = 200,
  radioButtons("beerhot", "Stores",choices = c("Sodiumgram","Proteingram")),
  actionBttn("btnnew","New permuted dataset"),
  h4("Using Cereals dataset")
  
  
)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width = 3,
           h3("The dataset"),
           tableOutput("tbl")  #display the data set
    ),
    column(width = 3,
           h3("A permuted dataset"),
           tableOutput("scramtbl")
          
    ),
    column(width = 3,
           h3("Analysis of actual dataset"),
           plotOutput("trueplot"),
           uiOutput("truediff"),
           uiOutput("trueratio"),
           uiOutput("truemed"),
           h3("Analysis of permuted dataset"),
           plotOutput("scrambleplot"),
           uiOutput("scramdiff"),
           uiOutput("scramratio"),
           uiOutput("scrammed")
    ),
    column(width = 3,
      h3("Permutation test"),
      sliderInput("nsample","Number of permuted samples",1000,20000,5000),
      radioButtons("btnstat","Statistic",
                   choiceNames = c("Difference of means","Ratio of means","Difference of medians"),
                   choiceValues = c("diff","ratio","mdiff")),
      actionBttn("btntest","Conduct the test"),
      plotOutput("hist"),
      uiOutput("pval")
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "purple") #other colors available


#Additional functions are OK here, but no variables
doPlot <- function(dset, consume) {
  formula <- as.formula(paste(consume,"~","Age"))
  boxplot(formula, dset)
}
scramble <- function(dset){
  dset$Age <- sample(dset$Age)
  return(dset)
}

doPermtest <- function(dset,consume,nPerm,stat){
  result <- numeric(nPerm)
  for (i in 1:nPerm){
    permset <- scramble(dset)
    men <- which(permset$Age == "F")
    result[i] <- switch(stat,
      diff =mean(permset[men,consume])-mean(permset[-men,consume]),
      ratio = mean(permset[,consume][men])/mean(permset[,consume][-men]),
      mdiff = median(permset[,consume][men])-median(permset[,consume][-men])
    )
  }
  return (result)
}

dset
result <- doPermtest(dset,"Sodiumgram",10,"mdiff");result


server <- function(session, input, output) {
  consume <- "Sodiumgram"
  output$tbl <- renderTable(dset)
  permset <- scramble(dset)
  output$scramtbl <- renderTable(permset)
  analyze <- function(dset,consume,realdata){
    if (realdata){
      output$trueplot <- renderPlot({doPlot(dset,consume)})
      men <- which(dset$Age == "F")
      output$truediff <- renderUI(h4(paste("Difference in means =",mean(dset[men,consume])-mean(dset[-men,consume]))))
      output$trueratio <- renderUI(h4(paste("Ratio of means =",
                                    round(mean(dset[men,consume])/mean(dset[-men,consume]),digits =2))))
      output$truemed <- renderUI(h4(paste("Difference in medians =",
                                    median(dset[men,consume])-median(dset[-men,consume]))))
    }
    else {
      output$scrambleplot <- renderPlot({doPlot(dset,consume)})
      men <- which(dset$Age == "F")
      output$scramdiff <- renderUI(h4(paste("Difference in means =",
                                            mean(dset[men,consume])-mean(dset[-men,consume]))))
      output$scramratio <- renderUI(h4(paste("Ratio of means =",
                                          round(mean(dset[men,consume])/mean(dset[-men,consume]),digits =2))))
      output$scrammed <- renderUI(h4(paste("Difference in medians =",
                                        median(dset[men,consume])-median(dset[-men,consume]))))
    }
  }
  analyze(dset,consume,TRUE)
  analyze(permset,consume,FALSE)
  
  observeEvent(input$beerhot,{
    consume <<- input$beerhot
    analyze(dset,consume,TRUE)
    analyze(permset,consume,FALSE)
  })
  observeEvent(input$btnnew,{
    permset <<- scramble(dset)
    output$scramtbl <- renderTable(permset)
    analyze(permset,consume,FALSE)
  })
  
  observeEvent(input$btntest,{
    result <- doPermtest(dset,consume,input$nsample,input$btnstat)
    men <- which(dset$Age == "F")
    vline <- switch(input$btnstat,
                    diff = mean(dset[men,consume])-mean(dset[-men,consume]),
                    ratio = mean(dset[men,consume])/mean(dset[-men,consume]),
                    mdiff = median(dset[men,consume])-median(dset[-men,consume])
    )
    output$hist <- renderPlot({
      hist(result)
      abline(v = vline, col = "blue")
    })
    pvalue <- (sum(result >= vline )+1)/(input$nsample+1)
    output$pval <- renderUI(h3(paste("Pvalue =", pvalue)))
  })
  
  

}

#Run the app
shinyApp(ui = ui, server = server)