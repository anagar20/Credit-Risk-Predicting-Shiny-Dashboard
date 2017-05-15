library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(party)
library(caret)
library(plotly)
library(curl)
library(RCurl)
library(e1071)
library(rpart)
library(ranger)

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Dashboard"
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Input", tabName = "Input", icon = icon("dashboard")),
                        menuItem("Summary", tabName = "Summary", icon = icon("th")),
                        menuItem("Visualizations", icon = icon("th"),
                                 menuSubItem("BarPlot", tabName = "BarPlot"),
                                 menuSubItem("ScatterPlot", tabName = "ScatterPlot"),
                                 menuSubItem("Histogram", tabName = "Histogram"),
                                 menuSubItem("Model", tabName = "Model")),
                        menuItem("Results", tabName = "Results", icon = icon("th"))
                      )
                    ),
                    dashboardBody(
                      # Boxes need to be put in a row (or column)
                      tabItems(
                        # First tab content
                        tabItem(tabName = "Input",
                                h2("Input Details"),
                                fluidRow(
                                  box(title = "Model Parameters",
                                      width = 3, background = "light-blue",
                                      radioButtons(inputId = "model",label=h5("Prediction Model"),
                                                   choices = c("rpart","ranger"),selected = "rpart"),
                                      numericInput("cv", label = h5("Cross Validations"), value = 3),
                                      numericInput("tl", label = h5("tuneLength"), value = 3)
                                  ),
                                  box(title = "Enter Loan Amount",
                                      height = 130,width = 3, background = "maroon",
                                      numericInput("loan_amnt", label = h6(""), value = 10000)
                                  ),
                                  box(title = "Select Intrest Rate Category",
                                      height = 130,width = 3, background = "olive",
                                      selectInput(inputId = "ir_cat",label=h6(""),
                                                  choices = c("0-8","8-11","11-13.5","13.5+","Missing"),selected = "8-11")
                                  ),
                                  box(title = "Select FICO Grade",
                                      height = 130,width = 3, background = "green",
                                      selectInput(inputId = "grade",label=h6(""),
                                                  choices = c("A","B","C","D","E",
                                                              "F","G"),selected = "C")
                                  ),
                                  box(title = "Select Employment Length",
                                      height = 130,width = 3, background = "purple",
                                      selectInput(inputId = "emp_cat",label=h6(""),
                                                  choices = c("0-2","2-4","4-6","6-10",
                                                              "10-15","15-30","30-45",
                                                              "45+","Missing"),selected = "6-10")
                                  ),
                                  box(title = "Select Home Ownership Type",
                                      height = 130,width = 3, background = "teal",
                                      selectInput(inputId = "home_ownership",label=h6(""),
                                                  choices = c("MORTGAGE","OTHER","OWN","RENT"),selected = "MORTGAGE")
                                  ),
                                  box(title = "Enter Annual Income",
                                      height = 130,width = 3, background = "blue",
                                      numericInput("annual_inc", label = h6(""), value = 100000)
                                  ),
                                  box(title = "Enter Customers Age",
                                      height = 130,width = 3, background = "orange",
                                      numericInput("age", label = h6(""), value = 37)
                                  ),
                                  box(title = "Submission",
                                      height = 90,width = 3, background = "red",
                                      actionButton("submit","Submit")
                                  )
                                )
                         ),
                       
                        tabItem(tabName = "BarPlot",
                                h2("Barplots"),
                                fluidRow(
                                  box(title = "Bar Plots",  width = 9,  background = "red",
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      plotlyOutput("view1")
                                  ),
                                  box(title = "X-Axis Variable",  background = "black", width = 3, height=120,
                                      status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                      selectInput(inputId = "two",label=h6(""),
                                                  choices = c("ir_cat","emp_cat","home_ownership","loan_status","grade"),selected = "grade")
                                  ),
                                  box(title = "Fill Variable",  width = 3,  background = "red", height=120,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      selectInput(inputId = "one",label=h6(""),
                                                  choices = c("ir_cat","emp_cat","home_ownership","loan_status","grade"),selected = "grade")
                                  ),
                                  box(title = "Opacity Control",  background = "black", width = 3,  height=140,
                                      status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                      sliderInput("slider1", "Opacity:", 0, 1, 0.85)
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "ScatterPlot",
                                h2("ScatterPlots"),
                                fluidRow(
                                  box(title = "Scatter Plots",  width = 9,  background = "red",
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      plotlyOutput("view2")
                                  ),
                                  box(title = "X Axis Variable",  width = 3,  background = "red", height=110,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      selectInput(inputId = "three",label=h6(""),
                                                  choices = c("age","loan_amnt","annual_inc"),selected = "age")
                                  ),
                                  box(title = "Y Axis Variable",  width = 3,  background = "red", height=110,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      selectInput(inputId = "four",label=h6(""),
                                                  choices = c("age","loan_amnt","annual_inc","ir_cat",
                                                              "emp_cat","home_ownership","loan_status","grade"),selected = "annual_inc")
                                  ),
                                  box(title = "Fill Variables",  width = 3,  background = "red", height=120,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      selectInput(inputId = "five",label=h6(""),
                                                  choices = c("ir_cat","emp_cat","home_ownership","loan_status","grade"),selected = "ir_cat")
                                  ),
                                  box(title = "Control",  background = "black", width = 3, height=270,
                                      status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                      sliderInput("slider4", label=h5("Dataset Length"), 0, 29091, 4500),
                                      sliderInput("slider2", label=h5("Opacity"), 0, 1, 0.85)
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "Histogram",
                                h2("Histograms"),
                                fluidRow(
                                  box(title = "Histogram",  width = 9,  background = "red",
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      plotlyOutput("view3")
                                  ),
                                  box(title = "X Axis Variable",  width = 3,  background = "red", height=120,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      selectInput(inputId = "six",label=h6(""),
                                                  choices = c("age","loan_amnt","annual_inc"),selected = "age")
                                  ),
                                  box(title = "Opacity Control",  background = "black", width = 3, height=135,
                                      status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                      sliderInput("slider3", "Opacity:", 0, 1, 0.85)
                                  ),
                                  box(title = "Fill Variable",  width = 3,  background = "red", height=120,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      selectInput(inputId = "seven",label=h6(""),
                                                  choices = c("ir_cat","emp_cat","home_ownership","loan_status","grade"),selected = "home_ownership")
                                  )
                                )
                        ),
                        tabItem(tabName = "Model",
                                h2("Model"),
                                fluidRow(
                                  box(title = "Bar Plots",  width = 9,  background = "red",
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      plotOutput("view4")
                                  )
                                )
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "Summary",
                                h2("Summary"),
                                fluidRow(
                                  box(title = "Dataset Summary", background = "purple",
                                      width = 10,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      verbatimTextOutput("summary1")
                                  ),
                                  box(title = "Model Summary", background = "purple",width = 7, 
                                      status = "primary",solidHeader = TRUE, collapsible = TRUE,
                                      verbatimTextOutput("summary")
                                  )
                                )
                        ),
                        tabItem(tabName = "Results",
                                h2("Prediction"),
                                fluidRow(
                                  box(title = "Entered Observation", background = "orange",width = 8,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      tableOutput("table")
                                  ),
                                  box(title = "Prediction", background = "red", 
                                      status = "primary",  solidHeader = TRUE, collapsible = TRUE,
                                      verbatimTextOutput("pred")
                                  )
                                )
                        )
                      )
                    )
)

server <- function(input, output) {
  datainput = reactive({
    
    loan_data <- read.csv(curl("https://github.com/anagar20/Credit-Risk-Modeling/raw/master/loan_data.csv"))
    index_highage=which(loan_data$age>122)
    new_data <- loan_data[-index_highage, ]
    loan_data = new_data
    
    
    loan_data$emp_cat <- rep(NA, length(loan_data$emp_length))
    loan_data$emp_cat[which(loan_data$emp_length <= 2)] <- "0-2"
    loan_data$emp_cat[which(loan_data$emp_length > 2 & loan_data$emp_length <= 4)] <- "2-4"
    loan_data$emp_cat[which(loan_data$emp_length > 4 & loan_data$emp_length <= 6)] <- "4-6"
    loan_data$emp_cat[which(loan_data$emp_length > 6 & loan_data$emp_length <= 10)] <- "6-10"
    loan_data$emp_cat[which(loan_data$emp_length > 10 & loan_data$emp_length <= 15)] <- "10-15"
    loan_data$emp_cat[which(loan_data$emp_length > 15 & loan_data$emp_length <= 30)] <- "15-30"
    loan_data$emp_cat[which(loan_data$emp_length > 30 & loan_data$emp_length <= 45)] <- "30-45"
    loan_data$emp_cat[which(loan_data$emp_length > 45)] <- "45+"
    loan_data$emp_cat[which(is.na(loan_data$emp_length))] <- "Missing"
    loan_data$emp_cat <- as.factor(loan_data$emp_cat)
    loan_data$emp_length=NULL
    
    
    loan_data$ir_cat <- rep(NA, length(loan_data$int_rate))
    loan_data$ir_cat[which(loan_data$int_rate <= 8)] <- "0-8"
    loan_data$ir_cat[which(loan_data$int_rate > 8 & loan_data$int_rate <= 11)] <- "8-11"
    loan_data$ir_cat[which(loan_data$int_rate > 11 & loan_data$int_rate <= 13.5)] <- "11-13.5"
    loan_data$ir_cat[which(loan_data$int_rate > 13.5)] <- "13.5+"
    loan_data$ir_cat[which(is.na(loan_data$int_rate))] <- "Missing"
    loan_data$ir_cat <- as.factor(loan_data$ir_cat) 
    loan_data$int_rate=NULL
    
    loan_data$loan_status = as.factor(loan_data$loan_status)
    loan_data
  })
  
  test = reactive({
    if (input$submit > 0) {
      df <- data.frame(loan_amnt = as.integer(input$loan_amnt),grade = (input$grade),
                       home_ownership = (input$home_ownership),
                       annual_inc = (input$annual_inc),age = as.integer(input$age),
                       emp_cat = (input$emp_cat),ir_cat = (input$ir_cat))
      return(list(df=df))
    }
    
  })
  
  fn2 = reactive({
    train = (datainput())
    train_x = train[ ,-1]
    train_y = train[ , 1]
    myFolds <- createFolds(train_y, k = input$cv)
    
    myControl = trainControl(
      method = "cv",
      verboseIter = TRUE,
      index = myFolds
    )
    
    model <- train(
      x=train_x, y=train_y,
      method = input$model,
      tuneLength = input$tl,
      trControl = myControl,
      preProcess = c("zv","center","scale","pca")
    )
    
    model
  })
  
  new_theme = theme(panel.background = element_blank(),
                    axis.line.x   = element_line(color='black'),
                    axis.line.y   = element_line(color='black'),
                    axis.ticks    = element_line(color='black'),
                    axis.title.x  = element_text(family="Times",face = "bold", size = 12),
                    axis.title.y  = element_text(family="Times",face = "bold", size = 12),
                    axis.text     = element_text(family="Trebuchet MS",face = "italic", size = 10),
                    legend.title  = element_text(family="Times",face = "bold", size = 8),
                    legend.text   = element_text(family="Trebuchet MS",face = "italic", size = 8))

  
  output$view1 = renderPlotly({
    if (input$submit > 0) {
      #barplot(table(datainput()$Sex), main="BarPlot - Sex Distribution", names.arg = c("Female","Male"),col="grey")
      p1 = ggplot(datainput(), aes_string(x = input$two, fill = input$one)) +
        geom_bar(alpha = input$slider1) + theme_igray()+ new_theme
      ggplotly(p1) 
    }
    else{
      NULL
    }
  })
  
  output$view2 = renderPlotly({
    if (input$submit > 0) {
      data = datainput()
      dat = data[1:input$slider4, ]
      #barplot(table(datainput()$Sex), main="BarPlot - Sex Distribution", names.arg = c("Female","Male"),col="grey")
      p2 = ggplot(dat, aes_string(x = input$three,y =input$four,  col = input$five)) +
        geom_point(alpha = input$slider2) + theme_igray()+ new_theme
      ggplotly(p2) 
    }
    else{
      NULL
    }
  })
  
  output$view3 = renderPlotly({
    if (input$submit > 0) {
      #barplot(table(datainput()$Sex), main="BarPlot - Sex Distribution", names.arg = c("Female","Male"),col="grey")
      p3 = ggplot(datainput(), aes_string(x = input$six, fill = input$seven)) +
        geom_histogram(alpha = input$slider3) + theme_igray()+ new_theme
      ggplotly(p3) 
    }
    else{
      NULL
    }
  })
  
  output$view4 = renderPlot({
    if (input$submit > 0) {
      model = fn2()
      plot(model)
    }
    else{
      NULL
    }
  })
  
  
  output$summary1 = renderPrint({
    if (input$submit > 0) {
      summary(datainput())
    }
    else{
      NULL
    }
  })
  
  output$table = renderTable({
    if (input$submit > 0) {
      test()$df
    }
    else{
      NULL
    }
  })
 
  output$pred <- renderPrint({
    if (input$submit > 0) {
      test = test()$df
      loan_data = datainput()
      loan_data_1 = loan_data[ ,c(2,3,4,5,6,7,8)]
      ww =(rbind(loan_data_1,test))
      test = ww[29092, ]
      model = fn2()
      pp = predict(model,test)
      if(pp==0) {
        print('Loan will not default')
      }
      else{
        print('Loan will default')
      }
    }
    else{
      NULL
    }
  })
  
  output$summary <- renderPrint({
    if (input$submit > 0) {
      (fn2())
    }
    
    else{
      NULL
    }
    
  })
}

shinyApp(ui, server)