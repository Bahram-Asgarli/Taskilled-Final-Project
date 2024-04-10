library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(skimr)
library(car)
library(h2o)
library(rlang)
library(glue)
library(highcharter)
library(lime)
library(inspectdf)
library(DataExplorer)
library(hrbrthemes)
library(corrplot)
library(ggplot2)
library(plotly)
library(gridExtra)
library(reticulate)
library(naniar)
library(dplyr)
library(tidyverse)
library(ROCR)
library(caret)
library(renv)
library(rsconnect)
library(XML)
library(shinyWidgets)

dataset <- read.csv("telco-customer-churn.csv")

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Menu",
    titleWidth = 230
  ),
  dashboardSidebar(
    tags$style(HTML("
      .sidebar-menu {
        padding-top: 15px;
      }
      .sidebar-menu> {
            color: #605ca8;
          }
          .sidebar-menu>li>a:hover {
            background-color: #605ca8 !important;
            color: white !important;
          }
    ")),
    sidebarMenu(
      menuItem("Main Page", tabName = "Main_Page", icon = icon("home", style = "padding-right: 2px;")),
      menuItem("Dataset", tabName = "Dataset", icon = icon("database", style = "padding-right: 4px;")),
      menuItem("Data Preparation", tabName = "Data_Preparation", icon = icon("cogs", style = "padding-right: 1px;")),
      menuItem("Explorative Data Analysis", tabName = "Explorative_Data_Analysis", icon = icon("columns", style = "padding-right: 4px;")),
      menuItem("AutoML", tabName = "AutoML", icon = icon("bullseye", style = "padding-right: 4px;"),
               menuSubItem("AUC and Gini", tabName = "AUC_and_Gini"),
               menuSubItem("Confusion Matrix", tabName = "Confusion_Matrix"),
               menuSubItem("Variable Importance", tabName = "Variable_importance"),
               menuSubItem("Evaluation Metrics", tabName = "Evaluation_Metrics")
      ),
      menuItem("Prediction", tabName = "Prediction", icon = icon("rocket", style = "padding-right: 4px;")
      )
    )
  ),
  dashboardBody(
    tags$style(
      HTML("
        .welcome {
          background-image: url('https://static.vecteezy.com/system/resources/previews/008/481/533/original/business-graph-with-arrow-and-target-board-for-data-analysis-3d-render-png.png');
          background-size: 700px;
          background-repeat: no-repeat;
          background-color: #57549b;
          background-position: calc(75% - 10px);
          height: 100vh;
          width: 100vw;
          display: flex;
          align-items: center; /* Align to the top */
          justify-content: flex-start; /* Align to the left */
          color: white;
          font-family: 'Red Rose', bold sans-serif;
          font-size: 80px;
          padding-left: 3%;
          padding-right: 10%;
          position: relative;
        }
        .metric-box {
          background-color: #4CAF50;
          color: white;
          padding: 20px;
          margin: 20px;
          border-radius: 10px;
          text-align: center;
          font-size: 20px;
        }
        .about-project {
          font-size: 50px; /* Set font size to 50 */
          font-weight: bold; /* Make text bold */
          padding-top: 50px; /* Add top padding */
        }
        .about-project-description {
          font-size: 30px; /* Set font size to 50 */
        }
        .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #57549b}
        .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #57549b}
        .js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #57549b}
      ")
    ),
    tabItems(
      tabItem(
        tabName = "Main_Page",
        tags$div(
          class = "welcome",
          "Telecommunication", br(),
          "Customer Churn", br(),
          "Analysis"
        ),
        tags$div(
          class = "about-project",
          "ABOUT PROJECT"
        ),
        tags$div(
          class = "about-project-description",
          "The purpose of this case study is to explore the Telco Customer Churn dataset and build a predictive model that can accurately identify customers who are likely to churn. The case study aims to demonstrate the importance of customer churn prediction for telecom companies and how it can impact their revenue and customer retention strategies. Furthermore, the case study aims to provide insights into the factors that influence customer churn and provide recommendations for telecom companies to reduce churn and improve customer retention strategies."
        ),
        tags$div(
          class = "about-project",
          "Significance"
        ),
        tags$div(
          class = "about-project-description",
          "The results of this case study have significant implications for telecom companies looking to reduce customer churn and improve customer retention strategies. The predictive model developed in this case study can help telecom companies identify customers who are at high risk of churning and take proactive measures to prevent them from leaving. Furthermore, the insights into the factors that influence customer churn can help telecom companies develop targeted retention strategies that address the specific needs of at-risk customers. Ultimately, reducing customer churn can lead to increased revenue, customer loyalty, and a stronger reputation for telecom companies."
        )
      ),
      tabItem(
        tabName = "Dataset",
        fluidRow(
          column(width = 12,
                 DTOutput("datatable"),
                 br(),
                 downloadButton("download_csv", label = "Download as CSV"),
                 downloadButton("download_excel", label = "Download as Excel")
          )
        )
      ),
      tabItem(
        tabName = "Data_Preparation",
        fluidRow(
          column(width = 6,
                 plotlyOutput("imbalance_plot"),
                 style = "margin-top: 80px; height = 360px; "
          ),
          column(width = 6,
                 selectInput("outlier_var", "Select Variable", choices = c("TotalCharges", "MonthlyCharges", "tenure")),
                 conditionalPanel(
                   condition = "input.outlier_var == 'TotalCharges'",
                   plotlyOutput("outlier_plot_total")
                 ),
                 conditionalPanel(
                   condition = "input.outlier_var == 'MonthlyCharges'",
                   plotlyOutput("outlier_plot_month")
                 ),
                 conditionalPanel(
                   condition = "input.outlier_var == 'tenure'",
                   plotlyOutput("outlier_plot_tenure")
                 )
          ),
          column(width = 12,
                 plotlyOutput("na_plot", height = "250px"),
                 style = "margin-top: 30px;"
          )
        )
      ),
      tabItem(
        tabName = "AUC_and_Gini",
        fluidRow(
          style = "padding-top: 50px;",
          column(6,
                 highchartOutput("auc_plot")),
          column(6,
                 highchartOutput("gini_plot"))
        )
      ),
      tabItem(tabName = "Confusion_Matrix",
              fluidRow(
                style = "padding-top: 50px;",
                column(12,
                       plotOutput("confusion_plot", width = "100%", height = "600px"))
              )
      ),
      tabItem(tabName = "Variable_importance",
              fluidRow(
                style = "padding-top: 50px;",
                column(12,
                       highchartOutput("variable_importance_plot", width = "100%", height = "600px"))
              )
      ),
      tabItem(
        tabName = "Evaluation_Metrics",
        fluidRow(
          div(style = "padding-left: 15px; padding-right: 15px; background-color: #605ca8;",
              verbatimTextOutput("threshold_summary")
          ),
          valueBoxOutput("precision"),
          valueBoxOutput("recall_sensitivity"),
          valueBoxOutput("specificity"),
          valueBoxOutput("accuracy"),
          valueBoxOutput("f1_score"),
          valueBoxOutput("balanced_accuracy")
        )
      ),
      tabItem(
        tabName = "Explorative_Data_Analysis",
        fluidRow(
          fluidRow(
            column(width = 6, plotOutput("correlation_plot")),
            column(width = 6, plotOutput("churn_plots"))
          ),
          fluidRow(
            column(width = 6,
                   plotOutput("churnvs_plot"),
                   style = "margin-top: 50px;" # Output for ggplot churn vs. plots
            ),
            column(width = 6,
                   div(
                     style = "padding-top: 20px;",
                     selectInput("histogram_selector",
                                 "Select Histogram:",
                                 choices = c("Tenure", "Monthly Charges", "Total Charges"))
                   ),
                   conditionalPanel(
                     condition = "input.histogram_selector == 'Tenure'",
                     plotlyOutput("tenure_histogram")
                   ),
                   conditionalPanel(
                     condition = "input.histogram_selector == 'Monthly Charges'",
                     plotlyOutput("monthly_charges_histogram")
                   ),
                   conditionalPanel(
                     condition = "input.histogram_selector == 'Total Charges'",
                     plotlyOutput("total_charges_histogram")
                   )
            )
          )
        )
      ),
      tabItem(tabName = "Prediction",
              h2("Predict Data"),
              fluidPage(
                column(width = 4,
                       radioGroupButtons(
                         inputId = "gender",
                         label = "Gender",
                         choices = unique(dataset[["gender"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       ),
                       radioGroupButtons(
                         inputId = "SeniorCitizen",
                         label = "SeniorCitizen",
                         choices = unique(dataset[["SeniorCitizen"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       ),
                       radioGroupButtons(
                         inputId = "Partner",
                         label = "Partner",
                         choices = unique(dataset[["Partner"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       ),
                       radioGroupButtons(
                         inputId = "Dependents",
                         label = "Dependents",
                         choices = unique(dataset[["Dependents"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       ),
                       radioGroupButtons(
                         inputId = "PhoneService",
                         label = "PhoneService",
                         choices = unique(dataset[["PhoneService"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       ),
                       radioGroupButtons(
                         inputId = "MultipleLines",
                         label = "MultipleLines",
                         choices = unique(dataset[["MultipleLines"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       ),
                       radioGroupButtons(
                         inputId = "InternetService",
                         label = "InternetService",
                         choices = unique(dataset[["InternetService"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       ),
                       radioGroupButtons(
                         inputId = "OnlineSecurity",
                         label = "OnlineSecurity",
                         choices = unique(dataset[["OnlineSecurity"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       )
                ),
                column(width = 4,
                       radioGroupButtons(
                         inputId = "OnlineBackup",
                         label = "OnlineBackup",
                         choices = unique(dataset[["OnlineBackup"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       ),
                       radioGroupButtons(
                         inputId = "DeviceProtection",
                         label = "DeviceProtection",
                         choices = unique(dataset[["DeviceProtection"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       ),
                       radioGroupButtons(
                         inputId = "TechSupport",
                         label = "TechSupport",
                         choices = unique(dataset[["TechSupport"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       ),
                       radioGroupButtons(
                         inputId = "StreamingTV",
                         label = "StreamingTV",
                         choices = unique(dataset[["StreamingTV"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       ),
                       radioGroupButtons(
                         inputId = "StreamingMovies",
                         label = "StreamingMovies",
                         choices = unique(dataset[["StreamingMovies"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       ),
                       radioGroupButtons(
                         inputId = "Contract",
                         label = "Contract",
                         choices = unique(dataset[["Contract"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       ),
                       radioGroupButtons(
                         inputId = "PaperlessBilling",
                         label = "PaperlessBilling",
                         choices = unique(dataset[["PaperlessBilling"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       ),
                       radioGroupButtons(
                         inputId = "PaymentMethod",
                         label = "PaymentMethod",
                         choices = unique(dataset[["PaymentMethod"]]),
                         individual = TRUE,
                         direction = "vertical",
                         selected = "",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-circle", 
                                        style = "color: #57549b"),
                           no = tags$i(class = "fa fa-circle-o", 
                                       style = "color: #57549b"))
                       )
                ),
                column(width = 4,
                       sliderInput("tenure", "Tenure",
                                   min = 0,
                                   max = 80,
                                   value = 0
                       ),
                       sliderInput("MonthlyCharges", "MonthlyCharges",
                                   min = 0,
                                   max = 150,
                                   value = 0
                       ),
                       sliderInput("TotalCharges", "TotalCharges",
                                   min = 0,
                                   max = 9000,
                                   value = 0
                       ),
                       
                       mainPanel(
                         fluidRow(
                           actionButton("predict_button", "Predict", style = "width: 130%; background-color: #605ca8; color: white;")
                         ),
                         fluidRow(
                           tableOutput("predicted_result")
                         )
                       )
                )
                
              )
      )
    )
  ),
  skin = "purple" # Change the dashboard skin color
)

server <- function(input, output, session) {
  
  dataset <- reactive({
    dataset <- read.csv("telco-customer-churn.csv")
    
    dataset
  })
  
  
  data <- reactive({
    data<- read.csv("data_copy.csv")
    
    data
  })
  
  # Render the dataset when the "Dataset" tab is active
  output$datatable <- renderDT({
    datatable(data(), options = list(scrollX = TRUE))
  })
  
  # Download data as CSV
  output$download_csv <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(dataset(), file, row.names = FALSE)
    }
  )
  
  # Download data as Excel
  output$download_excel <- downloadHandler(
    filename = "data.xlsx",
    content = function(file) {
      write.xlsx(dataset(), file)
    }
  )
  
  
  # Data Preparation Tab
  
  # Convert columns to factor
  
  # Recode target column
  data_with_factor <- reactive({
    data_temp <- na.omit(dataset())
    data_temp$Churn = recode(data_temp$Churn, 'Yes' = 1, 'No' = 0) %>% as.factor()
    data_temp$SeniorCitizen = recode(data_temp$SeniorCitizen, '1' = 'Yes', '0' = 'No') %>% as.factor()
    data_temp <- data_temp %>% mutate_if(is.character, as.factor)
    
    data_with_factor <- data_temp
    
    data_with_factor
  })
  
  data1 <- reactive({
    data1 <- data_with_factor()
    
    data1
  })
  
  # Check for missing values
  columns_to_visualize <- reactive({
    columns_to_visualize <- c("Churn", "TotalCharges", "MonthlyCharges", "tenure", "Contract")
    
    columns_to_visualize
  })
  
  existing_columns <- reactive({
    existing_columns <- intersect(columns_to_visualize(), colnames(data()))
    
    existing_columns
  })
  
  output$na_plot <- renderPlotly({
    plot_missing_custom <- dataset()[, columns_to_visualize()] %>% plot_missing()
    plot_missing_custom <- plot_missing_custom +
      theme_minimal() +  # Example: Adding a theme
      scale_fill_manual(values = c("#605ca8"))  # Customizing fill color
    ggplotly(plot_missing_custom)
  })
  
  # Interactive plot for outliers
  
  num_vars <- reactive({
    num_vars <- data1() %>% select_if(is.numeric)
    
    num_vars
  })
  
  target <- reactive({
    target <- "Churn"
    
    target
  })
  
  output$outlier_plot_total <- renderPlotly({
    # Assuming you have a numerical column named "TotalCharges"
    p <- ggplot(data1(), aes(x = "", y = TotalCharges)) +
      geom_boxplot(color="#222d32",fill="#605ca8") +
      theme_minimal() +
      labs(y = "Total Charges", title = "Outliers Detection") +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    
    ggplotly(p)
  })
  
  # Interactive plot for outliers
  output$outlier_plot_month <- renderPlotly({
    # Assuming you have a numerical column named "MonthlyCharges"
    p <- ggplot(data1(), aes(x = "", y = MonthlyCharges)) +
      geom_boxplot(color="#222d32",fill="#605ca8") +
      theme_minimal() +
      labs(y = "Monthly Charges", title = "Outliers Detection") +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    
    ggplotly(p)
  })
  
  # Interactive plot for outliers
  output$outlier_plot_tenure <- renderPlotly({
    # Assuming you have a numerical column named "tenure"
    p <- ggplot(data1(), aes(x = "", y = tenure)) +
      geom_boxplot(color="#222d32",fill="#605ca8") +
      theme_minimal() +
      labs(y = "tenure", title = "Outliers Detection") +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    
    ggplotly(p)
  })
  
  
  # Interactive plot for imbalance in target column
  output$imbalance_plot <- renderPlotly({
    data1() %>%
      ggplot(aes(x = factor(Churn))) +
      geom_bar(fill = "#555299") +
      theme_minimal() +
      labs(title = "Imbalance in Target Column")
  })
  
  output$tenure_histogram <- renderPlotly({
    p <- ggplot(data1(), aes(x = tenure)) +
      geom_histogram(binwidth = 5, color = "white", fill = "#605ca8") +
      labs(title = "Distribution of Tenure",
           x = "Tenure (Months)",
           y = "Frequency") +
      scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 5)) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Render histogram of Monthly Charges
  output$monthly_charges_histogram <- renderPlotly({
    p <- ggplot(data = data1(), aes(x = MonthlyCharges)) +
      geom_histogram(binwidth = 10, color = "white", fill = "#66bb6a") +
      labs(title = "Distribution of Monthly Charges",
           x = "Monthly Charges ($)",
           y = "Frequency") +
      scale_x_continuous(limits = c(0, 130), breaks = seq(0, 150, 20)) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Render histogram of Total Charges
  output$total_charges_histogram <- renderPlotly({
    p <- ggplot(data = data1(), aes(x = TotalCharges)) +
      geom_histogram(binwidth = 500, color = "white", fill = "#ef5350") +
      labs(title = "Distribution of Total Charges",
           x = "Total Charges ($)",
           y = "Frequency") +
      scale_x_continuous(limits = c(0, 8800), breaks = seq(0, 9000, 1000)) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$churn_plots <- renderPlot({
    g6 <- data1() %>% ggplot(aes(x=ifelse(SeniorCitizen=='Yes', "Senior", "Not Senior"), fill=fct_rev(Churn)))+  geom_bar(alpha=0.6) + labs(title="Customer Churn on Senior Citizens", y="Count of Senior Citizen")
    g7 <- data1() %>% ggplot(aes(x=gender, fill=fct_rev(Churn)))+  geom_bar(alpha=0.6) + labs(title="Customer Churn on Gender", y="Count of Gender")
    g8 <- data1() %>% ggplot(aes(x=Partner, fill=fct_rev(Churn))) + geom_bar(alpha=0.6) + labs(title="Customer Churn on Partner", y="Count of Partner")
    g9 <- data1() %>% ggplot(aes(x=Dependents, fill=fct_rev(Churn)))+  geom_bar(alpha=0.6) + labs(title="Customer Churn on Dependents", y="Count of Dependents")
    
    grid.arrange(g6, g7, g8, g9, ncol = 2)
  })
  
  output$churnvs_plot <- renderPlot({
    g10 <- data1() %>% ggplot(aes(x=PhoneService, fill=fct_rev(Churn)))+  geom_bar(alpha=0.6) + labs(title="Customer Churn on Phone Service", y="Count of Phone Service")
    g11 <- data1() %>% ggplot(aes(x=MultipleLines, fill=fct_rev(Churn)))+  geom_bar(alpha=0.6) + labs(title="Customer Churn on Multiple Lines", y="Count of Mulitple Lines")
    g12 <- data1() %>% ggplot(aes(x=OnlineSecurity, fill=fct_rev(Churn))) + geom_bar(alpha=0.6) + labs(title="Customer Churn on Online Security", y="Count of Online Security")
    g13 <- data1() %>% ggplot(aes(x=OnlineBackup, fill=fct_rev(Churn)))+  geom_bar(alpha=0.6) + labs(title="Customer Churn on Online Backup", y="Count of Online Backup")
    
    grid.arrange(g10, g11, g12, g13, ncol=2)
  })
  
  
  # Variable_importance
  output$variable_importance_plot <- renderHighchart({
    
    model() %>% h2o.varimp() %>%
      as.data.frame() %>%
      .[.$percentage != 0,] %>%
      select(variable, percentage) %>%
      hchart("pie", hcaes(x = variable, y = percentage)) %>%
      hc_colors(colors = "#605ca8") %>%
      hc_xAxis(visible=T) %>%
      hc_yAxis(visible=T)
  })
  
  
  # Initialize H2O
  h2o.init()
  
  model <- reactive({
    model <- h2o.loadModel("GBM_grid_1_AutoML_3_20240326_122010_model_11")
    
    model
  })
  
  h2o_data <- reactive({
    h2o_data <- data1() %>% as.h2o()
    
    h2o_data
  })
  
  # Datanın bölünməsi - Splitting the data
  
  test <- reactive({
    test <- read.csv("test_data.csv") %>% as.h2o()
    
    test
  })
  
  deep_metrics <- reactive({
    model() %>%
      h2o.performance(test()) %>%
      h2o.metric() %>%
      select(threshold,precision,recall,tpr,fpr) %>%
      add_column(tpr_r=runif(nrow(.),min=0.001,max=1)) %>%
      mutate(fpr_r=tpr_r) %>%
      arrange(tpr_r,fpr_r) -> deep_metrics
    
    deep_metrics
  })
  
  output$auc_plot <- renderHighchart({
    metrices <- model() %>%
      h2o.performance(test()) %>%
      h2o.metric() %>%
      select(threshold, precision, recall, tpr, fpr) %>%
      add_column(random_tpr = runif(nrow(.), min = 0.001, max = 1)) %>%
      mutate(random_fpr = random_tpr) %>%
      arrange(random_tpr, random_fpr)
    
    auc <- model() %>%
      h2o.performance(test()) %>%
      h2o.auc() %>%
      round(2)
    
    highchart() %>%
      hc_add_series(metrices, "scatter", hcaes(y = tpr, x = fpr), color = "green", name = "TPR") %>%
      hc_add_series(metrices, "line", hcaes(y = random_tpr, x = random_fpr), color = "orange", name = "Random guess") %>%
      hc_add_annotation(
        labels = list(
          point = list(xAxis = 0, yAxis = 0, x = 0.3, y = 0.6),
          text = glue("AUC = {enexpr(auc)}"))
      ) %>%
      hc_subtitle(text = "The model performs better than a random guess")
    
  })
  
  # Calculate Gini coefficient 
  output$gini_plot <- renderHighchart({
    auc <- model() %>%      h2o.performance(test()) %>%
      h2o.auc() %>%      round(2)
    gini <- 2 * auc - 1  # Calculate Gini coefficient inside renderHighchart
    highchart() %>%
      hc_add_series(deep_metrics(), 
                    "scatter", 
                    hcaes(y = tpr, x = fpr), 
                    color = 'green', name = 'TPR') %>%
      hc_add_series(deep_metrics(), 
                    "line", 
                    hcaes(y = tpr_r, x = fpr_r), 
                    color = 'red', name = 'Random Guess') %>%
      hc_add_annotation(labels = list(
        point = list(xAxis = 0, yAxis = 0, x = 0.3, y = 0.6), text = glue('Gini = {enexpr(gini)}'))  # Display Gini coefficient instead of AUC
      ) %>% hc_title(text = "Calculation of Gini coefficient") %>%
      hc_subtitle(text = "Model is performing much better than random guessing")  })
  
  
  # Treshold
  threshold <- reactive({
    threshold <- model() %>%
      h2o.performance(test()) %>%
      h2o.find_threshold_by_max_metric('f1')
    
    threshold
  })
  
  output$threshold_summary <- renderText({
    paste("Threshold Value:", round(threshold(), 2))
  })
  
  
  
  # Confusion Matrix
  output$confusion_plot <- renderPlot({
    
    model() %>%
      h2o.confusionMatrix(test()) %>%
      as_tibble() %>%
      select("0","1") %>%
      .[1:2,] %>% t() %>%
      fourfoldplot(conf.level = 0, color = c("red", "darkgreen"),
                   main = paste("Accuracy = ",
                                round(sum(diag(.))/sum(.)*100,1),"%"))
  })
  
  # Calculate confusion matrix
  
  output$precision <- renderValueBox({
    pred <- model() %>%
      h2o.predict(test()) %>% as.data.frame() %>%
      select(p1, predict)
    paste("Maximum probability for class 0:", max(pred[pred$predict == 0, "p1"]), "\n",          "Minimum probability for class 1:", min(pred[pred$predict == 1, "p1"]))
    actuals <- test() %>% as_data_frame() %>% pull(target())
    predictions <- pred$predict
    cm <- table(actuals, predictions)
    tp <- cm[2, 2]
    tn <- cm[1, 1]
    fp <- cm[1, 2]
    fn <- cm[2, 1]
    precision <- tp / (tp + fp)
    valueBox(
      value = round(precision, 2),      subtitle = "Precision",
      icon = icon("crosshairs"),      color = "green"
    )  })
  output$recall_sensitivity <- renderValueBox({
    pred <- model() %>% h2o.predict(test()) %>%
      as.data.frame() %>% select(p1, predict)
    paste("Maximum probability for class 0:", max(pred[pred$predict == 0, "p1"]), "\n",
          "Minimum probability for class 1:", min(pred[pred$predict == 1, "p1"]))
    actuals <- test() %>% as_data_frame() %>% pull(target())
    predictions <- pred$predict
    cm <- table(actuals, predictions)
    tp <- cm[2, 2]
    tn <- cm[1, 1]
    fp <- cm[1, 2]
    fn <- cm[2, 1]
    recall_sensitivity <- tp / (tp + fn)
    valueBox(      value = round(recall_sensitivity, 2),
                   subtitle = "Recall Sensitivity",      icon = icon("bullseye"),
                   color = "red"    )
  })
  pred_df <- reactive({
    model() %>%
      h2o.predict(test()) %>% as.data.frame() %>%
      select(p1, predict)  })
  
  output$prediction_data <- renderDataTable({
    pred_df()  })
  
  output$specificity <- renderValueBox({
    pred <- model() %>% h2o.predict(test()) %>%
      as.data.frame() %>% select(p1, predict)
    paste("Maximum probability for class 0:", max(pred[pred$predict == 0, "p1"]), "\n",
          "Minimum probability for class 1:", min(pred[pred$predict == 1, "p1"]))
    actuals <- test() %>% as_data_frame() %>% pull(target())
    predictions <- pred$predict
    cm <- table(actuals, predictions)
    tp <- cm[2, 2]
    tn <- cm[1, 1]
    fp <- cm[1, 2]
    fn <- cm[2, 1]
    specificity <- tn / (tn + fn)
    valueBox(      value = round(specificity, 2),
                   subtitle = "Specificity",      icon = icon("crosshairs"),
                   color = "purple"    )
  })
  output$accuracy <- renderValueBox({
    pred <- model() %>%
      h2o.predict(test()) %>% as.data.frame() %>%
      select(p1, predict)
    paste("Maximum probability for class 0:", max(pred[pred$predict == 0, "p1"]), "\n",          "Minimum probability for class 1:", min(pred[pred$predict == 1, "p1"]))
    actuals <- test() %>% as_data_frame() %>% pull(target())
    predictions <- pred$predict
    cm <- table(actuals, predictions)
    tp <- cm[2, 2]
    tn <- cm[1, 1]
    fp <- cm[1, 2]
    fn <- cm[2, 1]
    accuracy <- (tp + tn) / (tp + tn + fp + fn)
    valueBox(      value = round(accuracy, 2),
                   subtitle = "Accuracy",      icon = icon("check-circle"),
                   color = "maroon"    )
  })
  output$f1_score <- renderValueBox({
    pred <- model() %>%
      h2o.predict(test()) %>% as.data.frame() %>%
      select(p1, predict)
    paste("Maximum probability for class 0:", max(pred[pred$predict == 0, "p1"]), "\n",          "Minimum probability for class 1:", min(pred[pred$predict == 1, "p1"]))
    actuals <- test() %>% as_data_frame() %>% pull(target())
    predictions <- pred$predict
    cm <- table(actuals, predictions)
    tp <- cm[2, 2]
    tn <- cm[1, 1]
    fp <- cm[1, 2]
    fn <- cm[2, 1]
    precision <- tp / (tp + fp)
    recall_sensitivity <- tp / (tp + fn)
    
    f1_score <- 2 * precision * recall_sensitivity / (precision + recall_sensitivity)
    valueBox(      value = round(f1_score, 2),
                   subtitle = "F1 Score",
                   icon = icon("balance-scale"),
                   color = "olive"
    )  })
  
  
  output$balanced_accuracy <- renderValueBox({
    pred <- model() %>%
      h2o.predict(test()) %>% as.data.frame() %>%
      select(p1, predict)
    paste("Maximum probability for class 0:", max(pred[pred$predict == 0, "p1"]), "\n",          "Minimum probability for class 1:",min(pred[pred$predict == 1, "p1"]))
    actuals <- test() %>% as_data_frame() %>% pull(target())
    predictions <- pred$predict
    cm <- table(actuals, predictions)
    tp <- cm[2, 2]
    tn <- cm[1, 1]
    fp <- cm[1, 2]
    fn <- cm[2, 1]
    recall_sensitivity <- tp / (tp + fn)
    specificity <- tn / (tn + fn)
    balanced_accuracy <- (recall_sensitivity + specificity) / 2
    valueBox(      value = round(balanced_accuracy, 2),
                   subtitle = "Balanced_accuracy",      icon = icon("check-circle"),
                   color = "blue"    )
  })
  
  
  
  
  
  # Explorative Data Analysis Tab
  
  # Compute correlation matrix after omitting missing values
  correlation_matrix <- reactive({
    correlation_matrix <- cor(na.omit(num_vars()))
    
    correlation_matrix
  })
  
  # Render correlation plot using corrplot
  output$correlation_plot <- renderPlot({
    corrplot(correlation_matrix(), addCoef.col = "white", order = "FPC",
             tl.col = "black",
             col = colorRampPalette(c("white", "#bcbcc4", "#605ca8"))(100))
  })
  
  observeEvent(input$predict_button, {
    input_data <- data.frame(
      gender = input$gender,
      SeniorCitizen = input$SeniorCitizen,
      Partner = input$Partner,
      Dependents = input$Dependents,
      PhoneService = input$PhoneService,
      MultipleLines = input$MultipleLines,
      InternetService = input$InternetService,
      OnlineSecurity = input$OnlineSecurity,
      OnlineBackup = input$OnlineBackup,
      DeviceProtection = input$DeviceProtection,
      TechSupport = input$TechSupport,
      StreamingTV = input$StreamingTV,
      StreamingMovies = input$StreamingMovies,
      Contract = input$Contract,
      PaperlessBilling = input$PaperlessBilling,
      PaymentMethod = input$PaymentMethod,
      tenure = input$tenure,
      MonthlyCharges = input$MonthlyCharges,
      TotalCharges = input$TotalCharges
    )
    
    h2o_input_data <- as.h2o(input_data)
    
    predictions <-  h2o.predict(model(), h2o_input_data)
    
    predictions_df <- as.data.frame(predictions)
    threshold <- 0.3230406
    predictions_df$prediction <- ifelse(predictions_df$p1 > threshold, "YES", "NO")
    predictions_df$probability <- ifelse(predictions_df$prediction == "YES", predictions_df$p1, predictions_df$p0)
    predictions_df$icon <- ifelse(predictions_df$prediction == "YES", "<span style='color: red; font-size: 20px;'>&#10006;</span>", "<span style='color: green; font-size: 20px;'>&#10004;</span>")
    
    output$predicted_result <- renderTable({
      probabilities <- ifelse(predictions_df$prediction == "YES",
                              paste0(predictions_df$icon, " The customer will churn with ", round(predictions_df$probability * 100, 2), "% probability"),
                              paste0(predictions_df$icon, " The customer will churn with ", round((1 - predictions_df$probability) * 100, 2), "% probability"))
      data.frame(Probability = probabilities)
    }, sanitize.text.function = function(x) x, rownames = FALSE)
    
  })
  
  pred <- reactive({
    target1 <- "Churn"
    features1 <- setdiff(colnames(data1()), target1)
    pred <- as.data.frame(h2o.predict(model(), test()))
    pred$predict <- as.character(pred$predict)
    
    pred
  })
  
  max_p1 <- reactive({
    pred_data <- pred()
    pred_data[pred_data$predict == "Churn_No", "p1"] %>% max()
  })
  
  min_p1 <- reactive({
    pred_data <- pred()
    pred_data[pred_data$predict == "Churn_Yes", "p1"] %>% min()
  })
}


# Run the application
shinyApp(ui = ui, server = server)
