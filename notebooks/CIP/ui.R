
fluidPage(
  titlePanel("Survival Prediction Model"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("predictor_sets",
                         "Select One or More Predictor Sets:",
                         choices = list("Severity" = "severity",
                                        "Comorbidity" = "comorbid",
                                        "Lab Values" = "lab"),
                         selected = "severity"),
      actionButton("run_model", "Run Model")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("ROC & Confusion Matrix",
                 br(),
                 fluidRow(
                  column(12,
                          plotOutput("roc_plot", height = "400px")
                   )
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          plotOutput("conf_matrix_plot", height = "400px")
                   )
                 ),
                 verbatimTextOutput("model_metrics")
        ),
        tabPanel("Calibration",
                 br(),
                 fluidRow(
                   column(12,
                          plotlyOutput("calibration_plot", height = "500px")
                   )
              )
        ),
        tabPanel("Data Dictionary",
                 br(),
                 DT::dataTableOutput("data_dictionary")
        )
      )
    )
  )
)

