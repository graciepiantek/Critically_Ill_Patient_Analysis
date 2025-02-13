fluidPage(
  titlePanel("Survival Prediction Model"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("predictor_sets",
                         "Select Predictor Sets:",
                         choices = list("Severity" = "severity",
                                        "Comorbidity" = "comorbid",
                                        "Lab Values" = "lab"),
                         selected = "severity"),
      actionButton("run_model", "Run Model")
    ),
    
    mainPanel(
      plotOutput("roc_plot"),
      plotOutput("conf_matrix_plot"),
      verbatimTextOutput("model_metrics")
    )
  )
)

