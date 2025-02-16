
server <- function(input, output, session) {
  
  model_results <- eventReactive(input$run_model, {
    selected_predictors <- list()
    
    if("severity" %in% input$predictor_sets) {
      selected_predictors$severity <- severity_vars
    }
    if("comorbid" %in% input$predictor_sets) {
      selected_predictors$comorbid <- comorbid_vars
    }
    if("lab" %in% input$predictor_sets) {
      selected_predictors$lab <- lab_value_vars
    }
    
    log_model_function(predictor_sets = selected_predictors)
  })
  
  output$roc_plot <- renderPlot({
    req(model_results())
    plot(model_results()$roc_curve,
         main = paste("ROC Curve\nAUC =", round(model_results()$auc, 3)),
         col = "blue",
         lwd = 2)
  })
  
  output$conf_matrix_plot <- renderPlot({
    req(model_results())
    conf_matrix <- model_results()$confusion_matrix
    
    total <- sum(conf_matrix)
    cell_pcts <- round(100 * conf_matrix / total, 1)
    
    image(1:2, 1:2, t(conf_matrix),
          col = colorRampPalette(c("white", "steelblue"))(100),
          xlab = "Actual",
          ylab = "Predicted",
          main = "Confusion Matrix",
          axes = FALSE)
    
    axis(1, at = 1:2, labels = c("0", "1"))
    axis(2, at = 1:2, labels = c("0", "1"))
    
    for(i in 1:2) {
      for(j in 1:2) {
        text(i, j, 
             paste0(conf_matrix[i,j], "\n(", cell_pcts[i,j], "%)"),
             col = "black",
             font = 2)
      }
    }
  })
  
  output$model_metrics <- renderPrint({
    req(model_results())
    cat("Model Performance Metrics:\n\n")
    cat("AUC:", round(model_results()$auc, 3), "\n")
    cat("Sensitivity:", round(model_results()$sensitivity, 3), "\n")
    cat("Specificity:", round(model_results()$specificity, 3), "\n")
  })
  
  output$calibration_plot <- renderPlotly({
    req(model_results())
    
    cal_data <- model_results()$calibration
    
    plot_ly() |>
      add_trace(
        data = cal_data,
        x = ~mean_pred,
        y = ~mean_actual,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Calibration curve',
        text = ~paste('Bin size:', n,
                      '<br>Predicted:', round(mean_pred, 3),
                      '<br>Actual:', round(mean_actual, 3)),
        hoverinfo = 'text'
      ) |>
      add_trace(
        x = c(0,1),
        y = c(0,1),
        type = 'scatter',
        mode = 'lines',
        line = list(dash = 'dash'),
        name = 'Perfect calibration'
      ) |>
      layout(
        title = "Calibration Curve",
        xaxis = list(title = "Predicted Probability"),
        yaxis = list(title = "Observed Proportion"),
        showlegend = TRUE
      )
  })
  output$data_dictionary <- DT::renderDataTable({
    DT::datatable(dictionary,
                  options = list(pageLength = 25,  
                                 scrollX = TRUE,
                                 scrollY = "500px"),
                                 rownames = FALSE)
  })
  
}


