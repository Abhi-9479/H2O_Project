## H2O Functions

#1

h2o_data_prep <- function(data, ratio = 0.8) {
  # Load H2O library
  library(h2o)
  # Start H2O cluster
  h2o.init()
  
  # Convert data to H2O object
  data_h2o <- as.h2o(data)
  
  # Split the data into training and testing sets
  splits <- h2o.splitFrame(data = data_h2o, ratios = ratio)
  train <- splits[[1]]
  test <- splits[[2]]
  
  # Return the splits
  return(list(train = train, test = test))
}


#2 Hashing out the function as Knit trying to run it

#aml_4 <- h2o.automl(
#  x = c( "creatinine","plasma_CA19_9","age","sex","LYVE1","REG1B","TFF1"), 
#  y = "diagnosis", # Target variable
#  training_frame = train_data, 
#  max_models = 2000,
#  seed = 1234, 
#  nfolds = 5,
#)
```
## H2O Results Exploration Functions

#1


CombinedLeaderboardPlot <- function(leaderboard, unique = FALSE) {
  # Load required libraries
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(dplyr)
  
  # Convert leaderboard to a data frame
  lb <- as.data.frame(leaderboard)
  
  # Check the available columns in the leaderboard
  available_metrics <- colnames(lb)
  
  # Select only the columns that exist 
  metrics_to_include <- c("rmse", "mse", "auc", "aucpr","logloss")
  existing_metrics <- metrics_to_include[metrics_to_include %in% available_metrics]
  
  if (unique) {
    # Extract method type from model_id (e.g., GBM, GLM, DRF)
    lb$method <- sub("_.*", "", lb$model_id)
    
    # Select the best-performing model for each method (using RMSE by default)
    best_unique <- lb %>%
      group_by(method) %>%
      slice_min(order_by = rmse, n = 1) %>%
      ungroup() %>%
      select(model_id, method, all_of(existing_metrics))  # Use the existing metrics
    
    # Reshape data into a long format for plotting
    best_long <- pivot_longer(best_unique, cols = -c(model_id, method), 
                              names_to = "metric", values_to = "value")
    
    # Create ggplot for unique models
    p <- ggplot(best_long, aes(x = model_id, y = value, color = metric, group = metric)) +
      geom_line(size = 1) +  # Add lines connecting metrics for each model
      geom_point(size = 3) +  # Add points on lines for clarity
      theme_minimal() +
      labs(title = "Metric Comparison Across Best Unique Models",
           x = "Model ID", y = "Value", color = "Metric") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else {
    # Select top 5 models from leaderboard
    top5 <- lb[1:5, c("model_id", all_of(existing_metrics))]
    
    # Reshape data into a long format for plotting
    top5_long <- pivot_longer(top5, cols = -model_id, 
                              names_to = "metric", values_to = "value")
    
    # Create ggplot for top 5 models
    p <- ggplot(top5_long, aes(x = model_id, y = value, color = metric, group = metric)) +
      geom_line(size = 1) +  # Add lines connecting metrics for each model
      geom_point(size = 3) +  # Add points on lines for clarity
      theme_minimal() +
      labs(title = "Metric Comparison Across Top 5 Models",
           x = "Model ID", y = "Value", color = "Metric") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Convert to an interactive plot using ggplotly
  interactive_plot <- ggplotly(p)
  
  # Return the interactive plot
  return(interactive_plot)
}


#2

model_metrics_cm <- function(aml_object, test_data, get_results = "top_5",target_col) {
  # Load required libraries
  library(h2o)
  library(dplyr)
  library(ggplot2)
  library(caret)
  
  # Ensure get_results is valid
  if (!get_results %in% c("top_5", "unique")) {
    stop("Invalid option for get_results. Choose either 'top_5' or 'unique'.")
  }
  
  # Extract the leaderboard as a data frame
  lb <- as.data.frame(aml_object@leaderboard)
  
  if (get_results == "top_5") {
    # Select top 5 models based on the leaderboard order
    top_models <- lb[1:5, ]
  } else if (get_results == "unique") {
    # Extract base model names (e.g., GBM, DRF, GLM) by removing suffixes
    lb$model_base <- sub("_.*", "", lb$model_id)
    
    # Group by base model and select the best-performing model for each base
    top_models <- lb %>%
      group_by(model_base) %>%
      slice_min(order_by = rmse, n = 1) %>%
      ungroup()
    
    # Select only the top 5 unique models (if there are more than 5)
    top_models <- top_models[1:min(5, nrow(top_models)), ]
  }
  
  # Initialize lists to store tables and metrics for plotting
  model_metrics_tables <- list()
  model_metrics <- list()
  
  # Loop through each model to calculate metrics
  for (i in 1:nrow(top_models)) {
    # Fetch model_id
    model_id <- as.character(top_models$model_id[i])
    
    # Get the model from the AutoML object
    model <- h2o.getModel(model_id)
    
    # Make predictions
    predictions <- h2o.predict(model, newdata = as.h2o(test_data))
    
    # Extract true and predicted labels
    true_labels <- as.vector(target_col)  # Replaces with the target column name
    pred_labels <- as.vector(predictions$predict)  # Predicted labels from the model
    
    # Convert to factors for confusionMatrix
    truth <- factor(true_labels)
    estimate <- factor(pred_labels)
    
    # Create confusion matrix using caret
    cm <- confusionMatrix(estimate, truth)
    
    # Extract the metrics for the model 
    metrics_byClass <- cm$byClass
    accuracy <- cm$overall['Accuracy']  
    kappa <- cm$overall['Kappa']
    
    # Combine all metrics into a data frame
    metrics_table <- data.frame(
      Accuracy = accuracy,
      Sensitivity = metrics_byClass['Sensitivity'],
      Specificity = metrics_byClass['Specificity'],
      Precision = metrics_byClass['Pos Pred Value'],
      Recall = metrics_byClass['Sensitivity'],
      F1_Score = metrics_byClass['F1'],
      Kappa = kappa
    )
    
    # Store the metrics table in the list
    model_metrics_tables[[model_id]] <- metrics_table
    model_metrics[[model_id]] <- metrics_byClass  # For plotting purposes
  }
  
  # ---- Create Plot ----
  # Initialize the ggplot object
  p <- ggplot()
  
  # Loop through each model and plot its metrics
  for (model_id in names(model_metrics)) {
    metrics <- model_metrics[[model_id]]
    
    # Add a line for each metric (you can add more metrics here)
    p <- p + 
      geom_line(data = data.frame(metric = names(metrics), value = metrics, model_id = model_id),
                aes(x = metric, y = value, color = model_id, group = model_id)) +
      geom_point(data = data.frame(metric = names(metrics), value = metrics, model_id = model_id),
                 aes(x = metric, y = value, color = model_id), size = 3)
  }
  
  # Customize plot labels
  p <- p + 
    theme_minimal() +
    labs(title = "Metric Comparison Across Top Models",
         x = "Metric", y = "Value", color = "Model ID") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Explicitly print the plot to ensure rendering
  print(p)
  
  # Return the list with tables and plot
  return(list(tables = model_metrics_tables, plot = p))
}
```



plot_variable_importance <- function(aml, n_models = 5, get_results = "top_5") {
  library(ggplot2)
  library(gridExtra)
  library(dplyr)
  
  # Get leaderboard
  leaderboard <- as.data.frame(h2o.get_leaderboard(aml, extra_columns = "ALL"))
  
  # Exclude models whose IDs start with 'Stacked' or 'Deep'
  leaderboard <- leaderboard[!grepl("^Stacked|^Deep", leaderboard$model_id), ]
  
  # Ensure get_results is valid
  if (!get_results %in% c("top_5", "unique")) {
    stop("Invalid option for get_results. Choose either 'top_5' or 'unique'.")
  }
  
  if (get_results == "top_5") {
    # Select top n_models based on the leaderboard order
    top_models <- leaderboard$model_id[1:min(n_models, nrow(leaderboard))]
  } else if (get_results == "unique") {
    # Extract base model names (e.g., GBM, DRF, GLM) by removing suffixes
    leaderboard$model_base <- sub("_.*", "", leaderboard$model_id)
    
    # Group by base model and select the best-performing model for each base (lowest RMSE)
    leaderboard <- leaderboard %>%
      group_by(model_base) %>%
      slice_min(order_by = rmse, n = 1) %>%
      ungroup()
    
    # Ensure we select only the top n_models
    top_models <- leaderboard$model_id[1:min(n_models, nrow(leaderboard))]
  }
  
  # Initialize list for plots
  plots <- list()
  
  # Loop through each model and generate variable importance plot
  for (model_id in top_models) {
    model <- h2o.getModel(model_id)
    
    # Check if variable importance is available
    varimp <- tryCatch(as.data.frame(h2o.varimp(model)), error = function(e) NULL)
    if (!is.null(varimp)) {
      # Extract base model name for the title (e.g., "GBM", "DRF")
      model_base <- sub("_.*", "", model_id)
      
      # Plot variable importance
      p <- ggplot(varimp, aes(x = reorder(variable, -scaled_importance), y = scaled_importance)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(title = paste(model_base),  # Use base model name
             x = "Variables", y = "Scaled Importance") +
        theme_minimal() +
        theme(plot.title = element_text(size = 10))  # Set smaller title size
      plots[[model_id]] <- p
    }
  }
  
  # Check if any plots were generated
  if (length(plots) == 0) {
    stop("No variable importance plots could be generated. Ensure the models support variable importance.")
  }
  
  # Arrange plots in a grid
  grid_arrange <- do.call(grid.arrange, c(plots, ncol = 2))
  
  return(grid_arrange)
}


#4 Extra function to extract Hyperparameters



extract_hyperparameters <- function(aml, n_models = 5, unique = FALSE) {
  # Check if the object is an AutoML object
  if (!inherits(aml, "H2OAutoML")) {
    stop("The provided object is not an H2OAutoML object.")
  }
  
  # Get leaderboard and include all models (no filtering for "Stacked" or "Deep")
  leaderboard <- as.data.frame(h2o.get_leaderboard(aml, extra_columns = "ALL"))
  
  # Use unique algorithms if unique = TRUE
  if (unique) {
    # Extract base model names (e.g., GBM, DRF, GLM) by removing suffixes
    leaderboard$model_base <- sub("_.*", "", leaderboard$model_id)
    
    # Group by base model and select the best-performing model for each base (lowest RMSE)
    leaderboard <- leaderboard %>%
      group_by(model_base) %>%
      slice_min(order_by = rmse, n = 1) %>%
      ungroup()
    
    # Select only the top `n_models` unique models (if there are more than `n_models`)
    top_models <- leaderboard$model_id[1:min(n_models, nrow(leaderboard))]
  } else {
    # Otherwise, select the top `n_models` models based on leaderboard rank
    top_models <- head(leaderboard$model_id, n_models)
  }
  
  # Initialize a list to store hyperparameters for each model
  hyperparameters_list <- list()
  
  # Loop through each selected model and extract hyperparameters
  for (model_id in top_models) {
    # Get the model from AutoML
    model <- h2o.getModel(model_id)
    
    # Check if the model has the expected hyperparameters
    if ("params" %in% slotNames(model)) {
      # Extract hyperparameters
      hyperparameters <- model@parameters
    } else {
      hyperparameters <- NULL
    }
    
    # Convert the hyperparameters to a data frame for easier readability
    if (!is.null(hyperparameters)) {
      hyperparameters_df <- data.frame(
        parameter = names(hyperparameters),
        value = as.character(hyperparameters),
        stringsAsFactors = FALSE
      )
    } else {
      hyperparameters_df <- data.frame(
        parameter = NA,
        value = NA,
        stringsAsFactors = FALSE
      )
    }
    
    # Add to the list with the model_id as the name
    hyperparameters_list[[model_id]] <- hyperparameters_df
  }
  
  return(hyperparameters_list)
}


