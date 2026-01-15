## EDA functions
#1


analyze_numeric_skewness <- function(data, scale_data = TRUE, boxplot_filename = NULL, pairsplot_filename = NULL) {
  # Load required libraries
  library(e1071)
  library(ggplot2)
  library(psych)
  library(reshape2)
  
  # Select only numeric columns
  numeric_data <- data[sapply(data, is.numeric)]
  
  # Check if numeric data exists
  if (ncol(numeric_data) == 0) {
    stop("No numeric columns in the dataset!")
  }
  
  # Calculate skewness
  skewness_values <- sapply(numeric_data, skewness, na.rm = TRUE)
  
  # Scale numeric data if scale_data is TRUE
  if (scale_data) {
    numeric_data <- as.data.frame(lapply(numeric_data, scale)) # Scale and convert back to data frame
  }
  
  # Prepare data for boxplot (reshape for ggplot)
  boxplot_data_long <- melt(numeric_data)
  
  # Generate the boxplot using ggplot2
  boxplot <- ggplot(boxplot_data_long, aes(x = variable, y = value)) +
    geom_boxplot() +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 8), # Rotated labels
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(
      title = "Boxplot of Numeric Data",
      x = "Variables",
      y = "Values"
    )
  
  # Save the boxplot if a filename is provided
  if (!is.null(boxplot_filename)) {
    ggsave(boxplot_filename, plot = boxplot, width = 8, height = 5)
  } else {
    print(boxplot) # Display the boxplot in the default graphics device
  }
  
  # Generate and save the pairs plot
  if (!is.null(pairsplot_filename)) {
    png(pairsplot_filename, width = 800, height = 800) # Open a graphics device
    pairs.panels(
      numeric_data,
      stars = TRUE,
      main = "Pairs Plot with Correlations",
      cex.labels = 0.8
    )
    dev.off() # Close the device
  } else {
    # Adjust graphical parameters for better plotting
    old_par <- par(no.readonly = TRUE) # Save current settings
    par(mfrow = c(1, 1)) # Reset to a single plot layout
    pairs.panels(
      numeric_data,
      stars = TRUE,
      main = "Pairs Plot with Correlations",
      cex.labels = 0.5
    )
    par(old_par) # Restore original settings
  }
  
  # Return a list containing skewness values and the boxplot
  return(list(skewness = skewness_values, boxplot = boxplot))
}


#2

normalize_skewed_variables <- function(data, vars_to_transform) {
  # Check if the specified variables exist in the dataset
  library(bestNormalize)
  if (!all(vars_to_transform %in% colnames(data))) {
    stop("Some variables specified are not in the dataset.")
  }
  
  # Initialize a list to store normalization details
  normalization_details <- list()
  
  # Loop over each variable to apply bestNormalize
  for (var in vars_to_transform) {
    if (!is.numeric(data[[var]])) {
      warning(paste("Skipping non-numeric variable:", var))
      next
    }
    
    # Find the best normalization
    bn_result <- bestNormalize(data[[var]], allow_orderNorm = TRUE)
    
    # Apply the transformation to replace the original variable
    data[[var]] <- predict(bn_result)
    
    # Save normalization details for each variable
    normalization_details[[var]] <- bn_result
    
    # Print the transformation type applied
    message(paste("Transformation applied to", var, ":", class(bn_result)[1]))
  }
  
  # Return the updated dataset and normalization details
  return(list(data = data, normalization_details = normalization_details))
}


#3

eda_categorical_analysis <- function(data, target) {
  library(ggplot2)
  library(dplyr)
  
  # Create a list to store the results
  eda_results <- list()
  
  # Frequency table for categorical variables
  cat_vars <- sapply(data, is.character)
  cat_data <- data[, cat_vars]
  
  # Store the frequency tables
  cat_tables <- lapply(cat_data, function(x) table(x, data[[target]]))
  eda_results$cat_tables <- cat_tables
  
  # Visualizing categorical vs target relationships
  cat_plots <- list()
  for (var in names(cat_data)) {
    p <- ggplot(data, aes_string(x = var, fill = target)) +
      geom_bar(position = "fill") +
      labs(title = paste("Proportion of", target, "by", var), y = "Proportion") +
      theme_minimal()
    cat_plots[[var]] <- p
  }
  eda_results$cat_plots <- cat_plots
  
  # Grouped box plots or violin plots for continuous vs categorical
  cont_vars <- sapply(data, is.numeric)
  cont_data <- data[, cont_vars]
  
  cont_plots <- list()
  for (var in names(cont_data)) {
    p <- ggplot(data, aes_string(x = target, y = var, fill = target)) +
      geom_violin(alpha = 0.5) +
      geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
      labs(title = paste("Distribution of", var, "by", target), y = var, x = target) +
      theme_minimal()
    cont_plots[[var]] <- p
  }
  eda_results$cont_plots <- cont_plots
  
  # Return the list containing all results
  return(eda_results)
}

#4


# Define the custom function
check_multicollinearity <- function(data, target_var, predictor_vars) {
  library(car)
  library(metan)
  
  # Check if target_var and predictor_vars are valid columns in the data
  if (!(target_var %in% colnames(data))) {
    stop(paste(target_var, "is not a valid column in the dataset."))
  }
  
  if (!all(predictor_vars %in% colnames(data))) {
    stop(paste("Some predictor variables are not in the dataset."))
  }
  
  # Create an empty list to store the results
  result_list <- list()
  
  # 1. VIF Test for Collinearity
  # Create a linear model with the target variable and predictor variables
  lm_model <- lm(as.formula(paste(target_var, "~", paste(predictor_vars, collapse = "+"))), data = data)
  vif_results <- vif(lm_model)
  result_list$vif_results <- vif_results
  
  # 2. Correlation Test (Correlation Matrix)
  corrl <- corr_coef(data[predictor_vars])
  result_list$correlation_values <- corrl
  
  # 3. Variance Check
  variance_results <- apply(data[predictor_vars], 2, var)
  result_list$variance_values <- variance_results
  
  # Return the result list
  return(result_list)
}

