# Step 1: Load the dataset
data<- read.csv('Debernardi_et_al_2020_data.csv')

# Step 2: Select relevant columns
data <- data %>%
select(creatinine, plasma_CA19_9, age, sex, LYVE1, REG1B, TFF1, diagnosis)

# Step 3: Handle missing values (
data <- data %>%
mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

#step 4:
result <- analyze_numeric_skewness(data, scale_data = TRUE)

#step5
vars_to_transform <- c("LYVE1", "creatinine", "plasma_CA19_9", "REG1B", "TFF1")
result_2 <- normalize_skewed_variables(data, vars_to_transform)

#step_6
# Convert diagnosis to a character with levels "Positive" and "Negative" (1 - Normal ,2 -other type ,3-PDAC )
data$diagnosis <- ifelse(data$diagnosis == 3, "Positive", "Negative")

#Step 7
eda_results <- eda_categorical_analysis(data = data, target = 'diagnosis')

#Step 8
data$diagnosis <- factor(data$diagnosis, levels = c("Negative", "Positive"))
data$sex <- ifelse(data$sex == 'M', 1, 0)
result_a <- check_multicollinearity(data = data_3$data, target_var = "diagnosis",
predictor_vars = c("creatinine","LYVE1","REG1B","age","TFF1"))

#Step 9
split_result <- h2o_data_prep(data, ratio = 0.8)
# Extract the train and test data
train_data <- split_result$train
test_data <- split_result$test

#Step 10: Running AutoML
aml_3 <- h2o.automl(
x = c("creatinine", "plasma_CA19_9", "age", "sex", "LYVE1", "REG1B", "TFF1"), # Predictor variables
y = "diagnosis", # Target variable
training_frame = train_data,
max_models = 2000,
seed = 1234,
)

#Step11
top_plot <- CombinedLeaderboardPlot(aml_3@leaderboard, unique = FALSE)
unique_plot <- CombinedLeaderboardPlot(aml_3@leaderboard, unique = TRUE)

#Step12
results <- model_metrics_cm(aml_3, test_data, get_results = "top_5")
results_u <- model_metrics_cm(aml_3, test_data, get_results = "unique")

#Step13
plot_variable_importance(aml_3, n_models = 5,unique = F)
plot_variable_importance(aml_4, n_models = 5, unique = TRUE)

#Step 14
hyperparams_u <- extract_hyperparameters(aml_3, n_models = 5, unique = TRUE)
hyperparams <- extract_hyperparameters(aml_3, n_models = 6, unique = FALSE)