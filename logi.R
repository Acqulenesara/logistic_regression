library(nnet)

# Load Titanic dataset
titanic_data <- read.csv("Titanic-Dataset.csv")

# Preprocess the Titanic dataset
# Convert 'Sex' and 'Pclass' to factors
titanic_data$Sex <- as.factor(titanic_data$Sex)
titanic_data$Pclass <- as.factor(titanic_data$Pclass)

# Handle missing values in Age and Fare columns
titanic_data$Age[is.na(titanic_data$Age)] <- median(titanic_data$Age, na.rm = TRUE)
titanic_data$Fare[is.na(titanic_data$Fare)] <- median(titanic_data$Fare, na.rm = TRUE)

# Fit the binomial logistic regression model to predict 'Survived'
binomial_model <- glm(Survived ~ Pclass + Sex + Age + Fare, data = titanic_data, family = binomial(link = "logit"))

# Load Mushroom dataset
mushroom_data <- read.csv("mushrooms.csv")

# Convert class (target) to a factor variable
mushroom_data$class <- as.factor(mushroom_data$class)

# Convert necessary features to factors
mushroom_data$cap.shape <- as.factor(mushroom_data$cap.shape)
mushroom_data$odor <- as.factor(mushroom_data$odor)
mushroom_data$habitat <- as.factor(mushroom_data$habitat)

# Fit the multinomial logistic regression model to predict 'class'
multinomial_model <- multinom(class ~ cap.shape + odor + habitat, data = mushroom_data)

# Define menu function
menu_function <- function() {
  repeat {
    cat("Choose the type of logistic regression:\n1. Binomial (Titanic Survival Prediction)\n2. Multinomial (Mushroom Classification)\n3. Exit\n")
    ch <- readline(prompt = "Enter your choice: ")
    
    if (ch == "1") {
      cat("\nBinomial Logistic Regression (Survival Prediction):\n")
      print(summary(binomial_model))
      
      # User inputs for survival prediction
      pclass_input <- as.factor(readline(prompt = "Enter Pclass (1, 2, or 3): "))
      sex_input <- as.factor(readline(prompt = "Enter Sex (male or female): "))
      age_input <- as.numeric(readline(prompt = "Enter Age: "))
      fare_input <- as.numeric(readline(prompt = "Enter Fare: "))
      
      new_data <- data.frame(Pclass = pclass_input, Sex = sex_input, Age = age_input, Fare = fare_input)
      
      # Predict the probability of survival
      predicted_prob <- predict(binomial_model, newdata = new_data, type = "response")
      predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
      
      cat("Predicted Survival Class:", predicted_class, "\n")
      
      # Predict on training data
      binomial_predictions <- predict(binomial_model, newdata = titanic_data, type = "response")
      binomial_predicted_class <- ifelse(binomial_predictions > 0.5, 1, 0)
      
      # Confusion Matrix
      binomial_cm <- table(Predicted = binomial_predicted_class, Actual = titanic_data$Survived)
      cat("Confusion Matrix for Binomial Logistic Regression (Survival Prediction):\n")
      print(binomial_cm)
      
      # Accuracy Calculation
      binomial_accuracy <- sum(diag(binomial_cm)) / sum(binomial_cm)
      cat("Binomial Model Accuracy:", round(binomial_accuracy, 4), "\n")
      
    } else if (ch == "2") {
      cat("\nMultinomial Logistic Regression (Mushroom Prediction):\n")
      print(summary(multinomial_model))
      
      # User inputs for mushroom classification
      cap_shape_input <- readline(prompt = "Enter Cap Shape (e.g., b, c, x): ")
      odor_input <- readline(prompt = "Enter Odor (e.g., a, l, p): ")
      habitat_input <- readline(prompt = "Enter Habitat (e.g., g, l, p): ")
      
      new_data <- data.frame(
        cap.shape = factor(cap_shape_input, levels = levels(mushroom_data$cap.shape)),
        odor = factor(odor_input, levels = levels(mushroom_data$odor)),
        habitat = factor(habitat_input, levels = levels(mushroom_data$habitat))
      )
      
      # Predict the probabilities and class
      predicted_probs <- predict(multinomial_model, newdata = new_data, type = "probs")
      predicted_class <- predict(multinomial_model, newdata = new_data, type = "class")
      
      cat("Predicted Probabilities for Edible or Poisonous:\n")
      print(predicted_probs)
      cat("Predicted Mushroom Class:", predicted_class, "\n")
      
      # Predict on training data
      multinomial_predicted_class <- predict(multinomial_model, newdata = mushroom_data, type = "class")
      
      # Confusion Matrix
      multinomial_cm <- table(Predicted = multinomial_predicted_class, Actual = mushroom_data$class)
      cat("Confusion Matrix for Multinomial Logistic Regression (Mushroom Prediction):\n")
      print(multinomial_cm)
      
      # Accuracy Calculation
      multinomial_accuracy <- sum(diag(multinomial_cm)) / sum(multinomial_cm)
      cat("Multinomial Model Accuracy:", round(multinomial_accuracy, 4), "\n")
      
    } else if (ch == "3") {
      cat("Exiting...\n")
      break
    } else {
      cat("Invalid choice. Please enter a valid option.\n")
    }
  }
}

# Call the menu function
menu_function()
