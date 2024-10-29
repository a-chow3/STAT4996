# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(Rtsne)
library(umap)
library(caret)

# Load data
data <- read.csv("C:/Users/Use This Account/Documents/Capstone/Dashboard/filtered_uva_games.csv")

# Assuming last column is the group label
X <- data[,c(7,26,27,28,29)]  # Features
y <- data[,"Pitcher"]    # Group labels

# Identify numeric and categorical columns
numeric_vars <- sapply(X, is.numeric)
categorical_vars <- !numeric_vars

# Handle missing values
# For numeric columns, replace NA with mean
X[numeric_vars] <- lapply(X[numeric_vars], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# For categorical columns, replace NA with mode
X[categorical_vars] <- lapply(X[categorical_vars], function(x) {
  x[is.na(x)] <- as.character(names(which.max(table(x))))
  return(x)
})

# One-hot encode categorical variables
X_cat <- X %>% select(which(categorical_vars))
dummy_vars <- dummyVars(" ~ .", data = X_cat)
X_cat_encoded <- predict(dummy_vars, newdata = X_cat) %>% as.data.frame()

# Combine numeric and encoded categorical variables
X_num <- X %>% select(which(numeric_vars))
X_combined <- bind_cols(X_num, X_cat_encoded)

# Scale numeric features
X_scaled <- scale(X_combined)

# Run t-SNE
tsne_result <- Rtsne(X_scaled, dims = 2, perplexity = 30, verbose = TRUE)

# Convert t-SNE result to data frame and add group labels
X_reduced <- as.data.frame(tsne_result$Y)
X_reduced$Group <- as.factor(y)

# 2D Scatter Plot
ggplot(X_reduced, aes(x = V1, y = V2, color = Group)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "2D t-SNE Point Cloud of Mixed Multidimensional Data",
       x = "t-SNE Dimension 1", y = "t-SNE Dimension 2") +
  theme_minimal()
