# Wczytanie danych treningowych i testowych w postaci plików CSV
df_train <- read.csv("train.csv", header = TRUE, row.names = 1)
df_test <- read.csv("test.csv", header = TRUE, row.names = 1)

# Przygotowanie danych treningowych
df_train <- subset(df_train, select = -c(Name, Cabin, Ticket))
df_train$Survived <- as.numeric(df_train$Survived)
df_train$Pclass <- as.numeric(df_train$Pclass)
df_train$Sex <- ifelse(df_train$Sex == "male", 1, 0)
df_train$Age[is.na(df_train$Age)] <- median(df_train$Age, na.rm = TRUE)
df_train$Fare[is.na(df_train$Fare)] <- median(df_train$Fare, na.rm = TRUE)
df_train$Embarked <- as.numeric(as.factor(df_train$Embarked))

# Przygotowanie danych testowych
df_test$Pclass <- as.numeric(df_test$Pclass)
df_test$Sex <- ifelse(df_test$Sex == "male", 1, 0)
df_test$Age[is.na(df_test$Age)] <- median(df_test$Age, na.rm = TRUE)
df_test$Fare[is.na(df_test$Fare)] <- median(df_test$Fare, na.rm = TRUE)
df_test$Embarked <- as.numeric(as.factor(df_test$Embarked))

# Budowanie modelu sieci neuronowej
library(neuralnet)
set.seed(1234)

nn <- neuralnet(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                data = df_train, hidden = c(6,3), linear.output = FALSE, threshold = 0.05, stepmax=1e+06)

# Predykcja dla danych testowych
df_new_data <- df_test[, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]

df_new_data$Pclass <- as.numeric(df_new_data$Pclass)
df_new_data$Sex <- ifelse(df_new_data$Sex == "male", 1, 0)
df_new_data$Age <- as.numeric(df_new_data$Age)
df_new_data$SibSp <- as.numeric(df_new_data$SibSp)
df_new_data$Parch <- as.numeric(df_new_data$Parch)
df_new_data$Fare <- as.numeric(df_new_data$Fare)

# wylosowanie 5 indeksów obserwacji
indices <- sample(nrow(df_test), 1)
for (col_name in names(df_test)) {
  print(paste(col_name, ": ", df_test[indices, col_name]))
}
df_new <- df_new_data[indices,]
df_new <- as.matrix(df_new) # Convert to numeric matrix

predictions <- predict(nn, newdata = new_data)

# Wyświetlenie wyników predykcji dla każdego pasażera
if (predictions == 0) {
  print(paste("Pasażer", indices, ": Nie przeżył"))
} else {
  print(paste("Pasażer", indices, ": Przeżył"))
}

# Predykcja dla nowych danych
new_data <- data.frame(Pclass = 1, Sex = "female", Age = 21, SibSp = 2,
                       Parch = 0, Fare = 61.3792, Embarked="Q")

new_data$Pclass <- as.numeric(new_data$Pclass)
new_data$Embarked <- as.numeric(as.factor(new_data$Embarked))
new_data$Sex <- ifelse(new_data$Sex == "male", 1, 0)
new_data$Age[is.na(new_data$Age)] <- median(df_train$Age, na.rm = TRUE)
new_data$Fare[is.na(new_data$Fare)] <- median(df_train$Fare, na.rm = TRUE)
new_data <- as.matrix(new_data)

prediction <- round(predict(nn, newdata = new_data), 0)
if (prediction == 0) {
  print("Nie przeżył")
} else {
  print("Przeżył")
}
