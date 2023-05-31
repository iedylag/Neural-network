# Wczytanie danych treningowych i testowych w postaci plikow CSV
df_train <- read.csv("train.csv", header = TRUE, row.names = 1)
df_test <- read.csv("test.csv", header = TRUE, row.names = 1)

# Przygotowanie danych treningowych
df_train <- subset(df_train, select = -c(Name, Cabin))
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
                data = df_train, hidden = c(6,3), threshold = 0.05, stepmax=1e+06,
                learningrate = 0.001, linear.output = FALSE)

plot(nn)

# Badanie jakosci predykcji
predictions <- predict(nn, newdata = df_train)
binary_predictions <- ifelse(predictions >= 0.5, 1, 0)
conf_mat <- table(binary_predictions, df_train$Survived)
conf_mat
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
precision <- conf_mat[2,2]/sum(conf_mat[,2])
recall <- conf_mat[2,2]/sum(conf_mat[2,])
f1_score <- 2 * precision * recall / (precision + recall)
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 score:", f1_score, "\n")

# Predykcja dla danych testowych
df_new_data <- df_test[, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]

# wylosowanie 5 indeksow obserwacji
indices <- sample(nrow(df_test), 1)
for (col_name in names(df_test)) {
  print(paste(col_name, ": ", df_test[indices, col_name]))
}
df_new <- df_new_data[indices,]
df_new <- as.matrix(df_new) # Convert to numeric matrix

predictions <- round(predict(nn, newdata = df_new),0)

# Wyswietlenie wynikow predykcji dla kazdego pasazera
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

# Wizualizacja zmiennych wplywajacych najbardziej na predykcje
weights <- nn$weights
for (i in seq_along(weights)) {
  cat(paste0("Weights for layer ", i, ":\n"))
  print(weights[[i]])
}

library(ggplot2)

ggplot(df_train, aes(x=Pclass, fill=as.factor(Survived))) + 
  geom_bar(position="dodge") + 
  labs(title="Stosunek przeżywalności w zależności od klasy", 
       x="Klasa", y="Liczba pasażerów", fill="Przeżył") + 
  scale_fill_manual(values=c("#FF9999", "#99FF99"), 
                    labels=c("Nie", "Tak"))
