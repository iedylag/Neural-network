library(neuralnet)

# przygotowanie danych treningowych
input <- data.frame(x1 = c(0, 0, 1, 1), x2 = c(0, 1, 0, 1))
output <- data.frame(y = c(0, 1, 1, 0))

# utworzenie i trenowanie sieci neuronowej
start_time <- Sys.time()
nn <- neuralnet(y ~ x1 + x2, data = cbind(input, output), hidden = 2, act.fct = "logistic")
end_time <- Sys.time()

plot(nn)

# testowanie sieci neuronowej
test_input <- data.frame(x1 = c(0, 0, 1, 1), x2 = c(0, 1, 0, 1))
predicted_output <- compute(nn, test_input)
round(predicted_output$net.result)

results <- data.frame(output$y, round(predicted_output$net.result,4))
colnames(results) <- c("Actual", "Predicted")
results

training_time <- end_time - start_time
print(paste("Czas trenowania modelu:", training_time))

