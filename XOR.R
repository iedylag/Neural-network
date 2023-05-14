library(neuralnet)

# przygotowanie danych treningowych
input <- data.frame(x1 = c(0, 0, 1, 1), x2 = c(0, 1, 0, 1))
output <- data.frame(y = c(0, 1, 1, 0))

# utworzenie i trenowanie sieci neuronowej
nn <- neuralnet(y ~ x1 + x2, data = cbind(input, output), hidden = 3, act.fct = "logistic")
plot(nn)

# testowanie sieci neuronowej
test_input <- data.frame(x1 = c(0, 0, 1, 1), x2 = c(0, 1, 0, 1))
predicted_output <- compute(nn, test_input)
round(predicted_output$net.result)
