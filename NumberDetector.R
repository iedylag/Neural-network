library(neuralnet)

toArray <- function(string) {
  if (nchar(string) != 7 * 7) {
    stop("string in wrong size")
  }
  return(as.numeric(strsplit(string, "")[[1]]) == "#")
}

zero <- toArray('########     ##     ##     ##     ##     ########')

one <- toArray('   #      #      #      #      #      #      #   ')

two <- toArray('########     #      #     #    #    #     #######')

three <- toArray('#######      #      # ######      #      ########')

four <- toArray('#     ##     ##     ########      #      #      #')

five <- toArray('########      #      #######      #      ########')

six <- toArray('      #    #    #     #######     ##     ########')

seven <- toArray('#######     #     #     #     #     #     #      ')

eight <- toArray('########     ##     #########     ##     ########')

nine <- toArray('########     ##     #######     #     #    #     ')

trainingData <- list(
  list(input = zero, output = c(zero = 1)),
  list(input = one, output = c(one = 1)),
  list(input = two, output = c(two = 1)),
  list(input = three, output = c(three = 1)),
  list(input = four, output = c(four = 1)),
  list(input = five, output = c(five = 1)),
  list(input = six, output = c(six = 1)),
  list(input = seven, output = c(seven = 1)),
  list(input = eight, output = c(eight = 1)),
  list(input = nine, output = c(nine = 1))
)

net <- neuralnet::neuralnet(output ~ input.1 + input.2 + input.3 + input.4 + input.5 + input.6 + input.7,
                            data = as.data.frame(t(simplify2array(trainingData))),
                            hidden = 5,
                            linear.output = FALSE)

result <- neuralnet::compute(net, toArray(
  '#######'
  '#     #'
  '#     #'
  '##  ###'
  '#     #'
  '#     #'
  '#######'
))

print(result)
