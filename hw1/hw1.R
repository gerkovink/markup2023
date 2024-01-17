#load package lattice
install.packages("stargazer")

library(lattice)
library(xtable) # generate the LaTeX code for tables
library(stargazer)
#fix the random generator seed
set.seed(123)
#create data
data <- rnorm(1000)

layout(matrix(c(1, 2, 3, 4), nrow = 2,
              ncol = 2, byrow = TRUE))

#plot histogram
histogram(data)
#plot density 
densityplot(data^12 / data^10, xlab = expression(data^12/data^10))
#plot stripplot
stripplot(data^2, xlab = expression(data^2))
#plot boxplot
bwplot(exp(data))
#matrix with all data used
data.all <- cbind(data = data, 
                  squared1 = data^12 / data^10,
                  squared2 = data^2,
                  exponent = exp(data))
data.all[1:9,]

stargazer(data.all[1:9,])
