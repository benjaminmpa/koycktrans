# koycktrans

Estimation d'un modele a retards echelonnes par la transformation de Koyck.

## Installation

install.packages("remotes")
remotes::install_github("benjaminmpa/koycktrans")

## Exemple

library(koycktrans)
?koyck_model

set.seed(123)
n <- 200
x <- rnorm(n)
y <- numeric(n)
for (t in 2:n) y[t] <- 2*(1 - 0.6) + 0.6*y[t-1] + 0.5*x[t] + rnorm(1, sd = 0.3)
mod <- koyck_model(y ~ x, data = data.frame(y = y, x = x))
mod
