g <- readline(prompt="Function: ")
g <- parse(text = g)
a <- as.numeric(readline(prompt="Enter first limit: "))
b <- as.numeric(readline(prompt="Enter second limit: "))
precision <- as.numeric(readline(prompt="Precision: "))

n <- 10
graphPointsFile <- NULL

f <- function(x){ eval( g[[1]] ) }
graph <- function() { 
  library(ggplot2)

  i <- a
  vectorX <- NULL
  vectorY <- NULL
  vectorXmin <- NULL
  repeat {
    if (i >= b) {
      break
    }
    vectorX <- c(vectorX, i)
    vectorY <- c(vectorY, f(i))
    vectorXmin <- c(vectorXmin, i+precision)

    i <- i+precision
  }
  graphPointsFile <- data.frame(start.points=vectorX, end.points=vectorY)
  graph <- ggplot(graphPointsFile) + stat_function(fun = f) + geom_rect(aes(xmin=vectorXmin, xmax=vectorX, ymin=0, ymax=vectorY), fill='gray')

  plot(graph)
}

integral <- function() {
  rightApproximation <- function(f, a, b) {
    area <- 0
    interval <- (b-a)/n
    for (i in 1:(n)) {
      area = area + (interval * f(interval*i))
    }
    return (area)
  }

  leftApproximation <- function(f, a, b) {
    area <- 0
    interval <- (b-a)/n
    for (i in 1:(n)) {
      area = area + (interval * f(interval*(i-1)))
    }
    return (area)
  }

  repeat {
    leftArea = leftApproximation(f, a, b)
    rightArea = rightApproximation(f, a, b)
    
    if (abs(rightArea - leftArea) > precision) {
      n <- n*100
    } else {
      print(rightArea)
      break
    }
  }
}

# graph()
# integral()
