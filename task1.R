library(GA)

f <- function(x, y) {
  (1 - x) * (1 - x) + exp(1) * (y - x^2) * (y - x^2)
}

plotGraph <- function (f, lower, upper, xy) {
  # First plot the graph
  x <- seq(from=lower[1], to=upper[1], length=20)
  y <- seq(from=lower[2], to=upper[2], length=20)
  z <- outer(x, y, f)
  res <- persp(x, y, z, theta=100, shade=0.2)
  
  # Then plot the points
  z = apply(xy, MARGIN=2, FUN=function(p) { f(p[1], p[2]) })
  
  points(trans3d(xy[1,], xy[2,], z, pmat = res), col=rgb(1, 0, 0, 1), pch=19)
}

GA <- ga(
  type = "real-valued",
  fitness = function(v) f(v[1], v[2]),
  
  lower = c(-1, -1),
  upper = c(1, 1),
  
  popSize = 100,
  
  pcrossover = 0.9,
  pmutation = 0.1,
  
  maxiter = 1000,
  
  run = 200,
  
  monitor = function(obj) {
    plotGraph(obj@fitness, obj@lower, obj@upper, t(obj@population))
    Sys.sleep(0.05)
  }
)

