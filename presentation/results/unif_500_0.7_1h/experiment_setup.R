set.seed(12)
experiment <- Experiment(
  x1.min=-4,
  x1.max=4,
  x2.min=-4,
  x2.max=4,
  grid.by=0.1,
  buffer=0.5,
  N.p=10000,
  EN.i=500,
  bandwidths=seq(0.3, 2.8, 0.1),
  #c1=0.0,
  #c2=0.0,
  sigma1=1.0,
  sigma2=1.0,
  rho=0,
  incident_rate=genhill(sigma=0.7)
)
