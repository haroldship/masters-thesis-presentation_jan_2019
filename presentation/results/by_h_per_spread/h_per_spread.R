library(Hmisc)
library(doublekernel)

sigmas <- c("0.7", "1.0", "1.4", "2.0", "2.8")

spread <- numeric()

oracle.nmise <- numeric()
silverman.nmise <- numeric()
cv.nmise <- numeric()

oracle.best_h1 <- numeric()
oracle.best_h2 <- numeric()
silverman.mean_h <- numeric()
cv.mean_h1 <- numeric()
cv.mean_h2 <- numeric()


for (sigma in sigmas) {
  DIR <- paste("/Users/harold/Dropbox/MA_Knowledge_and_Info/Thesis/thesis/results/unif_100_", sigma, "_1h", sep="")
  setwd(DIR)
  source("experiment_setup.R")
  load("output/experiment.RData")

  factor <- experiment$EN.i
  spread <- c(spread, sigma)

  oracle.nise <- 10**9 * compare_peaks.result$oracle.ise / factor**2
  silverman.nise <- 10**9 * compare_peaks.result$silverman.ise / factor**2
  cv.nise <- 10**9 * compare_peaks.result$cv.ise / factor**2

  oracle.nmise <- c(oracle.nmise, mean(oracle.nise))
  silverman.nmise <- c(silverman.nmise, mean(silverman.nise))
  cv.nmise <- c(cv.nmise, mean(cv.nise))

  oracle.best_h1 <- c(oracle.best_h1, oracle.result$best.bandwidth[1])
  oracle.best_h2 <- c(oracle.best_h2, oracle.result$best.bandwidth[2])
  silverman.mean_h <- c(silverman.mean_h, mean(compare_peaks.result$silverman.bandwidth))
  cv.mean_h1 <- c(cv.mean_h1, mean(compare_peaks.result$cv.bandwidth.x1))
  cv.mean_h2 <- c(cv.mean_h2, mean(compare_peaks.result$cv.bandwidth.x2))

}

setwd("/Users/harold/Dropbox/MA_Knowledge_and_Info/Thesis/thesis/results/by_h_per_spread/")

df <- data.frame(spread, oracle.best_h1, oracle.best_h2, silverman.mean_h, cv.mean_h1, cv.mean_h2,
                 oracle.nmise, silverman.nmise, cv.nmise)

df.latex <- latex(df, file="h_per_spread.tex",
                  title="h_per_spread",
                  where="htbp",
                  label="tab:results:bandwidth_vs_spread",
                  rowname=NULL,
                  booktabs=TRUE,
                  cgroup=c("", "Mean Bandwidths", "NMISE"),
                  n.cgroup=c(1, 5, 3),
                  colheads=c("$\\sigma_i$", '$h_{o1}$', '$h_{o2}$', '$h_{s}$', '$h_{cv1}$', '$h_{cv2}$', 'Oracle', 'Silverman', 'CV'),
                  cdec=c(1, rep(1, 5), rep(3, 3)),
                  caption.loc="bottom",
                  caption="Bandwidth and accuracy by spread for uniform population of 10,000 with single-peak risk function with expected number of incidents 100. The NMISE values are scaled by $10^9$.",
                  caption.lot="Bandwidth and accuracy by spread of incidents")
