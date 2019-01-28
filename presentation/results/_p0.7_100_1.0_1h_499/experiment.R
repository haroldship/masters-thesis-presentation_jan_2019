# 0: setup the physical environment

deps <- c("ggplot2", "lattice", "latticeExtra", "parallel", "xtable")
installed <- installed.packages()

for (lib in deps) {
  if (! lib %in% installed) {
    install.packages(lib, quiet=TRUE, repos="http://cran.rstudio.com")
  }
  library(lib, character.only=TRUE)
}

install.packages("../doublekernel_0.0.0.9000.tar.gz", repos=NULL, type="source")
library(doublekernel)

outdir <- "output"
if (! dir.exists(outdir)) {
  dir.create(outdir)
  Sys.chmod(outdir, mode="0777", use_umask=FALSE)
}

# 1: create the experimental setup
source("experiment_setup.R")
factor <- experiment$EN.i / 100

MC.oracle <- 499
MC <- 1000

# 2. Compute the Oracle bandwidth
oracle.result <- oracle(experiment, MC=MC.oracle, nCores=detectCores()-1)

# 3. Run the simulations
compare_peaks.result <- compare_peaks(experiment, oracle.result, MC=MC, nCores=detectCores()-1)

compare_peaks.result$oracle.nise <- compare_peaks.result$oracle.ise / factor
compare_peaks.result$silverman.nise <- compare_peaks.result$silverman.ise / factor
compare_peaks.result$cv.nise <- compare_peaks.result$cv.ise / factor

# 4. Compute the mean and standard deviation of the measures from the simulation output
mean_values <- data.frame(
  silverman.rmise=mean(compare_peaks.result$silverman.rise),
  silverman.rmiae=mean(compare_peaks.result$silverman.riae),
  silverman.mise=mean(compare_peaks.result$silverman.ise),
  silverman.miae=mean(compare_peaks.result$silverman.iae),
  silverman.msup=mean(compare_peaks.result$silverman.sup),
  silverman.merr=mean(compare_peaks.result$silverman.peak_err),
  silverman.mdist=mean(compare_peaks.result$silverman.peak_dist),
  silverman.mcerr=mean(compare_peaks.result$silverman.cent_err),
  silverman.mcdist=mean(compare_peaks.result$silverman.cent_dist),
  silverman.bandwidth=mean(compare_peaks.result$silverman.bandwidth),
  oracle.rmise=mean(compare_peaks.result$oracle.rise),
  oracle.rmiae=mean(compare_peaks.result$oracle.riae),
  oracle.mise=mean(compare_peaks.result$oracle.ise),
  oracle.miae=mean(compare_peaks.result$oracle.iae),
  oracle.msup=mean(compare_peaks.result$oracle.sup),
  oracle.merr=mean(compare_peaks.result$oracle.peak_err),
  oracle.mdist=mean(compare_peaks.result$oracle.peak_dist),
  oracle.mcerr=mean(compare_peaks.result$oracle.cent_err),
  oracle.mcdist=mean(compare_peaks.result$oracle.cent_dist),
  oracle.bandwidth.x1=oracle.result$best.bandwidth[1],
  oracle.bandwidth.x2=oracle.result$best.bandwidth[2],
  cv.rmise=mean(compare_peaks.result$cv.rise),
  cv.rmiae=mean(compare_peaks.result$cv.riae),
  cv.mise=mean(compare_peaks.result$cv.ise),
  cv.miae=mean(compare_peaks.result$cv.iae),
  cv.msup=mean(compare_peaks.result$cv.sup),
  cv.merr=mean(compare_peaks.result$cv.peak_err),
  cv.mdist=mean(compare_peaks.result$cv.peak_dist),
  cv.mcerr=mean(compare_peaks.result$cv.cent_err),
  cv.mcdist=mean(compare_peaks.result$cv.cent_dist),
  cv.bandwidth.x1=mean(compare_peaks.result$cv.bandwidth.x1),
  cv.bandwidth.x2=mean(compare_peaks.result$cv.bandwidth.x2)
)

sd_values <- data.frame(
  silverman.rmise=sd(compare_peaks.result$silverman.rise),
  silverman.rmiae=sd(compare_peaks.result$silverman.riae),
  silverman.mise=sd(compare_peaks.result$silverman.ise),
  silverman.miae=sd(compare_peaks.result$silverman.iae),
  silverman.msup=sd(compare_peaks.result$silverman.sup),
  silverman.merr=sd(compare_peaks.result$silverman.peak_err),
  silverman.mdist=sd(compare_peaks.result$silverman.peak_dist),
  silverman.mcerr=sd(compare_peaks.result$silverman.cent_err),
  silverman.mcdist=sd(compare_peaks.result$silverman.cent_dist),
  silverman.bandwidth=sd(compare_peaks.result$silverman.bandwidth),
  oracle.rmise=sd(compare_peaks.result$oracle.rise),
  oracle.rmiae=sd(compare_peaks.result$oracle.riae),
  oracle.mise=sd(compare_peaks.result$oracle.ise),
  oracle.miae=sd(compare_peaks.result$oracle.iae),
  oracle.msup=sd(compare_peaks.result$oracle.sup),
  oracle.merr=sd(compare_peaks.result$oracle.peak_err),
  oracle.mdist=sd(compare_peaks.result$oracle.peak_dist),
  oracle.mcerr=sd(compare_peaks.result$oracle.cent_err),
  oracle.mcdist=sd(compare_peaks.result$oracle.cent_dist),
  oracle.bandwidth.x1=0,
  oracle.bandwidth.x2=0,
  cv.rmise=sd(compare_peaks.result$cv.rise),
  cv.rmiae=sd(compare_peaks.result$cv.riae),
  cv.mise=sd(compare_peaks.result$cv.ise),
  cv.miae=sd(compare_peaks.result$cv.iae),
  cv.msup=sd(compare_peaks.result$cv.sup),
  cv.merr=sd(compare_peaks.result$cv.peak_err),
  cv.mdist=sd(compare_peaks.result$cv.peak_dist),
  cv.mcerr=sd(compare_peaks.result$cv.cent_err),
  cv.mcdist=sd(compare_peaks.result$cv.cent_dist),
  cv.bandwidth.x1=sd(compare_peaks.result$cv.bandwidth.x1),
  cv.bandwidth.x2=sd(compare_peaks.result$cv.bandwidth.x2)
)

unlink("output/experiment.RData")
save(oracle.result, compare_peaks.result, mean_values, sd_values, file="output/experiment.RData")
