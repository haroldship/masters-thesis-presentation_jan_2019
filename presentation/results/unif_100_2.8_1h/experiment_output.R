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
factor <- experiment$EN.i

# 2-4: load previous experiment, compute normalized
load("output/experiment.RData")
compare_peaks.result$oracle.nise <- 10**9 * compare_peaks.result$oracle.ise / factor**2
compare_peaks.result$silverman.nise <- 10**9 * compare_peaks.result$silverman.ise / factor**2
compare_peaks.result$cv.nise <- 10**9 * compare_peaks.result$cv.ise / factor**2

compare_peaks.result$oracle.niae <- compare_peaks.result$oracle.iae / factor
compare_peaks.result$silverman.niae <- compare_peaks.result$silverman.iae / factor
compare_peaks.result$cv.niae <- compare_peaks.result$cv.iae / factor

compare_peaks.result$oracle.nsup <- compare_peaks.result$oracle.sup / factor
compare_peaks.result$silverman.nsup <- compare_peaks.result$silverman.sup / factor
compare_peaks.result$cv.nsup <- compare_peaks.result$cv.sup / factor

mean_values$oracle.nmise <- 10**9 * mean_values$oracle.mise / factor**2
mean_values$silverman.nmise <- 10**9 * mean_values$silverman.mise / factor**2
mean_values$cv.nmise <- 10**9 * mean_values$cv.mise / factor**2

mean_values$oracle.nmiae <- mean_values$oracle.miae / factor
mean_values$silverman.nmiae <- mean_values$silverman.miae / factor
mean_values$cv.nmiae <- mean_values$cv.miae / factor

mean_values$oracle.nmsup <- mean_values$oracle.msup / factor
mean_values$silverman.nmsup <- mean_values$silverman.msup / factor
mean_values$cv.nmsup <- mean_values$cv.msup / factor

# 5. Output the results

# a) population
pdf(file=paste(outdir, "population-heatmap.pdf", sep="/"))
p <- levelplot(experiment$pop.true,
               row.values=experiment$region$x1,
               column.values=experiment$region$x2,
               col.regions=gray.colors(100),
               xlab=expression(x[1]),
               ylab=expression(x[2]))
print(p)
dev.off()

pop <- as.data.frame(experiment$population)
names(pop) <- c('X1', 'X2')

pdf(file=paste(outdir, "population-points.pdf", sep="/"))
p <- xyplot(X2 ~ X1, data=pop,
            cex=0.2,
            alpha=0.5,
            col="black",
            aspect=1,
            xlim=c(experiment$region$x1.min, experiment$region$x1.max),
            ylim=c(experiment$region$x2.min, experiment$region$x2.max),
            xlab=expression(x[1]),
            ylab=expression(x[2]))
print(p)
dev.off()

# b) error histograms

error_hist <- function(df, title, e, m, filename) {
  oe <- paste('oracle', e, sep='.')
  se <- paste('silverman', e, sep='.')
  ce <- paste('cv', e, sep='.')
  om <- paste('oracle', m, sep='.')
  sm <- paste('silverman', m, sep='.')
  cm <- paste('cv', m, sep='.')
  if (hasArg(filename)) {
    pdf(file=paste(outdir, filename, sep="/"))
  }
  g <- ggplot(df) +
#    ggtitle(title) +
    theme_classic() +
    theme(axis.ticks.y=element_blank(), axis.text.y=element_blank()) +
    theme(legend.text=element_text(size=16, family='NewCenturySchoolbook'),
          legend.title=element_text(size=16, family='NewCenturySchoolbook'),
          legend.key.size=unit(1.5, 'cm')) +
    ylab(NULL) +
    xlab(NULL) +
    scale_linetype_manual(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
                          values=c("twodash", "solid", "dashed")) +
#    scale_colour_manual(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
#                        values=gray.colors(3)) +
#    stat_density(aes_string(x=oe, colour='"Oracle"', linetype='"Oracle"'), geom='line', size=0.8) +
#    stat_density(aes_string(x=se, colour='"Silverman"', linetype='"Silverman"'), geom='line', size=0.8) +
#    stat_density(aes_string(x=ce, colour='"CV"', linetype='"CV"'), geom='line', size=0.8) +
#    geom_vline(data=mean_values, aes_string(xintercept=om, colour='"Oracle"', linetype='"Oracle"'),
#               size=0.8, show.legend=FALSE) +
#    geom_vline(data=mean_values, aes_string(xintercept=sm, colour='"Silverman"', linetype='"Silverman"'),
#               size=0.8, show.legend=FALSE) +
#    geom_vline(data=mean_values, aes_string(xintercept=cm, colour='"CV"', linetype='"CV"'),
#               size=0.8, show.legend=FALSE)
    stat_density(aes_string(x=oe, linetype='"Oracle"'), geom='line', size=0.8) +
    stat_density(aes_string(x=se, linetype='"Silverman"'), geom='line', size=0.8) +
    stat_density(aes_string(x=ce, linetype='"CV"'), geom='line', size=0.8) +
    geom_vline(data=mean_values, aes_string(xintercept=om, linetype='"Oracle"'),
               size=0.8, show.legend=FALSE) +
    geom_vline(data=mean_values, aes_string(xintercept=sm, linetype='"Silverman"'),
               size=0.8, show.legend=FALSE) +
    geom_vline(data=mean_values, aes_string(xintercept=cm, linetype='"CV"'),
               size=0.8, show.legend=FALSE)
  print(g)
  if (hasArg(filename)) {
    dev.off()
  }
}

error_hist(compare_peaks.result, "Relative ISE", 'rise', 'rmise', 'ise-relative-histogram.pdf')
error_hist(compare_peaks.result, "Normalized ISE", 'nise', 'nmise', 'ise-normalized-histogram.pdf')
error_hist(compare_peaks.result, "ISE", 'ise', 'mise', 'ise-histogram.pdf')

error_hist(compare_peaks.result, "Relative IAE", 'riae', 'rmiae', 'iae-relative-histogram.pdf')
error_hist(compare_peaks.result, "Normalized IAE", 'niae', 'nmiae', 'iae-normalized-histogram.pdf')
error_hist(compare_peaks.result, "IAE", 'iae', 'miae', 'iae-histogram.pdf')

error_hist(compare_peaks.result, "Supremum", 'sup', 'msup', 'maxerr-histogram.pdf')

error_hist(compare_peaks.result, "Relative Peak distances - centroid", 'cent_dist', 'mcdist', 'centroid-dist-histogram.pdf')
error_hist(compare_peaks.result, "Peak distances", 'peak_dist', 'mdist', 'peak-dist-histogram.pdf')

error_hist(compare_peaks.result, "Peak error - centroid", 'cent_err', 'mcerr', 'centroid-height-histogram.pdf')
error_hist(compare_peaks.result, "Peak error", 'peak_err', 'merr', 'peak-height-histogram.pdf')


# c) bandwidth plots
pdf(file=paste(outdir, "bandwidths-x1.pdf", sep="/"))
ggplot(compare_peaks.result) +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  geom_histogram(aes(x=cv.bandwidth.x1), alpha=.5, bins=16, colour="black", fill="white")
dev.off()

pdf(file=paste(outdir, "bandwidths-x2.pdf", sep="/"))
ggplot(compare_peaks.result) +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  geom_histogram(aes(x=cv.bandwidth.x2), alpha=.5, bins=16, colour="black", fill="white")
dev.off()

pdf(file=paste(outdir, "bandwidths-silverman.pdf", sep="/"))
ggplot(compare_peaks.result) +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  geom_histogram(aes(x=silverman.bandwidth), alpha=.5, bins=16, colour="black", fill="white")
dev.off()

pdf(file=paste(outdir, "bandwidths-difference.pdf", sep="/"))
ggplot(compare_peaks.result) +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  geom_histogram(aes(x=cv.bandwidth.x1 - cv.bandwidth.x2), alpha=0.5, bins=16, colour="black", fill="white")
dev.off()

# 5 output results table
#options(digits=4)
max.val <- max(experiment$Rtrue)
base <- experiment$region$x1.max - experiment$region$x1.min - 2*experiment$region$buffer

measures <- c("MISE", "Relative MISE", "Normalized MISE",
              "MIAE", "Relative MIAE", "Normalized MIAE",
              "Supremum error", "Normalized Sup error",
              "Peak bias", "Relative Peak bias",
              "Peak drift", "Relative Peak drift",
              "Centroid bias", "Relative Centroid bias",
              "Centroid drift", "Relative Centroid drift")

meansdf <- data.frame(
  Oracle=c(mean_values$oracle.mise[1], mean_values$oracle.rmise[1],
           10**9 * mean_values$oracle.mise[1]/factor**2,
           mean_values$oracle.miae[1], mean_values$oracle.rmiae[1],
           mean_values$oracle.miae[1]/factor,
           mean_values$oracle.msup[1],
           mean_values$oracle.msup[1]/factor,
           mean_values$oracle.merr[1], mean_values$oracle.merr[1]/max.val,
           mean_values$oracle.mdist[1], mean_values$oracle.mdist[1]/base,
           mean_values$oracle.mcerr[1], mean_values$oracle.mcerr[1]/max.val,
           mean_values$oracle.mcdist[1], mean_values$oracle.mcdist[1]/base),
  Silverman=c(mean_values$silverman.mise[1], mean_values$silverman.rmise[1],
              10**9 * mean_values$silverman.mise[1]/factor**2,
              mean_values$silverman.miae[1], mean_values$silverman.rmiae[1],
              mean_values$silverman.miae[1]/factor,
              mean_values$silverman.msup[1],
              mean_values$silverman.msup[1]/factor,
              mean_values$silverman.merr[1], mean_values$silverman.merr[1]/max.val,
              mean_values$silverman.mdist[1], mean_values$silverman.mdist[1]/base,
              mean_values$silverman.mcerr[1], mean_values$silverman.mcerr[1]/max.val,
              mean_values$silverman.mcdist[1], mean_values$silverman.mcdist[1]/base),
  CV=c(mean_values$cv.mise[1], mean_values$cv.rmise[1],
       10**9 * mean_values$cv.mise[1]/factor**2,
       mean_values$cv.miae[1], mean_values$cv.rmiae[1],
       mean_values$cv.miae[1]/factor,
       mean_values$cv.msup[1],
       mean_values$cv.msup[1]/factor,
       mean_values$cv.merr[1], mean_values$cv.merr[1]/max.val,
       mean_values$cv.mdist[1], mean_values$cv.mdist[1]/base,
       mean_values$cv.mcerr[1], mean_values$cv.mcerr[1]/max.val,
       mean_values$cv.mcdist[1], mean_values$cv.mcdist[1]/base)#,
)
rownames(meansdf) <- measures

mean_table <- xtable(meansdf, digits=6, align="lrrr")
sink(paste(outdir, "mean_table.tex", sep="/"))
print(mean_table, include.rownames=TRUE, floating=FALSE, booktabs = TRUE)
sink()

stddevdf <- data.frame(
  Oracle=c(sd_values$oracle.mise[1], sd_values$oracle.rmise[1],
           10**9 * sd_values$oracle.mise[1]/factor**2,
           sd_values$oracle.miae[1], sd_values$oracle.rmiae[1],
           sd_values$oracle.miae[1]/factor,
           sd_values$oracle.msup[1],
           sd_values$oracle.msup[1]/factor,
           sd_values$oracle.merr[1], sd_values$oracle.merr[1]/max.val,
           sd_values$oracle.mdist[1], sd_values$oracle.mdist[1]/base,
           sd_values$oracle.mcerr[1], sd_values$oracle.mcerr[1]/max.val,
           sd_values$oracle.mcdist[1], sd_values$oracle.mcdist[1]/base),
  Silverman=c(sd_values$silverman.mise[1], sd_values$silverman.rmise[1],
              10**9 * sd_values$silverman.mise[1]/factor**2,
              sd_values$silverman.miae[1], sd_values$silverman.rmiae[1],
              sd_values$silverman.miae[1]/factor,
              sd_values$silverman.msup[1],
              sd_values$silverman.msup[1]/factor,
              sd_values$silverman.merr[1], sd_values$silverman.merr[1]/max.val,
              sd_values$silverman.mdist[1], sd_values$silverman.mdist[1]/base,
              sd_values$silverman.mcerr[1], sd_values$silverman.mcerr[1]/max.val,
              sd_values$silverman.mcdist[1], sd_values$silverman.mcdist[1]/base),
  CV=c(sd_values$cv.mise[1], sd_values$cv.rmise[1],
       10**9 * sd_values$cv.mise[1]/factor**2,
       sd_values$cv.miae[1], sd_values$cv.rmiae[1],
       sd_values$cv.miae[1]/factor,
       sd_values$cv.msup[1],
       sd_values$cv.msup[1]/factor,
       sd_values$cv.merr[1], sd_values$cv.merr[1]/max.val,
       sd_values$cv.mdist[1], sd_values$cv.mdist[1]/base,
       sd_values$cv.mcerr[1], sd_values$cv.mcerr[1]/max.val,
       sd_values$cv.mcdist[1], sd_values$cv.mcdist[1]/base)#,
)
rownames(stddevdf) <- measures

std_table <- xtable(stddevdf, digits=6, align="lrrr")
sink(paste(outdir, "std_table.tex", sep="/"))
print(std_table, include.rownames=TRUE, floating=FALSE, booktabs = TRUE)
sink()

# print/plot one simulation
x <- one_sim(experiment, oracle.result, plot=TRUE, outputdir=outdir)

