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

# 2-4: load previous experiment
load("output/experiment.RData")
compare_peaks.result$oracle.nise <- 10**9 * compare_peaks.result$oracle.ise / factor**2
compare_peaks.result$silverman.nise <- 10**9 * compare_peaks.result$silverman.ise / factor**2
compare_peaks.result$cv.nise <- 10**9 * compare_peaks.result$cv.ise / factor**2

compare_peaks.result$oracle.niae <- 10**9 * compare_peaks.result$oracle.iae / factor**2
compare_peaks.result$silverman.niae <- 10**9 * compare_peaks.result$silverman.iae / factor**2
compare_peaks.result$cv.niae <- 10**9 * compare_peaks.result$cv.iae / factor**2

compare_peaks.result$oracle.nsup <- 10**9 * compare_peaks.result$oracle.sup / factor**2
compare_peaks.result$silverman.nsup <- 10**9 * compare_peaks.result$silverman.sup / factor**2
compare_peaks.result$cv.nsup <- 10**9 * compare_peaks.result$cv.sup / factor**2

# 5. Output the results

# a) population
pdf(file=paste(outdir, "population-heatmap.pdf", sep="/"))
p <- levelplot(experiment$pop.true,
               row.values=experiment$region$x1,
               column.values=experiment$region$x2,
               xlab=expression(X[1]),
               ylab=expression(X[2]),
               main="True population distribution")
print(p)
dev.off()

pop <- as.data.frame(experiment$population)
names(pop) <- c('X1', 'X2')

pdf(file=paste(outdir, "population-points.pdf", sep="/"))
p <- xyplot(X2 ~ X1, data=pop,
            cex=0.2,
            aspect=1,
            xlim=c(experiment$region$x1.min, experiment$region$x1.max),
            ylim=c(experiment$region$x2.min, experiment$region$x2.max),
            xlab=expression(X[1]),
            ylab=expression(X[2]),
            main="Population locations")
print(p)
dev.off()

# b) error histograms

pdf(file=paste(outdir, "ise-relative-histogram.pdf", sep="/"))
ggplot(compare_peaks.result) +
  ggtitle("Relative ISE") +
  theme_linedraw() +
  theme(axis.ticks=element_blank(), axis.text.y=element_blank()) +
  xlab("Relative ISE") +
  ylab(NULL) +
  geom_density(aes(x=oracle.rise, colour="Oracle", linetype="Oracle"), size=0.8) +
  geom_density(aes(x=silverman.rise, colour="Silverman", linetype="Silverman"), size=0.8) +
  geom_density(aes(x=cv.rise, colour="CV", linetype="CV"), size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=oracle.rmise, colour="Oracle", linetype="Oracle"),
             size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=silverman.rmise, colour="Silverman", linetype="Silverman"),
             size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=cv.rmise, colour="CV", linetype="CV"),
             size=0.8) +
  scale_linetype_manual(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
                        values=c("dashed", "solid", "dotted")) +
  scale_colour_brewer(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
                      palette="Dark2")
dev.off()

pdf(file=paste(outdir, "ise-normalized-histogram.pdf", sep="/"))
ggplot(compare_peaks.result) +
  ggtitle("Normalized ISE") +
  theme_linedraw() +
  theme(axis.ticks=element_blank(), axis.text.y=element_blank()) +
  xlab("Normalized ISE") +
  ylab(NULL) +
  geom_density(aes(x=oracle.nise, colour="Oracle", linetype="Oracle"), size=0.8) +
  geom_density(aes(x=silverman.nise, colour="Silverman", linetype="Silverman"), size=0.8) +
  geom_density(aes(x=cv.nise, colour="CV", linetype="CV"), size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=oracle.mise/factor, colour="Oracle", linetype="Oracle"),
             size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=silverman.mise/factor, colour="Silverman", linetype="Silverman"),
             size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=cv.mise/factor, colour="CV", linetype="CV"),
             size=0.8) +
  scale_linetype_manual(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
                        values=c("dashed", "solid", "dotted")) +
  scale_colour_brewer(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
                      palette="Dark2")
dev.off()


pdf(file=paste(outdir, "ise-histogram.pdf", sep="/"))
ggplot(compare_peaks.result) +
  ggtitle("ISE") +
  theme_linedraw() +
  theme(axis.ticks=element_blank(), axis.text.y=element_blank()) +
  xlab("ISE") +
  ylab(NULL) +
  geom_density(aes(x=oracle.ise, colour="Oracle", linetype="Oracle"), size=0.8) +
  geom_density(aes(x=silverman.ise, colour="Silverman", linetype="Silverman"), size=0.8) +
  geom_density(aes(x=cv.ise, colour="CV", linetype="CV"), size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=oracle.mise, colour="Oracle", linetype="Oracle"),
             size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=silverman.mise, colour="Silverman", linetype="Silverman"),
             size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=cv.mise, colour="CV", linetype="CV"),
             size=0.8) +
  scale_linetype_manual(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
                        values=c("dashed", "solid", "dotted")) +
  scale_colour_brewer(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
                      palette="Dark2")

dev.off()

pdf(file=paste(outdir, "iae-relative-histogram.pdf", sep="/"))
ggplot(compare_peaks.result) +
  ggtitle("Relative IAE") +
  theme_linedraw() +
  theme(axis.ticks=element_blank(), axis.text.y=element_blank()) +
  xlab("Relative IAE") +
  ylab(NULL) +
  geom_density(aes(x=oracle.riae, colour="Oracle", linetype="Oracle"), size=0.8) +
  geom_density(aes(x=silverman.riae, colour="Silverman", linetype="Silverman"), size=0.8) +
  geom_density(aes(x=cv.riae, colour="CV", linetype="CV"), size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=oracle.rmiae, colour="Oracle", linetype="Oracle"),
             size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=silverman.rmiae, colour="Silverman", linetype="Silverman"),
             size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=cv.rmiae, colour="CV", linetype="CV"),
             size=0.8) +
  scale_linetype_manual(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
                        values=c("dashed", "solid", "dotted")) +
  scale_colour_brewer(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
                      palette="Dark2")
dev.off()

pdf(file=paste(outdir, "iae-histogram.pdf", sep="/"))
ggplot(compare_peaks.result) +
  ggtitle("IAE") +
  theme_linedraw() +
  theme(axis.ticks=element_blank(), axis.text.y=element_blank()) +
  xlab("IAE") +
  ylab(NULL) +
  geom_density(aes(x=oracle.iae, colour="Oracle", linetype="Oracle"), size=0.8) +
  geom_density(aes(x=silverman.iae, colour="Silverman", linetype="Silverman"), size=0.8) +
  geom_density(aes(x=cv.iae, colour="CV", linetype="CV"), size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=oracle.miae, colour="Oracle", linetype="Oracle"),
             size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=silverman.miae, colour="Silverman", linetype="Silverman"),
             size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=cv.miae, colour="CV", linetype="CV"),
             size=0.8) +
  scale_linetype_manual(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
                        values=c("dashed", "solid", "dotted")) +
  scale_colour_brewer(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
                      palette="Dark2")
dev.off()

pdf(file=paste(outdir, "maxerr-histogram.pdf", sep="/"))
ggplot(compare_peaks.result) +
  ggtitle("Sup (Maximum)") +
  theme_linedraw() +
  theme(axis.ticks=element_blank(), axis.text.y=element_blank()) +
  xlab("Sup (Maximum) Error") +
  ylab(NULL) +
  geom_density(aes(x=oracle.sup, colour="Oracle", linetype="Oracle"), size=0.8) +
  geom_density(aes(x=silverman.sup, colour="Silverman", linetype="Silverman"), size=0.8) +
  geom_density(aes(x=cv.sup, colour="CV", linetype="CV"), size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=oracle.msup, colour="Oracle", linetype="Oracle"),
             size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=silverman.msup, colour="Silverman", linetype="Silverman"),
             size=0.8) +
  geom_vline(data=mean_values, aes(xintercept=cv.msup, colour="CV", linetype="CV"),
             size=0.8) +
  scale_linetype_manual(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
                        values=c("dashed", "solid", "dotted")) +
  scale_colour_brewer(name="Bandwidth\nSelector", labels=c("CV", "Oracle", "Silverman"),
                      palette="Dark2")
dev.off()

pdf(file=paste(outdir, "centroid-dist-histogram.pdf", sep="/"))
ggplot(compare_peaks.result) +
  ggtitle("Relative Peak distances - centroid") +
  geom_histogram(aes(x=cv.cent_dist, fill="CV"), alpha=.5) +
  geom_histogram(aes(x=oracle.cent_dist, fill="Oracle"), alpha=.5) +
  geom_histogram(aes(x=silverman.cent_dist, fill="Silverman"), alpha=.5) +
  geom_vline(data=mean_values, aes(xintercept=oracle.mcdist, colour="Oracle"),
             linetype="dashed", size=1, show.legend=FALSE) +
  geom_vline(data=mean_values, aes(xintercept=cv.mcdist, colour="CV"),
             linetype="dashed", size=1, show.legend=FALSE) +
  geom_vline(data=mean_values, aes(xintercept=silverman.mcdist, colour="Silverman"),
             linetype="dashed", size=1, show.legend=FALSE)
dev.off()

pdf(file=paste(outdir, "peak-dist-histogram.pdf", sep="/"))
ggplot(compare_peaks.result) +
  ggtitle("Peak distances") +
  geom_histogram(aes(x=cv.peak_dist, fill="CV"), alpha=.5) +
  geom_histogram(aes(x=oracle.peak_dist, fill="Oracle"), alpha=.5) +
  geom_histogram(aes(x=silverman.peak_dist, fill="Silverman"), alpha=.5) +
  geom_vline(data=mean_values, aes(xintercept=oracle.mdist, colour="Oracle"),
             linetype="dashed", size=1, show.legend=FALSE) +
  geom_vline(data=mean_values, aes(xintercept=cv.mdist, colour="CV"),
             linetype="dashed", size=1, show.legend=FALSE) +
  geom_vline(data=mean_values, aes(xintercept=silverman.mdist, colour="Silverman"),
             linetype="dashed", size=1, show.legend=FALSE)
dev.off()

pdf(file=paste(outdir, "centroid-height-histogram.pdf", sep="/"))
ggplot(compare_peaks.result) +
  ggtitle("Peak height error - centroid") +
  geom_histogram(aes(x=cv.cent_err, fill="CV"), alpha=.5) +
  geom_histogram(aes(x=oracle.cent_err, fill="Oracle"), alpha=.5) +
  geom_histogram(aes(x=silverman.cent_err, fill="Silverman"), alpha=.5) +
  geom_vline(data=mean_values, aes(xintercept=oracle.mcerr, colour="Oracle"),
             linetype="dashed", size=1, show.legend=FALSE) +
  geom_vline(data=mean_values, aes(xintercept=cv.mcerr, colour="CV"),
             linetype="dashed", size=1, show.legend=FALSE) +
  geom_vline(data=mean_values, aes(xintercept=silverman.mcerr, colour="Silverman"),
             linetype="dashed", size=1, show.legend=FALSE)
dev.off()

pdf(file=paste(outdir, "peak-height-histogram.pdf", sep="/"))
ggplot(compare_peaks.result) +
  ggtitle("Peak height error") +
  geom_histogram(aes(x=cv.peak_err, fill="CV"), alpha=.5) +
  geom_histogram(aes(x=oracle.peak_err, fill="Oracle"), alpha=.5) +
  geom_histogram(aes(x=silverman.peak_err, fill="Silverman"), alpha=.5) +
  geom_vline(data=mean_values, aes(xintercept=oracle.merr, colour="Oracle"),
             linetype="dashed", size=1, show.legend=FALSE) +
  geom_vline(data=mean_values, aes(xintercept=cv.merr, colour="CV"),
             linetype="dashed", size=1, show.legend=FALSE) +
  geom_vline(data=mean_values, aes(xintercept=silverman.merr, colour="Silverman"),
             linetype="dashed", size=1, show.legend=FALSE)
dev.off()

# c) bandwidth plots
pdf(file=paste(outdir, "bandwidths-x1.pdf", sep="/"))
ggplot(compare_peaks.result) +
  ggtitle("CV Bandwidths X1") +
  geom_histogram(aes(x=cv.bandwidth.x1, fill="X1"), alpha=.5)
dev.off()

pdf(file=paste(outdir, "bandwidths-x2.pdf", sep="/"))
ggplot(compare_peaks.result) +
  ggtitle("CV Bandwidths X2") +
  geom_histogram(aes(x=cv.bandwidth.x2, fill="X2"), alpha=.5)
dev.off()

pdf(file=paste(outdir, "bandwidths-silverman.pdf", sep="/"))
ggplot(compare_peaks.result) +
  ggtitle("Silverman Bandwidths") +
  geom_histogram(aes(x=silverman.bandwidth, fill="Silverman"), alpha=.5)
dev.off()

pdf(file=paste(outdir, "bandwidths-difference.pdf", sep="/"))
ggplot(compare_peaks.result) +
  ggtitle(expression(X[1] - X[2])) +
  geom_histogram(aes(x=cv.bandwidth.x1 - cv.bandwidth.x2), fill="blue", alpha=0.5)
dev.off()

# 5 output results table
#options(digits=4)
max.val <- max(experiment$Rtrue)
base <- experiment$region$x1.max - experiment$region$x1.min - 2*experiment$region$buffer

measures <- c("MISE", "Relative MISE", "Normalized MISE",
              "MIAE", "Relative MIAE", "Normalized MIAE",
              "Max Error", "Normalized Max Error",
              "Peak bias", "Relative Peak bias",
              "Peak drift", "Relative Peak drift",
              "Centroid bias", "Relative Centroid bias",
              "Centroid drift", "Relative Centroid drift")

meansdf <- data.frame(
  Oracle=c(mean_values$oracle.mise[1], mean_values$oracle.rmise[1],
           10**9 * mean_values$oracle.mise[1]/factor**2,
           mean_values$oracle.miae[1], mean_values$oracle.rmiae[1],
           10**9 * mean_values$oracle.miae[1]/factor**2,
           mean_values$oracle.msup[1],
           10**9 * mean_values$oracle.msup[1]/factor**2,
           mean_values$oracle.merr[1], mean_values$oracle.merr[1]/max.val,
           mean_values$oracle.mdist[1], mean_values$oracle.mdist[1]/base,
           mean_values$oracle.mcerr[1], mean_values$oracle.mcerr[1]/max.val,
           mean_values$oracle.mcdist[1], mean_values$oracle.mcdist[1]/base),
  Silverman=c(mean_values$silverman.mise[1], mean_values$silverman.rmise[1],
              10**9 * mean_values$silverman.mise[1]/factor**2,
              mean_values$silverman.miae[1], mean_values$silverman.rmiae[1],
              10**9 * mean_values$silverman.miae[1]/factor**2,
              mean_values$silverman.msup[1],
              10**9 * mean_values$silverman.msup[1]/factor**2,
              mean_values$silverman.merr[1], mean_values$silverman.merr[1]/max.val,
              mean_values$silverman.mdist[1], mean_values$silverman.mdist[1]/base,
              mean_values$silverman.mcerr[1], mean_values$silverman.mcerr[1]/max.val,
              mean_values$silverman.mcdist[1], mean_values$silverman.mcdist[1]/base),
  CV=c(mean_values$cv.mise[1], mean_values$cv.rmise[1],
       10**9 * mean_values$cv.mise[1]/factor**2,
       mean_values$cv.miae[1], mean_values$cv.rmiae[1],
       10**9 * mean_values$cv.miae[1]/factor**2,
       mean_values$cv.msup[1],
       10**9 * mean_values$cv.msup[1]/factor**2,
       mean_values$cv.merr[1], mean_values$cv.merr[1]/max.val,
       mean_values$cv.mdist[1], mean_values$cv.mdist[1]/base,
       mean_values$cv.mcerr[1], mean_values$cv.mcerr[1]/max.val,
       mean_values$cv.mcdist[1], mean_values$cv.mcdist[1]/base)#,
)
rownames(meansdf) <- measures

mean_table <- xtable(meansdf, digits=6, align="lrrr")
sink(paste(outdir, "mean_table.tex", sep="/"))
print(mean_table, include.rownames=TRUE, floating=FALSE)
sink()

stddevdf <- data.frame(
  Oracle=c(sd_values$oracle.mise[1], sd_values$oracle.rmise[1],
           10**9 * sd_values$oracle.mise[1]/factor**2,
           sd_values$oracle.miae[1], sd_values$oracle.rmiae[1],
           10**9 * sd_values$oracle.miae[1]/factor**2,
           sd_values$oracle.msup[1],
           10**9 * sd_values$oracle.msup[1]/factor**2,
           sd_values$oracle.merr[1], sd_values$oracle.merr[1]/max.val,
           sd_values$oracle.mdist[1], sd_values$oracle.mdist[1]/base,
           sd_values$oracle.mcerr[1], sd_values$oracle.mcerr[1]/max.val,
           sd_values$oracle.mcdist[1], sd_values$oracle.mcdist[1]/base),
  Silverman=c(sd_values$silverman.mise[1], sd_values$silverman.rmise[1],
              10**9 * sd_values$silverman.mise[1]/factor**2,
              sd_values$silverman.miae[1], sd_values$silverman.rmiae[1],
              10**9 * sd_values$silverman.miae[1]/factor**2,
              sd_values$silverman.msup[1],
              10**9 * sd_values$silverman.msup[1]/factor**2,
              sd_values$silverman.merr[1], sd_values$silverman.merr[1]/max.val,
              sd_values$silverman.mdist[1], sd_values$silverman.mdist[1]/base,
              sd_values$silverman.mcerr[1], sd_values$silverman.mcerr[1]/max.val,
              sd_values$silverman.mcdist[1], sd_values$silverman.mcdist[1]/base),
  CV=c(sd_values$cv.mise[1], sd_values$cv.rmise[1],
       10**9 * sd_values$cv.mise[1]/factor**2,
       sd_values$cv.miae[1], sd_values$cv.rmiae[1],
       10**9 * sd_values$cv.miae[1]/factor**2,
       sd_values$cv.msup[1],
       10**9 * sd_values$cv.msup[1]/factor**2,
       sd_values$cv.merr[1], sd_values$cv.merr[1]/max.val,
       sd_values$cv.mdist[1], sd_values$cv.mdist[1]/base,
       sd_values$cv.mcerr[1], sd_values$cv.mcerr[1]/max.val,
       sd_values$cv.mcdist[1], sd_values$cv.mcdist[1]/base)#,
)
rownames(stddevdf) <- measures

std_table <- xtable(stddevdf, digits=6, align="lrrr")
sink(paste(outdir, "std_table.tex", sep="/"))
print(std_table, include.rownames=TRUE, floating=FALSE)
sink()

# print/plot one simulation
x <- one_sim(experiment, oracle.result, plot=TRUE, outputdir=outdir)

