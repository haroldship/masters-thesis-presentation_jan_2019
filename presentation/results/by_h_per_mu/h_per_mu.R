library(doublekernel)
library(Hmisc)
library(ggplot2)

ENs <- c(50, 100, 200, 500, 1000)

mu <- numeric()

oracle.nmise <- numeric()
silverman.nmise <- numeric()
cv.nmise <- numeric()

oracle.best_h1 <- numeric()
oracle.best_h2 <- numeric()
silverman.mean_h <- numeric()
cv.mean_h1 <- numeric()
cv.mean_h2 <- numeric()


for (EN.i in ENs) {
  DIR <- paste("/Users/harold/Dropbox/MA_Knowledge_and_Info/Thesis/thesis/results/unif_", EN.i, "_1.0_1h", sep="")
  setwd(DIR)
  source("experiment_setup.R")
  load("output/experiment.RData")

  factor <- experiment$EN.i
  mu <- c(mu, factor)

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

setwd("/Users/harold/Dropbox/MA_Knowledge_and_Info/Thesis/thesis/results/by_h_per_mu/")

df <- data.frame(mu, oracle.best_h1, oracle.best_h2, silverman.mean_h, cv.mean_h1, cv.mean_h2,
                 oracle.nmise, silverman.nmise, cv.nmise)

df.latex <- latex(df,
                  file="h_per_mu.tex",
                  title="h_per_mu",
                  where="htbp",
                  label="tab:results:bandwidth_vs_mu",
                  rowname=NULL,
                  booktabs=TRUE,
                  cgroup=c("", "Mean Bandwidths", "NMISE"),
                  n.cgroup=c(1, 5, 3),
                  colheads=c("$N_I$", '$h_{o1}$', '$h_{o2}$', '$h_{s}$', '$h_{cv1}$', '$h_{cv2}$', 'Oracle', 'Silverman', 'CV'),
                  cdec=c(0, rep(1, 5), rep(3, 3)),
                  caption.loc="bottom",
                  caption="Bandwidth and accuracy by expected number of incidents for uniform population of 10,000 with single-peak risk function, spread of 1.0. The NMISE values are scaled by $10^9$.",
                  caption.lot="Bandwidth and accuracy by expected number of incidents")

fit_ho1 <- lm(log(oracle.best_h1) ~ log(mu), data=df)
coef_ho1 <- coef(fit_ho1)
fit_ho2 <- lm(log(oracle.best_h2) ~ log(mu), data=df)
coef_ho2 <- coef(fit_ho2)
fit_hs <- lm(log(silverman.mean_h) ~ log(mu), data=df)
coef_hs <- coef(fit_hs)
fit_hcv1 <- lm(log(cv.mean_h1) ~ log(mu), data=df)
coef_hcv1 <- coef(fit_hcv1)
fit_hcv2 <- lm(log(cv.mean_h2) ~ log(mu), data=df)
coef_hcv2 <- coef(fit_hcv2)

pdf(file="oracle_bandwidth_x1_vs_mu.pdf")
ggplot(df) +
  geom_point(aes(x=mu, y=oracle.best_h1), size=3) +
  stat_function(fun=function(x) {exp(coef_ho1[1])*x**(coef_ho1[2])}, xlim=c(45,1100)) +
  coord_trans(x='log10', y='log10') +
  annotation_logticks(scaled=FALSE) +
  xlab(expression(N[I])) +
  ylab(expression(paste("Oracle Bandwidth ", x[1])))
dev.off()

pdf(file="oracle_bandwidth_x2_vs_mu.pdf")
ggplot(df) +
  geom_point(aes(x=mu, y=oracle.best_h2), size=3) +
  stat_function(fun=function(x) {exp(coef_ho2[1])*x**(coef_ho2[2])}, xlim=c(45,1100)) +
  coord_trans(x='log10', y='log10') +
  annotation_logticks(scaled=FALSE) +
  xlab(expression(N[I])) +
  ylab(expression(paste("Oracle Bandwidth ", x[2])))
dev.off()

pdf(file="silverman_bandwidth_vs_mu.pdf")
ggplot(df) +
  geom_point(aes(x=mu, y=silverman.mean_h), size=3) +
  stat_function(fun=function(x) {exp(coef_hs[1])*x**(coef_hs[2])}, xlim=c(45,1100)) +
  coord_trans(x='log10', y='log10') +
  annotation_logticks(scaled=FALSE) +
  xlab(expression(N[I])) +
  ylab("Silverman Bandwidth")
dev.off()

pdf(file="cv_bandwidth_x1_vs_mu.pdf")
ggplot(df) +
  geom_point(aes(x=mu, y=cv.mean_h1), size=3) +
  stat_function(fun=function(x) {exp(coef_hcv1[1])*x**(coef_hcv1[2])}, xlim=c(45,1100)) +
  coord_trans(x='log10', y='log10') +
  annotation_logticks(scaled=FALSE) +
  xlab(expression(N[I])) +
  ylab(expression(paste("CV Bandwidth ", x[1])))
dev.off()

pdf(file="cv_bandwidth_x2_vs_mu.pdf")
ggplot(df) +
  geom_point(aes(x=mu, y=cv.mean_h2), size=3) +
  stat_function(fun=function(x) {exp(coef_hcv2[1])*x**(coef_hcv2[2])}, xlim=c(45,1100)) +
  coord_trans(x='log10', y='log10') +
  annotation_logticks(scaled=FALSE) +
  xlab(expression(N[I])) +
  ylab(expression(paste("CV Bandwidth ", x[2])))
dev.off()

Selector <- c("Oracle Bandwidth $x_1$",
                      "Oracle Bandwidth $x_1$",
                      "Silverman",
                      "CV Bandwidth $x_1$",
                      "CV Bandwidth $x_2$",
                      "Theory")
Slope <- c(coef_ho1[2],
           coef_ho2[2],
           coef_hs[2],
           coef_hcv1[2],
           coef_hcv2[2],
           -1/6)

df.alpha <- data.frame(Selector, Slope)
df.alpha.latex <- latex(df.alpha,
                        title="alpha_by_selector",
                        where="htbp",
                        label="tab:results:bandwidth_alpha_by_selector",
                        rowname=NULL,
                        booktabs=TRUE,
                        cdec=c(0,3),
                        caption.loc="bottom",
                        caption="Bandwidth convergence rate $\\alpha$ of for different bandwidth selectors for a single-peak risk function with spread of 1.0 on a uniform population of 10,000.",
                        caption.lot="Bandwidth convergence rate of bandwidth selectors")

