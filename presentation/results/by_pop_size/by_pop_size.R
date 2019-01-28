setwd("/Users/harold/Dropbox/MA_Knowledge_and_Info/Thesis/thesis/results/by_pop_size")

source("../plots.R")

Incidents <- c(50, 100, 200, 500, 1000)
PopStrs <- c("5k_50", "10k_100", "20k_200", "50k_500", "100k_1000")
Dirs <- sapply(PopStrs, function(pop) paste('../unif', pop, '_1.0_1h/output/', sep=''))
Files <- sapply(Dirs, function(dir) paste(dir, 'mean_table.tex', sep=''))

N <- length(Incidents)
Errs <- character()

matO <- NULL
matS <- NULL
matC <- NULL


for (i in 1:N) {
  Inc <- Incidents[i]
  File <- Files[i]
  lines <- readLines(File)
  lines <- lines[7:22]
  Os <- numeric()
  Ss <- numeric()
  Cs <- numeric()
  for (line in lines) {
    fline <- gsub("\\", "", line, fixed=TRUE)
    partsl <- strsplit(fline, "&", fixed=TRUE)
    parts <- partsl[[1]]
    key <- trimws(parts[1], which="both")
    O <- as.numeric(parts[2])
    S <- as.numeric(parts[3])
    C <- as.numeric(parts[4])
    if (! key %in% Errs) {
      Errs <- c(Errs, key)
    }
    Os <- c(Os, O)
    Ss <- c(Ss, S)
    Cs <- c(Cs, C)
  }
  M <- length(Os)
  if (is.null(matO)) matO <- matrix(0, nrow=N, ncol=M, dimnames=list(NULL, Errs))
  if (is.null(matS)) matS <- matrix(0, nrow=N, ncol=M, dimnames=list(NULL, Errs))
  if (is.null(matC)) matC <- matrix(0, nrow=N, ncol=M, dimnames=list(NULL, Errs))
  matO[i,] <- Os
  matS[i,] <- Ss
  matC[i,] <- Cs
}

dfO <- as.data.frame(matO)
dfO$Incidents <- Incidents
dfO$Bandwidth <- "Oracle"

dfS <- as.data.frame(matS)
dfS$Incidents <- Incidents
dfS$Bandwidth <- "Silverman"

dfC <- as.data.frame(matC)
dfC$Incidents <- Incidents
dfC$Bandwidth <- "CV"

df <- rbind(dfO, dfS, dfC)
df$Bandwidth <- as.factor(df$Bandwidth)

coefO <- coef(lm(log(dfO$`Relative MISE`) ~ log(dfO$Incidents)))
coefS <- coef(lm(log(dfS$`Relative MISE`) ~ log(dfS$Incidents)))
coefC <- coef(lm(log(dfC$`Relative MISE`) ~ log(dfC$Incidents)))


pdf(file="MISE-vs-population.pdf")
make_accuracy_plot(df, expression(N[I])) +
  geom_point(aes(x=Incidents, y=MISE, shape=Bandwidth), size=3)
dev.off()
pdf(file="RMISE-vs-population.pdf")
make_accuracy_plot(df, expression(N[I])) +
  geom_point(aes(x=Incidents, y=`Relative MISE`, shape=Bandwidth), size=3)
dev.off()
pdf(file="RMISE-vs-population-log-log.pdf")
make_accuracy_plot(df, expression(N[I])) +
  geom_point(aes(x=Incidents, y=`Relative MISE`, shape=Bandwidth), size=3) +
  coord_trans(x='log10', y='log10') +
  annotation_logticks(scaled=FALSE)
dev.off()

coefO <- coef(lm(log(dfO$`Normalized MISE`) ~ log(dfO$Incidents)))
coefS <- coef(lm(log(dfS$`Normalized MISE`) ~ log(dfS$Incidents)))
coefC <- coef(lm(log(dfC$`Normalized MISE`) ~ log(dfC$Incidents)))

pdf(file="NMISE-vs-population.pdf")
make_accuracy_plot(df, expression(N[I])) +
  geom_point(aes(x=Incidents, y=`Normalized MISE`, shape=Bandwidth), size=3) +
  stat_function(fun=function(x) {exp(coefO[1])*x**(coefO[2])}, aes(linetype="Oracle"), xlim=c(40,1100)) +
  stat_function(fun=function(x) {exp(coefS[1])*x**(coefS[2])}, aes(linetype="Silverman"), xlim=c(40,1100)) +
  stat_function(fun=function(x) {exp(coefC[1])*x**(coefC[2])}, aes(linetype="CV"), xlim=c(40,1100))
dev.off()
pdf(file="NMISE-vs-population-log-log.pdf")
make_accuracy_plot(df, expression(N[I])) +
  geom_point(aes(x=Incidents, y=`Normalized MISE`, shape=Bandwidth), size=3) +
  stat_function(fun=function(x) {exp(coefO[1])*x**(coefO[2])}, aes(linetype="Oracle"), xlim=c(40,1100)) +
  stat_function(fun=function(x) {exp(coefS[1])*x**(coefS[2])}, aes(linetype="Silverman"), xlim=c(40,1100)) +
  stat_function(fun=function(x) {exp(coefC[1])*x**(coefC[2])}, aes(linetype="CV"), xlim=c(40,1100)) +
  coord_trans(x='log10', y='log10') +
  annotation_logticks(scaled=FALSE)
dev.off()

Selector <- c("Oracle", "Silverman", "CV")
Slope <- c(coefO[2], coefS[2], coefC[2])

df.alpha <- data.frame(Selector, Slope)
df.alpha.latex <- latex(df.alpha,
                        title="nmise_convergence_table",
                        where="htbp",
                        label="tab:results:nmise_convergence_by_sample_size",
                        rowname=NULL,
                        booktabs=TRUE,
                        cdec=c(0,3),
                        caption.loc="bottom",
                        caption="NMISE convergence rate by sample size for different bandwidth selectors for a fixed, single-peak risk function with expected number of incidents proportional to population size on a uniform population.",
                        caption.lot="NMISE Convergence rate by sample size")

