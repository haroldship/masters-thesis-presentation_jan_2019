setwd("/Users/harold/Dropbox/MA_Knowledge_and_Info/Thesis/thesis/results/by_cases_spread")

source("../plots.R")

Spreads <- c(0.7, 1, 1.4, 2, 2.8)
Dirs <- sapply(Spreads, function(rate) paste('../unif_100_', format(rate, digits=2, nsmall=1), '_1h/output/', sep=''))
Files <- sapply(Dirs, function(dir) paste(dir, 'mean_table.tex', sep=''))

N <- length(Spreads)
Errs <- character()

matO <- NULL
matS <- NULL
matC <- NULL


for (i in 1:N) {
  Spread <- Spreads[i]
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
dfO$Spread <- Spreads
dfO$Bandwidth <- "Oracle"

dfS <- as.data.frame(matS)
dfS$Spread <- Spreads
dfS$Bandwidth <- "Silverman"

dfC <- as.data.frame(matC)
dfC$Spread <- Spreads
dfC$Bandwidth <- "CV"

df <- rbind(dfO, dfS, dfC)
df$Bandwidth <- as.factor(df$Bandwidth)

coefO <- coef(lm(log(dfO$`Relative MISE`) ~ log(dfO$Spread)))
coefS <- coef(lm(log(dfS$`Relative MISE`) ~ log(dfS$Spread)))
coefC <- coef(lm(log(dfC$`Relative MISE`) ~ log(dfC$Spread)))


pdf(file="MISE-vs-risk-spread.pdf")
make_accuracy_plot(df, expression(sigma[I])) +
  geom_point(aes(x=Spread, y=MISE, shape=Bandwidth), size=3)
dev.off()
pdf(file="RMISE-vs-risk-spread.pdf")
make_accuracy_plot(df, expression(sigma[I])) +
  geom_point(aes(x=Spread, y=`Relative MISE`, shape=Bandwidth), size=3)
dev.off()
pdf(file="RMISE-vs-risk-spread-log-log.pdf")
make_accuracy_plot(df, expression(sigma[I])) +
  geom_point(aes(x=Spread, y=`Relative MISE`, shape=Bandwidth), size=3) +
  stat_function(fun=function(x) {exp(coefO[1])*x**(coefO[2])}, aes(linetype="Oracle"), xlim=c(0.4, 3.0)) +
  stat_function(fun=function(x) {exp(coefS[1])*x**(coefS[2])}, aes(linetype="Silverman"), xlim=c(0.4, 3.0)) +
  stat_function(fun=function(x) {exp(coefC[1])*x**(coefC[2])}, aes(linetype="CV"), xlim=c(0.4, 3.0)) +
  coord_trans(x='log10', y='log10') +
  annotation_logticks(scaled=FALSE)
dev.off()

# redo for NMISE
coefO <- coef(lm(log(dfO$`Normalized MISE`) ~ log(dfO$Spread)))
coefS <- coef(lm(log(dfS$`Normalized MISE`) ~ log(dfS$Spread)))
coefC <- coef(lm(log(dfC$`Normalized MISE`) ~ log(dfC$Spread)))

pdf(file="NMISE-vs-risk-spread.pdf")
make_accuracy_plot(df, expression(sigma[I])) +
  geom_point(aes(x=Spread, y=`Normalized MISE`, shape=Bandwidth), size=3) +
  stat_function(fun=function(x) {exp(coefO[1])*x**(coefO[2])}, aes(linetype="Oracle"), xlim=c(0.4, 3.0)) +
  stat_function(fun=function(x) {exp(coefS[1])*x**(coefS[2])}, aes(linetype="Silverman"), xlim=c(0.4, 3.0)) +
  stat_function(fun=function(x) {exp(coefC[1])*x**(coefC[2])}, aes(linetype="CV"), xlim=c(0.4, 3.0))
dev.off()
pdf(file="NMISE-vs-risk-spread-log-log.pdf")
make_accuracy_plot(df, expression(sigma[I])) +
  geom_point(aes(x=Spread, y=`Normalized MISE`, shape=Bandwidth), size=3) +
  stat_function(fun=function(x) {exp(coefO[1])*x**(coefO[2])}, aes(linetype="Oracle"), xlim=c(0.4, 3.0)) +
  stat_function(fun=function(x) {exp(coefS[1])*x**(coefS[2])}, aes(linetype="Silverman"), xlim=c(0.4, 3.0)) +
  stat_function(fun=function(x) {exp(coefC[1])*x**(coefC[2])}, aes(linetype="CV"), xlim=c(0.4, 3.0)) +
  coord_trans(x='log10', y='log10') +
  annotation_logticks(scaled=FALSE)
dev.off()

Selector <- c("Oracle", "Silverman", "CV")
Slope <- c(coefO[2], coefS[2], coefC[2])

df.alpha <- data.frame(Selector, Slope)
df.alpha.latex <- latex(df.alpha,
                        title="nmise_convergence_table",
                        where="htbp",
                        label="tab:results:nmise_convergence_by_cases_spread",
                        rowname=NULL,
                        booktabs=TRUE,
                        cdec=c(0,3),
                        caption.loc="bottom",
                        caption="NMISE convergence rate by spread for different bandwidth selectors for a single-peak risk function with expected number of incidents 100 on a uniform population of 10,000.",
                        caption.lot="NMISE Convergence rate by spread for 100 cases")
