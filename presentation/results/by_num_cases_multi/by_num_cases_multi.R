library(ggplot2)
library(Hmisc)

setwd("/Users/harold/Dropbox/MA_Knowledge_and_Info/Thesis/thesis/results/by_num_cases_multi")

Decays <- c("0.7", "1.0", "1.4", "2.0", "unif")
DecayStrs <- c("0.7_1h", "1.0_1h", "1.4_1h", "2.0_1h", "unif")
M <- length(Decays)

decay <- numeric()
alphaO <- numeric()
alphaS <- numeric()
alphaC <- numeric()
kO <- numeric()
kS <- numeric()
kC <- numeric()

for (j in 1:M) {
  Cases <- c(50, 100, 200, 500, 1000)
  Decay <- Decays[j]
  DecayStr <- DecayStrs[j]
  Dirs <- sapply(Cases, function(case) paste('../unif_', case, '_', DecayStr, '/output/', sep=''))
  Files <- sapply(Dirs, function(dir) paste(dir, 'mean_table.tex', sep=''))

  N <- length(Cases)
  Errs <- character()

  matO <- NULL
  matS <- NULL
  matC <- NULL


  for (i in 1:N) {
    Case <- Cases[i]
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
  dfO$Case <- Cases
  dfO$Bandwidth <- "Oracle"

  dfS <- as.data.frame(matS)
  dfS$Case <- Cases
  dfS$Bandwidth <- "Silverman"

  dfC <- as.data.frame(matC)
  dfC$Case <- Cases
  dfC$Bandwidth <- "CV"

  df <- rbind(dfO, dfS, dfC)
  df$Bandwidth <- as.factor(df$Bandwidth)

  coefO <- coef(lm(log(dfO$`Relative MISE`) ~ log(dfO$Case)))
  coefS <- coef(lm(log(dfS$`Relative MISE`) ~ log(dfS$Case)))
  coefC <- coef(lm(log(dfC$`Relative MISE`) ~ log(dfC$Case)))

  if (Decay == "unif") decayN <- Inf else decayN <- as.numeric(Decay)
  decay <- c(decay, decayN)
  kO <- c(kO, coefO[1])
  alphaO <- c(alphaO, coefO[2])
  kS <- c(kS, coefS[1])
  alphaS <- c(alphaS, coefS[2])
  kC <- c(kC, coefC[1])
  alphaC <- c(alphaC, coefC[2])

  # pdf(file="MISE-vs-cases.pdf")
  # ggplot(df) +
  #   geom_point(aes(x=Case, y=MISE, colour=Bandwidth, shape=Bandwidth), size=3)
  # dev.off()
  # pdf(file="RMISE-vs-cases.pdf")
  # ggplot(df) +
  #   geom_point(aes(x=Case, y=`Relative MISE`, colour=Bandwidth, shape=Bandwidth), size=3) +
  #   stat_function(fun=function(x) {exp(coefO[1])*x**(coefO[2])}, aes(colour="Oracle"), xlim=c(45,1100)) +
  #   stat_function(fun=function(x) {exp(coefS[1])*x**(coefS[2])}, aes(colour="Silverman"), xlim=c(45,1100)) +
  #   stat_function(fun=function(x) {exp(coefC[1])*x**(coefC[2])}, aes(colour="CV"), xlim=c(45,1100))
  # dev.off()
  # pdf(file="RMISE-vs-cases-log-log.pdf")
  # ggplot(df) +
  #   geom_point(aes(x=Case, y=`Relative MISE`, colour=Bandwidth, shape=Bandwidth), size=3) +
  #   stat_function(fun=function(x) {exp(coefO[1])*x**(coefO[2])}, aes(colour="Oracle"), xlim=c(45,1100)) +
  #   stat_function(fun=function(x) {exp(coefS[1])*x**(coefS[2])}, aes(colour="Silverman"), xlim=c(45,1100)) +
  #   stat_function(fun=function(x) {exp(coefC[1])*x**(coefC[2])}, aes(colour="CV"), xlim=c(45,1100)) +
  #   coord_trans(x='log10', y='log10') +
  #   annotation_logticks(scaled=FALSE)
  # dev.off()
}

df <- data.frame(decay, alphaO, alphaS, alphaC)

df.latex <- latex(df,
                  title="rmise_convergence_table",
                  where="htbp",
                  label="tab:results:rmise_convergence_by_cases_and_spread",
                  rowname=NULL,
                  booktabs=TRUE,
                  cdec=c(1,rep(3, 3)),
                  caption.loc="bottom",
                  caption="RMISE convergence rates by case for different bandwidth selectors and different spreads when the population of 10,000 is uniformly distributed.",
                  caption.lot="RMISE Convergence rate by case for different spreads")

