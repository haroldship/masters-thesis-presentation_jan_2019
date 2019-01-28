setwd("/Users/harold/Dropbox/MA_Knowledge_and_Info/Thesis/thesis/results/by_two_peaks")

source("../plots.R")

Gaps <- c(1, 2, 3, 4)
GapStrs <- c("2h_1", "2h_2", "2h_3", "2h_4")
Dirs <- sapply(GapStrs, function(gap) paste('../unif_100_1_', gap, '/output/', sep=''))
Files <- sapply(Dirs, function(dir) paste(dir, 'mean_table.tex', sep=''))

N <- length(Gaps)
Errs <- character()

matO <- NULL
matS <- NULL
matC <- NULL


for (i in 1:N) {
  Gap <- Gaps[i]
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
dfO$Gap <- Gaps
dfO$Bandwidth <- "Oracle"

dfS <- as.data.frame(matS)
dfS$Gap <- Gaps
dfS$Bandwidth <- "Silverman"

dfC <- as.data.frame(matC)
dfC$Gap <- Gaps
dfC$Bandwidth <- "CV"

df <- rbind(dfO, dfS, dfC)
df$Bandwidth <- as.factor(df$Bandwidth)

coefO <- coef(lm(log(dfO$`Relative MISE`) ~ dfO$Gap))
coefS <- coef(lm(log(dfS$`Relative MISE`) ~ dfS$Gap))
coefC <- coef(lm(log(dfC$`Relative MISE`) ~ dfC$Gap))


pdf(file="MISE-vs-risk-peak-gap.pdf")
make_accuracy_plot(df, "Distance between peaks") +
  geom_point(aes(x=Gap, y=MISE, shape=Bandwidth), size=3)
dev.off()
pdf(file="RMISE-vs-risk-peak-gap.pdf")
make_accuracy_plot(df, "Distance between peaks") +
  geom_point(aes(x=Gap, y=`Relative MISE`, shape=Bandwidth), size=3)
dev.off()

coefO <- coef(lm(log(dfO$`Normalized MISE`) ~ dfO$Gap))
coefS <- coef(lm(log(dfS$`Normalized MISE`) ~ dfS$Gap))
coefC <- coef(lm(log(dfC$`Normalized MISE`) ~ dfC$Gap))

pdf(file="NMISE-vs-risk-peak-gap.pdf")
make_accuracy_plot(df, "Distance between peaks") +
  geom_point(aes(x=Gap, y=`Normalized MISE`, shape=Bandwidth), size=3)
dev.off()

pdf(file="peak-bias-vs-risk-peak-gap.pdf")
make_accuracy_plot(df, "Distance between peaks") +
  geom_point(aes(x=Gap, y=`Peak bias`, shape=Bandwidth), size=3)
dev.off()
pdf(file="peak-drift-vs-risk-peak-gap.pdf")
make_accuracy_plot(df, "Distance between peaks") +
  geom_point(aes(x=Gap, y=`Peak drift`, shape=Bandwidth), size=3)
dev.off()
pdf(file="centroid-bias-vs-risk-peak-gap.pdf")
make_accuracy_plot(df, "Distance between peaks") +
  geom_point(aes(x=Gap, y=`Centroid bias`, shape=Bandwidth), size=3)
dev.off()
pdf(file="centroid-drift-vs-risk-peak-gap.pdf")
make_accuracy_plot(df, "Distance between peaks") +
  geom_point(aes(x=Gap, y=`Centroid drift`, shape=Bandwidth), size=3)
dev.off()

