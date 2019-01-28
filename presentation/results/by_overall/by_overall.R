library(ggplot2)

setwd("/Users/harold/Dropbox/MA_Knowledge_and_Info/Thesis/thesis/results/by_overall")

Dirs <- list.files("..", pattern="^[up][^l].*", full.names=TRUE)
Files <- sapply(Dirs, function(dir) paste(dir, '/output/mean_table.tex', sep=''))

N <- length(Files)

Errs <- character()

matO <- NULL
matS <- NULL
matC <- NULL

PopPeaks <- c()
RiskPeaks <- c()

for (i in 1:N) {
  File <- Files[i]
  UnifPeak <- substring(File,4,4)
  PopPeak <- (UnifPeak=='p')
  PopPeaks <- c(PopPeaks, PopPeak)
  UnifPeakR <- substring(strsplit(File, "_")[[1]][3],1,1)
  RiskPeak <- (UnifPeakR!='u')
  RiskPeaks <- c(RiskPeaks, RiskPeak)
  print(paste("File=", File, " UnifPeak=", UnifPeak, " PopPeak=", PopPeak, " UnifPeakR=", UnifPeakR, " RiskPeak=", RiskPeak, sep=""))
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
dfO$Bandwidth <- "Oracle"
dfO$PopPeak <- PopPeaks
dfO$RiskPeak <- RiskPeaks

dfS <- as.data.frame(matS)
dfS$Bandwidth <- "Silverman"
dfS$PopPeak <- PopPeaks
dfS$RiskPeak <- RiskPeaks

dfC <- as.data.frame(matC)
dfC$Bandwidth <- "CV"
dfC$PopPeak <- PopPeaks
dfC$RiskPeak <- RiskPeaks

df <- rbind(dfO, dfS, dfC)
df$Bandwidth <- factor(df$Bandwidth, levels=c("Oracle", "CV", "Silverman"))

dfS.diff <- dfS[,1:3] - dfO[,1:3]
dfS.diff$Bandwidth <- "Silverman"
dfS.diff$PopPeak <- PopPeaks
dfS.diff$RiskPeak <- RiskPeaks

dfC.diff <- dfC[,1:3] - dfO[,1:3]
dfC.diff$Bandwidth <- "CV"
dfC.diff$PopPeak <- PopPeaks
dfC.diff$RiskPeak <- RiskPeaks

df.diff <- rbind(dfC.diff, dfS.diff)
df.diff$Bandwidth <- as.factor(df.diff$Bandwidth)

# NMISE
pdf("normalized-mise-boxplot.pdf")
boxplot(df[df$PopPeak==FALSE,]$`Normalized MISE` ~ df[df$PopPeak==FALSE,]$Bandwidth)
dev.off()
pdf("normalized-mise-peakpop-boxplot.pdf")
boxplot(df[df$PopPeak==TRUE,]$`Normalized MISE` ~ df[df$PopPeak==TRUE,]$Bandwidth)
dev.off()

# NMIAE
pdf("normalized-miae-boxplot.pdf")
boxplot(df[df$PopPeak==FALSE,]$`Normalized MIAE` ~ df[df$PopPeak==FALSE,]$Bandwidth)
dev.off()
pdf("normalized-miae-peakpop-boxplot.pdf")
boxplot(df[df$PopPeak==TRUE,]$`Normalized MIAE` ~ df[df$PopPeak==TRUE,]$Bandwidth)
dev.off()

# NSUP
pdf("normalized-sup-error-boxplot.pdf")
boxplot(df[df$PopPeak==FALSE,]$`Normalized Sup error` ~ df[df$PopPeak==FALSE,]$Bandwidth)
dev.off()
pdf("normalized-sup-error-peakpop-boxplot.pdf")
boxplot(df[df$PopPeak==TRUE,]$`Normalized Sup error` ~ df[df$PopPeak==TRUE,]$Bandwidth)
dev.off()

# NMISE difference
pdf("normalized-mise-diff-boxplot.pdf")
boxplot(df.diff[df.diff$PopPeak==FALSE,]$`Normalized MISE` ~ df.diff[df.diff$PopPeak==FALSE,]$Bandwidth)
dev.off()
pdf("normalized-mise-diff-peakpop-boxplot.pdf")
boxplot(df.diff[df.diff$PopPeak==TRUE,]$`Normalized MISE` ~ df.diff[df.diff$PopPeak==TRUE,]$Bandwidth)
dev.off()

# Relative peak bias
pdf("relative-peak-bias-boxplot.pdf")
boxplot(df[df$PopPeak==FALSE&df$RiskPeak==TRUE,]$`Relative Peak bias` ~ df[df$PopPeak==FALSE&df$RiskPeak==TRUE,]$Bandwidth)
dev.off()
pdf("relative-peak-bias-peakpop-boxplot.pdf")
boxplot(df[df$PopPeak==TRUE&df$RiskPeak==TRUE,]$`Relative Peak bias` ~ df[df$PopPeak==TRUE&df$RiskPeak==TRUE,]$Bandwidth)
dev.off()

# Relative centroid bias
pdf("relative-centroid-bias-boxplot.pdf")
boxplot(df[df$PopPeak==FALSE&df$RiskPeak==TRUE,]$`Relative Centroid bias` ~ df[df$PopPeak==FALSE&df$RiskPeak==TRUE,]$Bandwidth)
dev.off()
pdf("relative-centroid-bias-peakpop-boxplot.pdf")
boxplot(df[df$PopPeak==TRUE&df$RiskPeak==TRUE,]$`Relative Centroid bias` ~ df[df$PopPeak==TRUE&df$RiskPeak==TRUE,]$Bandwidth)
dev.off()

# Relative peak drift
pdf("relative-peak-drift-boxplot.pdf")
boxplot(df[df$PopPeak==FALSE&df$RiskPeak==TRUE,]$`Relative Peak drift` ~ df[df$PopPeak==FALSE&df$RiskPeak==TRUE,]$Bandwidth)
dev.off()
pdf("relative-peak-drift-peakpop-boxplot.pdf")
boxplot(df[df$PopPeak==TRUE&df$RiskPeak==TRUE,]$`Relative Peak drift` ~ df[df$PopPeak==TRUE&df$RiskPeak==TRUE,]$Bandwidth)
dev.off()

# Relative centroid drift
pdf("relative-centroid-drift-boxplot.pdf")
boxplot(df[df$PopPeak==FALSE&df$RiskPeak==TRUE,]$`Relative Centroid drift` ~ df[df$PopPeak==FALSE&df$RiskPeak==TRUE,]$Bandwidth)
dev.off()
pdf("relative-centroid-drift-peakpop-boxplot.pdf")
boxplot(df[df$PopPeak==TRUE&df$RiskPeak==TRUE,]$`Relative Centroid drift` ~ df[df$PopPeak==TRUE&df$RiskPeak==TRUE,]$Bandwidth)
dev.off()
