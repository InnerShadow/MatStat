
library(moments)

Norm30 <- rnorm(30, 230, 27)
Norm70 <- rnorm(70, 230, 27)
Norm170 <- rnorm(170, 230, 27)
Norm300 <- rnorm(300, 230, 27)
Norm1000 <- rnorm(1000, 230, 27)

Exp30 <- rexp(30, 0.0048)
Exp70 <- rexp(70, 0.0048)
Exp170 <- rexp(170, 0.0048)
Exp300 <- rexp(300, 0.0048)
Exp1000 <- rexp(1000, 0.0048)

write.table(Norm30, "Norm30.txt")
write.table(Norm70, "Norm70.txt")
write.table(Norm170, "Norm170.txt")
write.table(Norm300, "Norm300.txt")
write.table(Norm1000, "Norm1000.txt")

write.table(Exp30, "Exp30.txt")
write.table(Exp70, "Exp70.txt")
write.table(Exp170, "Exp170.txt")
write.table(Exp300, "Exp300.txt")
write.table(Exp1000, "Exp1000.txt")

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

Stat <- function(vec, name){
  Mean <- mean(vec)
  Mediane <- median(vec)
  Mode <- getmode(vec)
  Dispersion <- var(vec)
  StandartDeviation <- sd(vec)
  Asymmetries <- skewness(vec)
  Excess <- kurtosis(vec)
  
  path <- paste0(name, "Statistics.txt")
  Statistics <- file(path)
  writeLines(c("Mean", Mean, "Mediane", Mediane, "Mode", Mode,
               "Dispersion", Dispersion, "StandartDeviation", StandartDeviation,
               "Asymmetries", Asymmetries, "Excess", Excess - 3), Statistics)
  close(Statistics)
}

Stat(Norm30, "N30")
Stat(Norm70, "N70")
Stat(Norm170, "N170")
Stat(Norm300, "N300")
Stat(Norm1000, "N1000")

Stat(Exp30, "Exp30")
Stat(Exp70, "Exp70")
Stat(Exp170, "Exp170")
Stat(Exp300, "Exp300")
Stat(Exp1000, "Exp1000")

SaveHistogram <- function(vec, name){
  path <- paste0("Hist", name, ".png")
  png(file = path)
  hist(vec, main = paste("Hist of", name))
  dev.off()
}

SaveHistogram(Norm300, "N300")
SaveHistogram(Norm1000, "N1000")

SaveHistogram(Exp300, "Exp300")
SaveHistogram(Exp1000, "Exp1000")

CountConfidenceIntervals <- function(vec, name){
  Mean <- mean(vec)
  SD <- sd(vec)
  n <- length(vec)
  margin1 <- qt((1 - 0.01 / 2), df = n - 1) * SD / sqrt(n)
  margin5 <- qt((1 - 0.05 / 2), df = n - 1) * SD / sqrt(n)
  margin10 <- qt((1 - 0.10 / 2), df = n - 1) * SD / sqrt(n)
  path <- paste0("ConfidenceIntervals", name, ".txt")
  CI <- file(path)
  writeLines(c("left1%", Mean - margin1, "right1%", Mean + margin1,
               "left5%", Mean - margin5, "right5%", Mean + margin5,
               "left10%", Mean - margin10, "right10%", Mean + margin10), CI)
  close(CI)
}

CountConfidenceIntervals(Norm30, "N30")
CountConfidenceIntervals(Norm70, "N70")
CountConfidenceIntervals(Norm170, "N170")

CountExpConfidenceIntervals <- function(vec, name){
  Mean <- mean(vec)
  n <- length(vec)
  path <- paste0("ConfidenceIntervals", name, ".txt")
  CI <- file(path)
  writeLines(c("left1%", (qchisq(0.01 / 2, 2 * n)) / (2 * n * Mean), 
             "right1%", (qchisq((1 - 0.01) / 2, 2 * n)) / (2 * n * Mean), 
             "left5%", (qchisq(0.05 / 2, 2 * n)) / (2 * n * Mean), 
             "right5%", (qchisq((1 - 0.05) / 2, 2 * n)) / (2 * n * Mean), 
             "left10%", (qchisq(0.1 / 2, 2 * n)) / (2 * n * Mean), 
             "right10%", (qchisq((1 - 0.1) / 2, 2 * n)) / (2 * n * Mean)), CI)
  close(CI)
}

CountExpConfidenceIntervals(Exp30, "Exp30")
CountExpConfidenceIntervals(Exp70, "Exp70")
CountExpConfidenceIntervals(Exp170, "Exp170")



















