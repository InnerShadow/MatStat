
library(moments)
library(BSDA)
library(ggplot2)

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

IsNormalDuringSkewnessAndKurtosis <- function(vec, name){
  n <- length(vec)
  Ds <- skewness(vec)
  De <- kurtosis(vec) - 3
  DS <- sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))
  DE <- sqrt((24 * n * (n - 1) ^ 2) / ((n - 3) * (n - 2) * (n + 3) * (n + 5)))
  path <- paste0("IsNormalDuringSkewnessAndKurtosis", name, ".txt")
  IsNormal <- file(path)
  writeLines(c("Is Normal During Skewness, ", paste("Ds =", Ds, "DS =", DS), (abs(Ds)) <= (3 * DS), 
               "Is Normal During Kurtosis", paste("De =", De, "DE =", DE), (abs(De)) <= (5 * DE)), IsNormal)
  close(IsNormal)
}

IsNormalDuringSkewnessAndKurtosis(Norm30, "N30")
IsNormalDuringSkewnessAndKurtosis(Norm70, "N70")

FTest <- function(vec1, vec2){
  path <- c("Ftest.txt")
  ftest <- file(path)
  resualt1 <- var.test(vec1, vec2, conf.level = 0.99)
  resualt5 <- var.test(vec1, vec2, conf.level = 0.95)
  resualt10 <- var.test(vec1, vec2, conf.level = 0.90)
  writeLines(c("1%", names(resualt1[[1]]) ,resualt1[[1]], names(resualt1[[2]]),
               resualt1[[2]], names(resualt1[[3]]), resualt1[[3]], names(resualt1[[4]]), resualt1[[4]], 
               names(resualt1[[5]]), resualt1[[5]], names(resualt1[[6]]), resualt1[[6]],
               names(resualt1[[7]]), resualt1[[7]], names(resualt1[[8]]), resualt1[[8]], 
               names(resualt1[[9]]), resualt1[[9]], "",
               "5%", names(resualt5[[1]]) ,resualt5[[1]], names(resualt5[[2]]),
               resualt5[[2]], names(resualt5[[3]]), resualt5[[3]], names(resualt5[[4]]), resualt5[[4]], 
               names(resualt5[[5]]), resualt5[[5]], names(resualt5[[6]]), resualt5[[6]],
               names(resualt5[[7]]), resualt5[[7]], names(resualt5[[8]]), resualt5[[8]], 
               names(resualt5[[9]]), resualt5[[9]], "",
               "10%", names(resualt10[[1]]) ,resualt10[[1]], names(resualt10[[2]]),
               resualt10[[2]], names(resualt10[[3]]), resualt10[[3]], names(resualt10[[4]]), resualt10[[4]], 
               names(resualt10[[5]]), resualt10[[5]], names(resualt10[[6]]), resualt10[[6]],
               names(resualt10[[7]]), resualt10[[7]], names(resualt10[[8]]), resualt10[[8]], 
               names(resualt10[[9]]), resualt10[[9]]
               ), ftest)
  close(ftest)
}

FTest(Norm30, Norm70)

TTest <- function(vec1, vec2){
  path <- c("Ttest.txt")
  ttest <- file(path)
  resualt1 <- t.test(vec1, vec2, conf.level = 0.99)
  resualt5 <- t.test(vec1, vec2, conf.level = 0.95)
  resualt10 <- t.test(vec1, vec2, conf.level = 0.90)
  writeLines(c("1%", names(resualt1[[1]]) ,resualt1[[1]], names(resualt1[[2]]),
               resualt1[[2]], names(resualt1[[3]]), resualt1[[3]], names(resualt1[[4]]), resualt1[[4]], 
               names(resualt1[[5]]), resualt1[[5]], names(resualt1[[6]]), resualt1[[6]],
               names(resualt1[[7]]), resualt1[[7]], names(resualt1[[8]]), resualt1[[8]], 
               names(resualt1[[9]]), resualt1[[9]], "",
               "5%", names(resualt5[[1]]) ,resualt5[[1]], names(resualt5[[2]]),
               resualt5[[2]], names(resualt5[[3]]), resualt5[[3]], names(resualt5[[4]]), resualt5[[4]], 
               names(resualt5[[5]]), resualt5[[5]], names(resualt5[[6]]), resualt5[[6]],
               names(resualt5[[7]]), resualt5[[7]], names(resualt5[[8]]), resualt5[[8]], 
               names(resualt5[[9]]), resualt5[[9]], "",
               "10%", names(resualt10[[1]]) ,resualt10[[1]], names(resualt10[[2]]),
               resualt10[[2]], names(resualt10[[3]]), resualt10[[3]], names(resualt10[[4]]), resualt10[[4]], 
               names(resualt10[[5]]), resualt10[[5]], names(resualt10[[6]]), resualt10[[6]],
               names(resualt10[[7]]), resualt10[[7]], names(resualt10[[8]]), resualt10[[8]], 
               names(resualt10[[9]]), resualt10[[9]]
  ), ttest)
  close(ttest)
}

TTest(Norm30, Norm70)

BartlettTest <- function(vec, name){
  n <- length(vec)
  tn <- 0
  for(i in 1 : n){
    tn <- tn + vec[i]
  }
  
  logSum <- 0
  for(i in 1 : n){
    logSum <- logSum + log(vec[i])
  }
  
  Br <- ((2 * n * (log(tn / n)) - (1 / n) * (logSum)) / (1 + ((n + 1) / 6)))
  BrLeft <- qchisq(0.025, n - 1)
  BrRight <- qchisq(0.975, n - 1)
  path <- paste0("BartlettTest", name, ".txt")
  Bart <- file(path)
  writeLines(c(paste("Br =", Br), paste("BrLeft =", BrLeft, "BrRight =", BrRight)), Bart)
  close(Bart)
}

BartlettTest(Exp70, "Exp70")
BartlettTest(Exp170, "Exp170")

z.test(Exp70, Exp170, sigma.x = sd(Exp70), sigma.y = sd(Exp170))

Kolmogorov_Smirnow <- function(vec, name){
  sorted <- sort(vec)
  Mean <- mean(sorted)
  SD <- sd(sorted)
  n <- length(vec)
  EmpricialDistribution <- rep(0, n)
  for(i in 1 : n){
    EmpricialDistribution[i] <- i / n
  }
  
  TeareticalDistribution <- rep(0, n)
  for(i in 1 : n){
    TeareticalDistribution[i] <- pnorm(sorted[i], Mean, SD)
  }
  
  Diffrence <- rep(0, n)
  for(i in 1 : n){
    Diffrence[i] <- abs(TeareticalDistribution[i] - EmpricialDistribution[i])
  }
  
  maxDiffrence <- max(Diffrence)
  Kolmogorov_SmirnovStatistics <- sqrt(n) * maxDiffrence
  C <- 1.358
  ChanceToAccept <- exp(-1 * maxDiffrence ^ 2 * n)
  
  path <- paste0("Kolmogorov-Smirnow", name, ".png")
  png(file = path)
  plot(EmpricialDistribution, col = "blue")
  lines(EmpricialDistribution, col = "blue")
  points(TeareticalDistribution, col = "red")
  lines(TeareticalDistribution, col = "red")
  title("red - Tearetical, blue - Empricial")
  dev.off()
  
  write.table(EmpricialDistribution, "EmpricialDistribution.txt")
  write.table(TeareticalDistribution, "TeareticalDistribution.txt")
  write.table(Diffrence, "Diffrence.txt")
  
  path <- paste0("Kolmogorov-Smirnow", name, ".txt")
  Kolmogorov_SmirnovInfo <- file(path)
  writeLines(c("Max diffrence:", maxDiffrence, "Kolmogorov_SmirnovStatistics", 
               Kolmogorov_SmirnovStatistics, "C: ", C, "ChanceToAccept", ChanceToAccept), 
             Kolmogorov_SmirnovInfo)
  close(Kolmogorov_SmirnovInfo)
}

Kolmogorov_Smirnow(Norm170, "N170")
Kolmogorov_Smirnow(Exp170, "Exp170")


Chi_square_graph <- function(vec, name){
  
  Min <- min(vec)
  Max <- max(vec)
  
  n <- 12
  b <- T
  while(b){
    b <- F
    tmp <- rep(0, n)
    xVec <- rep(0, n + 1)
    Step <- (Max - Min) / n
    for(i in 1 : n + 1){
      xVec[i] <- Min + (i - 1) * (Step)
    }
    for(i in 1 : length(vec)){
      for(j in 1 : n){
        if(vec[i] >= xVec[j] && vec[i] <= xVec[j + 1]){
          tmp[j] <- tmp[j] + 1
        }
      }
    }
    
    for(i in 1 : n){
      if(tmp[i] < 3){
        b = T
        n <- n - 1
        break
      }
    }
    
  }
  
  Mean <- mean(vec)
  SD <- sd(vec)
  teoretic <- rep(0, length(tmp))
  for(i in 1 : length(tmp)){
    teoretic[i] <- pnorm((xVec[i + 1] - Mean) / SD) - pnorm((xVec[i] - Mean) / SD)
  }
  
  practical <- rep(0, length(tmp))
  for(i in 1 : length(tmp)){
    practical[i] <- tmp[i] / length(vec)
  }
  
  
  path <- paste0("Chi-squer", name, ".png")
  png(file = path)
  plot(practical, col = "red")
  lines(practical, col = "red")
  points(teoretic, col = "blue")
  lines(teoretic, col = "blue")
  title("red - practical, blue - teoretical")
  dev.off()
  
  write.table(teoretic, paste0("TeoretacialChi-squear", name, ".txt"))
  write.table(practical, paste0("PracticalChi-squear", name, ".txt"))
  write.table(xVec, paste0("XIntervalsChi-squear", name, ".txt"))
}

Chi_square_graph(Norm300, "N300")
Chi_square_graph(Exp300, "Exp300")

