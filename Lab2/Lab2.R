setwd("Lab2Resources")
library(xlsx)
origin <- read.csv("var19.csv")
origin
print(sub1 <- origin[1:10, ]$x)
print(sub2 <- origin[1:50, ]$x)
print(sub3 <- origin[TRUE]$x)
Analyze <- function(x) {
  # Среднее выборочное
  result.mean <- mean(x)
  # Несмещенная выборочная дисперсия
  result.varS <- var(x)
  # Выборочная дисперсия
  result.varP <- result.varS * length(x - 1) / length(x)
  # Квантили
  result.quantile <- quantile(x)
  # Математическое ожидание
  result.a <- mean(x)
  # Среднеквадратическое отклонение
  result.sd <- sd(x)
  # Функция плотности нормального распределения
  hist(
    x,
    main = "Гистограмма нормального распределения",
    freq = FALSE,
    xlab = "x",
    ylab = "h_i",
    col = "pink"
  )
  curve(
    dnorm(x,
          mean = result.a,
          sd = result.sd),
    col = "red",
    lwd = 2,
    add = TRUE
  )
  
  plot(
    ecdf(x),
    pch = 20,
    main = "Графики функций распределения",
    xlab = "x",
    ylab = "F(x)",
    col = "black"
  )
  curve(
    pnorm(x,
          mean = result.a,
          sd = result.sd),
    pch = 20,
    lwd = 1,
    col = "red",
    add = TRUE
  )
  
  analyze.Coinf <- function(q) {
    # Уровень значимости
    epsilon <- 1 - q
    
    n <- length(x)
    # Квантиль распределения Стьюдента
    t <- qt(1 - epsilon / 2, n - 1)
    # Доверительный интервал для параметра a
    Pa <-
      c(result.a - t * sqrt(result.varS) / sqrt(n),
        result.a + t * sqrt(result.varS) / sqrt(n))
    # Квантили распределения ksi
    ksi1 <- qchisq(epsilon / 2, n - 1)
    ksi2 <- qchisq(1 - epsilon / 2, n - 1)
    # Доверительный интервал для параментра sigma^2
    Psigma <-
      c((n - 1) * result.varS / ksi2, (n - 1) * result.varS / ksi1)
    
    return(list(Pa = Pa, Psigma = Psigma))
  }
  q <- 0.95
  result.coinf <- analyze.Coinf(q)
  print(result.coinf$Pa)
  print(result.coinf$Psigma)
  
  q <- seq(0.9, 0.99999, length.out = 100)
  result.coinfArray <- (lapply(q, analyze.Coinf))
  print(result.coinfArray.Psigma <- t(sapply(result.coinfArray, FUN = function(x) x$Psigma )))
  print(result.coinfArray.Pa <- t(sapply(result.coinfArray, FUN = function(x) x$Pa )))
  
  plot(
    y = abs(result.coinfArray.Pa[, 2] - result.coinfArray.Pa[, 1]),
    x = q,
    type = "o",
    pch = 20,
    main = "График изм. длины доверительного интервала",
    xlab = "q",
    ylab = "Pa",
    col = "black"
  )
  plot(
    y = abs(result.coinfArray.Psigma[, 2] - result.coinfArray.Psigma[, 1]),
    x = q,
    type = "o",
    pch = 20,
    main = "График изм. длины доверительного интервала",
    xlab = "q",
    ylab = "Psigma",
    col = "black"
  )
}

f <- Analyze(sub1)
f <- Analyze(sub2)
f <- Analyze(sub3)