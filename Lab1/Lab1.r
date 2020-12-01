# Task 1
num <- seq(1, 15)
a_hard <-
  c(1.13,
    1.17,
    1.20,
    1.17,
    1.18,
    1.23,
    1.21,
    1.22,
    1.14,
    1.14,
    1.14,
    1.18,
    1.13,
    1.06,
    1.26)
a_neutral <-
  c(1.48,
    1.54,
    1.47,
    1.45,
    1.46,
    1.55,
    1.45,
    1.44,
    1.53,
    1.46,
    1.53,
    1.53,
    1.53,
    1.51)
a_soft <-
  c(1.49,
    1.50,
    1.53,
    1.54,
    1.52,
    1.49,
    1.49,
    1.51,
    1.54,
    1.60,
    1.50,
    1.53,
    1.53)

a <- do.call(pmax, (lapply(list(a_soft, a_neutral, a_hard), FUN = function(x) c(x, rep(0, 15 - length(x))))))

b <-
  sample(c("Samsung", "LG", "Ariston"),
         size = 15,
         replace = TRUE)

b_factor <- factor(b)

num
a
b
b_factor



# Task 2
my_frame <- data.frame(row.names = num,
                       Lifetime = a,
                       Manufacturer = b_factor)

my_frame
nrow(my_frame)
ncol(my_frame)
str(my_frame)
colnames(my_frame)
head(my_frame, 4)
my_frame[my_frame$Lifetime >= 1.54, ]
my_frame[my_frame$Manufacturer == "LG", ]
my_frame[c(1, 3, 6, 9, 10), ]
min(my_frame$Lifetime)



# Task 3
my_frame <- rbind(my_frame,
                  data.frame(
                    Lifetime  = c(1.24, 1.55, 1.46),
                    Manufacturer  = c("Samsung", "LG", "LG")
                  ))

my_mean <- mean(my_frame$Lifetime)
my_sd <- sd(my_frame$Lifetime)
New <- rnorm(nrow(my_frame), mean = my_mean, sd = my_sd)
my_frame <- cbind(my_frame, data.frame(Distribution = New))
my_frame


# Task 4
setwd("Lab1Resources")
library(xlsx)
flat <- read.xlsx("Flat98.xlsx", sheetIndex = 1)
write.table(head(flat, 5), "output.txt")

my_function <- function(x.vector){
  sort(x.vector)[1:5]
}

write.table(flat[flat$dist %in% my_function(flat$dist),], "closest.txt")
write.table(head(flat[ flat$price %in% my_function(flat[!is.na(flat$tel),][flat$tel,]$price) & flat$tel,], 5), "cheapest.txt")
