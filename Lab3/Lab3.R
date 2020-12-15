setwd("Resources")


# Часть 1

# Импорт данных
origin <- read.csv("bmw.csv", header = TRUE)

# Выбор автомобилей нужной марки
origin.filtred <- origin[origin$model == " X3",]
subject.table <- table(origin.filtred$year, origin.filtred$engineSize)
chisq.test(subject.table)

# Исключение нулевого столбца, объединение строк для исключение частот <5
origin.filtred$year_fixed = origin.filtred$year
origin.filtred$year_fixed[origin.filtred$year %in% c(2004, 2006, 2007, 2009, 2011, 2012, 2013, 2014)] <- "2004-2014"
subject.table <- table(origin.filtred$year_fixed, origin.filtred$engineSize)
subject.table <- subject.table[,c("2","3")]

# Вывод данных выборки и проверка гипотезы назвисимости с помощью критерия chi^2
print(subject.table)
chisq.result <- chisq.test(subject.table)
qchisq.result <- qchisq(p = 0.999, df = 6)
print(chisq.result)
print(qchisq.result)
# Так как значение выборочного chi^2 меньше критического, гипотеза независимости не отклоняется


# Часть 2

# Выбор автомобилей нужной марки и исключение нечисловых столбцов
origin.filtred <- origin[origin$model == " X3",]
origin.filtred$model <- NULL
origin.filtred$transmission <- NULL
origin.filtred$fuelType <- NULL

# Создание коорреляционной модели таблицы
library(corrplot)
cor.table = cor(origin.filtred)
corrplot(cor.table, method = "color", addCoef.col = "black", number.digits = 1, number.cex = 1.3)
print(cor.table)
# Создание моделей линейной регрессии
model_1 = lm(price ~ year + mileage + tax + mpg + engineSize, origin.filtred)
summary(model_1)

# Формула регрессии 1 модели price = (2.139e+03 * year) + 
#                                    (-1.402e-01 * mileage) + 
#                                    (-1.769e+01 * tax) + 
#                                    (-2.835e+02 * mpg) + 
#                                    (4.546e+03 * engineSize -4.277e+06


# Создание и вывод 2 модели линейной регрессии
model_2 = lm(price ~ year + tax + mpg + engineSize, origin.filtred)
summary(model_2)

# Формула регрессии 2 модели price = (3.511e+03 * year) + 
#                                    (-3.493e+00 * tax) + 
#                                    (-3.008e+02 * mpg) + 
#                                    (4.187e+03 * engineSize) -7.049e+06


# Создание и вывод Q-Q графика 1 модели
e = residuals(model_1)
qqnorm(e, pch = 1)
qqline(e, lty = 2)

# Создание и вывод Q-Q графика 2 модели
r = residuals(model_2)
qqnorm(r, pch = 1)
qqline(r, lty = 2)


# Создание данных для предсказания
predict_data = data.frame(year = 2025, mileage = 10000, tax = 125, mpg = 50, engineSize = 3)

# Предсказание по 1 модели
price_1 = predict(model_1, predict_data)
price_1

# Предсказание по 2 модели
price_2 = predict(model_2, predict_data)
price_2





