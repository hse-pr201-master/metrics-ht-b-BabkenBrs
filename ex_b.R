library(tidyverse)
library(rio)
library(skimr)
library(tidyr)
library(readxl)
library(knitr)
library(ggplot2)
library(glmnet)
library(MASS) 
library(quantreg)
library(car)
library(lmtest)

library(lubridate) # работа с датами
library(zoo) # временные ряды
library(xts)
library(forecast)
library(dplyr)
theme_set(theme_bw())
opts_chunk$set(fig.align = 'center')

getwd()
setwd('C:/Coursera_R/b')
df <- read.csv('datasets.csv')
view(df)
# 1 задание ------------------------------------ 
# Я хочу изучить, как связаны ВВП на душу населения и показатели, которые будут указаны в задании 2. Для этого в качестве зависимой переменной я беру ВВП на душу населения»
# ----------------------------------------------


# 2 задание ------------------------------------ 
# (a) Плотность населения на km2
# Данный фактор интересен тем, что имеет двоякий эффект. С одной стороны, кажется, что чем больше плотность, тем больше людей включены в экономику и тем выше должен быть ВВП на душу населения, но с другой стороны, чем больше плотность населения, тем и больше ожидаемое кол-во населения, которое учитывается в ВВП на душу населения, что влияет на данный показатель отрицательно.
# (b) Соотношение мужчин и женщин
# Данный фактор мы сделаем бинарным, если мужчин больше, чем девушек, то 1, а если меньше, то 0. Этот показатель, мне кажется важным, потому что моя гипотеза заключается в том, что кол-во стран в которых мужчин больше, будут иметь больший ВВП на душу населения в связи с тем, что не все женщины - активны.
Man = as.numeric(df$Sex.ratio..m.per.100.f..2017. > 100)
df$Man = Man
colnames(df)
# Теперь в наших данных есть колонка Man, которая отвечает за то, больше ли в стране мужчин или нет.
# (c) Квадрат расходов на образование
df$Education..Government.expenditure....of.GDP. <- as.numeric(df$Education..Government.expenditure....of.GDP.)
Educ2 <- df$Education..Government.expenditure....of.GDP. ^ 2
df$Educ2 = Educ2
# ----------------------------------------------


# 3 задание ------------------------------------ 
ggplot(data = df, aes(x = df$Unemployment....of.labour.force., y = df$GDP.per.capita..current.US..)) + geom_point() +
  labs(x = "Безработиа",
       y = "ВВП на душу населения",
       title = "ВВП на душу населения в долларах США")

# По данному график несложно заметить, что многие страны имеют ВВП на душу населения ниже 0 (выбросы), то есть отрицательные, что, конечно, не укладывается в экономическую интерпретацию. Поэтому стоило бы в дальнейшем либо избавиться от данных наблюдений, либо взять усредненные.
is.numeric(df$Employment..Agriculture....of.employed.)
#Данный тест по ряду столбцов выведет False по той причине, что в столбцах находятся неизвестные значения "...", выраженные троеточием. Конечно, от них нам тоже следует избавиться, но давайте проведем те же тесты со столбцами, которые нам пригодятся в анализе.
ggplot(data = df, aes(x = df$GDP.per.capita..current.US.., y = df$Education..Government.expenditure....of.GDP.)) + geom_point() +
  labs(x = "ВВП на душу населения",
       y = "Расходы государства",
       title = "Расходы государства в проценте от ВВП")
hist(df$Education..Government.expenditure....of.GDP., xlab = "Расходы", ylab = 'Частота', main = 'Расходы государства. От ВВП ')
df$Unemployment....of.labour.force. = as.numeric(df$Unemployment....of.labour.force.)
hist(df$Unemployment....of.labour.force., xlab = "Расходы", ylab = 'Частота', main = 'Расходы государства. От ВВП ')
# В государственных расходах также видно, что существуют отрицательные показатели.
# Что делать с выбросами? Изначально мне казалось, что логично их всех просто удалить из выборки, но потом, когда я воплотил это, я понял, что это неправильно по той причине, что остается очень мало наблюдений, по которым трудно что-либо говорить. Поэтому я решил в отсутствующих частях выборки ставить средние показатели по выборке. Конечно, это искажает ситуацию, но хотя бы дает какую-то оценку.
#Я буду все это делать по след. алгоритму: сначала я считаю кол-во выбросов и пропущенных значений, потом заменяю их на 0, затем нахожу среднее арифметическое и приравниваю вместо 0. Так я буду работать со всем данными.
#
#ВВП на душу населения
gdp_nan = sum(df$GDP.per.capita..current.US.. < 0)

df$GDP.per.capita..current.US..[df$GDP.per.capita..current.US.. < 0] = 0

gdp_sum = sum(df$GDP.per.capita..current.US..)

df$GDP.per.capita..current.US..[df$GDP.per.capita..current.US.. == 0] = gdp_sum / (length(df$GDP.per.capita..current.US..) - gdp_nan)
#Безработица

unem_nan = sum(df$Unemployment....of.labour.force. < 0 | df$Unemployment....of.labour.force. == 2)

df$Unemployment....of.labour.force.[df$Unemployment....of.labour.force. < 0 | df$Unemployment....of.labour.force. == 2] = 0

unem_sum = sum(df$Unemployment....of.labour.force.)

df$Unemployment....of.labour.force.[df$Unemployment....of.labour.force. == 0] = unem_sum / (length(df$Unemployment....of.labour.force.) - unem_nan)

#Расходы на образование государства
df$Education..Government.expenditure....of.GDP. = as.numeric(df$Education..Government.expenditure....of.GDP.)

expen_nan = sum(df$Education..Government.expenditure....of.GDP. <= 0 | df$Education..Government.expenditure....of.GDP. == 2)

df$Education..Government.expenditure....of.GDP.[df$Education..Government.expenditure....of.GDP. < 0 | df$Education..Government.expenditure....of.GDP. == 2] = 0

expen_sum = sum(df$Education..Government.expenditure....of.GDP.)

df$Education..Government.expenditure....of.GDP.[df$Education..Government.expenditure....of.GDP. == 0] = expen_sum / (length(df$Education..Government.expenditure....of.GDP.) - expen_nan)
Educ2 <- (df$Education..Government.expenditure....of.GDP.) ^ 2
# ----------------------------------------------
# 4 задание ------------------------------------ 

# (а) Мультиколлинеарность
m.poly <- lm(df$GDP.per.capita..current.US.. ~ I(df$Man) + I(df$Unemployment....of.labour.force.) + I(df$Education..Government.expenditure....of.GDP.) + I(df$Educ2), data = df)
summary(m.poly)
#На первый взгляд признаков мудтиколлинерности не так много. проведем для точного анализ тесты CI и VIF.
#CI
X <- model.matrix( ~ I(df$Man) + I(df$Unemployment....of.labour.force.) + I(df$Education..Government.expenditure....of.GDP.) + I(df$Educ2), data = df)
XX <- t(X) %*% X
eigen <- eigen(XX)
eigen$values
CI <- sqrt(max(eigen$values) / min(eigen$values))
CI
# CI = 3960.399 > 30 - явный признак мультиколлинеарности.
# VIF
vif(m.poly)
# Все показатели VIF < 10, что говорит о том, что подозревать мудбтиколлинеарность не стоит

# (b) Гетероскедастичность

# Тест Бройша-Пагана
bptest(m.poly)
# H0 не отвергается в сидлу большого p-value = 0.499, следовательно гетероскедастичность отсутствует в наших данных

# Тест Голфелда-Квандта
gqtest(m.poly, fraction = 0.2)
# Результат аналогичен тесту Бройша-Пагана

# (c) Эндогенность

# ----------------------------------------------
#
#
#
#------------------------------------------------
#
#
# Часть Б
# 1 задание -------------------------------------
# AR(1)
y1 <- arima.sim(n=120, list(ar=0.8))
plot(y1)
# Данный процесс является стационарным, так как все AR(1) процессы с коэффициентом перед лаговой переменной по модулю меньши единицы являются стационарными.

# AR(3)
y2 <- arima.sim(n=120, list(ar = c(0.1, 0.2, 0.3)))
tsdisplay(y2)

# MA(2)
y3 <- arima.sim(n = 120, list(ma = c(1.2, 2)))
tsdisplay(y3)
# MА процессы всегда являются стационарными
#------------------------------------------------

# 2 Задание -------------------------------------

# ARIMA-0-1-2
set.seed(100)
arima012 <- arima.sim(n=120, list(c(0, 1, 2)))
ggtsdisplay(arima012)

# ARIMA(0-0-0)
set.seed(47)
arima000 <- Arima(y, order = c(0, 0, 0))
ggtsdisplay(arima000)

# ARIMA(3-0-0)
set.seed(100)
arima300 <- arima.sim(n=120, list(c(3, 0, 0)))
ggtsdisplay(arima300)
#---------------------------------------------

# 3 Задание
# Random Walk не является стационарным решением
set.seed(123)
TT <- 100
xx <- ww <- rnorm(n = TT, mean = 0, sd = 1)
x2 <- cumsum(ww)
ggtsdisplay(x2)
#-----------------------------------------------

# 4 Задание-------------------------------------
# Сравнение случайного блуждания с AR(1)
set.seed(100)
y <- arima.sim(n = 120, list(ar = 0.8, ma = 0))
ggtsdisplay(y)
#-----------------------------------------------

#5 Задание--------------------------------------
set.seed(100)
# (a)
y = arima.sim(n=120, list(c(2, 0, 3)))

# (b)
train <- head(y, round(length(y) * 5/6)) 

# (c)
test <- tail(y, length(y) - length(train)) 
arima203 <- Arima(train, order = c(2, 0, 3)) 

# (d)
future <- forecast(arima203, h = 20, level=95)

# (e)е
autoplot(future) + autolayer(test)
#-----------------------------------------------