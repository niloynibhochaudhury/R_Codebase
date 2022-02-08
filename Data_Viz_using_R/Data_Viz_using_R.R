# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .R
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.13.3
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

# +
# par(mfrow = c(3, 1))
# -

install.packages("ggplot2")

install.packages("readxl")

library(datasets)

library(readxl)

# +
library(repr)

# Change plot size to 4 x 3
options(repr.plot.width=15, repr.plot.height=7)
# -

aq <- datasets::airquality

head(aq)

par(mfrow = c(1, 2))
plot(aq$Month, aq$Ozone, pch = 16, col = "red")
plot(aq$Month, aq$Temp, col = "blue")
# plot(aq$Month, aq$Solar.R)

cars <- datasets::mtcars

head(cars)

str(cars)

par(mfrow = c(1, 1))

plot(cars$cyl, cars$hp, pch = 16, col = "red")

cars$cyl <- factor(cars$cyl)

str(cars)

plot(cars$cyl, cars$hp, pch = 16, col = "red")

pr <- datasets::pressure

plot(pr$temperature, pr$pressure)

par()

head(data())

head(data(package = .packages(all.available = TRUE)))

par(mfcol = c(1, 2))
plot(pr$temperature, pr$pressure,
    xlab = "Temp",
    ylab = "Pressure",
    main = "T vs P",
    type = "o",
    col = "red")
plot(pr$temperature, pr$pressure,
    xlab = "Temp",
    ylab = "Pressure",
    main = "T vs P",
    type = "o",
    col = "blue",
    col.main = "darkorange",
    cex.axis = 1.25,
    lty = 4,
    pch = 5)

lc <- read_excel("./Data_Viz_using_R/LungCap.xls")

head(lc)

dim(lc)

names(lc)

str(lc)

lc$Gender <- factor(lc$Gender)

lc$Smoke <- factor(lc$Smoke)

str(lc)

?barplot

table(lc$Gender)

cnt <- table(lc$Gender)
cnt

typeof(cnt)

per <- table(lc$Gender) / 725

per

par(mfrow = c(1, 2))
barplot(cnt)

barplot(per, main = "Male/Female Ratio", xlab = "Gender", ylab = "Percent", las = 1,
        names.arg = c("Female", "Male"), horiz = TRUE)

par(mfrow = 1)
pie(per)

box(lty = '1373', col = 'red')

par(mfrow = c(1, 2))
barplot(cnt, 
        main = "# of Males and Females", font.main = 25, 
        xlab = "Gender", ylab = "Count", las = 1, names.arg = c("Female", "Male"),
        cex.names = 1.3, cex.lab = 1.5, cex.main = 2, col.main = "red", col = "lightblue", col.lab = "darkgreen")
barplot(per, 
        main = "% of Males and Females", font.main = 25, 
        xlab = "Gender", ylab = "Percent", las = 1, names.arg = c("Female", "Male"),
        cex.names = 1.3, cex.lab = 1.5, cex.main = 2, col.main = "red", col = "lightblue", col.lab = "darkgreen")

library(help = "datasets")

par(mfrow = c(1, 1))

table1 <- table(lc$Smoke, lc$Gender)
table1

barplot(table1, beside = TRUE, las = 1, col = c(2, 4), legend.text = c("Non-Smoke", "Smoke"),
        main = "Gender vs Smoker Info", xlab = "Gender")

mosaicplot(table1, col = c(2, 4))

counts <- table(mtcars$vs, mtcars$carb)
counts
barplot(counts, main="Car Distribution by Count of V-shaped engine and Carb",
        xlab="Number of Carb", ylab="Count of cars", col=c("Yellow","red"),
        legend = c("Not vs","vs"))

counts <- table(mtcars$vs, mtcars$carb)
barplot(counts, main="Car Distribution by Count of V-shaped engine and Carb",  xlab="Number of Carb",
        ylab="Count of cars", col=c("Yellow","red"), legend = c("Not vs","vs"), beside=TRUE)

boxplot(lc$`LungCap(cc)`, main = "Lung Capqacity (in cc)", las = 1, ylim = c(0, 16))

boxplot(lc$`LungCap(cc)` ~ lc$Gender, main = "Lung Capqacity (in cc) by Gender", las = 1,
         ylim = c(0, 16), ylab = "Lung Capacity", xlab = "Gender",
        col = c("orange", "red"), legend = TRUE)

?boxplot

quantile(lc$`LungCap(cc)`)