install.packages("lattice")
library(lattice)
help(package = "lattice")
data(package = "lattice")
data(environmental)

xyplot(ozone ~ radiation,  data = environmental, main = "Ozone vs Radiation")

xyplot(ozone ~ temperature,  data = environmental, main = "Ozone vs Temp")

summary(environmental$temperature)

tmp.cut <- equal.count(environmental$temperature, 4)
tmp.cut

xyplot(ozone ~ radiation | tmp.cut,  data = environmental,
       main = "Ozone vs Radiation")

xyplot(ozone ~ radiation | tmp.cut,  data = environmental, layout = c(1, 4),
       main = "Ozone vs Radiation")

xyplot(ozone ~ radiation | tmp.cut,  data = environmental, layout = c(1, 4), as.table = TRUE,
       main = "Ozone vs Radiation")

xyplot(ozone ~ radiation | tmp.cut,  data = environmental, as.table = TRUE,
       main = "Ozone vs Radiation")

xyplot(ozone ~ radiation | tmp.cut,  data = environmental, as.table = TRUE,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         fit <- lm(y ~ x)
         panel.abline(fit)
       },
       main = "Ozone vs Radiation")

xyplot(ozone ~ radiation | tmp.cut,  data = environmental, as.table = TRUE, pch = 20,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         fit <- lm(y ~ x)
         panel.abline(fit, lwd = 2)
       },
       main = "Ozone vs Radiation")

xyplot(ozone ~ radiation | tmp.cut,  data = environmental, as.table = TRUE, pch = 20,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.loess(x, y)
       },
       main = "Ozone vs Solar Radiation", xlab = "Solar Radiation", ylab = "Ozone (ppb)")

wind.cut <- equal.count(environmental$wind, 4)
wind.cut

xyplot(ozone ~ radiation | tmp.cut * wind.cut,  data = environmental, as.table = TRUE, pch = 20,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.loess(x, y)
       },
       main = "Ozone vs Solar Radiation", xlab = "Solar Radiation", ylab = "Ozone (ppb)")

splom(~environmental)

histogram(~ozone | wind.cut, data = environmental)

histogram(~ozone | wind.cut * tmp.cut, data = environmental)


