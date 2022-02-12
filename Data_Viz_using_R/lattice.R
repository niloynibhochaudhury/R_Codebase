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

uk2007 <- data.frame("Commodity" = factor(c("Cow Milk", "Wheat", "Sugar Beet", "Potatoes", "Barley"),
                                          levels = c("Cow Milk", "Wheat", "Sugar Beet", "Potatoes", "Barley")),
                     "Production" = c(14023, 13221, 6500, 5635, 5079))

barchart(Production ~ Commodity, data = uk2007, main = "UK Argicultural Production",
         xlab = "Commodity", ylab = "Production", cex.axis = 2)

irrigation.df <- data.frame("Region" = rep(c("Africa", "Latin America", "North America", "Euope"), 4),
                            "Year" = factor(c(rep(1980, 4), rep(1990, 4), rep(2000, 4), rep(2007, 4))),
                            "Area" = c(9.3, 12.7, 21.2, 18.8, 11.9, 15.5, 21.6, 25.3, 13.2, 17.3,
                                       23.3, 26.7, 13.6, 17.3, 23.8, 26.3))

irrigation.df

dotplot(Region ~ Area, data = irrigation.df, groups = Year, main = "Irrigation by Region",
        key=list(space="right",
         points=list(col=c("purple","darkgreen", "red", "blue"), pch = 12, cex = 1.25),
         text=list(c("1980", "1990", "2000", "2007"))
        ))

df <- read.csv("../Data_Viz_using_R/england-premier-league-matches-2018-to-2019-stats.csv")

str(df)

df[1:5,]

histogram(~ attendance | home_team_name, data = df)

uk_df <- data.frame(Year = 2000:2009, Population = c(59131, 59363, 59618, 59894, 60186, 60489, 60504,
                                                    61139, 61461, 61796))

uk_df

trellis.par.get()

tmp.theme <- trellis.par.get()

tmp.theme$fontsize$text <- 14
tmp.theme$plot.symbol$cex <- 1
tmp.theme$plot.symbol$pch <- rep(16, 7)
tmp.theme$axis.line$col <- "blue"

trellis.par.set(tmp.theme)

xyplot(Population ~ Year, data = uk_df,
       main = "UK Pop Disribution",
       scales = list(x = list(at = seq(2000, 2009, 1)))
      )

trellis.par.set()

weather.df <- read.csv("../Data_Viz_using_R/Southamton_Weather_Data.csv")

weather.df[1:12,]

str(weather.df)

weather.df$tmax <- as.numeric(weather.df$tmax)

weather.df$mm <- factor(weather.df$mm, labels = c("Jan", "Feb", "Mar", "Apr","May", "Jun",
                                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# weather.df$mm <- as.factor(weather.df$mm)
str(weather.df)

bwplot(tmax ~ mm, data = weather.df, xlab = "Month", ylab = "Max Temp (degC)")

bwplot(tmax ~ mm, data = weather.df, xlab = "Month", ylab = "Max Temp (degC)", panel = panel.violin)
