library(tidyverse)
library(lattice)

VADeathRate <- data.frame(VADeaths) %>%
rownames_to_column() %>%
pivot_longer(cols = 2:5, names_to = "Var2", values_to = "Freq") %>%
rename(Var1 = rowname) %>%
mutate(Var2 = str_replace(Var2, "\\." , " "))


barchart(Freq ~ Var1, data = VADeathRate)
barchart(Freq ~ Var1 | Var2, data = VADeathRate, layout = c(4,1), scales = list(x = list(rot =90)))

dotplot(Freq ~ Var1, data = VADeathRate)
dotplot(Freq ~ Var1, data = VADeathRate, groups = Var2, auto.key = list(space = "right"))

histogram(~ Freq, data = VADeathRate)
histogram(~ Freq | Var2, data = VADeathRate)

xyplot(mpg ~ hp, data = mtcars)

bwplot(~ Freq, data = VADeathRate)
bwplot(Freq ~ Var1, data = VADeathRate, horiz = FALSE)
