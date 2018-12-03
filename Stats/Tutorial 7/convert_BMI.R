setwd("~/Desktop/R tutorials/2017/Tutorial 7")

BMI <- read.csv('Tutorial_7_BMI_source.csv', stringsAsFactors = F)

N <- nrow(BMI)

# BMI goes from 19 to 30

BMI$BMI <- BMI$BMI - min(BMI$BMI)
BMI$BMI <- BMI$BMI/max(BMI$BMI) * 11
BMI$BMI <- 30 - BMI$BMI # + runif(N,0,1.2)
BMI$BMI <- round(BMI$BMI, digits=0)

# physical activity could be the number of days per week you work out? (no need to transform)

# impulse control (Barratt Impulsiveness Scale goes from 30 to 120)

BMI$impulsectrl <- 120 - 2*(max(BMI$impulsectrl) - BMI$impulsectrl) # + runif(N,0,3)
BMI$impulsectrl <- round(BMI$impulsectrl)

# percentage carbs in diet (should be around 50-60% and not won't go all the way up or down)

BMI$carbpercent <- ((BMI$carbpercent - mean(BMI$carbpercent)) / 2) + 47
# BMI$carbpercent <- BMI$carbpercent + runif(1, 0, 3)
BMI$carbpercent <- round(BMI$carbpercent)

# Pittsburgh Sleep Quality index goes from 0 to 21 (but a score above 5 is problematic)

BMI$sleepquality <-  BMI$sleepquality - min(BMI$sleepquality)
BMI$sleepquality <- ((BMI$sleepquality / max(BMI$sleepquality))^1.5)
BMI$sleepquality <- ((max(BMI$sleepquality) - BMI$sleepquality) / max(BMI$sleepquality)) * 10
BMI$sleepquality <- round(BMI$sleepquality)

pairs.panels(BMI[c(2,3,4,5,1)])

write.csv(BMI, 'Tutorial_7_BMI.csv', row.names = F)