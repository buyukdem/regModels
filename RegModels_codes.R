data(mtcars)

# Preprocessing
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am <- factor(mtcars$am,labels=c('Automatic','Manual'))
str(mtcars)

par(mfrow = c(1, 2))
# Histogram with Normal Curve
x <- mtcars$mpg
h <- hist(x, breaks=10, col="salmon", xlab="Miles Per Gallon",
          main="Histogram of Miles per Gallon")

boxplot(mpg~am, data = mtcars,
        col = c("light blue", "light pink"),
        xlab = "Transmission",
        ylab = "Miles per Gallon",
        main = "MPG by Transmission Type")

# Hypotheses testing
autoData <- mtcars[mtcars$am == "Automatic",]
manualData <- mtcars[mtcars$am == "Manual",]
t.test(autoData$mpg, manualData$mpg) 

# variable selection, checking
pairs(mtcars)
data(mtcars)
sort(cor(mtcars)[1,])

# models
model1 <- lm(mpg~am, data = mtcars)
summary(model1)

model2 <- lm(mpg~., data = mtcars)
summary(model2)

best <- step(model2, direction = "both", trace = FALSE)
summary(best)

# residual diagnostics
par(mfrow = c(2,2))
plot(best)
