path<-("D:/bloodpressure.csv")
dataset<-read.csv(path)
print(dataset)
age <- c(20, 24, 60, 50, 40, 50)
blood.pressure <- c(10, 20, 30, 40, 80, 90)
data <- data.frame(age, blood.pressure)
print(data)
plot(data$age, data$blood.pressure,
     main = "Scatterplot of Blood Pressure vs Age",
     xlab = "Age",
     ylab = "Blood Pressure",
     pch = 19, 
     col = "pink")
model <- lm(blood.pressure ~ age, data = data)
abline(model, col = "blue")
age_breaks <- seq(floor(min(data$age)/10)*10, ceiling(max(data$age)/10)*10, by = 10)
age_labels <- paste0(seq(floor(min(data$age)/10)*10, ceiling(max(data$age)/10)*10 - 10, by = 10), "-", 
                     seq(floor(min(data$age)/10)*10 + 9, ceiling(max(data$age)/10)*10 - 1, by = 10))

data$AgeGroup <- cut(data$age, breaks = age_breaks, labels = age_labels, include.lowest = TRUE)
mean_bp <- tapply(data$blood.pressure, data$AgeGroup, mean, na.rm = TRUE)
barplot(mean_bp,
        main = "Bar Chart of Mean Blood Pressure by Age Group",
        xlab = "Age Group",
        ylab = "Mean Blood Pressure",
        col = "orange",
        border = "black",
        names.arg = names(mean_bp),
        las = 2) # Rotate axis labels