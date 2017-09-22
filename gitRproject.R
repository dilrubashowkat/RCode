md <- read.csv('t3.csv')
library(ARTool)
m=art(md$QID27_1 ~ QID13,data=md)
anova(m)
m=art(md$QID27_2 ~ QID13,data=md)
anova(m)
m=art(md$QID27_3 ~ QID13,data=md)
anova(m)
m=art(md$QID27_4 ~ QID13,data=md)
anova(m)
m=art(md$QID27_5 ~ QID13,data=md)
anova(m)

##ANOVA
model1 <- aov(md$QID27_1 ~ QID13, data=md) #Anova test
summary(model1)
model1 <- aov(md$QID27_2 ~ QID13, data=md) #Anova test
summary(model1)
model1 <- aov(md$QID27_3 ~ QID13, data=md) #Anova test
summary(model1)
model1 <- aov(md$QID27_4 ~ QID13, data=md) #Anova test
summary(model1)
model1 <- aov(md$QID27_5 ~ QID13, data=md) #Anova test
summary(model1)
##Boxplot
boxplot(md$QID27_1 ~ md$QID13,col=c("red","lightblue"),ylab="Gender Bias",xlab="Gender",main="Gender Bias vs Gender",las=1)
boxplot(md$QID27_2 ~ md$QID13,col=c("red","lightblue"),ylab="Gender Bias",xlab="Gender",main="Gender Bias vs Gender",las=1)
boxplot(md$QID27_3 ~ md$QID13,col=c("red","lightblue"),ylab="Gender Bias",xlab="Gender",main="Gender Bias vs Gender",las=1)
boxplot(md$QID27_4 ~ md$QID13,col=c("red","lightblue"),ylab="Gender Bias",xlab="Gender",main="Gender Bias vs Gender",las=1)
boxplot(md$QID27_5 ~ md$QID13,col=c("red","lightblue"),ylab="Gender Bias",xlab="Gender",main="Gender Bias vs Gender",las=1)
##t-test
t.test(md$QID27_1 ~ md$QID13)
t.test(md$QID27_2 ~ md$QID13)
t.test(md$QID27_3 ~ md$QID13)
t.test(md$QID17_4 ~ md$QID13)
t.test(md$QID27_5 ~ md$QID13)
##Levene-test
library(car)
leveneTest(md$QID27_1 ~ md$QID13,data=md)
leveneTest(md$QID27_2 ~ md$QID13,data=md)
leveneTest(md$QID27_3 ~ md$QID13,data=md)
leveneTest(md$QID27_4 ~ md$QID13,data=md)
leveneTest(md$QID27_5 ~ md$QID13,data=md)
##Mann-whitney-test
wilcox.test(md$QID27_1 ~ md$QID13)
wilcox.test(md$QID27_2 ~ md$QID13)
wilcox.test(md$QID27_3 ~ md$QID13)
wilcox.test(md$QID27_4 ~ md$QID13)
wilcox.test(md$QID27_5 ~ md$QID13)
##Linear Regression with R^2
model1 <- lm(md$QID27_1 ~ md$QID13,data=md)
anova(model1)
model1$coef
summary(model1)$r.squared

model1 <- lm(md$QID27_2 ~ md$QID13,data=md)
anova(model1)
model1$coef
summary(model1)$r.squared

model1 <- lm(md$QID27_3 ~ md$QID13,data=md)
anova(model1)
model1$coef
summary(model1)$r.squared

model1 <- lm(md$QID27_4 ~ md$QID13,data=md)
anova(model1)
model1$coef
summary(model1)$r.squared

model1 <- lm(md$QID27_5 ~ md$QID13,data=md)
anova(model1)
model1$coef
summary(model1)$r.squared

