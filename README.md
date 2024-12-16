# P717-Project
Quantitative Methods for Public Health Course Project
library(ggplot2)
library(dplyr)

PC <- read.csv(file = "WASH_data.csv", header = TRUE)
View(PC)
mean(PC$age_hh)
sd(PC$age_hh)

table(PC$improved_water_source)

PC$improved_water_source <- ifelse(PC$improved_water_source == 1, "Yes",
                                   ifelse(PC$improved_water_source == 2, "No", PC$improved_water_source))
table(PC$improved_water_source)

table(PC$marital_status_hh)
table(PC$ZSHP_initiation)
table(PC$ZSHP_initiation, PC$improved_water_source)
table(PC$improved_water_source)

table(PC$improved_water_source, PC$type_toilet)
prop.table(table(PC$improved_water_source, PC$type_toilet), 1)

table(PC$improved_water_source, PC$wealth)
prop.table(table(PC$improved_water_source, PC$wealth), 1)

table(PC$improved_water_source, PC$age_hh)
prop.table(table(PC$improved_water_source, PC$age_hh))

table(PC$improved_water_source, PC$ZSHP_initiation)
prop.table(table(PC$improved_water_source, PC$ZSHP_initiation), 1)

table(PC$improved_water_source, PC$water_source)
prop.table(table(PC$improved_water_source, PC$water_source), 1)

table(PC$improved_water_source)

data.class(PC$improved_water_source)

PC$improved_water_source <- as.factor(PC$improved_water_source)
data.class(PC$type_toilet)
PC$type_toilet <- as.factor(PC$type_toilet)

attach(PC)

water_source <- as.factor(water_source)
type_toilet <- as.factor(type_toilet)

Toilet_type.ggp <- ggplot(PC, aes(x = type_toilet, fill = improved_water_source)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("lightsalmon", "gold")) +
  scale_x_discrete(labels = c("Flush toilet/VIP", "Latrine with cement slab", "Latrine without cement slab", "No toilet")) +
  theme_bw() +
  labs(title = "Toilet Type Improvement", x = "Type of Toilet", y = "Households", fill = "Improved Water Source")+
  theme(legend.position = "bottom")

Toilet_type.ggp

wealth <- as.factor(wealth)

Wealth.ggp <- ggplot(PC, aes (x = wealth, fill = improved_water_source))+
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("pink", "purple1")) +
  scale_x_discrete(labels = c("Poorest20%tile", "Second20%tile", "Third20%tile", "Fourth20%tile", "Wealthiest20%tile")) +
  theme_bw() +
  labs(title = "Wealth Improvement", x = "Wealth", y = "Households", fill = "Improved Water Source")+
  theme(legend.position = "bottom")

Wealth.ggp

prop.table(table(PC$ZSHP_initiation,improved_water_source),1)
table(PC$ZSHP_initiation,PC$improved_water_source)

RRtable<-matrix(c(401,209,322,238),nrow=2,ncol=2)
prop.test(RRtable)
riskratio.wald(RRtable)

PC$improved_water_source <- as.factor(PC$improved_water_source)
data.class(PC$improved_water_source)
PC$ZSHP_initiation <- as.factor(PC$ZSHP_initiation)
PC$wealth <- as.factor(PC$wealth)
PC$type_toilet <- as.factor(PC$type_toilet)

View(PC)


LogitModel <-glm(PC$improved_water_source ~ PC$ZSHP_initiation + PC$wealth + PC$type_toilet,family = binomial(link=logit))
summary(LogitModel)
exp(LogitModel$coefficients)         
exp(confint(LogitModel))
