#0. Loaded required libraries

library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)



#1. Assigned the dataset

data <- combined_influencer_data_final



#2. Inspected the data

#We check the first few rows and the structure of the dataset, including data types and column names

head(data) 
str(data) 



#3. Converted categorical variables to factors

#We converted key demographic and treatment variables into factors so they can be properly analyzed in statistical tests.

data$influencer_type <- as.factor(data$influencer_type)
data$age <- as.factor(data$age)
data$gender <- as.factor(data$gender)
data$social_media_hours <- as.factor(data$social_media_hours)



#4. Compared perceived trust between influencer types

#We pooled all variables of perceived trust and ran a t-test to see if the mean perceived trust differs significantly between virtual and human influencers.

t.test(perceived_trust ~ influencer_type, data = data)



#5. Visualized perceived trust

#We created a boxplot showing differences in perceived trust between the two influencer types, including mean and standard deviation.
ggboxplot(data, x = "influencer_type", y = "perceived_trust",
          color = "influencer_type", palette = "jco",
          add = "mean_sd", ylab = "Confianza Percibida", xlab = "Tipo de Influencer") +
  ggtitle("Confianza Percibida por Tipo de Influencer")

#We create a dispersion plot with jitter to show individual trust scores by influencer type.

ggplot(data, aes(x = influencer_type, y = perceived_trust, color = influencer_type)) +
  geom_jitter(width = 0.2, height = 0, size = 3, alpha = 0.7) +  # scatter with jitter
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "black") + # mean points
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, color = "black") + # error bars for SE
  labs(title = "Dispersion of Trust Scores by Influencer Type",
       x = "Influencer Type",
       y = "Trust Score") +
  theme_minimal() +
  theme(legend.position = "none")



#6. Analyzed purchase intention

#Another t-test to see if the type of influencer affects consumers' purchase intent.
t.test(endorsement_q14 ~ influencer_type, data = data)



#7. Run multiple linear regression

#We built a regression model to test whether influencer type still predicts perceived trust after controlling for other variables (like age, gender, etc.).

modelo <- lm(perceived_trust ~ influencer_type + familiarity + age + gender + social_media_hours, data = data)
summary(modelo)



#8. Group results by age and influencer type

#We calculated the average perceived trust per age group and influencer type to identify any patterns by demographic.

data %>%
  group_by(age, influencer_type) %>%
  summarise(mean_confianza = mean(perceived_trust, na.rm = TRUE),
            n = n())



#9. Created a causal DAG

#This part builds a causal diagram (DAG) that shows the assumed relationships between variables. It shows:
#Main effect: influencer_type → perceived_trust
#Confounders: age, gender, familiarity, social media usage


install.packages("dagitty") 
install.packages("ggdag")  # solo una vez
library(ggdag)
library(dagitty)

library(ggdag)

dag <- dagify(
  perceived_trust ~ influencer_type + familiarity + age + gender + social_media_hours,
  familiarity ~ age + social_media_hours,
  social_media_hours ~ age,
  exposure = "influencer_type",
  outcome = "perceived_trust"
)

ggdag(dag, text = FALSE, use_labels = "name") +
  theme_minimal() +
  ggtitle("DAG: Influence of Influencer Type on Perceived Trust")
