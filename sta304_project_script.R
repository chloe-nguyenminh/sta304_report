my_data <- read.csv("STA304 Group Project Dataset.csv", header=TRUE, sep=',')
# Datapreprocessing
# Remove column 'X'
data_cleaned <- my_data[,!(names(my_data)=="X")]
# Remove fake responses on row 3 and 12
data_cleaned <- data_cleaned[-c(3, 12),]
data_cleaned

# Calculating sample size
N = 200
B = .25
D = (B^2)/4
sigma = sd(data_cleaned$academic_workload)
n = ceiling((N * sigma^2) / ((N-1) * D + sigma^2))
n

# Select a random sample of n rows
set.seed(1)
random_indices <- sample(1:nrow(data_cleaned), n)
sample_data <- data_cleaned[random_indices, ]
sample_data

# new changes to ANOVA between stress and academic workload
sample_data$stress_numeric <- as.numeric(factor(sample_data$stress, 
                                                levels = c("Never", "Sometimes", "Always")))

anova_result <- aov(stress_numeric ~ factor(academic_workload), data = sample_data)
summary(anova_result)

# new changes to ANOVA between stress and hours_sleep
sample_data$stress_numeric <- as.numeric(factor(sample_data$stress, 
                                                levels = c("Never", "Sometimes", "Always")))

sample_data$sleep_category <- cut(sample_data$hours_sleep, 
                                  breaks = c(0, 6, 8, Inf),
                                  labels = c("Less than 6", "6-8", "More than 8"))

anova_result <- aov(stress_numeric ~ sleep_category, data = sample_data)
summary(anova_result)

# new changes to ANOVA between stress and missed social events
sample_data$study_category <- cut(sample_data$hours_study, 
                                  breaks = c(0, 10, 20, Inf),
                                  labels = c("Low", "Medium", "High"))

anova_social <- aov(stress_numeric ~ factor(missed_social_events), data = sample_data)
print(summary(anova_social))
