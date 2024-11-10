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

#Conduct Barlett's test to 
bartlett.test(academic_workload ~ stress, data = sample_data)
print(barlett_result$p.value)

# new changes to ANOVA between stress and academic workload
sample_data$workload <-as.numeric(factor(sample_data$academic_workload,levels =c(1,2,3,4,5)))

sample_data$stress_numeric <- as.numeric(factor(sample_data$stress, 
                                                levels = c("Never", "Sometimes", "Always")))
                                                
sample_data$anxiety_numeric <- as.numeric(factor(sample_data$anxiety,levels = c("Never", "Sometimes", "Always")))

sample_data$concentration_numeric <- as.numeric(factor(sample_data$concentration,levels = c("Never", "Sometimes", "Always")))

sample_data$living <- as.numeric(factor(sample_data$living_situation,
                                        levels = c("Living alone", "Living with family", "Living with roommates", "Living on campus")))

sample_data$time <-as.numeric(factor(sample_data$time_management,levels =c(0,1,2,3,4)))

sample_data$finances <-as.numeric(factor(sample_data$financial_problems,levels =c(0,1,2,3,4)))

sample_data$socials <-as.numeric(factor(sample_data$missed_social_events,levels =c(0,1,2,3,4)))

#Academic Workload vs Stress
new_table <- table(sample_data$workload, sample_data$stress_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Academic Workload vs Anxiety
new_table <- table(sample_data$workload, sample_data$anxiety_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Academic Workload vs Concentration
new_table <- table(sample_data$workload, sample_data$concentration_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Academic Workload vs Time
new_table <- table(sample_data$workload, sample_data$time)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Academic Workload vs Financials
new_table <- table(sample_data$workload, sample_data$finances)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Living Situation vs Stress
new_table <- table(sample_data$living, sample_data$stress_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Living Situation vs Anxiety
new_table <- table(sample_data$living, sample_data$anxiety_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Living Situation vs Concentration
new_table <- table(sample_data$living, sample_data$concentration_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Time Management vs Stress
new_table <- table(sample_data$time, sample_data$stress_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Time Management vs Anxiety
new_table <- table(sample_data$time, sample_data$anxiety_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Time Management vs Concentration
new_table <- table(sample_data$time, sample_data$concentration_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Financials vs Stress
new_table <- table(sample_data$finances, sample_data$stress_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Financials vs Anxiety
new_table <- table(sample_data$finances, sample_data$anxiety_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)

#Financials vs Concentration
new_table <- table(sample_data$finances, sample_data$concentration_numeric_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)