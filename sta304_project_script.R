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

my_data <- read.csv("STA304 Group Project Dataset.csv", header=TRUE, sep=',')
# Datapreprocessing
# Remove column 'X'
data_cleaned <- my_data[,!(names(my_data)=="X")]
# Remove fake responses on row 3 and 12
data_cleaned <- data_cleaned[-c(3, 12),]
data_cleaned

N = 200
B = .29
D = (B^2)/4
sigma = sd(data_cleaned$academic_workload)
n = ceiling((N * sigma^2) / ((N-1) * D + sigma^2))
n

# Select a random sample of n rows
set.seed(1)
random_indices <- sample(1:nrow(data_cleaned), n)
sample_data <- data_cleaned[random_indices, ]
sample_data

#####Simple and Multiple Linear Regressions#####

# Research Question 1 - Simple Linear Regression
sample_data$stress_numeric <- as.numeric(factor(sample_data$stress, levels = c("Never", "Sometimes", "Always")))
rq1_stress.lm = lm(stress_numeric ~ academic_workload, data = sample_data)
summary(rq1_stress.lm)


# Assumptions for Research Question 1 - Simple Linear Regression
png("residuals_rq1.png", width = 800, height = 600)
# Plot code goes here, for example:
plot(residuals(rq1_stress.lm))
dev.off()
# Research Question 1 - Boxplot
options(warn = -1)
library(ggplot2)
library(grid)

sample_data$stress <- factor(sample_data$stress, levels = c("Never", "Sometimes", "Always"))
sample_data$stress_numeric <- as.numeric(factor(sample_data$stress, levels = c("Never", "Sometimes", "Always")))

boxplot_plot <- ggplot(sample_data, aes(x = factor(stress), y = academic_workload, fill = stress)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Never" = "#E25C5C", "Sometimes" = "#66B266", "Always" = "#6C8B8B")) +
  labs(title = "Boxplot of Academic Workload by Stress Level",
       x = "Stress Level",
       y = "Academic Workload") +
  theme_minimal() +
  theme(legend.position = "none")

print(boxplot_plot)
grid.rect(gp = gpar(fill = NA, col = "black", lwd = 4)) # lwd = thickness of border

# Save the plot as a PNG file
# ggsave("boxplot_academic_workload_by_stress.png", plot = boxplot_plot, width = 8, height = 6, dpi = 300)



# Research Question 2 - Multiple Linear Regression
rq2.lm = lm(hours_sleep ~ academic_workload + missed_social_events, data = sample_data)
summary(rq2.lm)
# Research Question 2 - Boxplot
boxplot_plot <- ggplot(sample_data, aes(x = factor(missed_social_events), y = hours_sleep, fill = factor(missed_social_events))) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("0" = "#E25C5C", "1" = "#66B266", "2" = "#6C8B8B", "3" = "#9E9E9E", "4" = "#D0A0A0")) +
  labs(title = "Boxplot of Hours of Sleep by Missed Social Events",
       x = "Missed Social Events",
       y = "Hours of Sleep") +
  theme_minimal() +
  theme(legend.position = "none")
  
print(boxplot_plot)
grid.rect(gp = gpar(fill = NA, col = "black", lwd = 4))

# Save the plot as a PNG file
# ggsave("boxplot_hours_sleep_by_missed_social_events.png", plot = boxplot_plot, width = 8, height = 6, dpi = 300)


{r}
# Assumptions for Research Question 2 - Multiple Linear Regression
png("residuals_rq2.png", width = 800, height = 600)
# Plot code goes here, for example:
plot(residuals(rq2_stress.lm))
dev.off()


# Assumptions for Research Question 2 - Multiple Linear Regression
png("qq_rq2.png", width = 800, height = 600)
# Plot code goes here, for example:
plot(rq2.lm, which=2)
dev.off()

# Assumptions for Research Question 2 - Multiple Linear Regression
library(car)
vif(rq2.lm)

# Research Question 3 - Multiple Linear Regression
rq3.lm <- lm(stress_numeric ~ missed_social_events + living_situation, data = sample_data)
summary(rq3.lm)

# Assumptions for Research Question 3 - Multiple Linear Regression
png("residuals_rq3.png", width = 800, height = 600)
# Plot code goes here, for example:
plot(residuals(rq3.lm))
dev.off()
# Research Question 3 - Boxplot
boxplot_plot <- ggplot(sample_data, aes(x = factor(missed_social_events), y = stress_numeric, fill = factor(missed_social_events))) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("0" = "#E25C5C", "1" = "#66B266", "2" = "#6C8B8B", "3" = "#9E9E9E", "4" = "#D0A0A0")) +
  labs(title = "Boxplot of Stress Level by Missed Social Events",
       x = "Missed Social Events",
       y = "Stress Level") +
  theme_minimal() +
  theme(legend.position = "none")

print(boxplot_plot)
grid.rect(gp = gpar(fill = NA, col = "black", lwd = 4))

# Save the plot as a PNG file
# ggsave("boxplot_missed_social_events_by_stress.png", plot = boxplot_plot, width = 8, height = 6, dpi = 300)


# Assumptions for Research Question 3 - Multiple Linear Regression
library(car)
vif(rq3.lm)


#####Kruskal-Wallis Test#####

# Kruskal-Wallis Test between stress and academic_workload

sample_data$stress_numeric <- as.numeric(factor(sample_data$stress, 
                                                levels = c("Never", "Sometimes", "Always")))
shapiro_result_workload <- shapiro.test(sample_data$academic_workload)
print(shapiro_result_workload)

kruskal_result_workload <- kruskal.test(stress_numeric ~ academic_workload, data = sample_data)
print(kruskal_result_workload)
tapply(sample_data$stress_numeric, sample_data$academic_workload, mean) #mean stress levels across different academic workload ratings

# Kruskal-Wallis Test between stress and hours_sleep
sample_data$stress_numeric <- as.numeric(factor(sample_data$stress, 
                                                levels = c("Never", "Sometimes", "Always")))

sample_data$sleep_category <- cut(sample_data$hours_sleep, 
                                  breaks = c(0, 6, 8, Inf),
                                  labels = c("less than 6 hours", "6-8 hours", "over 8 hours"),
                                  include.lowest = TRUE, right = FALSE)

shapiro_result_hours_sleep <- shapiro.test(sample_data$hours_sleep)
print(shapiro_result_hours_sleep)

kruskal_result_sleep <- kruskal.test(stress_numeric ~ sleep_category, data = sample_data)
print(kruskal_result_sleep)
tapply(sample_data$stress_numeric, sample_data$sleep_category, mean) #mean stress levels across different sleep catagories

# Kruskal-Wallis Test between stress and missed social events
sample_data$stress_numeric <- as.numeric(factor(sample_data$stress, 
                                                levels = c("Never", "Sometimes", "Always")))

shapiro_result_workload <- shapiro.test(sample_data$missed_social_events)
print(shapiro_result_workload)

kruskal_result_social <- kruskal.test(stress_numeric ~ missed_social_events, data = sample_data)
print(kruskal_result_social)

tapply(sample_data$stress_numeric, sample_data$missed_social_events, mean) #mean stress levels across different missed social events ratings

#####Chi Square Test#####

sample_data$workload <-as.numeric(factor(sample_data$academic_workload,levels =c(1,2,3,4,5)))

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
new_table <- table(sample_data$finances, sample_data$concentration_numeric)
chisq_result <- chisq.test(new_table)
print(chisq_result)
