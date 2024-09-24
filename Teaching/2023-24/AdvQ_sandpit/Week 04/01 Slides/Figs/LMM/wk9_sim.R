# Generate random salary data

# Set up
library(dplyr) # data wrangling
library(lme4) # modeling
library(ggplot2) # visualization
library(ggpubr)
library(arm) # standard errors

# Parameters for generating faculty salary data
departments <- c('sociology', 'biology', 'english', 'informatics', 'statistics')
base.salaries <- c(40000, 50000, 60000, 70000, 80000)
annual.raises <- c(2000, 500, 500, 1700, 500)
faculty.per.dept <- 20
total.faculty <- faculty.per.dept * length(departments)

# Generate dataframe of faculty and (random) years of experience
ids <- 1:total.faculty
department <- rep(departments, faculty.per.dept)
experience <- floor(runif(total.faculty, 0, 10))
bases <- rep(base.salaries, faculty.per.dept) * runif(total.faculty, .9, 1.1) # noise
raises <- rep(annual.raises, faculty.per.dept) * runif(total.faculty, .9, 1.1) # noise
df <- data.frame(ids, department, bases, experience, raises)

# Generate salaries (base + experience * raise)
df <- df %>% mutate(
  salary = bases + experience * raises
)

# Model without respect to grouping
m0 <- lm(salary ~ experience, data=df)
predict(m0)
df$simple.model <- predict(m0)

# Model with varying intercept
m1 <- lmer(salary ~ experience + (1|department), data = df)
df$random.intercpet.preds <- predict(m1)

# Model with varying slope
m2 <- lmer(salary ~ experience + (0 + experience|department), data=df)
df$random.slope.preds <- predict(m2)

# Model with varying slope and intercept
m3 <- lmer(salary ~ experience + (1 + experience|department), data=df)
df$random.slope.int.preds <- predict(m3)

# Save data and models
#write.csv(df, "public/data/faculty-data.csv", row.names=F)

# Visualize fixed intercept and slope
pdf("lmm0.pdf", height=3, width=4.5)
fix_ab <- ggplot(data=df, aes(x=experience, y=simple.model)) +
  geom_line() + 
  ylim(40000, 90000) + 
  labs(x="Experience", y="Predicted Salary") +
  ggtitle("Fixed Intercept and Slope")
fix_ab
dev.off()

# Visualize random intercept
pdf("lmm1.pdf", height=3, width=4.5)
vary.int.graph <- ggplot(data=df, aes(x=experience, y=random.intercpet.preds, group = department, colour = department)) +
  geom_line() + 
  ylim(40000, 90000) + 
  labs(x="Experience", y="Predicted Salary") +
  ggtitle("Varying Intercept and Fixed Slope") + 
  scale_colour_discrete('Department')
vary.int.graph
dev.off()

# Visualize random slope
pdf("lmm2.pdf", height=3, width=4.5)
vary.slope.graph <- ggplot(data=df, aes(x=experience, y=random.slope.preds, group = department, colour = department)) +
  geom_line() + 
  ylim(40000, 90000) + 
  labs(x="Experience", y="Predicted Salary") +
  ggtitle("Fixed Intercept and Varying Slope") + 
  scale_colour_discrete('Department')
vary.slope.graph
dev.off()

# Visualize random slope + intercept
pdf("lmm3.pdf", height=3, width=4.5)
vary.slope.int.graph <- ggplot(data=df, aes(x=experience, y=random.slope.int.preds, group = department, colour = department)) +
  geom_line() + 
  ylim(40000, 90000) +
  labs(x="Experience", y="Predicted Salary") +
  ggtitle("Varying Intercept and Slope") + 
  scale_colour_discrete('Department')
vary.slope.int.graph
dev.off()

summary(df)
