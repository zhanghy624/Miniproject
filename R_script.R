
library("dplyr")
require(lme4)
library(ggplot2)

# Import raw data
setwd("C:/Users/道可道/Desktop/EEC_MSC/Mini_project")
raw_data <- read.csv("./raw_data.csv")

# Look at information available in this dataset
head(raw_data)

# Exclude traits inferred from other species
data <- subset(raw_data, Inference== "NO")

# Checking data size of each species
hist (data$Total.individuals)

# Exclude species with less than 5 individuals
data <- subset(data, Total.individuals >= 5)
summary(data$Total.individuals)

# Subset traits used in following analysis
trait_data <- select(data, Total.individuals, Order1, Family1, Species1, 
                     Hand.Wing.Index, Habitat, Range.Size)

# Remove rows with NAs
trait_data <- na.omit(trait_data)

# Calculate total species and individual number
nrow(trait_data)
sum(trait_data$Total.individuals)

# Visualize relationship between range size and body trait
plot(trait_data$Hand.Wing.Index,trait_data$Range.Size)

# Plot boxplot on random effects
par(mfrow=c(1,3))

ggplot(trait_data, aes(x = Order1, y = Range.Size)) +
  geom_boxplot() +
  labs(title = "Range Size by Order",
       x = "Order",
       y = "Range Size") +
  theme_minimal()

ggplot(trait_data, aes(x = Family1, y = Range.Size)) +
  geom_boxplot() +
  labs(title = "Range Size by Family",
       x = "Family",
       y = "Range Size") +
  theme_minimal()

ggplot(trait_data, aes(x = Habitat, y = Range.Size)) +
  geom_boxplot() +
  labs(title = "Range Size by Habitat",
       x = "Habitat",
       y = "Range Size") +
  theme_minimal()

# Examine the normality of variables
hist(trait_data$Hand.Wing.Index)
hist(trait_data$Range.Size)

# Transform the range size to make it closer to normal distribution
trait_data$Log_RS <- log(trait_data$Range.Size)

# Lmm model
m2 <- lmer(Log_RS~Hand.Wing.Index +(1|Habitat)+(1|Order1/Family1),
           data=trait_data)

# Look at model output
summary(m2)

# Extract variances from output
variances <- c(0.6829, 0.4788, 0.4765, 4.7913)

# Calculate total variance
total_variance <- sum(variances)

# Calculate proportion of variance that each random effect account for
contribution_ratio <- variances / total_variance

# Print proportion of variance that random effects accounted for
contribution_df <- data.frame(
  Group = c("Family1:Order1", "Order1", "Habitat", "Residual"),
  Contribution_Ratio = contribution_ratio)
print(contribution_df)

# Model validation
plot(m2)

#Plot the model
ggplot(trait_data, aes(x = Hand.Wing.Index, y = Log_RS)) +
  geom_point(color = "#82B0D2") +
  geom_abline(intercept = 12.09, slope = 0.06,color = "#3A5F87",size=1.1) +
  labs(x = "HWI",
       y = "log (range size) (km^2)") +
  theme_minimal() +
  theme(
    text = element_text(size = 15), 
    axis.title = element_text(size = 15, margin = margin(t = 20)),
    axis.title.x = element_text(margin = margin(t = 20)), 
    axis.title.y = element_text(margin = margin(r = 20)))

