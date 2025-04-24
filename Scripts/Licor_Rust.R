# Pull
# April 20 2025
# SMS

# check packages out of the library
library(dplyr)
library(MASS)
library(ggplot2)

library(effects) # vis option 1
library(ggeffects) # vis option 2

######################################################################.
# Clean Data ----
######################################################################.

### GSW ----
# pull in Licor data
licor_folder_path <- "~/Desktop/Work /Research/Keller/poplarF1/OriginalData/licor"

csv_files <- list.files(path = licor_folder_path, pattern = "\\.csv$", full.names = TRUE)

data_list <- lapply(csv_files, read.csv, skip=2)

merged_data <- bind_rows(data_list)

# create new file with one column for plant id and one for GSW
subset_data <- merged_data[, 7:10]

subset_data$plant_id <- paste(subset_data[[1]], subset_data[[2]], subset_data[[3]], sep = "_")

final_data <- data.frame(
  plant_id = subset_data$plant_id,
  gsw = subset_data[[4]]
)

setwd("~/Desktop/Work /Research/Keller/poplarF1/CleanedData")
write.csv(final_data, "gsw_byPlant.csv", row.names = FALSE)

gsw_data <- read.csv(file = "gsw_byPlant.csv")


### Rust ----

# Pull in rust data
setwd("~/Desktop/Work /Research/Keller/poplarF1/OriginalData/melampsora")
rust_data <- read.csv(file = "rust_data.csv")

# remove rows with NA vals
rust_data <- na.omit(rust_data)

# combine id in one column if needed
subset_data <- rust_data[, 2:5]
subset_data$plant_id <- paste(subset_data[[1]], subset_data[[2]], subset_data[[3]], sep = "_")

rust_data <- data.frame(
  plant_id = subset_data$plant_id,
  disease_score = subset_data[[4]]
)

### Merge & Mean ----

# clean the rust data as needed and merge with gsw data making sure it is aligned by plant id
# this is to ensure we are comparing data from the same plant
rust <- merge(gsw_data, rust_data, by="plant_id")

# save final combined file to cleaned data
setwd("~/Desktop/Work /Research/Keller/poplarF1/CleanedData")
write.csv(rust, "gsw_rust.csv", row.names = FALSE)

# simplfy to mean gsw (vs three reading per plant)
meanGSW_rust <- aggregate(x = gsw_rust_data, by = list(gsw_rust_data$plant_id), FUN = "mean")

meanGSW_rust <- data.frame(
  plant_id = meanGSW_rust$Group.1,
  disease_score = meanGSW_rust$disease_score,
  gsw = meanGSW_rust$gsw
)

write.csv(meanGSW_rust, "meanGSW_rust.csv", row.names = FALSE)




######################################################################.
# Plots ----
######################################################################.

### w/ 3 licor reps ----
setwd("~/Desktop/Work /Research/Keller/poplarF1/CleanedData")
gsw_rust_data <- read.csv(file = "gsw_rust.csv")
meanGSW_rust <- read.csv(file = "meanGSW_rust.csv")

# Boxplot grouping by disease state
ggplot(gsw_rust_data, aes(x = factor(disease_score), y = gsw)) +
  geom_boxplot() +
  labs(x = "Disease Severity Score", y = "GSW", title = "GSW by Disease Severity")

# ANOVA
anova_result <- aov(gsw ~ factor(disease_score), data = gsw_rust_data)
summary(anova_result)

# Ordinal logistic regression

# order
gsw_rust_data$disease_score <- ordered(gsw_rust_data$disease_score)
# Fit the ordinal logistic regression model
model <- polr(disease_score ~ gsw, data = gsw_rust_data, Hess = TRUE)
# View results
summary(model)

# p?
ctable <- coef(summary(model))
p_vals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p_vals)
ctable

# If the estimate is negative, higher GSW is associated with lower odds of being in a higher disease severity category # If it’s positive, higher GSW is associated with higher disease risk

# two ways to visualize get some feedback on this and talk it though more
plot(allEffects(model))

preds <- ggpredict(model, terms = "gsw")
plot(preds)


######################################################################.
### avg gsw ----

# Boxplot grouping by disease state
ggplot(meanGSW_rust, aes(x = factor(disease_score), y = gsw)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Disease Severity Score", y = "GSW", title = "GSW by Disease Severity") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18))


anova_result <- aov(gsw ~ factor(disease_score), data = meanGSW_rust)
summary(anova_result)

# Ordinal logistic regression

# order
meanGSW_rust$disease_score <- ordered(meanGSW_rust$disease_score)
# Fit the ordinal logistic regression model
model <- polr(disease_score ~ gsw, data = meanGSW_rust, Hess = TRUE)
# View results
summary(model)

# p?
ctable <- coef(summary(model))
p_vals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p_vals)
ctable

# If the estimate is negative, higher GSW is associated with lower odds of being in a higher disease severity category # If it’s positive, higher GSW is associated with higher disease risk

# two ways to visualize get some feedback on this and talk it though more

preds <- ggpredict(model, terms = "gsw")
plot(preds)
plot(preds) +
  labs(
    title = "Predicted Disease Score by GSW",
    x = "GSW",
    y = "Predicted Probability"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 8)
  )

eff <- allEffects(model)
plot(eff,
     main = "Effect of GSW on Disease Score",
     xlab = "GSW",
     ylab = "Predicted Probability")










