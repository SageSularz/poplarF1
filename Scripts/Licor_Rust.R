# Pull
# April 20 2025
# SMS

# check packages out of the library
library(dplyr)
library(MASS)
library(ggplot2)

library(effects) # vis option 1
library(ggeffects) # vis option 2

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

write.csv(final_data, "gsw_byPlant.csv", row.names = FALSE)

gsw_data <- read.csv(file = "gsw_byPlant.csv")

# Pull in rust data
rust_data <- read.csv(file = "rust_data.csv")

# clean the rust data as needed and merge with gsw data making sure it is aligned by plant id
# this is to ensure we are comparing data from the same plant
write.csv(data, "gsw_rust.csv", row.names = FALSE)


## Plots
gsw_rust_data <- read.csv(file = "gsw_rust.csv")

# Boxplot grouping by disease state
ggplot(gsw_rust_data, aes(x = factor(disease_score), y = gsw)) +
  geom_boxplot() +
  labs(x = "Disease Severity Score", y = "GSW", title = "GSW by Disease Severity")


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
