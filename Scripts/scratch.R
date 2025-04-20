# Scratch

# make fake data for rust score to test main script
set.seed(123)
gsw_data$disease_score <- sample(0:4, size = nrow(merged_data), replace = TRUE)
head(gsw_data)
# give it the right name so you dont have to change shit later
gsw_rust_data <- gsw_data
head(gsw_rust_data)












