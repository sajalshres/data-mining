# Load data
a_train_df <- read.table("data8_A_train.txt", header = TRUE)
b_train_df <- read.table("data8_B_train.txt", header = TRUE)

# Merge two data set
train_df <- merge(a_train_df, b_train_df, by = "id", all = TRUE)

# Sort the data by id
train_df <- train_df[order(train_df$id), ]

print(nrow(train_df))


# plot the missing data
print("Missing data:")
print(md.pattern(train_df))
