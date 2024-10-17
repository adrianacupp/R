## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load the data
data <- read.csv("C:/Users/ReDI/Downloads/ds_research_data.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(data)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(data)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Convert all column names to lowercase
colnames(data) <- tolower(colnames(data))

# Display the updated column names to confirm the change
colnames(data)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check the structure of the relevant columns: province, gender, birthyear
str(data[, c("province", "gender", "birthyear")])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Remove the first two rows that contain metadata
data <- data[-c(1, 2), ]

# Reset row names after removing rows
rownames(data) <- NULL


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Convert the 'birthyear' column to numeric
data$birthyear <- as.numeric(data$birthyear)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check the structure of the relevant columns again
str(data[, c("province", "gender", "birthyear")])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check for missing values in each column
num_missing_province <- sum(is.na(data$province))
num_missing_gender <- sum(is.na(data$gender))
num_missing_birthyear <- sum(is.na(data$birthyear))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Print the number of missing values for each column
cat("Number of missing values in 'province':", num_missing_province, "\n")
cat("Number of missing values in 'gender':", num_missing_gender, "\n")
cat("Number of missing values in 'birthyear':", num_missing_birthyear, "\n")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Summary statistics for 'province' column
province_summary <- table(data$province)
print(province_summary)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot distribution of province
library(ggplot2)
ggplot(data, aes(x = province)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Respondents by Province", x = "Province", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Summary statistics for 'gender' column
gender_summary <- table(data$gender)
print(gender_summary)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot distribution of gender
ggplot(data, aes(x = gender)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Respondents by Gender", x = "Gender", y = "Count") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Summary statistics for 'birthyear' column
birthyear_summary <- summary(data$birthyear)
print(birthyear_summary)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot distribution of birth year
ggplot(data, aes(x = birthyear)) +
  geom_histogram(binwidth = 5, fill = "coral", color = "black") +
  labs(title = "Distribution of Respondents by Birth Year", x = "Birth Year", y = "Count") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a subset of the data for provinces (merge BC and Territories) and gender
# Calculate the proportion of male and female respondents by province

gender_province_filtered <- data %>%
  filter(gender %in% c("Male", "Female")) %>%
  mutate(province = ifelse(province %in% c("Territories","BC"), "BC and Territories", province)) %>%
  group_by(province, gender) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ungroup() %>%
  group_by(province) %>%
  mutate(prop = count / sum(count) * 100) %>%
  ungroup()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Adding a full sample row
full_sample <- gender_province_filtered %>%
  group_by(gender) %>%
  summarise(count = sum(count), .groups = 'drop') %>%
  mutate(province = "Full sample", prop = ceiling(count / sum(count) * 100))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
full_sample


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Combine the full sample with the original data
gender_province_combined <- bind_rows(full_sample, gender_province_filtered)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
gender_province_combined


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Reshape the dataframe: pivot wider to have Male and Female as columns
gender_province_wide <- gender_province_combined %>%
  select(province, gender, prop) %>%  # Select only the necessary columns
  pivot_wider(names_from = gender, values_from = prop) %>%  # Pivot wider to spread 'gender' into columns
  arrange(province)  # Arrange by province for readability


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
#sort it
gender_province_wide_sorted <- gender_province_wide %>%
  arrange(province)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
gender_province_wide_sorted


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
gender_province_combined$province <- factor(gender_province_combined$province,
                                            levels = rev(unique(gender_province_combined$province)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
gender_province_combined


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plotting the gender distribution across provinces with the reversed order
ggplot(gender_province_combined, aes(x = province, y = prop, fill = gender)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(ceiling(prop), "%")),
            position = position_fill(vjust = 0.5),
            color = "white", size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Male" = "#2c3e50", "Female" = "#e74c3c"),
                    name = "Gender",
                    breaks = c("Male", "Female"),  # Ensures correct order in legend
                    labels = c("Male", "Female")) +  # Correct labels
  coord_flip() +
  labs(title = "Gender distribution across Canadian provinces and territories",
       x = "", y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "bottom")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
when_ready_data <- data %>%
  select(gender, birthyear, whenready_1, whenready_2, whenready_3, whenready_4, whenready_5,
         whenready_6, whenready_7, whenready_8, whenready_9, whenready_10)

head(when_ready_data)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
str(when_ready_data)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(when_ready_data)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
when_ready_data <- when_ready_data %>%
  mutate(age = 2024 - birthyear,  # Assuming current year is 2024
         age_group = case_when(
           age >= 18 & age <= 34 ~ "18-34 years",
           age >= 35 & age <= 49 ~ "35-49 years",
           age >= 50 & age <= 64 ~ "50-64 years",
           age >= 65 ~ "65+ years",
           TRUE ~ "Unknown"  # Handles missing or incorrect values
         ))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Display the first few rows to check the age groups
head(when_ready_data)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
when_ready_data %>%
  filter(is.na(age_group) | age_group == "Unknown")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
#Add a unique ID for each respondent
when_ready_data <- when_ready_data %>%
  mutate(ID = row_number())


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(when_ready_data)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
when_ready_long <- when_ready_data %>%
  pivot_longer(
    cols = starts_with("whenready_"),
    names_to = "behavior",
    values_to = "readiness"
  )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(when_ready_long)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
str(when_ready_long)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Reorder columns to ensure 'ID' comes before 'behavior'
when_ready_long <- when_ready_long %>%
  select(ID, birthyear, age, age_group, behavior, readiness)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(when_ready_long)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
unique(when_ready_long$readiness)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
#Standardize the responses by trimming whitespace
when_ready_long <- when_ready_long %>%
  mutate(readiness = trimws(readiness))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
#Replace emoty space/ NA values in 'readiness' with "Not applicable for me"
when_ready_long <- when_ready_long %>%
  mutate(readiness = ifelse(readiness == "" | is.na(readiness), "Not applicable for me", readiness))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Convert the responses to numeric values
when_ready_long <- when_ready_long %>%
  mutate(readiness_numeric = case_when(
    readiness == "I am already ready or doing it" ~ 5,
    readiness == "In 1-3 months" ~ 4,
    readiness == "In 3-6 months" ~ 3,
    readiness == "In more than 6 months" ~ 2,
    readiness == "When a vaccine will be ready or the virus will have disappeared" ~ 1,
    readiness == "I don’t foresee a time when I will be ready to do it again" ~ 0,
    readiness == "Not applicable for me" ~ -1,
    TRUE ~ NA_real_  # Handles any unexpected values
  ))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(when_ready_long)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
unique(when_ready_long$readiness_numeric)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
options(repr.plot.width = 12, repr.plot.height = 8)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Stacked bar chart showing readiness across different behaviors and age groups
ggplot(when_ready_long, aes(x = behavior, fill = readiness)) +
  geom_bar(position = "fill") +
  facet_wrap(~ age_group) +
  labs(title = "Readiness to Resume Activities by Age Group",
       x = "Behavior",
       y = "Proportion of Responses",
       fill = "Readiness Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Save the plot
ggsave("readiness_by_age_group.png", width = 12, height = 8, dpi = 300)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
#Calculate average readiness score for each behavior and age group
average_readiness <- when_ready_long %>%
  group_by(age_group, behavior) %>%
  summarize(avg_score = round(mean(readiness_numeric, na.rm = TRUE), 2), .groups = "drop")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(average_readiness)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# create heatmap of average readiness scores
heatmap_plot <- ggplot(average_readiness, aes(x = behavior, y = age_group, fill = avg_score)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "red", name = "Avg Readiness Score") +
  labs(
    title = "Average Readiness to Resume Activities by Age Group",
    x = "Behavior",
    y = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels
    axis.text.y = element_text(size = 12),                         # Increase y-axis text size
    plot.title = element_text(size = 16, hjust = 0.5),             # Center and increase title size
    legend.text = element_text(size = 10),                         # Increase legend text size
    legend.title = element_text(size = 12)                         # Increase legend title size
  )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
heatmap_plot


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
ggsave("average_readiness_heatmap.png", plot = heatmap_plot, width = 10, height = 6, dpi = 300)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(data, n = c(1, 35))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Add a unique ID column to the dataset
data <- data %>%
  mutate(ID = row_number())


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
#Extract and Clean Familiarity Data
familiarity_data <- data %>%
  select(ID, starts_with("news_canada_")) %>%
  pivot_longer(
    cols = starts_with("news_canada_"),
    names_to = "familiarity_source",
    values_to = "source_name"
  ) %>%
  filter(!is.na(source_name) & source_name != "0" & source_name != "")  # Keep only recognized sources


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(familiarity_data)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
#Extract and Clean Trust Data
trust_data <- data %>%
  select(ID, starts_with("news_trust_")) %>%
  pivot_longer(
    cols = starts_with("news_trust_"),
    names_to = "trust_source",
    values_to = "trust_value"
  ) %>%
  filter(!is.na(trust_value) & trust_value != "0" & trust_value != "")  # Keep only valid trust responses


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(trust_data, n=12)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
familiarity_columns <- colnames(data)[4:30]
trust_columns <- colnames(data)[32:60]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(familiarity_columns, n=28)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(trust_columns, n=28)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create a manual mapping based on the first row inspection
mapping <- data.frame(
  familiarity_source = familiarity_columns,
  trust_source = trust_columns[1:length(familiarity_columns)]
)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
print(mapping)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Merge familiarity data with trust data using the ID column and the mapping
combined_data <- familiarity_data %>%
  inner_join(mapping, by = "familiarity_source") %>%
  inner_join(trust_data, by = c("ID", "trust_source"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
str(combined_data)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(combined_data, n= 22)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
unique(trust_data$trust_value)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Summarize the data to count the number of "Yes", "No", and "Not sure" responses for each media source
summary_data <- combined_data %>%
  group_by(source_name) %>%
  summarize(
    familiarity_count = n(),  # Count of recognitions
    trust_yes_count = sum(trust_value == "Yes", na.rm = TRUE),  # Count of "Yes" responses
    trust_no_count = sum(trust_value == "No", na.rm = TRUE),    # Count of "No" responses
    trust_not_sure_count = sum(trust_value == "Not sure", na.rm = TRUE)  # Count of "Not sure" responses
  ) %>%
  arrange(desc(familiarity_count))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
str(summary_data)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
summary_data


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Reshape data for plotting
trust_plot_data <- summary_data %>%
  pivot_longer(cols = starts_with("trust_"), names_to = "trust_level", values_to = "count")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot the data
ggplot(trust_plot_data, aes(x = reorder(source_name, -familiarity_count), y = count, fill = trust_level)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Trust Levels for Media Sources Among Canadians",
    x = "Media Source",
    y = "Count of Trust Responses"
  ) +
  scale_fill_manual(values = c("trust_yes_count" = "green", "trust_no_count" = "red", "trust_not_sure_count" = "orange"),
                    name = "Trust Level",
                    labels = c("No", "Not Sure", "Yes")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
ggsave(filename = "trust_levels_for_media_sources_among_canadians.png", width = 10, height = 6, dpi = 300)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
birthyear_data <- data %>%
  select(ID, birthyear)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
combined_with_birthyear <- combined_data %>%
  inner_join(birthyear_data, by = "ID")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(combined_with_birthyear)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
age_summary <- combined_with_birthyear %>%
  group_by(birthyear, source_name) %>%
  summarize(
    familiarity_count = n(),
    trust_yes_count = sum(trust_value == "Yes", na.rm = TRUE),
    trust_no_count = sum(trust_value == "No", na.rm = TRUE),
    trust_not_sure_count = sum(trust_value == "Not sure", na.rm = TRUE),
    .groups = 'drop'  # This removes the grouping after summarizing
  ) %>%
  arrange(desc(familiarity_count))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(age_summary)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
age_group_summary <- combined_with_birthyear %>%
  mutate(
    age = 2024 - birthyear,  # Calculate age assuming the current year is 2024
    age_group = case_when(
      age >= 18 & age <= 34 ~ "18-34 years",
      age >= 35 & age <= 49 ~ "35-49 years",
      age >= 50 & age <= 64 ~ "50-64 years",
      age >= 65 ~ "65+ years",
      TRUE ~ "Unknown"  # Handles missing or incorrect values
    )
  ) %>%
  group_by(age_group, source_name) %>%
  summarize(
    familiarity_count = n(),
    trust_yes_count = sum(trust_value == "Yes", na.rm = TRUE),
    trust_no_count = sum(trust_value == "No", na.rm = TRUE),
    trust_not_sure_count = sum(trust_value == "Not sure", na.rm = TRUE),
    .groups = 'drop'  # To remove grouping after summarizing
  )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(age_group_summary)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Reshape data for plotting with grouped bars
trust_plot_data_age_group <- age_group_summary %>%
  pivot_longer(cols = starts_with("trust_"), names_to = "trust_level", values_to = "count")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot grouped bar plot with age groups
ggplot(trust_plot_data_age_group, aes(x = reorder(source_name, -familiarity_count), y = count, fill = trust_level)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ age_group) +  # Separate plots for each age group
  labs(
    title = "Trust Levels for Media Sources by Age Group",
    x = "Media Source",
    y = "Count of Trust Responses"
  ) +
  scale_fill_manual(values = c("trust_yes_count" = "green", "trust_no_count" = "red", "trust_not_sure_count" = "orange"),
                    name = "Trust Level",
                    labels = c("No", "Not sure", "Yes")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
    )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Save the last displayed plot directly
ggsave(filename = "trust_levels_by_age_group.png", width = 10, height = 6, dpi = 300)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
gender_data <- data %>%
  select(ID, gender)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
combined_with_gender <- combined_data %>%
  inner_join(gender_data, by = "ID")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(combined_with_gender)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
gender_summary <- combined_with_gender %>%
  group_by(gender, source_name) %>%
  summarize(
    familiarity_count = n(),
    trust_yes_count = sum(trust_value == "Yes", na.rm = TRUE),
    trust_no_count = sum(trust_value == "No", na.rm = TRUE),
    trust_not_sure_count = sum(trust_value == "Not sure", na.rm = TRUE),
    .groups = 'drop'  # To remove grouping after summarizing
  )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(gender_summary)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Reshape data to a long format for plotting
trust_plot_data_gender <- gender_summary %>%
  pivot_longer(
    cols = starts_with("trust_"),
    names_to = "trust_level",
    values_to = "count"
  )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(trust_plot_data_gender)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot grouped bar plot with gender
ggplot(trust_plot_data_gender, aes(x = reorder(source_name, -familiarity_count), y = count, fill = trust_level)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ gender) +  # Separate plots for each gender
  labs(
    title = "Trust Levels for Media Sources by Gender",
    x = "Media Source",
    y = "Count of Trust Responses"
  ) +
  scale_fill_manual(values = c("trust_yes_count" = "green", "trust_no_count" = "red", "trust_not_sure_count" = "orange"),
                    name = "Trust Level",
                    labels = c("No", "Not sure", "Yes")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)  )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Save the last displayed plot directly
ggsave(filename = "trust_levels_by_gender.png", width = 10, height = 6, dpi = 300)

