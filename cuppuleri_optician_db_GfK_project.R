# Load necessary libraries and data --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(sf)
library(readxl)


# Load the data
data <- read_excel("/Users/adrianacuppuleri/Library/Mobile Documents/com~apple~CloudDocs/Bewerbung/GfK/optician_basedata.xlsx")


# Data Exploration --------------------------------------------------------------------------------------------------------------------------------------------------------------



# Data summary
head(data)
dim(data)

str(data)

# Convert all column names to lowercase
colnames(data) <- tolower(colnames(data))
# Display the updated column names to confirm the change
colnames(data)
# Check for na
colSums(is.na(data))


# Check the distribution of chain vs. non-chain stores
table(data$natchain_y)
table(is.na(data$natchain_y))
# Replace NA values with "n" for non-chain stores
data$natchain_y[is.na(data$natchain_y)] <- "n"
table(data$natchain_y)

# sc_flag
table(data$sc_flag)

#convert str into num value
data <- data %>%
  mutate(natchain_y = ifelse(natchain_y == "y", 1, 0),
         sc_flag = ifelse(sc_flag == "yes", 1, 0))

table(data$sc_flag)
table(data$natchain_y)

# Select relevant columns for descriptive statistics that are not geodata
relevant_columns <- data[, c("id", "distribution_area", "einwohner", "haushalte", "kk_summe", "kk_ew", "kk_ew_index", "natchain_y", "old", "opt_cell", "opt_focal", "poi_cell", "poi_focal","sc_flag", "young")]

summary(relevant_columns)


# Select distribution_area and see their distribution
table(relevant_columns$distribution_area)


## Plot Distribution --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot for sc_flag
barplot(table(relevant_columns$sc_flag),
        col = "lightblue",
        main = "Stores Near Shopping Centers (sc_flag)",
        xlab = "Proximity to Shopping Center (no/yes)",
        ylab = "Count")

# Plot for natchain_y
barplot(table(relevant_columns$natchain_y),
        col = "lightgreen",
        main = "Nationwide Chain Stores (natchain_y)",
        xlab = "Chain Store (n/y)",
        ylab = "Count")

#Plot for distribution_area
barplot(table(relevant_columns$distribution_area),
        col = "red",
        main = "distribution area of stores",
        xlab = "Store (north-east, south, west)",
        ylab = "Count")

# most of stores are 
# - part of a chain 
# - they are not located near shopping centers
# - South is the area with highest distribution 


# Create an interactive map of distribution_area with x and y coordinates

# Define a color palette for the distribution_area
palette <- colorFactor(palette = c("red", "lightgreen", "blue"), 
                       domain = data$distribution_area)

# Create the interactive map
leaflet(data) %>%
  addTiles() %>%  # Add default map tiles
  addCircleMarkers(~x, ~y, popup = ~gc_ort, color = ~palette(distribution_area), 
                   radius = 4, fillOpacity = 0.8) %>%  # Plot locations with popups
  addLegend("bottomright", pal = palette,
            values = data$distribution_area, title = "Distribution Area of Optician Stores")

# old population percentage
ggplot(relevant_columns, aes(x = old)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Older Population Share", x = "Old Population Share (%)", y = "Density")

#  young population percentage
ggplot(relevant_columns, aes(x = young)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Density Plot of Younger Population Share", x = "Young Population Share (%)", y = "Density")

# Density plot for total purchasing power (kk_summe)
ggplot(relevant_columns, aes(x = kk_summe)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(title = "Density Plot of Total Purchasing Power (kk_summe)", 
       x = "Total Purchasing Power", 
       y = "Density")

# Density plot for purchasing power per inhabitant (kk_ew)
ggplot(relevant_columns, aes(x = kk_ew)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(title = "Density Plot of Purchasing Power per Inhabitant (kk_ew)", 
       x = "Purchasing Power per Inhabitant", 
       y = "Density")


#  competing opticians in the immediate area (opt_cell)
ggplot(relevant_columns, aes(x = opt_cell)) +
  geom_bar(fill = "red") +
  labs(title = "Competition in Immediate Area (opt_cell)", x = "Competing Opticians in 250m", y = "Count")

# competing opticians in neighboring areas (opt_focal)
ggplot(relevant_columns, aes(x = opt_focal)) +
  geom_bar(fill = "darkred") +
  labs(title = "Competition in Neighboring Areas (opt_focal)", x = "Competing Opticians in Surrounding Areas", y = "Count")

# Plot the distribution of POIs in the immediate area (poi_cell)
ggplot(relevant_columns, aes(x = poi_cell)) +
  geom_histogram(binwidth = 5, fill = "cyan", color = "black") +
  labs(title = "Points of Interest in Immediate Area (poi_cell)", x = "POIs in 250m Grid", y = "Count")

# Plot the distribution of POIs in neighboring areas (poi_focal)
ggplot(relevant_columns, aes(x = poi_focal)) +
  geom_histogram(binwidth = 10, fill = "darkcyan", color = "black") +
  labs(title = "Points of Interest in Surrounding Areas (poi_focal)", x = "POIs in Neighboring Grids", y = "Count")



# Data Preparation & Selection Criteria for 180 stores --------------------------------------------------------------------------------------------------------------------------------------------------------------

# Demographics: Target locations with a higher concentration of older adults (old > young).
# Purchasing Power: Prioritize locations with higher purchasing power (kk_summe and kk_ew).
# Competition: Minimize stores in areas with high competition (opt_cell and opt_focal).
# Proximity to Points of Interest: Focus on stores that are closer to shopping centers or have a high density of POIs (poi_cell, poi_focal, and sc_flag).
# Selling power: is the store part of a chain (natchain_y)? balance
# Regional Balance: Ensure an equal distribution of stores across the regions (distribution_area).

summary(relevant_columns)


## Creating a ranking score + Min-Max normalization --------------------------------------------------------------------------------------------------------------------------------------------------------------

# min_max
min_max_norm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

relevant_columns_score <- relevant_columns %>%
  mutate(
    kk_summe_scaled = min_max_norm(kk_summe),
    kk_ew_scaled = min_max_norm(kk_ew),
    opt_cell_scaled = min_max_norm(opt_cell),
    opt_focal_scaled = min_max_norm(opt_focal),
    poi_cell_scaled = min_max_norm(poi_cell),
    poi_focal_scaled = min_max_norm(poi_focal),
    sc_flag_scaled = sc_flag * 5  # Keep the shopping center flag weighted more heavily
  ) %>%
  mutate(score = round((old / (young + 1)) + 
           kk_summe_scaled + 
           kk_ew_scaled + 
           (opt_cell_scaled * -2) +   # Use the scaled competition values
           (opt_focal_scaled * -2) + 
           poi_cell_scaled + poi_focal_scaled +
           sc_flag_scaled + 
           ifelse(natchain_y == 1, 5, 2), 2))
    
summary(relevant_columns_score$score)

# Inspect how the scores are distributed
hist(relevant_columns_score$score, breaks = 20, col = "lightblue", 
     main = "Distribution of Scores", xlab = "Score", ylab = "Frequency")

## without penalty for the competition

relevant_columns_score1 <- relevant_columns %>%
  mutate(
    kk_summe_scaled = min_max_norm(kk_summe),
    kk_ew_scaled = min_max_norm(kk_ew),
    opt_cell_scaled = min_max_norm(opt_cell),
    opt_focal_scaled = min_max_norm(opt_focal),
    poi_cell_scaled = min_max_norm(poi_cell),
    poi_focal_scaled = min_max_norm(poi_focal),
    sc_flag_scaled = sc_flag * 5  # Keep the shopping center flag weighted more heavily
  ) %>%
  mutate(score1 = round((old / (young + 1)) + 
                          kk_summe_scaled + 
                          kk_ew_scaled + 
                          opt_cell_scaled * +   # Use the scaled competition values
                          opt_focal_scaled + 
                          poi_cell_scaled + poi_focal_scaled +
                          sc_flag_scaled + 
                          ifelse(natchain_y == 1, 5, 2), 2))

summary(relevant_columns_score1$score1)

## how competition affects the score (opt_cell and opt_focal) --------------------------------------------------------------------------------------------------------------------------------------------------------------

# visualizing relationship between relevant columns and score

# Scatter plot for competition in immediate area (opt_cell) vs. score
ggplot(relevant_columns_score, aes(x = opt_cell, y = score)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add trend line
  labs(title = "Competition in Immediate Area (opt_cell) vs. Score", 
       x = "Number of Competing Opticians (opt_cell)", 
       y = "Score")

# Scatter plot for competition in neighboring areas (opt_focal) vs. score
ggplot(relevant_columns_score, aes(x = opt_focal, y = score)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add trend line
  labs(title = "Competition in Neighboring Areas (opt_focal) vs. Score", 
       x = "Number of Competing Opticians (opt_focal)", 
       y = "Score")

# Correlation between opt_cell (immediate competition) and score
cor_opt_cell <- cor(relevant_columns_score$opt_cell, relevant_columns_score$score)
print(cor_opt_cell)

# Correlation between opt_focal (neighboring competition) and score
cor_opt_focal <- cor(relevant_columns_score$opt_focal, relevant_columns_score$score)
print(cor_opt_focal)


## how pursuing power and proximity to shopping centers affects the score (kk_summe and kk_ew, sc_flag) --------------------------------------------------------------------------------------------------------------------------------------------------------------

# Scatter plot for purchasing power (kk_summe) vs. score
ggplot(relevant_columns_score, aes(x = kk_summe_scaled, y = score)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Purchasing Power (kk_summe) vs. Score", x = "Scaled kk_summe", y = "Score")

# Scatter plot for purchasing power per inhabitant (kk_ew) vs. score
ggplot(relevant_columns_score, aes(x = kk_ew_scaled, y = score)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Purchasing Power per Inhabitant (kk_ew) vs. Score", x = "Scaled kk_ew", y = "Score")  
  
# Scatter plot for proximity to shopping centers (sc_flag_scaled) vs. score
ggplot(relevant_columns_score, aes(x = sc_flag_scaled, y = score)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Proximity to Shopping Centers (sc_flag_scaled) vs. Score", x = "Scaled sc_flag", y = "Score")

## model 0: multiple linear regression --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Multiple linear regression to assess the contribution of each factor to the score
mlr <- lm(score ~ kk_summe_scaled + kk_ew_scaled + opt_cell_scaled + opt_focal_scaled + 
                  poi_cell_scaled + poi_focal_scaled + sc_flag_scaled, 
            data = relevant_columns_score)

# Summary of the regression model
summary(mlr)
# Multiple R-squared:  0.6784,	Adjusted R-squared:  0.6776 


# Load the car package to calculate Variance Inflation Factor (multicollinearity)
library(carData)

#vif(mlr)

#visualize mlr
library(broom)
tidy_model <- tidy(mlr)

# Create a coefficient plot
ggplot(tidy_model, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  coord_flip() +  # Flip the axes for better readability
  labs(title = "Coefficient Plot of Multiple Linear Regression Model", x = "Variables", y = "Estimate") +
  theme_minimal()

## model 1: remove poi_focal_scaled (collinear variable) --------------------------------------------------------------------------------------------------------------------------------------------------------------

# Multiple linear regression to assess the contribution of each factor to the score
mlr1 <- lm(score ~ kk_summe_scaled + kk_ew_scaled + opt_cell_scaled + opt_focal_scaled + 
            poi_cell_scaled + sc_flag_scaled, 
          data = relevant_columns_score)

# Summary of the regression model
summary(mlr1)
# Multiple R-squared:  0.6778,	Adjusted R-squared:  0.6771 

## model 2: remove opt_focal_scaled (collinear variable) --------------------------------------------------------------------------------------------------------------------------------------------------------------

# Multiple linear regression to assess the contribution of each factor to the score
mlr2 <- lm(score ~ kk_summe_scaled + kk_ew_scaled + opt_cell_scaled + poi_focal_scaled + 
             poi_cell_scaled + sc_flag_scaled, 
           data = relevant_columns_score)

# Summary of the regression model
summary(mlr2)
# Multiple R-squared:  0.6756,	Adjusted R-squared:  0.6749

## model 3: Principal Component Analysis (PCA) on opt_focal_scaled and poi_focal_scaled) --------------------------------------------------------------------------------------------------------------------------------------------------------------

# Perform PCA on opt_focal_scaled and poi_focal_scaled
pca_result <- prcomp(relevant_columns_score[, c("opt_focal_scaled", "poi_focal_scaled")], scale. = TRUE)

# Add the first principal component to the dataset
relevant_columns_score$pca_focal <- pca_result$x[,1]

# Update the model with the new PCA component
model_pca <- lm(score ~ kk_summe_scaled + kk_ew_scaled + 
                  opt_cell_scaled + pca_focal + 
                  poi_cell_scaled + sc_flag_scaled, 
                data = relevant_columns_score)

# Summary of the model with the PCA component
summary(model_pca)
## Multiple R-squared:  0.6767,	Adjusted R-squared:  0.676 

## model 4: MLR with score1 (no penalties for competition) --------------------------------------------------------------------------------------------------------------------------------------------------------------

mlr_score1 <- lm(score1 ~ kk_summe_scaled + kk_ew_scaled + opt_cell_scaled + opt_focal_scaled + 
            poi_cell_scaled + poi_focal_scaled + sc_flag_scaled, 
          data = relevant_columns_score1)

# Summary of the regression model
summary(mlr_score1)


# Load necessary libraries
library(ggplot2)

# Coefficients from your model
coefficients <- data.frame(
  Variable = c("(Intercept)", "kk_summe_scaled", "kk_ew_scaled", "opt_cell_scaled", 
               "opt_focal_scaled", "poi_cell_scaled", "poi_focal_scaled", "sc_flag_scaled"),
  Estimate = c(3.56687, 0.85280, 0.34700, 1.16257, 0.53042, 2.39565, 0.99706, 1.16405)
)

# Create the coefficient plot
ggplot(coefficients, aes(x = Estimate, y = Variable)) +
  geom_point(size = 4) +
  geom_segment(aes(x = 0, xend = Estimate, y = Variable, yend = Variable)) +
  labs(title = "Coefficient Plot of MLR Model",
       x = "Estimate", y = "Variables") +
  theme_minimal()


## model 5: MLR only high kk_summe and kk_ew (25%) --------------------------------------------------------------------------------------------------------------------------------------------------------------

# Define a threshold for the top 25% of stores based on purchasing power per inhabitant (kk_ew)
threshold_kk_ew <- quantile(relevant_columns_score1$kk_ew, 0.75)

# Filter stores with purchasing power above the threshold
top_25_kk_stores <- relevant_columns_score1 %>%
  filter(kk_ew >= threshold_kk_ew)

#ensure top score are equally distributed among 3 areas
top_25_kk_stores <- relevant_columns_score1 %>%
  group_by(distribution_area) %>%         
  arrange(desc(score1)) %>%                
  slice_head(n = 60)

# Merge the location information (x, y coordinates) with the top stores
top_25_stores_full <- top_25_kk_stores %>%
  left_join(data[, c("id", "gc_strasse", "gc_hausnr", "gc_plz", "gc_ort", "x", "y")], by = "id")

# Create a popup for store information
top_25_stores_full <- top_25_stores_full %>%
  mutate(popup_info = paste0("Store: ", gc_ort, "<br>",
                             "Address: ", gc_strasse, " ", gc_hausnr, "<br>",
                             "Postcode: ", gc_plz, "<br>",
                             "Purchasing Power: ", kk_ew))

# Create a color palette based on the distribution area
palette <- colorFactor(palette = c("red", "green", "blue"), 
                       domain = top_25_stores_full$distribution_area)
# Create the map
leaflet(top_25_stores_full) %>%
  addTiles() %>%  # Add default map tiles
  addCircleMarkers(~x, ~y, popup = ~popup_info, color = ~palette(distribution_area), 
                   radius = 5, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = palette, 
            values = top_stores_full$distribution_area, 
            title = "Distribution Area of Top Stores")

# Rank stores based on the score and score1 --------------------------------------------------------------------------------------------------------------------------------------------------------------
#score
# Ensure equal distribution of 60 stores from each region (North-East, West, South)
top_stores <- relevant_columns_score %>%
  group_by(distribution_area) %>%         
  arrange(desc(score)) %>%                
  slice_head(n = 60)                      

table(top_stores$distribution_area)

# Print the top stores
head(top_stores)
order(top_stores$score, decreasing = TRUE)

#score1
# Ensure equal distribution of 60 stores from each region (North-East, West, South)
top_stores1 <- relevant_columns_score1 %>%
  group_by(distribution_area) %>%         
  arrange(desc(score1)) %>%                
  slice_head(n = 60)                      

table(top_stores1$distribution_area)

# Print the top stores
head(top_stores1)
order(top_stores1$score1, decreasing = TRUE)
## visualization of top stores --------------------------------------------------------------------------------------------------------------------------------------------------------------

#mjoin "id"                "gc_strasse"        "gc_hausnr"        "gc_plz"            "gc_ort"            "x"                "y" to relevant_columns_score
top_stores_full1 <- top_stores1 %>%
  left_join(data[, c("id", "gc_strasse", "gc_hausnr", "gc_plz", "gc_ort", "x", "y")], by = "id")



# Check the merged data
head(top_stores_full1)

# Create a color palette based on the distribution area
palette <- colorFactor(palette = c("red", "green", "blue"), 
                       domain = top_stores_full$distribution_area)

# Visualize the top stores with colors based on distribution area
leaflet(top_stores_full) %>%
  addTiles() %>%
  addCircleMarkers(~x, ~y, popup = ~gc_ort, color = ~palette(distribution_area), 
                   radius = 4, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = palette, 
            values = top_stores_full$distribution_area, 
            title = "Distribution Area of Top Stores")

# Verify the count of selected stores per region
table(top_stores_full$distribution_area)

# Combine the store details into a single string for the popup
top_stores_full$popup_info <- paste(
  "ID: ", top_stores_full$id, "<br>",
  "Street: ", top_stores_full$gc_strasse, " ", top_stores_full$gc_hausnr, "<br>",
  "Postcode: ", top_stores_full$gc_plz, "<br>",
  "Municipality: ", top_stores_full$gc_ort, "<br>",
  "Distribution Area: ", top_stores_full$distribution_area, "<br>"
)

### Map the top stores with detailed info in the popup --------------------------------------------------------------------------------------------------------------------------------------------------------------
leaflet(top_stores_full) %>%
  addTiles() %>%
  addCircleMarkers(~x, ~y, popup = ~popup_info, color = ~palette(distribution_area), 
                   radius = 4, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = palette, 
            values = top_stores_full$distribution_area, 
            title = "Distribution Area of Top Stores")

### plot the map with kk_summe and kk_ew --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a color palette based on kk_summe values (total purchasing power)
palette_kk_summe <- colorNumeric(palette = "YlOrRd", domain = top_stores_full$kk_summe)

# Visualize the stores with color-coded kk_summe values
leaflet(top_stores_full) %>%
  addTiles() %>%
  addCircleMarkers(~x, ~y, popup = ~popup_info, color = ~palette_kk_summe(kk_summe), 
                   radius = 4, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = palette_kk_summe, 
            values = top_stores_full$kk_summe, 
            title = "Total Purchasing Power (kk_summe)")

# Create a color palette based on kk_ew values (purchasing power per inhabitant)
palette_kk_ew <- colorNumeric(palette = "Blues", domain = top_stores_full$kk_ew)

# Visualize the stores with color-coded kk_ew values
leaflet(top_stores_full) %>%
  addTiles() %>%
  addCircleMarkers(~x, ~y, popup = ~popup_info, color = ~palette_kk_ew(kk_ew), 
                   radius = 4, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = palette_kk_ew, 
            values = top_stores_full$kk_ew, 
            title = "Purchasing Power per Inhabitant (kk_ew)")

### plot the map with competition opt_cell, opt_focal --------------------------------------------------------------------------------------------------------------------------------------------------------------
 
# Create a color palette based on opt_cell values (immediate competition)
palette_opt_cell <- colorNumeric(palette = "Reds", domain = top_stores_full$opt_cell)

# Visualize the stores with color-coded opt_cell values (immediate competition)
leaflet(top_stores_full) %>%
  addTiles() %>%
  addCircleMarkers(~x, ~y, popup = ~popup_info, color = ~palette_opt_cell(opt_cell), 
                   radius = 4, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = palette_opt_cell, 
            values = top_stores_full$opt_cell, 
            title = "Immediate Competition (opt_cell)")

# Create a color palette based on opt_focal values (neighboring competition)
palette_opt_focal <- colorNumeric(palette = "Purples", domain = top_stores_full$opt_focal)

# Visualize the stores with color-coded opt_focal values (neighboring competition)
leaflet(top_stores_full) %>%
  addTiles() %>%
  addCircleMarkers(~x, ~y, popup = ~popup_info, color = ~palette_opt_focal(opt_focal), 
                   radius = 4, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = palette_opt_focal, 
            values = top_stores_full$opt_focal, 
            title = "Neighboring Competition (opt_focal)")

### plot map distribution of older population --------------------------------------------------------------------------------------------------------------------------------------------------------------

# Create a color palette based on the 'old' variable (percentage of older population)
palette_old <- colorNumeric(palette = "Oranges", domain = top_stores_full$old)

# Visualize the stores with color-coded 'old' values (percentage of older population)
leaflet(top_stores_full) %>%
  addTiles() %>%
  addCircleMarkers(~x, ~y, popup = ~popup_info, color = ~palette_old(old), 
                   radius = 4, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = palette_old, 
            values = top_stores_full$old, 
            title = "Percentage of Older Population (old)")


### plot map with POI--------------------------------------------------------------------------------------------------------------------------------------------------------------

# Create a color palette based on the 'poi_cell' variable (POIs in the immediate area) with a pink color scale
palette_poi_pink <- colorNumeric(palette = "RdPu", domain = top_stores_full$poi_cell)

# Visualize the stores with color-coded 'poi_cell' values (POIs in the immediate area)
leaflet(top_stores_full) %>%
  addTiles() %>%
  addCircleMarkers(~x, ~y, popup = ~popup_info, color = ~palette_poi_pink(poi_cell), 
                   radius = 4, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = palette_poi_pink, 
            values = top_stores_full$poi_cell, 
            title = "Points of Interest (Immediate Area, poi_cell)")
