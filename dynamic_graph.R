# Load necessary libraries
library('ggplot2')
library('dplyr')
library('tidyr')
library('hrbrthemes')
library('dygraphs')
library('xts')          # To make the convertion data-frame / xts format
library('tidyverse')
library('lubridate')
library('lintr')
library('gganimate')
library('babynames')
library('hrbrthemes')
library('ggthemes')
library('av')
library('gifski')

artists_data0 <- read.csv("C:\\Users\\user\\OneDrive\\Desktop\\ENGPRJ\\songs_normalize.csv")

# Preprocessing: Standardize gender capitalization
artists_data <- artists_data0 %>%
  filter(year != 1998 ) %>%  # Remove entries from the year 1998
  filter(year != 1999 ) %>%  # Remove entries from the year 1999
  filter(year != 2020 ) %>%  # Remove entries from the year 2020
  mutate(gender = case_when(
    tolower(gender) == "male" ~ "Male",
    tolower(gender) == "female" ~ "Female",
    TRUE ~ as.character(gender) # Keeps original value if not "male" or "female"
  ))

# Assuming 'popularity' is a column that determines the popularity of the artist
# Filtering to include only the top 10 male and female artists per year based on popularity
top_artists_per_year <- artists_data %>%
  group_by(year, gender) %>%
  filter(gender %in% c("Male", "Female")) %>%  # Ensure only Male and Female are considered
  arrange(desc(popularity)) %>%  # Arrange in descending order of popularity
  slice_head(n = 10) %>%  # Take the top 10 entries for each group
  ungroup()  # Optionally remove grouping

# Aggregate popularity by gender and year
gender_popularity_per_year <- top_artists_per_year %>%
  filter(gender %in% c("Male", "Female")) %>%  # Ensure only Male and Female data is considered
  group_by(year, gender) %>%
  summarise(Total_Popularity = sum(popularity), .groups = 'drop')  # Sum popularity for each gender per year

# Assuming gender_popularity_per_year is already prepared with the required data
# Plotting the accumulated popularity of Male and Female artists over time
plot <- gender_popularity_per_year %>%
  ggplot(aes(x = year, y = Total_Popularity, group = gender, color = gender)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Male" = "#4393c3", "Female" = "#d6604d")) +  # Custom colors for each gender
  labs(title = "Accumulated Popularity Trends by Gender Over Time",
       x = "Year",
       y = "Total Popularity",
       color = "Gender") +
  theme_ipsum() +  # Clean and professional theme
  theme(plot.title = element_text(hjust = 0.5),  # Center plot title
        axis.title.x = element_text(hjust = 0.5),  # Center x-axis title
        axis.title.y = element_text(hjust = 0.5)) +  # Center y-axis title
  transition_reveal(year)  # Animate changes over years

# Render the plot
animate(plot, renderer = gifski_renderer(), width = 800, height = 600, fps = 10)