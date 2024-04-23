# Load necessary libraries
library('ggplot2')
library('dplyr')
library('tidyr')
library('hrbrthemes')
library('dygraphs')
library('xts')          # To make the convertion data-frame / xts format
library('tidyverse')
library('lubridate')

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

# Calculating the count of artists by gender and genre
genre_counts <- artists_data %>%
  group_by(gender, genre) %>%
  summarise(Artist_Count = n(), .groups = 'drop')  # Replace n() with count(distinct(artist)) if artists are not unique

# Calculating combined counts across genders for each genre
combined_genre_counts <- genre_counts %>%
  group_by(genre) %>%
  summarise(Total_Artist_Count = sum(Artist_Count), .groups = 'drop')

# Identify the top 5 genres based on combined counts
top_genres_combined <- combined_genre_counts %>%
  top_n(5, Total_Artist_Count) %>%
  pull(genre)  # Get the list of top 5 genres

# Filter original data to include only the top 5 genres
top_genre_data <- genre_counts %>%
  filter(genre %in% top_genres_combined)


# Creating separate plots for Male and Female using facet_wrap
ggplot(top_genre_data, aes(x = genre, y = Artist_Count, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~gender, scales = "free_y") +  # Each gender gets a separate plot
  labs(title = "Top 5 Genres by Combined Artist Counts",
       x = "Genre",
       y = "Artist Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "#ADD8E6", "Female" = "#FFC0CB")) +  # Light blue and light pink
  coord_flip() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none")  # Removing the legend as colors are obvious

# Save the plot
ggsave("genre_plot", plot = p, width = 5, height = 5, dpi = 300)