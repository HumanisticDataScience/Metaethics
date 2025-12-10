# This code is used to replicate analyses and figures appearing in the paper
# Rethinking the Foundations of Ethics in Information Systems Research

library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(gridExtra)
df <- read_csv("Metaethics_in_IS_public.csv")

kw_authors <- df |>
  select(Year, `Author Keywords`) |>
  filter(!is.na(`Author Keywords`)) |>
  mutate(
    # replace separators and split
    `Author Keywords` = str_replace_all(`Author Keywords`, "\\s*;\\s*", ";")
  ) |>
  separate_rows(`Author Keywords`, sep = ";") |>
  transmute(
    Year,
    keyword = `Author Keywords` |> str_trim() |> str_to_lower()
  ) |>
  filter(keyword != "") |>
  mutate(
    keyword = keyword |>
      str_replace_all("[[:punct:]]", " ") |>
      str_replace_all("[[:digit:]]", " ") |>
      str_squish(),
    # define decades explicitly
    Decade = case_when(
      Year >= 1986 & Year < 1996 ~ "1986–1996",
      Year >= 1996 & Year < 2006 ~ "1996–2006",
      Year >= 2006 & Year < 2016 ~ "2006–2016",
      Year >= 2016 & Year <= 2025 ~ "2016–2025",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(Decade))

# remove stopwords from counts of frequent keywords
custom_stop <- c(
  "patrick mikalef aleš popovic", "and","or","of","in","on","for","to","with","the","a","an",
  "research","study","methods","approach","approaches",
  "information","systems", "c c", "system","s", "is","it","its",  "ethical", "ethicality", "ethics"
)

kw_authors <- kw_authors |> filter(!keyword %in% custom_stop)

# count per decade
freq_by_decade <- kw_authors |>
  count(Decade, keyword, sort = TRUE) |>
  group_by(Decade) |>
  slice_max(n, n = 3, with_ties = FALSE) |>
  ungroup()

print(freq_by_decade)

#visualize
ggplot(freq_by_decade, aes(x = reorder(keyword, n), y = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~Decade, scales = "free_y") +
  labs(
    title = "Top 3 Author Keywords by Decade",
    x = "Keyword",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)


# create summary data
df_summary <- df %>%
  group_by(Year) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(cum_sum = cumsum(count))

# Scaling factor for secondary axis
scale_factor <- max(df_summary$cum_sum) / max(df_summary$count)

# Dual-axis plot 
dual_axis_plot <- ggplot(df_summary, aes(x = Year)) +
  # yearly counts (solid line)
  geom_line(aes(y = count), size = 0.9, linetype = "solid", color = "black") +
  geom_point(aes(y = count), color = "black", size = 1.8) +
  
  # cumulative counts 
  geom_line(aes(y = cum_sum / scale_factor), size = 0.9, linetype = "dashed", color = "red") +
  
  # axes and labels
  scale_y_continuous(
    name = "Articles per Year",
    sec.axis = sec_axis(~ . * scale_factor, name = "Cumulative Articles Published")
  ) +
  scale_x_continuous(breaks = seq(1980, 2025, by = 2)) +
  labs(
    title = "Ethics-oriented Publications Over Time in IS Basket of Eight",
    x = "Publication Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.y.left = element_text(size = 11),
    axis.title.y.right = element_text(size = 11),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

dual_axis_plot

# Ethical kernel theory percentages
df %>%
  group_by(`Ethics Theory Applied`) %>%
  summarize(ct = n()) %>%
  mutate(pct = 100 * ct / sum(ct)) %>%
  slice_max(order_by = ct, n = 10) %>%
  ggplot(aes(reorder(`Ethics Theory Applied`, ct), ct)) +
  geom_bar(stat = "identity", width = 0.5, fill = "gray40") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.1, size = 2.5) +
  coord_flip() +
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6)) +
  theme_minimal() +
  labs(
    x = "",
    y = "Number of Articles",
    title = "Article Counts by Ethical Theory"
  ) +
  ylim(0, max(df %>% count(`Ethics Theory Applied`) %>% pull(n)) * 1.15)

# metaethical positions
df %>%
  group_by(`\nMetaethical Position`) %>%
  summarize(ct = n()) %>%
  mutate(pct = 100 * ct / sum(ct)) %>%
  slice_max(order_by = ct, n = 10) %>%
  ggplot(aes(x = reorder(`\nMetaethical Position`, ct), y = ct)) +
  geom_bar(stat='identity') +  # bars fully occupy y-slots
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.1, size = 2.2, color = "black") +
  coord_flip(clip = "off") +
  scale_x_discrete(expand = c(0, 0)) +   # no top/bottom gap
  scale_y_continuous(expand = c(0, 0.03)) +
  theme_minimal(base_size = 8.5) +
  theme(
    axis.text.y = element_text(size = 5.8, margin = margin(r = 1)),
    axis.text.x = element_text(size = 5.8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 6, 0, 0),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 9),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 8)
  ) +
  labs(
    y = "Number of Articles",
    title = "Article Counts by Metaethical Position"
  ) +
  ylim(0, max(df %>% count(`\nMetaethical Position`) %>% pull(n)) * 1.1)