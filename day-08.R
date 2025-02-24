#Stephanie Farmer
#02/23/2025
#Exercise 08

library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)
head(covid, 5)
df <- data.frame(
  region = state.region,
  state_abb = state.abb,
  state_name = state.name)
head(df)
covid_state_data <- covid %>%
  left_join(df, by = c("state" = "state_name"))
head(covid_state_data)

covid_region_data <- covid_state_data %>%
  group_by(date, region) %>%
  summarize(
    total_cases = sum(cases, na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE)
  ) %>%
  ungroup()
head(covid_region_data)

covid_long <- covid_region_data %>%
  pivot_longer(cols = c(total_cases, total_deaths),
               names_to = "metric",
               values_to = "count")


head(covid_long)


plot <- ggplot(covid_long, aes(x = date, y = count, color = metric)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Cumulative COVID-19 Cases & Deaths by USA Region",
    x = "Date",
    y = "Count",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )


print(plot)

