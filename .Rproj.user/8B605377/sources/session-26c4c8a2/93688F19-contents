#Stephanie Farmer
#date: 02/18/2025
#Daily exercise 7

#Question 1
library(tidyverse)
url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid <- read_csv(url)

top_states <- covid %>%
  filter(date == max(date)) %>%
  group_by(state) %>% mutate(cumulative_cases = cumsum(cases)) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE)) %>%
  arrange(desc(total_cases)) %>%
  slice_head(n = 6) %>%
  pull(state)

print(top_states)

covid_filtered <- covid %>%
  filter(state %in% top_states) %>%
  group_by(state, date) %>%
  summarize(totCase = sum(cases))


p <- ggplot(covid_filtered, aes(x = date, y = totCase, group = state, color = state)) +
  geom_line() +
  facet_wrap(~state, scales = "free_y") +
  labs(
    title = "COVID-19 Cases Over Time in the 6 Most Affected States",
    x = "Date",
    y = "Cases",
    caption = "Source: NY Times COVID-19 Data"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

p
ggsave("img/covid_cases_top6_states.png", plot = p, width = 10, height = 6, dpi = 300)

#Question 2
library(tidyverse)
url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid <- read_csv(url)

daily_cases <- covid %>%
  group_by(date) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE))

p <- ggplot(daily_cases, aes(x = date, y = total_cases)) +
  geom_col(fill = "steelblue") +
  labs(title = "Daily Total COVID-19 Cases in the USA",
       x = "Date",
       y = "Total Cases") +
  theme_minimal()
p
ggsave("img/daily_cases_plot.png", plot = p, width = 8, height = 5, dpi = 300)
