
library(tidyverse)
library(tidymodels)


covid_url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
pop_url   <- 'https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/co-est2023-alldata.csv'


covid_data <- readr::read_csv(covid_url)
census_data <- readr::read_csv(pop_url)


covid_latest <- covid_data %>%
  group_by(state) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(state, cases, deaths)

census_clean <- census_data %>%
  select(STNAME, POPESTIMATE2023) %>%
  group_by(STNAME) %>%
  summarise(population = sum(POPESTIMATE2023)) %>%
  rename(state = STNAME)

covid_joined <- covid_latest %>%
  inner_join(census_clean, by = "state") %>%
  mutate(
    cases_per_100k = cases / population * 100000,
    deaths_per_100k = deaths / population * 100000
  )

covid_model_data <- covid_joined %>%
  select(deaths, cases_per_100k, population) %>%
  mutate(
    log_population = log(population)
  )

set.seed(123)
data_split <- initial_split(covid_model_data, prop = 0.8)
train_data <- training(data_split)
test_data  <- testing(data_split)

covid_recipe <- recipe(deaths ~ ., data = train_data) %>%
  step_normalize(all_predictors())

lm_model <- linear_reg() %>%
  set_engine("lm")

covid_workflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(covid_recipe)

lm_fit <- covid_workflow %>%
  fit(data = train_data)

predictions <- predict(lm_fit, new_data = test_data) %>%
  bind_cols(test_data %>% select(deaths))


predictions %>%
  metrics(truth = deaths, estimate = .pred)

ggplot(predictions, aes(x = deaths, y = .pred)) +
  geom_point(color = "steelblue", size = 3) +
  geom_abline(linetype = "dashed", color = "red") +
  labs(
    title = "Predicted vs Truth COVID-19 Deaths by State",
    x = "Actual Deaths",
    y = "Predicted Deaths"
  ) +
  theme_minimal()

