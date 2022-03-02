librarian::shelf(tidyverse, readxl)

claims <- read_csv('data/AdvStateUnempFundsTitleXII_20200301_20220228.csv') %>% janitor::clean_names() %>%
  rename(date = record_date) %>%
  mutate(date = lubridate::as_date(date, format = '%m/%d/%y')) %>%
  select(date, calendar_year, state_name, outstanding_advance_balance, interest_paid_amount, interest_rate_percent) %>%
  arrange(date)

claims %>%
  filter(outstanding_advance_balance > 0,
         date > '2021-09-06') %>%
  count(state_name)


claims_annual <-
  claims %>%
  group_by(calendar_year, state_name) %>%
  summarise(across(c(outstanding_advance_balance, interest_paid_amount),
                   ~ sum(.x) / 1e9),
            .groups = 'drop')

claims_annual %>%
  filter(outstanding_advance_balance > 0) %>%
  ggplot(aes(x = state_name, y = outstanding_advance_balance)) +
  geom_col() +
  coord_flip() +
  facet_wrap(. ~ calendar_year)



