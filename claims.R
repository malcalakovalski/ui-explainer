# Setup -------------------------------------------------------------------

librarian::shelf(tidyverse, ggbrookings, lubridate)
theme_set(theme_brookings())
update_geom_defaults('line', list(size = 1.5))
claims_raw <- readxl::read_xlsx('data/ui_claims.xlsx',
                            range = "A5:J97")


# Data prep ---------------------------------------------------------------


claims <-
  claims_raw %>%
  mutate(date = as_date(date)) %>%
  mutate(other = extended_benefits + stc_workshare + regular_federal_employees + regular_veterans) %>%
  select(date, total, regular_state, pua, peuc, other) %>%
  pivot_longer(-c(date, total)) %>%
  mutate(name = as_factor(name)) %>%
  mutate(name = fct_relevel(name, c('regular_state', 'pua', 'peuc', 'other')))


# Chart -------------------------------------------------------------------
program_labels <- c("Regular State UI", "PUA", "PEUC", "Other programs (mostly STC and EB)")

claims %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_color_brookings(labels = program_labels) +
  scale_x_date(date_labels = "%b %y'",
               date_breaks = "2 months") +
  scale_y_continuous(labels = scales::label_comma(scale = 1 / 1e6, accuracy = 1, suffix = "M"),
                     breaks = scales::pretty_breaks()) +
  labs(title = "Continuing Unemployment Claims in All Programs",
       x = NULL,
       y = NULL,
       caption = "**Source:** Department of Labor")

ggsave(
  here::here('figures', 'claims.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  device = ragg::agg_png()
)
