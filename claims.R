# Setup -------------------------------------------------------------------

librarian::shelf(tidyverse, ggbrookings, lubridate, showtext, ggtext)

claims_raw <- readxl::read_xlsx('data/ui_claims.xlsx',
                            range = "A5:J97")

## Fonts
font_add("roboto", regular = "Roboto-Regular.ttf",
         bold = "Roboto-Bold.ttf",
         italic = "Roboto-Italic.ttf",
         bolditalic = "Roboto-BoldItalic.ttf")

# This tells R what engine to render under (sort of). It's important this is inserted before any graphics rendering.
showtext_auto()
# This corrects sizing issues that occur when rendering.
showtext_opts(dpi = 300)

## Theme
theme_set(theme_brookings(base_family = 'roboto', web = TRUE))
update_geom_defaults('line', list(size = 1.5))

# Data prep ---------------------------------------------------------------


claims <-
  claims_raw %>%
  mutate(date = as_date(date)) %>%
  mutate(other = extended_benefits + stc_workshare + regular_federal_employees + regular_veterans) %>%
  select(date, total, regular_state, pua, peuc, other) %>%
  pivot_longer(-c(date, total)) %>%
  mutate(name = as_factor(name)) %>%
  mutate(name = fct_relevel(name, rev(c('regular_state', 'peuc', 'pua', 'other'))))


# Chart -------------------------------------------------------------------
program_labels <- (rev(c("Regular State UI", "PEUC<sup>†</sup>", "PUA<sup>*</sup>",  "Other")))

claims %>%
  ggplot(aes(x = date, y = value, fill = name)) +
  geom_col() +
  scale_fill_brookings(labels = program_labels,
                       palette = 'categorical_expanded',
                       reverse = TRUE) +
  scale_x_date(date_labels = "%b %y'",
               date_breaks = "2 months",
               expand = expansion(0, 20)) +
  scale_y_continuous(labels = scales::label_comma(scale = 1 / 1e6, accuracy = 1, suffix = "M"),
                     limits = c(0, 35 * 1e6),
                     breaks = scales::pretty_breaks(8)) +
  labs(title = "Continuing Unemployment Claims in All Programs",
       x = NULL,
       y = NULL,
       caption = "**Source:** Department of Labor<br>
       <sup>†</sup>Pandemic Emergency Unemployment Compensation (extended weeks of benefits);<br><sup>*</sup>Pandemic Unemployment Assistance (expanded eligibility)") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.background = element_rect(fill = "#FAFAFA"),
        legend.text = element_markdown()) +
  geom_vline(xintercept = as_date('2021-09-06'),
             linetype = 'dashed') +
  # annotate(geom = 'segment',
  #          arrow = arrow(),
  #          x = as_date("2021-07-06"),
  #          xend = as_date("2021-09-01"),
  #          y = 25 * 1e6,
  #          yend = 15 * 1e6) +
  annotate(geom = "text",
           label = "Expanded benefits end",
           y = 20 * 1e6,
           x = as_date("2021-06-30"),
           family = 'Roboto')


ggsave(
  here::here('figures', 'claims.png'),
  width = 90 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  bg = '#FAFAFA',
  type = 'cairo',
  device = ragg::agg_png()
)


# Logo --------------------------------------------------------------------

chart_wlogo <- add_logo('figures/claims.png',
                        'hc',
                        height_padding = 0.02)
magick::image_write(chart_wlogo, 'figures/claims_wlogo.png')
