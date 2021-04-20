

# LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggrepel)

# SCRIPT ------------------------------------------------------------------

minnesota_data <-
  read_csv("https://covidtracking.com/data/download/minnesota-history.csv") %>%
  mutate(population = 5639632)

northdakota_data <-
  read_csv("https://covidtracking.com/data/download/north-dakota-history.csv") %>%
  mutate(population = 762062)

combined_data <-
  bind_rows(minnesota_data, northdakota_data) %>%
  mutate(
    negativity_rate = negative / population * 1000 ,
    positivity_rate = positive / population * 1000 
  )

dates <-
  tribble(
    ~ state,
    ~ date,
    ~ label,
    "ND",
    ymd(20201114),
    "ND Mask Mandate",
    "MN",
    ymd(20200725),
    "MN Mask Mandate",
    "ND",
    ymd(20210118),
    "ND Lifts\nMask Mandate"
  ) %>%
  left_join(combined_data)

# DATA ------------------------------------------------------------------

citation("tidyverse")

ggplot(data = combined_data) +
  geom_histogram(
    mapping = aes(x = negativity_rate),
    bins = 15,
    fill = "pink",
    color = "black"
  ) +
  labs(x = "Cumulative Number of Negative Cases Rate Per 1000 People", y = "Frequency (Number Of Days)") +
  facet_wrap(~ state)

ggplot(data = combined_data) +
  geom_histogram(
    mapping = aes(x = positivity_rate),
    bins = 15,
    fill = "pink",
    color = "black"
  ) +
  labs(x = "Cumulative Number of Positive Cases Per 1000 People", y = "Frequency (Number of Days)") +
  facet_wrap(~ state)


ggplot(data = combined_data,
       mapping = aes(x = date, y = negativity_rate, color = state)) +
  geom_line(size = 1) +
  labs(y = "Cumulative Number of Negative Cases Rate Per 1000 People", x = "Date", color = "State") +
  geom_point(data = dates, size = 3.5) +
  geom_text_repel(
    mapping = aes(label = label),
    data = dates,
    color = "black",
    size = 3
  )

ggplot(data = combined_data,
       mapping = aes(x = date, y = positivity_rate, color = state)) +
  geom_line(size = 1) +
  labs(y = "Cumulative Number of Positive Cases Per 1000 People", x = "Date", color = "State") +
  geom_point(data = dates, size = 3.5) +
  geom_text_repel(
    mapping = aes(label = label),
    data = dates,
    color = "black",
    size = 3
  )

