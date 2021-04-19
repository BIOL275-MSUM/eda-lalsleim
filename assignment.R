
# LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(lubridate)

# SCRIPT ------------------------------------------------------------------

minnesota_data <-
  read_csv("https://covidtracking.com/data/download/minnesota-history.csv")

northdakota_data <-
  read_csv("https://covidtracking.com/data/download/north-dakota-history.csv")

bind_rows(minnesota_data,northdakota_data)

# STATISTICAL VALUES  ------------------------------------------------------------------

northdakota_summarised<-
summarise(
  northdakota_data,
  sample_size = n(),
  mean = mean(negative),
  sd = sd(negative),
  var = var(negative),
  stderr = sd / sqrt(negative),
  ci_upper = mean + 2 * stderr,
  ci_lower = mean - 2 * stderr
)

minnesota_summarised<-
summarise(
  minnesota_data,
  sample_size = n(),
  mean = mean(negative),
  sd = sd(negative),
  var = var(negative),
  stderr = sd / sqrt(negative),
  ci_upper = mean + 2 * stderr,
  ci_lower = mean - 2 * stderr
)

# DATA ------------------------------------------------------------------

citation("tidyverse")

ggplot(data = minnesota_data) +
  geom_histogram(mapping = aes(x = negative), bin = 15,
                 fill = "pink", color = "black") +
  labs(x = "Negative Tests", y = "Frequency (Daily Count)") 

ggplot(data = northdakota_data) +
  geom_histogram(mapping = aes(x = negative), bin = 15,
                 fill = "pink", color = "black") +
  labs(x = "Negative Tests", y = "Frequency (Daily Count)") 

ggplot(data = minnesota_data) +
  geom_histogram(mapping = aes(x = positive), bin = 15,
                 fill = "pink", color = "black") +
  labs(x = "Positive Tests", y = "Frequency (Daily Count)") 

ggplot(data = northdakota_data) +
  geom_histogram(mapping = aes(x = positive), bin = 15,
                 fill = "pink", color = "black") +
  labs(x = "Positive Tests", y = "Frequency (Daily Count)")

ggplot(data = minnesota_data) +
  geom_area(mapping = aes(x = date, y=negative), color='black', fill='pink')

ggplot(data = northdakota_data) +
  geom_area(mapping = aes(x = date, y=negative), color='black', fill='pink')

ggplot(data = minnesota_data) +
  geom_point(mapping = aes(x = date , y = negative, color='pink'),alpha = 10)

ggplot(data = northdakota_data) +
  geom_point(mapping = aes(x = date , y = negative, color='pink'),alpha = 10)

ggplot(data = minnesota_data) +
  geom_point(mapping = aes(x = date , y = positive, color='pink'),alpha = 10)

ggplot(data = northdakota_data) +
  geom_point(mapping = aes(x = date , y = positive, color='pink'),alpha = 10)

