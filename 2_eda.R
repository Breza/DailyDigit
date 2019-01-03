library(tidyverse)
library(lubridate)
library(feather)
library(prophet)

all_daily_digits <- read_feather("all_daily_digits.feather")

# When you get the full dataset, use different values for testing and training
dd_train <- all_daily_digits
dd_test  <- all_daily_digits

# Plot all the values
ggplot(all_daily_digits, aes(x = date, y = daily_digit)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

# Is there a difference between different days of the week?
all_daily_digits %>%
  mutate(wday = wday(date, label = TRUE, abbr = FALSE)) %>%
  group_by(wday) %>%
  summarise(daily_digit = mean(daily_digit, na.rm = TRUE)) %>%
  ggplot(aes(x = wday, y = daily_digit)) +
  geom_col() +
  theme_bw()

aov(daily_digit ~ wday(date), data = all_daily_digits) %>% broom::tidy()

# # Put multiple years on the same chart
# ggplot(all_daily_digits, aes(x = ymd(paste0(2019, "-", month(all_daily_digits$date), "-", day(all_daily_digits$date))), y = daily_digit, color = factor(year(all_daily_digits$date)))) +
#   geom_point() +
#   geom_smooth()

dd_train_prophet <- set_names(dd_train, c("ds", "y"))
dd_test_prophet <- set_names(dd_test, c("ds", "y"))

model_prophet <- prophet(dd_train_prophet)

#future <- make_future_dataframe(model_prophet, periods = 365)

forecast <- predict(model_prophet, dd_test_prophet)
