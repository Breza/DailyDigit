library(conflicted)
library(tidyverse)
library(glue)
library(lubridate)
library(stringr)
library(rvest)
library(feather)
filter <- dplyr::filter

# Get a list of all URLs containing a Daily Digit
get_digit_urls <- function(from = 1) {
  read_html(
    glue(
      "https://www.washingtonpost.com/pb/api/v2/render/feature/section/story-list?addtl_config=blog-front&content_origin=content-api-query&size=10&from={from}&primary_node=/local/weather/capital-weather-gang"
    )
  ) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    as_tibble() %>%
    filter(str_detect(value, "-area-forecast")) %>%
    distinct() %>%
    mutate(link = str_sub(value, start = 3L,-3L)) %>%
    mutate(link = paste0("http://washingtonpost.com", link)) %>%
    pull(link)
}

# Extract the Daily Digit from each URL
get_daily_digit <-
  function(day_url =
             "http://washingtonpost.com/weather/2019/01/01/dc-area-forecast-breezy-today-with-increased-sunshine-more-rain-come-this-week/") {
    # day_date <-
    #   ymd(str_sub(
    #     string = day_url,
    #     start = 35,
    #     end = str_locate(string = day_url, pattern = "/dc-area-forecast")[1] - 1
    #   ))
    
    day_date <- str_extract_all(day_url, "[[:digit:]]+")[[1]][1:3] %>% 
      paste(collapse = "-") %>% 
      ymd()
    
    daily_forecast <- read_html(day_url)
    
    daily_forecast %>%
      html_nodes("b, strong") %>%
      html_text() %>%
      str_trim() %>%
      as_tibble() %>%
      filter(str_detect(value, "10:")) %>%
      mutate(value = str_replace_all(value, ":", "")) %>%
      mutate(value = str_replace_all(value, "/10", "")) %>%
      transmute(daily_digit = as.numeric(value)) %>%
      mutate(date = day_date) %>%
      select(date, daily_digit)
  }

get_daily_digit <- possibly(get_daily_digit, otherwise = tibble(date = NA, daily_digit = NA))

all_url <- map(0:556 * 10 + 1, get_digit_urls) %>% unlist()

all_daily_digits <- map_dfr(all_url, get_daily_digit)

all_daily_digits <- na.omit(all_daily_digits)

write_feather(all_daily_digits, "all_daily_digits.feather")
