library(rvest)
library(dplyr)
library(stringr)
library(lubridate)

# Download schedule and parse HTML nodes
schedule_url <- "http://conference.collegeart.org/schedule/"
schedule_html <- schedule_url %>% html()

title <- schedule_html %>% html_nodes(".program_blurb") %>% html_text()
raw_content <- schedule_html %>% html_nodes("#program") %>% html_text()
link <- schedule_html %>% html_nodes("#program > a") %>% html_attr("href")

# Extract date, time, location, and chair information
raw_schedule <- data.frame(title, raw_content, link)
schedule <- raw_schedule %>%
  mutate(
    date = mdy(str_match(raw_content, "Time: (\\d{2}/\\d{2}/\\d{4})")[,2]),
    time = str_match(raw_content, "(\\d{1,2}:\\d{2} [APM]{2}—\\d{1,2}:\\d{2} [APM]{2})")[,2],
    location = str_match(raw_content, "Location: (.*?)\\r")[,2],
    chairs = str_match(raw_content, "Chairs?: (.*?)\\n")[,2],
    session_text = str_match(raw_content, ".*?\\r.*?\\r(?:Chairs?:.*?\\n)?(.*?)Full Details")[,2]
    )
write.csv(schedule, "schedule.csv", row.names = FALSE)


dig_words <- c("digital", "computer", "internet")

