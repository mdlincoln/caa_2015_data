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
category <- schedule_html %>% html_nodes("td[width='30%']") %>% html_text()

# Extract date, time, location, chair, category, and other sesison text information
raw_schedule <- data.frame(title, raw_content, link, category)
schedule <- raw_schedule %>%
  mutate(
    date = str_match(raw_content, "Time: (\\d{2}/\\d{2}/\\d{4})")[,2],
    starttime = paste(date, str_match(raw_content, "(\\d{1,2}:\\d{2} [APM]{2})")[,2] %>% str_trim()) %>% parse_date_time("%m %d %y %I %M %p"),
    endtime = paste(date, str_match(raw_content, "—(\\d{1,2}:\\d{2} [APM]{2})")[,2] %>% str_trim()) %>% parse_date_time("%m %d %y %I %M %p"),
    location = str_match(raw_content, "Location: (.*?)\\r")[,2] %>% str_trim(),
    chairs = str_match(raw_content, "Chairs?: (.*?)\\n")[,2] %>% str_trim(),
    session_text = str_match(raw_content, ".*?\\r.*?\\r(.*?)Full Details")[,2] %>%
      str_replace("Chairs?: .*?\\n", "") %>%
      str_trim() %>%
      str_replace_all("\n", "; ") %>%
      str_replace_all("([a-z])([A-Z])", "\1; \2"),
    category = str_replace(category, ":", "") %>% str_trim(),
    title = title  %>% str_replace_all("([a-z])([A-Z])", "\1; \2")
    ) %>%
  select(-raw_content, -date)
write.csv(schedule, "schedule.csv", row.names = FALSE)
save(schedule, file = "schedule.RData")
