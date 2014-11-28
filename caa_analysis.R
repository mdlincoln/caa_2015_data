library(gender)
library(dplyr)
library(openNLP)
library(stringr)

load("schedule.RData")

# Gender

first_name <- str_match(schedule$chairs, "^(\\w+)")[,2]
other_names_list <- str_match_all(schedule$chairs, "; (\\w+)")
other_names <- sapply(other_names_list, function(x) x[2])
first_gender <- sapply(gender(first_name), function(x) x[["gender"]])
other_gender <- sapply(gender(other_names), function(x) x[["gender"]])
data.frame(first_gender, other_gender) %>% count(first_gender)
data.frame(first_gender, other_gender) %>% count(other_gender)


# NER
s <- paste(schedule$session_text, collapse = " ")
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
en_an <- Maxent_Entity_Annotator()
entities <- en_an(s, a2)
names <- sapply(entities, function(x) str_sub(s, x$start, x$end))

genders <- sapply(gender(names, method = "ipums"), function(x) x$gender)
counts <- data.frame(genders) %>% count(genders)

# Find digital panels
any_match <- function(string, array) {
  expr <- paste0("(?:", paste(array, collapse = ")?(?:"), ")?")
  matches <-
  return(any(matches))
}

keywords <- c("[Dd]igital", "[Cc]omputer", "[Ii]nternet")
digital <- schedule %>% filter(str_detect(session_text, "(?:[Dd]igital)?(?:?:[Ccomputer") | str_detect(session_text, "[Cc]omputer"))

str_detect(schedule$session_text, keyword)
matches <- sapply(keywords, function(x) str_detect(schedule$session_text, x))

paste0("(?:", paste(keywords, collapse = ")?(?:"), ")")

any_match(schedule$title, keywords)


ggplot(schedule, aes(x = wday(starttime, label = TRUE), fill = category))+ geom_histogram(position = "dodge")

