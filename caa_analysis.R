library(gender)
library(dplyr)
library(ggplot2)

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
s <- schedule$session_text[5]
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
entity_ann <- Maxent_Entity_Annotator()
annotations <- annotate(s, entity_ann, a2)

# Find digital panels
any_match <- function(string, array) {
  matches <- sapply(array, function(x) str_detect(string, x))
  return(any(matches))
}

keywords <- c("digital", "computer", "internet")
schedule %>% filter(any_match(session_text, keywords)) %>% write.csv("digital.csv")



ggplot(schedule, aes(x = wday(starttime, label = TRUE), fill = category))+ geom_histogram(position = "dodge")

