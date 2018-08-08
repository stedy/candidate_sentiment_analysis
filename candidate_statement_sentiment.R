library(dplyr)
library(tidytext)
library(ggplot2)

`%notin%` <- function(x,y) !(x %in% y)
texts <- list.files(pattern = "txt")

values <- c()
for(x in texts){
  raw_lines <- paste(readLines(x), collapse = " ")
  temp2 <- tibble(text = raw_lines) %>% unnest_tokens(word, text) %>% count(word)
  if(x == "stockwell.txt"){
    temp2 <- data.frame(word = NA, n = NA)
  }
  temp2$candidate <- unlist(stringr::str_split(x, ".txt"))[1]
  values <- rbind.data.frame(values, temp2)
}

values_df <- data.frame(values)

cleaned <- values_df %>% anti_join(stop_words)

nrc_sentiment <- cleaned %>%
  group_by(candidate) %>%
  right_join(get_sentiments("nrc")) %>%
  count(sentiment, sort = T)

final_nrc <-
  nrc_sentiment %>%
  group_by(candidate) %>%
  subset(sentiment %in% c("positive", "negative")) %>%
  distinct

final_nrc_forplot <-
  final_nrc %>%
  subset(complete.cases(.))

#results

election_results <- read.csv(list.files(pattern = "csv")) %>%
  subset(Race == "U.S. Senator")

candidate_names <- data.frame(stringr::str_split_fixed(election_results$Candidate, pattern = " ", n = 3)) %>%
  purrr::map_if(is.factor, as.character)
candidate_names$clean <- ifelse(candidate_names$X3 != "", candidate_names$X3, candidate_names$X2)
candidate_names$clean <- ifelse(candidate_names$X1 == "GoodSpaceGuy","GoodSpaceGuy", candidate_names$clean)
candidate_names$clean <- ifelse(candidate_names$X1 == "James","Deal", candidate_names$clean)

election_results$candidate <- tolower(candidate_names$clean)
election_results$candidate <- gsub(" ", "_", election_results$candidate)
election_results$Party <- gsub("\\(Prefers ", "", election_results$Party)
election_results$Party <- gsub(" Party\\)", "", election_results$Party)
election_results$color <- ifelse(election_results$Party == "Democratic", "blue", "")
election_results$color <- ifelse(election_results$Party == "Republican", "red", election_results$color)
election_results$color <- ifelse(election_results$Party %notin% c("Democratic", "Republican"), "tan4", election_results$color)

merged_results <- merge(election_results, final_nrc_forplot, all.y=T)

ggplot(subset(merged_results, sentiment == "positive"),
       aes(x=candidate, y=nn, color=color)) + geom_point(size=3) +
  scale_colour_manual(labels = c("Democrat", "Republican", "Third party"), values = c("blue", "red", "tan4")) +
  theme(axis.text.x = element_text(angle =45, hjust=1)) +
  labs(y = "Number of positive words", color = 'Party') +
  ggtitle("Positive sentiment in Candidate Statement", subtitle = "U.S. Senate Candidates from WA State in August 7, 2018 Primary")
ggsave("WA_candidate_positive.png")

ggplot(subset(merged_results, sentiment == "negative"),
       aes(x=candidate, y=nn, color=color)) + geom_point(size=3) +
       scale_colour_manual(labels = c("Democrat", "Republican", "Third party"), values = c("blue", "red","tan4")) +
  theme(axis.text.x = element_text(angle =45, hjust=1)) +
  labs(y = "Number of negative words", color = 'Party') +
  ggtitle("Negative sentiment in Candidate Statement", subtitle = "U.S. Senate Candidates from WA State in August 7, 2018 Primary")
ggsave("WA_candidate_negative.png")

#positive vs negative words as a function of votes
ggplot(subset(merged_results, sentiment == "positive"),
       aes(x=log10(Votes), y=nn, color=color, label = candidate)) + geom_point(size=3) +
  geom_text(aes(label=candidate), hjust=-0.1, vjust=-0.1) +
  scale_colour_manual(labels = c("Democrat", "Republican", "Third party"), values = c("blue", "red","tan4")) +
  labs(y = "Number of positive words", color = 'Party') +
  ggtitle("Positive sentiment in Candidate Statement vs. vote count", subtitle = "U.S. Senate Candidates from WA State in August 7, 2018 Primary")
ggsave("WA_candidate_positive_vote_count.png")

ggplot(subset(merged_results, sentiment == "negative"),
       aes(x=log10(Votes), y=nn, color=color, label = candidate)) + geom_point(size=3) +
  geom_text(aes(label=candidate), hjust=-0.1, vjust=-0.1) +
  scale_colour_manual(labels = c("Democrat", "Republican", "Third party"), values = c("blue", "red","tan4")) +
  labs(y = "Number of negative words", color = 'Party') +
  ggtitle("Negative sentiment in Candidate Statement vs. vote count", subtitle = "U.S. Senate Candidates from WA State in August 7, 2018 Primary")
ggsave("WA_candidate_negative_vote_count.png")