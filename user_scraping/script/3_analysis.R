'
GOALS FOR PRESENTATION:
Descriptive summary of data:
  What were our source-subreddits (sub_list_reduced), and why?
  How did we collect the data?
      
  How many subreddits, users and posts in total? How many after reduction?
      We scraped 1,753,752 posts, but the subreddit list was not well defined. 
      After retroactively redefining it we are left with 73,496 posts by 136 
      users, that posted in 3532 subreddits 
  How many groups, and what percent of subreddits were we able to categorize?


Which type of subreddits, other than extreme-right subreddits is our sample active in?
  Any interesting observations? Maybe if we look inside the types (e.g., covid)?
  

Visualizations:
- I would prefer 


'

library(tidyverse)
library(scales) 

rm(list = ls())
data_grouped <- readRDS(file = "../data/data_grouped.rds") 

data <- data_grouped %>%
  group_by(sub_groups) %>%
  mutate(group_count = n()) %>%
  ungroup() %>%
  group_by(data.subreddit) %>%
  mutate(sub_count = n()) %>%
  ungroup()


##### BASIC BAR GRAPHS
## Logarithmic scale because otherwise smaller subreddits not visible?
plot <- data %>%
  filter(sub_groups != "source subs") %>%
  ggplot(aes(x = reorder(sub_groups,group_count), y = group_count)) +
  geom_col() +
  coord_flip() +
  scale_y_log10() +
  labs(x = "Subreddit groups", 
       y = "Number of posts (logarithmic!)") +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=14,face="bold"))
plot

# or non logarithmic
plot <- data %>%
  filter(sub_groups != "source subs") %>%
  ggplot(aes(x = reorder(sub_groups,group_count), y = group_count)) +
  geom_col() +
  coord_flip() +
  labs(x = "Subreddit groups", 
       y = "Number of posts") +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold")) +
  scale_y_continuous(labels = scales::label_number(scale = 1/1000))
plot
ggsave(plot, filename = "../output/sub_groups.png",height = 6, width = 10)

#Top subreddits, colored by subreddit type
plot <- data %>%
  filter(sub_groups != "source subs" & sub_count > 500) %>%
  ggplot(aes(x = reorder(sub_title,sub_count), y = sub_count, color = sub_groups)) +
  geom_count()  +
  coord_flip() 
plot

# Smaller subreddits
plot <- data %>%
  filter(sub_groups != "source subs" & group_count < 1000) %>%
  ggplot(aes(x = reorder(sub_groups,group_count), y = group_count)) +
  geom_col() +
  coord_flip() +
  labs(x = "Subreddit groups", 
       y = "Number of posts") +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold")) +
  scale_y_continuous(labels = scales::label_number(scale = 1/1000))
plot
ggsave(plot, filename = "../output/sub_groups_small.png",height = 6, width = 10)

# Case Study: schadenfreude group of subreddits
# 
# Perhaps the best way to compare this is in terms of ranking:
# ranking of sub_group count 
# 
# as compared to average overall ranking of subreddits:
# #
# # subreddit ranks (based on total subscribers, not posts; source: frontpagemetrics.com; 
# in future research, we'd like to develop a more consistent comparison)
# total number of subreddits as of 6/24/2022: 3,544,167
# 
# yesyesyesno : rank 704
# winstupidprizes: rank 254
# thatlookedexpensive: rank 799
# publicfreakouts: rank 18,256
# publicfreakoutsreborn: rank 11,514
#actualpublicfreakouts: rank 1,063
# instantregret: rank 1244
#  instantkarma: rank 253
#  robbersgettingfucked: 4,997
# averageredditor: 6372
# whatcouldgowrong: rank 267
# nsfl__ 
# pettyrevenge: rank 364 
# sadcringe: 511
# 
# 
# 

# covid: mark obvious disinfo subreddits and broad anti corona regulation subs

plot <- data %>%
  mutate(misinfo = if_else(
    sub_title %in% c("nurembergtwo", "ahomeforplaguerats", "debatevaccines", 
                     "coronaviruscirclejerk", "churchofcovid", "cultofcorona", 
                     "lockdownskepticismau", "lockdowncriticalleft", 
                     "vaccinememes", "vaccinepassport", "vaccinelonghauler", 
                     "actualscience", "coronavirusuncensored", 
                     "norcallockdownskeptic", "ahomeforplagueratscan", 
                     "covidmemes", "imdonewithcovid" ), "Misinfo/\nAnti-reg.", "Other")) %>%
  filter(sub_groups == "covid" & group_count > 200) %>%
  ggplot(aes(x = reorder(sub_title,sub_count), y = sub_count)) +
  geom_count()  +
  coord_flip() +
  labs(x = "Subreddits within the -covid- category", 
       y = "Number of posts") +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15,face="bold"),
        legend.text = element_text(size=13),
        legend.position = c(.98, .5),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank()) 
plot 
ggsave(plot, filename = "../output/covid_bw.png",height = 6, width = 10)

plot <- data %>%
  mutate(misinfo = if_else(
    sub_title %in% c("nurembergtwo", "ahomeforplaguerats", "debatevaccines", 
                     "coronaviruscirclejerk", "churchofcovid", "cultofcorona", 
                     "lockdownskepticismau", "lockdowncriticalleft", 
                     "vaccinememes", "vaccinepassport", "vaccinelonghauler", 
                     "actualscience", "coronavirusuncensored", 
                     "norcallockdownskeptic", "ahomeforplagueratscan", 
                     "covidmemes", "imdonewithcovid" ), "Misinfo/\nAnti-reg.", "Other")) %>%
  filter(sub_groups == "covid" & group_count > 200) %>%
  ggplot(aes(x = reorder(sub_title,sub_count), y = sub_count, color = misinfo)) +
  geom_count()  +
  coord_flip() +
  labs(x = "Subreddits within the -covid- category", 
       y = "Number of posts") +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15,face="bold"),
        legend.text = element_text(size=13),
        legend.position = c(.98, .5),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank()) 
plot 
ggsave(plot, filename = "../output/covid_misinfo.png",height = 6, width = 10)




##### WORD COUNTING
# politician mentioned!
## steps: get stopword list of all english words, eliminate these from posts, 
##        look through remaining data and filter out politicians
## OR:    make list of relevant politicians and search for them

#simple word count with stopwords, seeing what is mentioned often in comments
#just in the subreddits classified as extreme
install.packages("stopwords")
install.packages("tokenizers")
install.packages("tidytext")
#install.packages("SnowballC ")  < not available for this version of R...
install.packages("hunspell")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("tm")
install.packages("NLP")
install.packages("textdata")
install.packages("reshape2")

library(reshape2)
library(NLP)
library(tm)
library(tokenizers)
library(tidyverse)
library(tidytext)
#library(SnowballC)
library(hunspell)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(textdata)

extr_subs <- data_grouped %>%
  mutate(big_groups = case_when(
    sub_groups %in% c("conspiracy","anarcho-capitalism",
                     "influencers, right","pro-republican",
                     "source subs", "extreme right","anti-communist") ~ "Right-wing",
    sub_groups == "general, pol" ~ "General, politics", 
    sub_groups %in% c("places","general, non-pol","hobbies/interests",
                      "gaming", "entertainment","occupation","sports",
                      "cars","tipps", "music", "support") ~ "Hobbies and co.",
    sub_groups == "schadenfreude" ~ "Schadenfreude"))
    


#%>%  # <<<<<<<<<<<<<<<<<<<<<<<< maybe more interesting if not only extreme right subreddits
#distinct(data.body) %>%
#filter(sub_groups %in% )

#stupid, but unfortunately no time for better
extr_words <- extr_subs %>%
  unnest_tokens(words, data.body) %>%
  filter(!(words %in% stopwords::stopwords(source = "stopwords-iso")))

extr_words$words <- gsub("https\\S*", "", extr_words$words) 
extr_words$words <- gsub("@\\S*", "", extr_words$words) 
extr_words$words <- gsub("amp", "", extr_words$words) 
extr_words$words <- gsub("[\r\n]", "", extr_words$words)
extr_words$words <- gsub("[[:punct:]]", "", extr_words$words)
extr_words$words <- gsub("[0123456789]", "", extr_words$words)

# prior to removing meaningless words, maybe a sentiment analysis 
# comparison within users' posts between the sentiments of their posts on extreme subreddits 
# vs the other, unrelated subreddits? 
#
# script for sentiment analysis

nrc_sentiments <- get_sentiments("nrc") %>%
  select(word, sentiment)

#preparing for being able to use inner_join (I think you need same column names (word))
extr_words_t <- extr_words %>%
  mutate(word = words)

#inner join and sentiment analysis of words
subreddit_sentiments <- extr_words_t %>% 
  inner_join(nrc_sentiments, by = "word") %>%
  select(big_groups, word, sentiment)

# need to count and compare the frequencies of each sentiment for 
#two categories: extreme vs other subreddits
#this would tell us something about how the engagement of these politically extreme users 
# compare sentiments between their extreme subreddits and their outside subreddits
# need to use variable with the two categories separated:

#Frequency of subreddit group sentiments <<<< recommend going with this: better potential for visualization, fewer NA fields
subgroup_sentiment_count <- extr_words_t %>%
  inner_join(nrc_sentiments, by = "word") %>%
  count(big_groups, sentiment) %>%
  spread(big_groups, n)

dat_l <- melt(subgroup_sentiment_count, id.vars = c("sentiment"))  %>%
  group_by(variable) %>%
  mutate(group = sum(value)) %>%
  ungroup() %>%
  mutate(perc = round((value/group)*100))

plot <- dat_l %>%
  filter(variable != "<NA>") %>%
  ggplot(aes(x = variable, y=perc, group=reorder(sentiment,value), fill = sentiment, color=perc)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15,face="bold")) +
  labs(x = "Subreddit groups", 
       y = "Percent of posts") 
plot
ggsave(plot, filename = "../output/sentiments.png",height = 6, width = 10)


#NOTE: just also need to rerun the script with a few additional newly categorized subreddits (see updates to script)

# probably also need to turn this into a proportion for better comparison
# subgroup_sentiment_count %>%
# mutate = 
# not sure how to code this part^

#Frequency of subreddit sentiments
sub_sentiment_count <- extr_words_t %>%
  inner_join(nrc_sentiments, by = "word") %>%
  count(data.subreddit, sentiment) %>% 
  spread(data.subreddit, n)

sub_sentiment_count

#summarize the overall sentiment of posts and users
# perhaps we could also do a word cloud for sentiments?


extr_words <- extr_words %>%
  filter(!extr_words$words=="",
         !str_detect(words, "^its$|^dont$|^2$|^day$|^im$|^thats$|^yeah$|^lol$|^lot$|^youre$"),   #<<<<<<<<<<<<<<< DELETE USELESS WORDS #added more words to delete here: yeah, lol, lot, youre
         !extr_words$words=="people") ##probably should not delete this 

extr_count <- extr_words %>% 
  count(words) %>%
  arrange(desc(n)) %>%
  slice(1:500)

view(extr_count)  #                  <<<<<<<<<<< CHECK FOR USELESS WORDS

set.seed(1234)
wordcloud(words = extr_count$words, freq = extr_count$n, min.freq = 300,
          max.words=300, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud2(extr_count,
           size = 0.5,
           color = "random-dark", 
           backgroundColor = "white",
           shuffle = TRUE)   #<<<<<<<<<<<<<<<<<<<<<<<<<<<<< WE SHOULD USE THIS, WITH OPTIONS
