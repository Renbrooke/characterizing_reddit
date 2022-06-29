'
In this script:
- the various scraped userdata files are combined and duplicates removed
- information on the source subreddits (the extreme subreddits we used to identify the users) is added
- complete data is saved as "final_data.rds"
'

library(tidyverse)

# List of userdata files in directory for loop
file_list <- dir("../data/",
             recursive=TRUE,
             full.names=TRUE,
             pattern=glob2rx("^users*rda$"))

# empty dataframe with correct collums for easy append
user_data <- data.frame(data.link_title=character(),
                       data.score=integer(), 
                       data.subreddit=character(), 
                       data.author=character(), 
                       data.body=character(),
                       stringsAsFactors=FALSE)

# Loop loading files and binding them to df user_data
# cannot load user4a.dta <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!!!
for (file in file_list) {
  print(file)
  load(file)
  user_data <- rbind(user_data, user_all)
  rm(user_all)
}

# delete duplicates
user_data <- distinct(user_data)


# add following information on users:
# - which subreddit were they sourced from
# - how many posts did they make in that subreddit
# - how many posts did they make in all 'extreme' subreddits
# - in how many 'extreme' subs did the user post
load("../data/subreddits.rda")
saveRDS(subreddits, file = "../data/subreddits.rds") #save it again as .rds because I can load that in pipe

subreddits <- readRDS(file = "../data/subreddits.rds") %>%
  filter(data.author != "[deleted]") %>%
  group_by(data.author) %>%
  mutate(posts_all = n()) %>%
  ungroup() %>%
  group_by(data.subreddit, data.author) %>%
  mutate(source_sub_posts = n()) %>%
  ungroup() %>%
  mutate(source_sub = data.subreddit) %>%
  select(data.author, source_sub, source_sub_posts, posts_all) %>%
  distinct() %>%
  group_by(data.author) %>%
  mutate(source_sub_count = n()) %>%
  ungroup()


# pivot source sub titles into wide format
source_subs <- subreddits %>%
  select(data.author, source_sub, source_sub_posts) %>%
  pivot_wider(names_from = source_sub,
              values_from = source_sub_posts,
              names_prefix = "source_")


# merge source subreddit info together
subreddits_fin <- subreddits %>%
  select(data.author, posts_all, source_sub_count) %>%
  distinct() %>%
  left_join(., source_subs, by = "data.author")

# merge source subreddit info with user data
user_data <- left_join(user_data, subreddits_fin, by = "data.author")

### OUTCOME: All posts of users and information on the subreddits we used to source these users
##            variable source_SUBNAME gives the number of posts of that user in this source subreddit

saveRDS(user_data, file = "../data/final_data.rds") 

