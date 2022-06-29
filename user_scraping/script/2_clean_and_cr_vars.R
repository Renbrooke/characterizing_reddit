'
In this script:
- reduce dataset to analysis sample
- identify other extreme right subreddits in subreddit list (?)
- create relevant vars for analysis
'

library(tidyverse)
library(openxlsx)

sub_list = c("AHomeForPlagueRats", "Anarcho_Capitalism", "AntiWhitePrejudice", 
             "AskThe_Donald", "benshapiro", "BidenBuzz", "BidenIsNotMyPresident", 
             "BreitbartNews", "CabalCrusher", "ChurchOfCOVID", "climateskeptics", 
             "CollegeRepublicans", "Conservativelifestyle", "ConservativeMemes", 
             "ConservativesOnly", "conspiracy", "CoronavirusCirclejerk", "DarkMAGA", 
             "DebateVaccines", "DNCleaks", "EasternSunRising", "FauciForPrison", 
             "FightingFakeNews", "Firearms", "FreedomConvoy2022", "GlobalLockdown", 
             "IvankaTrump", "libertarianmeme", "libsofreddit", "LouderWithCrowder", 
             "MensRights", "non_msm", "NPCMemes", "NurembergTwo", "progun", 
             "QuiteFrankly", "Red_Suppository", "RedPillWomen", "rittenhouse", 
             "samharris", "The_Farage", "TheBidenshitshow", "TheTrumpZone", 
             "TrueChristian", "trump", "Trumpgret", "tucker_carlson", "UNAgenda21", 
             "walkaway")


final_data <- readRDS(file = "../data/final_data.rds") 

data <- final_data %>%
  select(-contains("source_")) %>%
  group_by(data.subreddit) %>%
  mutate(comment_count = n()) %>%
  ungroup()

#how many users
length(unique(data$data.author))
#how many subreddits
length(unique(data$data.subreddit))
#how many subreddits without extreme right subreddits
length(unique(data$data.subreddit[!data$data.subreddit %in% sub_list]))
#how many posts overall
length(unique(data$data.body))
#how many posts without extreme right subreddits
length(unique(data$data.body[!data$data.subreddit %in% sub_list]))

###! Problem: We would have to identify other extreme right subreddits in the data, but how? We can't go through 22,526 subreddits.
## Reduce data, only keep user that have >=25 posts
data <- data %>%
  filter(posts_all >= 25) %>%
  filter(comment_count >= 10)

#save list of subreddits to look through:
all_subs <- data %>%
  select(data.subreddit, comment_count) %>%
  filter(!data.subreddit %in% sub_list) %>%
  distinct() %>%
  arrange(data.subreddit)
  
write.xlsx(all_subs, file = "../reduced_sublist.xlsx")

#check numbers again after the reduction of dataset
#how many users
length(unique(data$data.author))
#how many subreddits
length(unique(data$data.subreddit))
#how many subreddits without extreme right subreddits
length(unique(data$data.subreddit[!data$data.subreddit %in% sub_list]))
#how many posts overall
length(unique(data$data.body))
#how many posts without extreme right subreddits
length(unique(data$data.body[!data$data.subreddit %in% sub_list]))


# How to identify meaningful groups of subreddits?
# Brooke will try word embedding


# Armin will use more or less manual coding with some pattern recognition
data <- data %>%
  mutate(sub_groups = case_when(str_detect(data.subreddit, "conspiracy") ~ "conspiracy",
                                str_detect(data.subreddit, "gun") ~ "guns",
                                str_detect(data.subreddit, "") ~ "",
                                str_detect(data.subreddit, "") ~ "",
                                str_detect(data.subreddit, "") ~ "",
                                str_detect(data.subreddit, "") ~ "",
                                str_detect(data.subreddit, "") ~ "",
                                str_detect(data.subreddit, "") ~ "",
                                ))

    filter(str_detect(data.subreddit, "conspiracy"),) 
