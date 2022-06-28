library(plyr)
library(devtools)
library(tidyverse)
library(jsonlite)

################################################################################
##  Scrape Subredddits (in order to identify posters)
################################################################################
sub_list_short = c("Anarcho_Capitalism", "AntiWhitePrejudice", 
                   "AskThe_Donald")

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

# Second solution. Still scraping twice but at least it is much cleaner.
# scrape, test if page is empty(aka if the last page is a dataframe), if yes--> really scrape, if no--> break
#
# Note for optimization: maybe it is possible to use a while loop that checks whether t[[i]] is a data frame!

scrape_subreddit <- function(subreddit){
  
  base_url <- paste0("https://old.reddit.com/r/", subreddit, "/.json")
  
  out <- list()
  out[[1]] <- fromJSON(base_url,flatten=T)$data$children
  
  t <- list()
  t[[1]] <- fromJSON(base_url,flatten=T)$data$children
  
  for(i in 2:1000){
    print(paste0("subreddit = ",subreddit,"; page = ",i))
    tmp <- out[[i-1]]
    last_id <- tmp[nrow(tmp),"data.name"]
    
    t[[i]] <- fromJSON(paste0(base_url,"?after=",last_id),flatten = T)$data$children
    
    if (is.data.frame(t[[i]])) {
      
      Sys.sleep(1)
      out[[i]] <- fromJSON(paste0(base_url,"?after=",last_id),flatten = T)$data$children
      
    } else {
      
      break
      
    }
    
  }
  
  do.call("rbind.fill",out)
  
}

# How to append many subreddits/users?
### create empty data frame with target variables
subreddits <- data.frame(data.subreddit=character(),
                         data.author_fullname=character(),
                         data.selftext=character(),
                         data.title=character(), 
                         data.score=integer(), 
                         data.author=character(), 
                         data.permalink=character(), 
                         data.url=character(),
                         stringsAsFactors=FALSE)

### run for loop for all subreddits/users (in this case users)
### within for loop, run scrape function, reduce dataset and append to previously created dataframe.
for (subreddit in sub_list) {
    subreddit <- scrape_subreddit(subreddit)
    subreddit <- select(subreddit, "data.subreddit", "data.author_fullname", 
                         "data.selftext", "data.title", "data.score",
                         "data.author", "data.permalink", "data.url")
    
    ##Collecting the relevant user names could/should be done in here without the 
    #need of creating a huge subreddit dataframe. But we do need this code for the users/posters
    #sub_poster <- unique(subreddit$data.author)
    #all_posters<- c(all_posters, sub_poster)
    
    subreddits <- rbind(subreddits, subreddit)
}
user_list <- unique(subreddits$data.author)

save(subreddits, file = "../data/subreddits.rda")

################################################################################
##  Scrape Users 
################################################################################
load(file = "../data/subreddits.rda")
user_list <- unique(subreddits$data.author)
user_list <- user_list[user_list != "[deleted]"]
#already scraped 91, continuing form 92
#user_list <- user_list[c(93:8547)]

#Armin
#user_list <- user_list[c(93:3000)]
#Armin2
#user_list <- user_list[c(3001:5999)]
#Brooke
user_list <- user_list[c(6000:8547)]

scrape_users <- function(user){
  
  base_url <- paste0("https://old.reddit.com/user/", user, "/comments/.json")
  
  t <- list()
  t[[1]] <- fromJSON(base_url,flatten=T)$data$children
  
  if (is.data.frame(t[[1]])) {

    out <- list()
    out[[1]] <- fromJSON(base_url,flatten=T)$data$children
    
    for(i in 2:1000){
      print(paste0("user = ",user,"; page = ",i))
      tmp <- out[[i-1]]
      last_id <- tmp[nrow(tmp),"data.name"]
      
      t[[i]] <- fromJSON(paste0(base_url,"?after=",last_id),flatten = T)$data$children
      
      if (is.data.frame(t[[i]])) {
        
        Sys.sleep(1)
        out[[i]] <- fromJSON(paste0(base_url,"?after=",last_id),flatten = T)$data$children
        
      } else {
          break
      }
    }
    
    do.call("rbind.fill",out)
    
    }
}

user_all <- data.frame(data.link_title=character(),
                       data.score=integer(), 
                       data.subreddit=character(), 
                       data.author=character(), 
                       data.body=character(),
                       stringsAsFactors=FALSE)

for (user in user_list) {
  user <- scrape_users(user)
  if (!is.null(user)) {
    user <- select(user, "data.link_title", "data.score", 
                   "data.subreddit", "data.author", "data.body")
    
    user_all <- rbind(user_all, user)
  }

}
#Armin1
#save(user_all, file = "../data/users2.rda")
#Armin2
#save(user_all, file = "../data/users3.rda")
#Brooke
save(user_all, file = "../data/users4.rda")
