## Testing the reddit data dump
## Source: https://files.pushshift.io/reddit/comments/
## Armin

#install.packages("RedditExtractoR")
#library(RedditExtractoR)
install.packages("shiny")
install.packages("miniUI")
install.packages("devtools")
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch") 
remotes::install_github("thekvs/zstdr", INSTALL_opts=c("--no-multiarch"))
install.packages("jsonlite")
install.packages("httr")
install.packages("RSelenium")

library(plyr)
library(devtools)
library(tidyverse)
library(tabulizer)
library(shiny)
library(miniUI)
library(zstdr)
library(jsonlite)
library(httr)
library(RSelenium)

################################################################################
##  Extract subreddit list from Hiaeschutter-Rice & Hawkins 2022
################################################################################
## Add subreddits! Remove left Subreddits (i think we should focus one direction for now, no?)
subreddits <- extract_text('../data/Data_Sheet_1_The Language of Extremism on Social Media An Examination of Posts, Comments, and Themes on Reddit.PDF',
                            pages = 4,
                            area = list(c(90, 0, 500, 600)))

subreddits <- strsplit(str_remove_all(subreddits, "\r\n"), " ")

subreddits <- c(subreddits[[1]])


################################################################################
##  Scrape Subreddits
################################################################################
## Actually seems to work
scrape_subreddit <- function(subreddit,n_pages=1){
  
  base_url <- paste0("https://old.reddit.com/r/",subreddit,"/.json")
  
  out <- list()
  out[[1]] <- fromJSON(base_url,flatten=T)$data$children
  
  for(x in 2:n_pages) {
    Sys.sleep(1)
    
    tmp <- out[[i-1]]
    last_id <- tmp[nrow(tmp),"data.name"]
    
    out[[i]] <- fromJSON(paste0(base_url,"?after=",last_id),flatten = T)$data$children  ##scrape only one item, to make faster
    ##find a way that it quits this for loop and continues with the rest without breaking
    n_pages <- x-1 #this object should be the correct number of maximum pages the subreddit has for next loop
  }

  
  
  scrape_subreddit <- function(subreddit,n_pages=1){
    
    base_url <- paste0("https://old.reddit.com/r/",subreddit,"/.json")
    
    out <- list()
    out[[1]] <- fromJSON(base_url,flatten=T)$data$children
    
    for(x in 2:n_pages) {  ##fyi, I learned that using infinity is not advisable, but instead we might consider using a "while" loop instead of a for loop. It's like a for loop but once certain criteria are not met, it stops or does something else (hopefully)
      Sys.sleep(1)
      
      tmp <- out[[i-1]]
      last_id <- tmp[nrow(tmp),"data.name"]
      
      out[[i]] <- fromJSON(paste0(base_url,"?after=",last_id),flatten = T)$data$children  ##scrape only one item, to make faster
      ##find a way that it quits this for loop and continues with the rest without breaking
      n_pages <- x-1 #this object should be the correct number of maximum pages the subreddit has for next loop
    }
    
 ##TO ADD: for error handling and as a way to push through the error:
    ## 
    ## for()
    ## possibleError <- tryCatch(
    ## thing(),
   ## error=function(e) e
   ## )
    ## if(inherits(possibleError, "error")) next

## TO ADD this will help with data cleaning
## us some version of the below as a function to remove the less important information for each entry:
##     new1 = df1[,c("ups", "title", "subreddit", "url")]

    
    for(i in 2:n_pages){
      
      Sys.sleep(1)
      
      tmp <- out[[i-1]]
      last_id <- tmp[nrow(tmp),"data.name"]
      
      
      out[[i]] <- fromJSON(paste0(base_url,"?after=",last_id),flatten = T)$data$children
      
    }
    
    do.call("rbind.fill",out)
  }
  
  
  for(i in 2:n_pages){
    
    Sys.sleep(1)
    
    tmp <- out[[i-1]]
    last_id <- tmp[nrow(tmp),"data.name"]
    
    
    out[[i]] <- fromJSON(paste0(base_url,"?after=",last_id),flatten = T)$data$children
    
  }
  
  do.call("rbind.fill",out)
  
}

# Identify users that post a lot
# Goal is obviously to have this function run through the whole list of subreddits and append the resulting data frames.
# Afterwards we count how much users post and then scrape the data of users that post a lot in the next step.

##Problem: How to identify max page before execution? We need that, otherwise we have to do it all by hand, as any loop would throw an error 
##Problem: How to append all subreddits

content_libsofreddit <- scrape_subreddit("libsofreddit", 3)  #30

content_libsofreddit <- content_libsofreddit %>%
  select(data.subreddit, data.author_fullname, data.selftext, data.title, 
         data.score, data.author, data.permalink, data.url) %>%
    group_by(data.author) %>%
  mutate(author_count = n())

head(content_libsofreddit)
content_AskThe_Donald <- scrape_subreddit("AskThe_Donald", 80)

content_FreedomConvoy2022 <- scrape_subreddit("FreedomConvoy2022", 30)
view(content_FreedomConvoy2022)



################################################################################
##  Scrape Users (comment history)
################################################################################
scrape_user <- function(user,n_pages=1){
  
  base_url <- paste0("https://old.reddit.com/user/", user, "/comments/.json")
  
  out <- list()
  out[[1]] <- fromJSON(base_url,flatten=T)$data$children
  
  for(i in 2:n_pages){
    
    Sys.sleep(1)
    
    tmp <- out[[i-1]]
    last_id <- tmp[nrow(tmp),"data.name"]
    
    
    out[[i]] <- fromJSON(paste0(base_url,"?after=",last_id),flatten = T)$data$children
    
  }
  
  do.call("rbind.fill",out)
  
}

##B attempt: 

skip_to_next <- FALSE

# Note that print(b) fails since b doesn't exist
scrape_user_v2 <- function(user,n_pages=1){
  

  base_url <- paste0("https://old.reddit.com/user/", user, "/comments/.json")
  
  out <- list()
  out[[1]] <- fromJSON(base_url,flatten=T)$data$children
  
  for(i in 2:n_pages){
    
    Sys.sleep(1)
    tmp <- out[[i-1]]
    last_id <- tmp[nrow(tmp),"data.name"]
    
    out[[i]] <- fromJSON(paste0(base_url,"?after=",last_id),flatten = T)$data$children
    
  }
  do.call("rbind.fill",out)
}
  


# Goal is to hand this function a list of users, and it outputs their whole comment history. 
# We can then count which other subreddits they are active (= commenting)
# and compare speech of comments in extreme subreddits and non-exteme subreddits
usertest <- scrape_user("Rizzzl", 14)
