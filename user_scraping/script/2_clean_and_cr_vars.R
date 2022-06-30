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
## Reduce data, only keep user that have >=25 posts ** 
data <- data %>%
  filter(posts_all >= 25) %>%
  filter(comment_count >= 10)

# changing to >=75 posts in order to simplify, but this leaves only 48 users
data <- data %>%
  filter(posts_all >= 75) %>%
  filter(comment_count >= 10)

# 


#save list of subreddits to look through:
all_subs <- data %>%
  select(data.subreddit, comment_count) %>%
  filter(!data.subreddit %in% sub_list) %>%
  distinct() %>%
  arrange(data.subreddit)

# repeated with only users with posts above 75
all_subs_75 <- data %>%
  select(data.subreddit, comment_count) %>%
  filter(!data.subreddit %in% sub_list) %>%
  distinct() %>%
  arrange(data.subreddit)

write.xlsx(all_subs, file = "../reduced_sublist_postsall_25.xlsx")

write.xlsx(all_subs_75, file = "../reduced_sublist_75.xlsx" )
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


# Armin will use more or less manual coding with some pattern recognition
cars <- c("4x4|acura|alfa-romeo|aston-martin|audi|bentley|bmw|bugatti|buick|cadillac|chevrolet|chrysler|citroen|dodge|ferrari|fiat|ford|gmc|honda|hyundai|infiniti|jaguar|jeep|kia|lamborghini|lancia|land rover|lexus|lincoln|maserati|maybach|mazda|mclaren|mercedes|mitsubishi|nissan|opel|pagani|peugeot|pontiac|porsche|renault|rolls-royce|skoda|subaru|suzuki|tesla|toyota|volkswagen|volvo|alpina|callaway|caparo|caterham|dacia|daihatsu|datsun|delage|delorean|deltawing racing cars|general motors|hummer|hyperion|lucid motors|morgan|mosler|mullen|nikola motor company|oldsmobile|polestar|radical sportscars|rezvani|rimac automobile|rinspeed|rivian|saab|saleen|shelby|spyker|ssangyong|studebaker|tata|touring superleggera|trident|triumph|tvr|ultima|vauxhall|venturi|vinfast|w motors|wiesmann|yamaha|zagato|zenvo")
hobbies <- c("cigar|simulated|pedsr|youtube_startups|aquarium|aquaticsnails|^art$|airsoft
             |woodworking|woodstoving|unity3d|languagelearning|preppers|writing|cooking
             |aviation|backyardchickens|ballpython")
music <- c("music|aliceinchains|musicproduction|zappa|edm|eminem")
sport <- c("bicycling|soccer|nfl|ufc|baseketball")
gaming <- c("witcher|gaming|xboxone|sonic|league|reddeadredemption|ghostrecon
            |acecombat|videogames|worldofwarships|rpg|aidungeon|battlefield3
            |battletech|valorant")
tipps <- c("travelhacks|coolguides|^travel$")
occupation <- c("engineering|auslaw|auslegal|supplychain|pharmacist")
tech <- c("beta|windows10|technology|android|androidapps|apolloapp|apple|adobeillustrator") 
covid <- c("^vacc|^vax|lockdownskeptic|realvaccinedebate|lockdowncrit|imdonewithcovid|corona|covid|plaguerats|actualscience")
leftwing <- c("agorism|appalachistan", "againsthatesubreddits|wayofthebern|antiwork|leftpodcasts")
finance <- c("ethere|ethtrader|bitcoin|bbig|ausfinance|algotrading|crypto|wallstreet|market|invest|stonk|stock|asx_bets")
people <- c("vaushv|joerogan|timpool|jordanpeterson|timdillon|normmacdonald|aoc|ronpaul|adamcarolla")
entertainment <- c("prequel|startrek|bikinibottomtwitter|bettercallsaul|beavisandbutthead|attackontitan|thelastairbender|gravityfalls|starwars|marvel|xmen|amphibia|animaniacs|animemes")
religion <- c("christ|relig|theis|awakened")
nsfw <- c("bigtitsinbikinis|bigboobsgw|^bbw$|bdsm|asianscuckingpinkies|^ass$")
drugs <- c("trees|benzorecovery|benzodiazepines|altcannabinoids|pedsr")
military <- c("army|combat|warvideo|navy")
places <- c("brasil", "sanfrancisco", "california", "arizona", 
            "ontario", "canada", "ontariocanada", "britishcolumbia", 
            "casualuk", "china", "france", "rochester", "rochester585",
            "albany", "argentina", "adelaide", "adirondacks",
            "auckland", "australia", "bayarea", "berkeley",
            "england", "europe")
discussion <- c("capitalismvsocialism", "nostupidquestions", "debate", 
                "debatereligion", "leftvsrightdebate") #isnt it all discussion? debate religion could just go to religion
support <- c("agoraphobia", "selfharm", "sex", "anxiety", "aspiememes", "adhd",
            "adhdmeme", "alcoholism", "autism", "depression") #(added depression) ## needs a better name, it's subreddits where people talk about private stuff like illness, sex and stuff and support each other
memes <- c("adviceanimals") ## this is a problem just using *meme* would take in too many political subreddits
#try to catch as much as possible with word searches
schadenfreude <- c("yesyesyesyesno", "yesyesyesno", "winstupidprizes", "thatlookedexpensive", 
                   "publicfreakout", "publicfreakouts", "publicfreakoutsreborn", 
                   "actualpublicfreakouts", "instantregret", "instantkarma", "robbersgettingfucked",
                   "averageredditor")
anger <- c("awfuleverything", "idiotsincars")
nice_stuff <- c("beamazed|^aww$")


armin <- data %>%
  mutate(sub_title = tolower(data.subreddit)) %>%
  mutate(sub_groups = case_when(str_detect(sub_title, cars) ~ "cars",                        ### via lists
                                str_detect(sub_title, hobbies) ~ "hobbies/interests",
                                str_detect(sub_title, sport) ~ "sports",                  
                                str_detect(sub_title, gaming) ~ "gaming",
                                str_detect(sub_title, tipps) ~ "tipps",
                                str_detect(sub_title, occupation) ~ "occupation",
                                str_detect(sub_title, tech) ~ "tech",
                                str_detect(sub_title, covid) ~ "covid",
                                str_detect(sub_title, leftwing) ~ "leftwing",
                                str_detect(sub_title, finance) ~ "investment/crypto",
                                str_detect(sub_title, people) ~ "people",
                                str_detect(sub_title, drugs) ~ "drugs",
                                str_detect(sub_title, religion) ~ "religion",
                                str_detect(sub_title, nsfw) ~ "nsfw",
                                str_detect(sub_title, military) ~ "military",
                                str_detect(sub_title, anger) ~ "anger",
                                str_detect(sub_title, nice_stuff) ~ "nice_stuff",
                                str_detect(sub_title, memes) ~ "memes",
                                str_detect(sub_title, entertainment) ~ "entertainment")) %>%
  mutate(sub_groups = case_when(str_detect(sub_title, "conspiracy|againsttheilluminati") ~ "conspiracy",       ###via pattern recognition
                                str_detect(sub_title, c("gun|ar15|ak47|mp5|m1911|glock|ccw|nfa|sks|2aliberals")) ~ "guns",
                                str_detect(sub_title, "freedomconvoy") ~ "extreme right",
                                str_detect(sub_title, "^ask|^ama$|amihot|amitheasshole|answers|^advice$") ~ "ask subreddits",
                                str_detect(sub_title, "conservative") ~ "pro-conservative",
                                str_detect(sub_title, "republican") ~ "pro-republican",
                                str_detect(sub_title, "libertarian|antigroyperaktion") ~ "libertarian",
                                str_detect(sub_title, "anarcho-cap|ancap") ~ "anarcho-capitalism",
                                str_detect(sub_title, "anarchy|anarchis") ~ "anarchy",
                                str_detect(sub_title, c("aznidentity|asianident|asiansoc|asianmasc")) ~ "asian",
                                TRUE ~ sub_groups)) %>%
    mutate(sub_groups = case_when(sub_title == "eve" ~ "gaming",                         ## via individual words
                                  TRUE ~ sub_groups))

#specific word list
armin <- armin %>%
  mutate(sub_groups = case_when(sub_title %in% c("askreddit", "zoomies", "memes", "pics",
                                                 "interestingasfuck", "unpopularopinion", 
                                                 "facepalm", "dankmemes", "polls", 
                                                 "damnthatsinteresting", "nextfuckinglevel",
                                                 "mildlyinfuriating", "holup", "funny", 
                                                 "teenagers", "crazyfuckingvideos", "unexpected", 
                                                 "shitposting") ~ "general, non-pol",
                                sub_title %in% c("politics", "worldnews", "worldnewsvideo", 
                                                 "politicalcompassmemes", "worldpolitics",
                                                 "news", "politicalhumor", "worldpolitics2",
                                                 "anime_titties", "anythinggoesnews", "autonewspaper",
                                                 "uspolitics") ~ "general, pol",
                                sub_title %in% c("world_politics", "wikileaks", "australianpolitics", 
                                                 "americanpolitics", "anonymous") ~ "pol",
                                sub_title == "free_market_anarchism" ~ "anarcho-capitalism",
                                sub_title %in% places ~ "places",
                                sub_title %in% discussion ~ "discussion",
                                sub_title %in% c("yourmomshousepodcast", "youshouldknow",
                                                 "yesyesyesno", "yesyesyesyesno", "kyle",
                                                 "menwritingwomen") ~ "misc",
                                sub_title %in% support ~ "support",
                                sub_title %in% schadenfreude ~ "schadenfreude",
                                sub_title %in% c("bad_cop_no_donut") ~ "anti-cop",
                                TRUE ~ sub_groups))
  
#check specific
unique(armin$sub_title[armin$sub_groups == "ask subreddits"])

#save unassigned
xls <- armin %>%
  filter(is.na(sub_groups)) %>%
  filter(!data.subreddit %in% sub_list) %>%
  select(sub_title, comment_count) %>%
  distinct() %>%
  arrange(sub_title) %>%
  mutate(url = paste0("https://www.reddit.com/r/",sub_title))

write.xlsx(xls, file = "../ungrouped_sublist.xlsx") 

#save unassigned (>=75)
xls_75 <- armin %>%
  filter(is.na(sub_groups)) %>%
  filter(!data.subreddit %in% sub_list) %>%
  select(sub_title, comment_count) %>%
  distinct() %>%
  arrange(sub_title) %>%
  mutate(url = paste0("https://www.reddit.com/r/",sub_title))

write.xlsx(xls_75, file = "../ungrouped_sublist.xlsx")   
    
'#notes for later:
lotuseaters_com << right wing stuff
zeducationsubmissions << right wing stuff
sounddoctrine << religious right
savingcanada << extreme right wing 
enoughcommiespam << right wing stuff
YUROP << pro europe meme subreddit
americabad << pro american subreddit
anticomaction << anti communist
anticommie << anti communist
antivegan << anti vegan
antiwar << antiwar
antiwoke << right wing stuff
anythingpolitical << right wing stuff
ADVChina << anti china subreddit?
arethestraightsok <<making fun of fragile heterosexuality
bidenbordercrisis <<right wing stuff
bidenisfinished << right wing stuff
bidenregret << right wing stuff
bidenwatch << right wing stuff
bigdongdesantis << right wing stuff
tolerantleft << right wing stuff
the_chocker << right wing stuff
the_chocker << right wing stuff
the_congress << right wing stuff
the_mueller << right wing stuff
the_seattle << right wing stuff'
