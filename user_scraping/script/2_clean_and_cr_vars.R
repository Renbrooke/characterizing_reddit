'
In this script:
- reduce dataset to analysis sample
- identify other extreme right subreddits in subreddit list (?)
- create relevant vars for analysis
'

library(tidyverse)
library(openxlsx)

sub_list = c("AHomeForPlagueRats", "Anarcho_Capitalism",  
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

sub_list_reduced = c("AskThe_Donald", "BidenBuzz", "BidenIsNotMyPresident",
                     "BreitbartNews", "CabalCrusher", "DarkMAGA", "DNCleaks", 
                     "FightingFakeNews", "FreedomConvoy2022", "libsofreddit", 
                     "MensRights", "non_msm", "NPCMemes", 
                     "QuiteFrankly", "Red_Suppository", "RedPillWomen", "rittenhouse",
                     "The_Farage", "TheBidenshitshow", "TheTrumpZone","trump", 
                     "Trumpgret", "UNAgenda21", "walkaway")


final_data <- readRDS(file = "../data/final_data.rds") 

data <- final_data %>%
  select(-contains("source_")) %>%
  filter(extreme_pol == 1) %>%              ## reduced to clearly extreme-right wing subreddits, no auxilliary stuff
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
# changing to >=75 posts in order to simplify, but this leaves only 48 users
#data <- data %>%
#  filter(posts_all >= 25)  #%>%
  #filter(comment_count >= 10)

# save subreddit list 
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


# data_grouped will use more or less manual coding with some pattern recognition
cars <- c("4x4|acura|alfa-romeo|aston-martin|audi|bentley|bmw|bugatti|buick|cadillac|chevrolet|chrysler|citroen|dodge|ferrari|fiat|ford|gmc|honda|hyundai|infiniti|jaguar|jeep|kia|lamborghini|lancia|land rover|lexus|lincoln|maserati|maybach|mazda|mclaren|mercedes|mitsubishi|nissan|opel|pagani|peugeot|pontiac|porsche|renault|rolls-royce|skoda|subaru|suzuki|tesla|toyota|volkswagen|volvo|alpina|callaway|caparo|caterham|dacia|daihatsu|datsun|delage|delorean|deltawing racing cars|general motors|hummer|hyperion|lucid motors|morgan|mosler|mullen|nikola motor company|oldsmobile|polestar|radical sportscars|rezvani|rimac automobile|rinspeed|rivian|saab|saleen|shelby|spyker|ssangyong|studebaker|tata|touring superleggera|trident|triumph|tvr|ultima|vauxhall|venturi|vinfast|w motors|wiesmann|yamaha|zagato|zenvo|electricvehicles") #added electric vehicles
hobbies <- c("cigar|simulated|pedsr|youtube_startups|aquarium|aquaticsnails|^art$|airsoft
             |woodworking|woodstoving|unity3d|languagelearning|preppers|writing|cooking
             |aviation|backyardchickens|ballpython|fishing") #added fishing
music <- c("music|aliceinchains|musicproduction|zappa|edm|eminem|classicrock|guitars") #added classicrock, guitars
sport <- c("bicycling|soccer|nfl|ufc|baseketball|^nba$")
gaming <- c("btd6|pcmasterrace|witcher|gaming|xboxone|sonic|league|reddeadredemption|ghostrecon
            |acecombat|videogames|worldofwarships|rpg|aidungeon|battlefield3
            |battletech|valorant|fivenightsatfreddys|slaythespire|^amongus")
tipps <- c("travelhacks|coolguides|^travel$")
occupation <- c("engineering|auslaw|auslegal|supplychain|pharmacist")
tech <- c("beta|windows10|technology|android|androidapps|apolloapp|apple|adobeillustrator|buildapc|firefox") #added buildapc, firefox
covid <- c("antivax|nurembergtwo|^debatevaccines|^vacc|^vax|lockdownskeptic|realvaccinedebate|lockdowncrit|imdonewithcovid|corona|covid|plaguerats|actualscience")
leftwing <- c("fuckthealtwrong|therightcantmeme|badchoicesgoodstories|agorism|appalachistan|againsthatesubreddits|wayofthebern|antiwork|leftpodcasts")
finance <- c("^gme$|ethere|ethtrader|bitcoin|bbig|ausfinance|algotrading|crypto|wallstreet|market|invest|stonk|stock|asx_bets|finance") #added finance
people <- c("adamcarolla")
linfl <- c("vaushv|aoc")
rinfl <- c("brandonherrara|joerogan|timpool|jordanpeterson|timdillon
           |normmacdonald|ronpaul|daverubin|louderwithcrowder|benshapiro
           |stevencrowder")
entertainment <- c("^movies$|prequel|startrek|bikinibottomtwitter
                   |bettercallsaul|beavisandbutthead|attackontitan
                   |thelastairbender|gravityfalls|starwars|marvel
                   |xmen|amphibia|animaniacs|animemes|television
                   |strangerthings")
religion <- c("christ|relig|theis|awakened")
nsfw <- c("bigtitsinbikinis|bigboobsgw|^bbw$|bdsm|asianscuckingpinkies|^ass$")
drugs <- c("pamedicalmarijuana|chsinfo|trees|benzorecovery|benzodiazepines|altcannabinoids|pedsr|artofrolling|cannabisgrowers|drugs") #added artofrolling, cannabisgrowers
military <- c("army|combat|warvideo|navy")

places <- c("brasil", "sanfrancisco", "california", "arizona", 
            "ontario", "canada", "ontariocanada", "britishcolumbia", 
            "casualuk", "china", "france", "rochester", "rochester585",
            "albany", "argentina", "adelaide", "adirondacks",
            "auckland", "australia", "bayarea", "berkeley",
            "england", "europe", "calgary", "chicago", "cuba",
            "ontariotheprovince", "newhampshire", "missouri") #added calgary, chicago, cuba
discussion <- c("capitalismvsocialism", "nostupidquestions", "debate", 
                "debatereligion", "leftvsrightdebate") #isnt it all discussion? debate religion could just go to religion
support <- c("agoraphobia", "selfharm", "sex", "anxiety", "aspiememes", "adhd",
             "adhdmeme", "alcoholism", "autism", "depression", "chronicpain", "chronicillness",
             "alcoholism_medication") #(added depression, chronicpain, chronicillness) ## needs a better name, it's subreddits where people talk about private stuff like illness, sex and stuff and support each other
memes <- c("adviceanimals") ## this is a problem just using *meme* would take in too many political subreddits
#try to catch as much as possible with word searches
schadenfreude <- c("yesyesyesyesno", "yesyesyesno", "winstupidprizes", "thatlookedexpensive", 
                   "publicfreakout", "publicfreakouts", "publicfreakoutsreborn", 
                   "actualpublicfreakouts", "instantregret", "instantkarma", "robbersgettingfucked",
                   "averageredditor")
anger <- c("awfuleverything|idiotsincars")
nice_stuff <- c("beamazed|^aww$|cozyplaces") #added cozyplaces
science <- c("science", "computerscience", "compsci", "datascience", "everythingscience", "dataisbeautiful", "csmajors", "cscareerquestions") #added categories here, but did not implement in below code
threats <- c("crimeinnyc", "fightporn", "clevercomebacks") # I wanted to label this conflict but it is a base r function
dating <- c("dating", "datingoverthirty", "datingoverforty")
extreme <- c("lotuseaters_com", "zeducationsubmissions", "savingcanada", "antiwoke", "anythingpolitical",
             "bidenbordercrisis", "bidenisfinished", "bidenregret", "bidenwatch", "bigdongdesantis", 
             "tolerantleft", "the_chocker", "the_chocker", "the_congress", "the_mueller", "the_seattle",
             "freedomconvoy", "thedonaldtrump2024", "defund_npr_and_pbs", "deplatformed_", "theleftistshitshow",
             "breakingpointusa", "breakingpointnews", "infosecnews", "joebidenisadisaster")

sub_list_reduced <- tolower(sub_list_reduced)

data_grouped <- data %>%
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
                                str_detect(sub_title, rinfl) ~ "influencers, right",
                                str_detect(sub_title, linfl) ~ "influencers, left",
                                str_detect(sub-title, music) ~ "music",
                                str_detect(sub_title, entertainment) ~ "entertainment")) %>%
  mutate(sub_groups = case_when(str_detect(sub_title, "conspiracy|againsttheilluminati") ~ "conspiracy",       ###via pattern recognition
                                str_detect(sub_title, c("gun|ar15|ak47|mp5|m1911|glock|ccw|nfa|sks|2aliberals|9mm|22lr")) ~ "guns",
                                str_detect(sub_title, "^ask|^ama$|amihot|amitheasshole|answers|^advice$") ~ "ask subreddits",
                                str_detect(sub_title, "conservative") ~ "pro-conservative",
                                str_detect(sub_title, "republican") ~ "pro-republican",
                                str_detect(sub_title, "libertarian|antigroyperaktion") ~ "libertarian",
                                str_detect(sub_title, "anarcho-cap|ancap|anarcho_capitalism") ~ "anarcho-capitalism",
                                str_detect(sub_title, "anarchy|anarchis") ~ "anarchy",
                                str_detect(sub_title, c("aznidentity|asianident|asiansoc|asianmasc")) ~ "asian",
                                TRUE ~ sub_groups)) %>%
  mutate(sub_groups = case_when(sub_title == "eve" ~ "gaming",                         ## via individual words
                                TRUE ~ sub_groups))

#specific word list
data_grouped <- data_grouped %>%
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
                                sub_title %in% extreme ~ "extreme right",
                                sub_title %in% c("anticomaction", "enoughcommiespam", "anticommie") ~ "anti-communist",
                                sub_title %in% sub_list_reduced ~ "source subs",
                                sub_title %in% c("pnwconservatives", "conservativeterrorism") ~ "anti-conservatives",
                                TRUE ~ sub_groups))

#check specific
unique(data_grouped$sub_title[data_grouped$sub_groups == "pro-conservative"])

unique(data_grouped$sub_groups)

#save dataset
saveRDS(data_grouped, file = "../data/data_grouped.rds") 

#save unassigned
xls <- data_grouped %>%
  filter(is.na(sub_groups)) %>%
  filter(!sub_title %in% sub_list_reduced) %>%
  select(sub_title, comment_count) %>%
  distinct() %>%
  arrange(desc(comment_count)) %>%
  mutate(url = paste0("https://www.reddit.com/r/",sub_title))

write.xlsx(xls, file = "../ungrouped_sublist.xlsx") 

# added some items in notes below as well    
'#notes for later:
badchoicesgoodstories << left wing political stuff

#clearly right wing stuff:
"lotuseaters_com", "zeducationsubmissions", "savingcanada", "antiwoke", "anythingpolitical",
"bidenbordercrisis", "bidenisfinished", "bidenregret", "bidenwatch", "bigdongdesantis", 
"tolerantleft", "the_chocker", "the_chocker", "the_congress", "the_mueller", "the_seattle"

#anti communist
anticomaction, enoughcommiespam, anticommie

#religious stuff
sounddoctrine, 

lotuseaters_com << right wing stuff
zeducationsubmissions << right wing stuff
sounddoctrine << religious right
savingcanada << extreme right wing 
enoughcommiespam << right wing stuff
anticomaction << anti communist
anticommie << anti communist

antiwoke << right wing stuff
anythingpolitical << right wing stuff
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
the_seattle << right wing stuff

____ ABOVE IS DONE
ADVChina << anti china subreddit?
arethestraightsok <<making fun of fragile heterosexuality
antivegan << anti vegan
antiwar << antiwar
charlottesville << a place but also might be political?
YUROP << pro europe meme subreddit
americabad << pro american subreddit'
