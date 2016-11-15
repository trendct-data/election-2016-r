
source("keys.R")
library(censusapi)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(scales)
library(ggplot2)
library(ggalt)
library(stringr)
library(rgdal)
require(maptools)
library(RCurl)
library(twitteR)
library(lubridate)
election_data_url <- "https://s3.amazonaws.com/election-data-2016/data/Electiondata_1.json"

lookup_url <- "https://s3.amazonaws.com/election-data-2016/data/Lookupdata_1.json"


election_data <- fromJSON(election_data_url, simplifyVector=F)
#town_results <- data.frame((election_data[[6]]))
town_results <- data.frame((election_data[[7]]))
town_results <- data.frame(t(town_results))
town_results$details <- rownames(town_results) 
rownames(town_results) <- NULL
town_results$type <- ""

town_results$type <- ifelse(grepl(".TO", town_results$details), "percent_votes", "votes")
town_results$candidate <- town_results$details
town_results$candidate <- gsub(".TO", "", town_results$candidate)
town_results$candidate <- gsub(".V", "", town_results$candidate)
town_results$town <- gsub("\\..*", "", town_results$candidate)
town_results$town <- gsub("X", "", town_results$town)
town_results$candidate <- gsub(".*\\.", "", town_results$candidate)
town_results$details <- NULL
town_results$number <- town_results$t.town_results.
town_results$t.town_results. <- NULL

lookup_data <- fromJSON(lookup_url, simplifyVector=F)
town_id <- data.frame((lookup_data[[10]]))
town_id <- data.frame(t(town_id))
town_id$town <- rownames(town_id)
rownames(town_id) <- NULL
town_id$town <- gsub("X", "", town_id$town)

town_results <- left_join(town_results, town_id)

candidate_id <- data.frame((lookup_data[[3]]))
candidate_id <- data.frame(t(candidate_id))
candidate_id$details <- rownames(candidate_id)
candidate_id$cat <- ""
candidate_id$cat <- ifelse(grepl(".CO", candidate_id$details), "county", candidate_id$cat)
candidate_id$cat <- ifelse(grepl(".NM", candidate_id$details), "name", candidate_id$cat)
candidate_id$cat <- ifelse(grepl(".LN", candidate_id$details), "last", candidate_id$cat)
candidate_id$cat <- ifelse(grepl(".MN", candidate_id$details), "middle", candidate_id$cat)
candidate_id$cat <- ifelse(grepl(".P", candidate_id$details), "party", candidate_id$cat)
candidate_id$cat <- ifelse(grepl(".FN", candidate_id$details), "first", candidate_id$cat)
candidate_id$cat <- ifelse(grepl(".AD", candidate_id$details), "address", candidate_id$cat)
candidate_id$candidate <- gsub("\\..*", "", candidate_id$details)
candidate_id$candidate <- gsub("X", "", candidate_id$candidate)
candidate_id$details <- NULL
candidate_id$text <- candidate_id$t.candidate_id.
candidate_id$t.candidate_id. <- NULL
candidate_id <- spread(candidate_id, cat, text)

town_results <- left_join(town_results, candidate_id)


party_id <- data.frame((lookup_data[[7]]))
party_id <- data.frame(t(party_id))
party_id$details <- rownames(party_id)
rownames(party_id) <- NULL
party_id$category <- ""
party_id$category <- ifelse(grepl(".P", party_id$details), "party_p", party_id$category)
party_id$category <- ifelse(grepl(".NM", party_id$details), "party_name", party_id$category)
party_id$category <- ifelse(grepl(".CD", party_id$details), "party_abbr", party_id$category)
party_id$party <- gsub("\\..*", "", party_id$details)
party_id$party <- gsub("X", "", party_id$party)

party_id$details <- NULL
party_id <- spread(party_id, category, t.party_id.)

town_results <- left_join(town_results, party_id)



pres_results <- filter(town_results, type=="percent_votes") %>%
  filter(first=="Donald" | first=="Hillary" | first=="Jill" | name=="Johnson and Weld") %>%
  select(number, t.town_id., name)

colnames(pres_results) <- c("percent", "id", "candidate")
pres_results$percent <- gsub("%", "", pres_results$percent)
pres_results$percent <- as.numeric(pres_results$percent )
pres_results <- pres_results  %>%
  spread(candidate, percent)


pres_results$winner <- ""
pres_results$winner <- ifelse((pres_results$`Clinton and Kaine` ==  pres_results$`Trump and Pence`), "Tie", pres_results$winner)
pres_results$winner <- ifelse((pres_results$`Clinton and Kaine`==0 & pres_results$`Trump and Pence`==0), "Results not ready", pres_results$winner)
pres_results$winner <- ifelse((pres_results$`Clinton and Kaine` >  pres_results$`Trump and Pence`), "Clinton", pres_results$winner)
pres_results$winner <- ifelse((pres_results$`Clinton and Kaine` <  pres_results$`Trump and Pence`), "Trump", pres_results$winner)
pres_results$winner <- factor(pres_results$winner, levels = c("Clinton", "Trump", "Tie", "Results not ready"))


total_results <- filter(town_results, type=="votes") %>%
  filter(first=="Donald" | first=="Hillary" | first=="Jill" | name=="Johnson and Weld") %>%
  select(number, t.town_id., name)

colnames(total_results) <- c("percent", "id", "candidate")
total_results$percent <- gsub("%", "", total_results$percent)
total_results$percent <- as.numeric(total_results$percent )
total_results <- total_results  %>%
  spread(candidate, percent)

total_results$winner <- ""
total_results$winner <- ifelse((total_results$`Clinton and Kaine` ==  total_results$`Trump and Pence`), "Tie", total_results$winner)
total_results$winner <- ifelse((total_results$`Clinton and Kaine`==0 & total_results$`Trump and Pence`==0), "Results not ready", total_results$winner)
total_results$winner <- ifelse((total_results$`Clinton and Kaine` >  total_results$`Trump and Pence`), "Clinton", total_results$winner)
total_results$winner <- ifelse((total_results$`Clinton and Kaine` <  total_results$`Trump and Pence`), "Trump", total_results$winner)
total_results$winner <- factor(total_results$winner, levels = c("Clinton", "Trump", "Tie", "Results not ready"))


colnames(total_results) <- c("Town", "Clinton", "Johnson16", "Stein", "Trump", "winner")
colnames(pres_results) <- c("Town", "Clinton", "Johnson16", "Stein", "Trump", "winner")

#total results!

total_town_stuff <- town_results %>%
  filter(type=="votes") %>%  
  filter(first=="Donald" | first=="Hillary" | first=="Jill" | name=="Johnson and Weld") %>%
  group_by(town) %>%
  summarise(total_votes=sum(as.numeric(as.character(number)), na.rm=T)) %>%
  left_join(town_id)

total_town_stuff$total_votes[167] <- 7395+7130+500+200
total_town_stuff$total_votes[14] <- 4492+3802+254+151


# registered

reg <- read.csv("data/reg2016.csv")
reg <- reg %>%
  group_by(town) %>%
  summarise(registered=sum(registered2016, na.rm=T))


total_town_stuff$town <- NULL
colnames(total_town_stuff) <- c("total_votes", "town")
total_town_stuff <- left_join(reg, total_town_stuff)

total_town_stuff$turnout <- round(total_town_stuff$total_votes/total_town_stuff$registered*100,2)


# Hispanic stuff


race_towns <- getCensus(name="acs5",
                        vintage=2014,
                        key=census_key,
                        vars=c("NAME", "B02001_001E", "B02001_002E", "B02001_003E",
                               "B02001_004E", "B02001_005E", "B03001_003E"),
                        region="county subdivision:*", regionin="state:09")

colnames(race_towns) <- c("town", "state", "county", "countysub", "total_pop", "White", "Black", "Indian", "Asian", "Hispanic")
race_towns <- race_towns[c("town", "total_pop", "White", "Black", "Indian", "Asian", "Hispanic")]
race_towns <- subset(race_towns, !grepl("County subdivisions", town))
race_towns$town <- gsub(" town.*", "", race_towns$town)

race_towns_long <- race_towns %>%
  gather("race_ethnicity", "population", 3:7) %>%
  mutate(percent_population=round(population/total_pop*100,2))

race_towns_hispanic <- race_towns_long %>%
  filter(race_ethnicity=="Hispanic") %>%
  select(town, percent_population)

colnames(race_towns_hispanic) <- c("town", "Percent_Hispanic")

total_town_stuff <- left_join(total_town_stuff, race_towns_hispanic)

pres_results2 <- pres_results
colnames(pres_results2) <- c("town", "clinton", "johnson", "stein", "trump", "winner")
total_town_stuff <- left_join(total_town_stuff, pres_results2)

# 2012 voter participation

t2012 <- read.csv("data/turnout.csv", stringsAsFactors=F)

t2012 <- left_join(t2012, total_town_stuff)

t2012$diff <- t2012$turnout-t2012$Percent


# By town type

urban <- read.csv("data/urban_rural.csv", stringsAsFactors=FALSE)
urban <- urban[c("NAME10", "Type")]
colnames(urban) <- c("town", "Type")

t2012 <- left_join(t2012, urban)

t2012 %>%
  group_by(winner) %>%
  summarise(average=mean(diff, na.rm=T))

t2012 %>%
  group_by(winner, Type) %>%
  summarise(average=mean(diff, na.rm=T))

t2012$hispanic_group <- ifelse(t2012$Percent_Hispanic=<10, "group 1", "")
t2012$hispanic_group <- ifelse(t2012$Percent_Hispanic<10, "group 1", "")