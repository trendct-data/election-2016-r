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
library(magick)
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



town_shape <- readOGR(dsn="maps", layer="ctgeo")
town_shape_df <- fortify(town_shape, region="NAME10")
voters_map <- left_join(town_shape_df, pres_results)

voters_map$color <- ""
voters_map$color <- ifelse(is.na(voters_map$winner) | voters_map$winner=="Results not ready", "gray84", voters_map$color)
voters_map$color <- ifelse(voters_map$winner=="", "gray50", voters_map$color)

voters_map$color <- ifelse(voters_map$winner=="Trump", "tomato", voters_map$color)
voters_map$color <- ifelse(voters_map$winner=="Clinton", "lightskyblue", voters_map$color)

# theme_opts <- list(theme(panel.grid.minor = element_blank(),
#                          panel.grid.major = element_blank(),
#                          panel.background = element_blank(),
#                          plot.background = element_rect(fill="#e6e8ed"),
#                          panel.border = element_blank(),
#                          axis.line = element_blank(),
#                          axis.text.x = element_blank(),
#                          axis.text.y = element_blank(),
#                          axis.ticks = element_blank(),
#                          axis.title.x = element_blank(),
#                          axis.title.y = element_blank(),
#                          plot.title = element_text(size=22)))
# voters_map$color <- NULL

time_is <- Sys.time()
hour_is <-  hour(time_is)
minute_is <- minute(time_is)
time_e <- ifelse(hour_is>12, "p.m.", "a.m.")
time_e <- ifelse(time_is==0 | time_is==24, "a.m.", time_e)
hour_is <- ifelse(hour_is>12, hour_is-12, hour_is)
time_e <- ifelse(hour_is==0, "a.m.", time_e)
hour_is <- ifelse(hour_is==0, 12, hour_is)

the_time <- paste0(hour_is, ":", minute_is, " ", time_e)

# plot map
gg <- ggplot(voters_map, aes(long,lat, group=group))
gg <- gg + geom_polygon(aes(fill=winner))
gg <- gg + geom_path(color="white")
gg <- gg + scale_fill_manual(values= c("Results not ready"="gainsboro", "Clinton" = "lightskyblue", "Trump"="tomato", "Tie"="gray50"))
gg <- gg + labs(x=NULL, y=NULL, 
                title="Where candidates lead",
#                title="Presidential results by town",
#                subtitle="Where candidates are currently leading in votes. Results are unofficial.",
                subtitle=paste0("Unofficial votes as of ", the_time),
                caption="Source: Office of Secretary of the State")
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=24))
gg <- gg + theme(plot.subtitle=element_text(family="Lato Regular", size=17))
gg <- gg + theme(plot.caption=element_text(family="Lato Regular", size=12, color="gray", margin=margin(t=10, r=80)))
gg <- gg + theme(axis.line =  element_blank(),
                 axis.text =  element_blank(),
                 axis.ticks =  element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 legend.title=element_blank()) 
gg <- gg + coord_map("sinusoidal") 

gg

ggsave(gg, file="output/pres_map.png", width=8, height=4, type="cairo-png")

date_time <- Sys.time()
date_time <- gsub(" EDT", "", date_time)
date_time <- gsub(" ", "", date_time)
date_time <- gsub("-", "", date_time)
date_time <- gsub(":", "", date_time)

ggsave(gg, file=paste0("output/archive/pres_map", date_time, ".png"), width=8, height=4, type="cairo-png")

# + theme_opts


#setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
#tweet("Boston Common", mediaPath = "map.png")

# Let's randomize
# 
# pres_results$`Clinton and Kaine` <- sample(1:100, 1, replace=TRUE)
# pres_results$`Trump and Pence` <- sample(1:100, 1, replace=TRUE)
# pres_results$`Johnson and Weld` <- sample(1:100, 1, replace=TRUE)
# pres_results$`Stein and Baraka` <- sample(1:100, 1, replace=TRUE)


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




chart_results <- total_results %>%
  select(id, `Clinton and Kaine`, `Trump and Pence`, `Johnson and Weld`, `Stein and Baraka`) %>%
  summarise(Clinton=sum(`Clinton and Kaine`, na.rm=T), Trump=sum(`Trump and Pence`), Johnson=sum(`Johnson and Weld`, na.rm=T), Stein=sum(`Stein and Baraka`)) %>%
  gather(candidate, votes, 1:4)

chart_results$candidate <- factor(chart_results$candidate, levels = chart_results$candidate[order(chart_results$votes)])

ymid <- mean(range(chart_results$votes))



gg <- ggplot(chart_results, aes(x=candidate, y=votes, color=NULL, fill=candidate))
gg <- gg + geom_bar(stat="identity", position="dodge")
gg <- gg + geom_text(aes(x=candidate, y=votes, ymax=votes, family="Lato", label=prettyNum(votes, big.mark=","), 
                         hjust=ifelse(votes < ymid, -0.1, 1.1), size = 3), 
                      position = position_dodge(width=1))
gg <- gg + coord_flip()
gg <- gg + scale_fill_manual(values= c("Stein"="lightgreen", "Johnson"="#ffec80", "Clinton" = "lightskyblue", "Trump"="tomato"))
gg <- gg + labs(x=NULL, y=NULL, 
                title=NULL)
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=24))
gg <- gg + theme(plot.subtitle=element_text(family="Lato Regular", size=17))
gg <- gg + theme(plot.caption=element_text(family="Lato Regular", size=12, color="gray", margin=margin(t=10, r=80)))
gg <- gg + theme(axis.text.y = element_text(family="Lato Regular", size=12))
gg <- gg + theme(legend.position="none")
gg <- gg + theme(axis.line =  element_blank(),
                 axis.text.x =  element_blank(),
                 axis.ticks =  element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 legend.title=element_blank()) 
gg

ggsave(gg, file="output/pres_chart.png", width=4.7, height=4, type="cairo-png")
ggsave(gg, file=paste0("output/archive/pres_chart", date_time, ".png"), width=4.7, height=4, type="cairo-png")


image_background <- image_read("background.png")


trend_logo <- image_read("trend_masthead.png")
pres_map <- image_read("output/pres_map.png") %>%
  image_crop("1700x1200+360")
pres_chart <- image_read("output/pres_chart.png")

pres_map_edit <- image_read("background.png") %>%
  image_composite(pres_map) %>%
  image_composite(image_scale(pres_chart, "x1000"), offset="+1225+180") %>%
  image_scale("x700") %>%
  image_write(path="output/pres_map.png", format="png")

pres_map_edit2 <- image_read("background.png") %>%
  image_composite(pres_map) %>%
  image_composite(image_scale(pres_chart, "x1000"), offset="+1225+180") %>%
  image_scale("x700") %>%
  image_write(path=paste0("output/archive/pres_map", date_time, ".png"), format="png")

# FTP UPLOADING


source("keys.R")

ftp_url <- paste0("ftp://", ftp_login, "@secure.ctmirror.org/projects.ctmirror.org/content/trend/2016/11/election/social_images/pres_map.png")
ftp_url_archive <-  paste0("ftp://", ftp_login, "@secure.ctmirror.org/projects.ctmirror.org/content/trend/2016/11/election/social_images/archive/pres_map", date_time, ".png")

ftpUpload("output/pres_map.png", ftp_url)
ftpUpload(paste0("output/archive/pres_map", date_time, ".png"), ftp_url_archive)

