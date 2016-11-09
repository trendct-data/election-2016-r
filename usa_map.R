# US Map generator
library(albersusa)
library(rgeos)
library(maptools)
library(ggplot2)
library(ggalt)
library(ggthemes)
library(dplyr)
library(magick)


date_time <- Sys.time()
date_time <- gsub(" EDT", "", date_time)
date_time <- gsub(" ", "", date_time)
date_time <- gsub("-", "", date_time)
date_time <- gsub(":", "", date_time)

us_results <- read.csv("https://docs.google.com/spreadsheets/d/1toi7RJluTCvzRd_IeUc0uc93nrvs6VV9Sof9nR3zaeo/pub?output=csv", stringsAsFactors=F)
us_map <- fortify(usa_composite(), region="name")

us_map <- left_join(us_map, us_results)

us_map$Winner <- ifelse(us_map$Winner=="", "Results not ready", us_map$Winner)
us_map$Winner <- factor(us_map$Winner, levels = c("Clinton", "Trump", "Tie", "Results not ready"))

# 
# gg <- ggplot()
# gg <- gg + geom_map(data=us_map, map=us_map,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.15, fill="#e5e3df")
# gg <- gg + coord_map("polyconic")
# gg <- gg + theme_map()
# gg
# 


gg <- ggplot(us_map, aes(long,lat, group=group))
gg <- gg + geom_polygon(aes(fill=Winner))
gg <- gg + geom_path(color="white", size=.25)
gg <- gg + scale_fill_manual(values= c("Results not ready"="gray84", "Clinton" = "lightskyblue", "Trump"="tomato", "Tie"="gray50"))
#gg <- gg + geom_path(color = "white") 
gg <- gg + labs(x=NULL, y=NULL, 
                title="Presidential results",
                subtitle="Where winners have been called.",
                caption="Source: The Associated Press")
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
gg <- gg + coord_map("polyconic")

gg
ggsave(gg, file="output/us_pres_map.png", width=9, height=4, type="cairo-png")



# Image merge time
# 
# trend_logo <- image_read("trend_masthead.png")
# 
# pres_map_edit <- image_read("output/us_pres_map.png") %>%
#   image_crop("2200x1200+250") %>%
#   image_composite(image_scale(trend_logo, "x50"), offset="+130+1130") %>%
#   image_scale("x700") %>%
#   image_write(path="output/us_pres_map.png", format="png")

electoral <- us_results %>%
  group_by(Winner) %>%
  summarise(Electoral_Votes=sum(Electoral.Votes, na.rm=T))

leftover <- subset(electoral, Winner=="")
leftover_count <- leftover$Electoral_Votes

electoral <- electoral %>%
  filter(Winner=="Clinton" | Winner=="Trump")

electoral$Electoral_Votes[2] <- 264

gg <- ggplot(electoral, aes(x = Winner, y = Electoral_Votes, fill = Winner)) 
gg <- gg + geom_bar(stat = "identity")
gg <- gg + geom_hline(yintercept=270)
gg <- gg + annotate("text", x = 1.5, y = 280, family="Lato Regular", size=6, label = "270 Electoral Votes to win")
gg <- gg + geom_text(aes(x=Winner, y=Electoral_Votes, ymax=Electoral_Votes, family="Lato", label=Electoral_Votes, 
                         vjust=ifelse(Electoral_Votes<255, -.5, 1.7), size = 3), 
                     position = position_dodge(width=1))
gg <- gg + scale_fill_manual(values= c("Stein"="lightgreen", "Johnson"="#ffec80", "Clinton" = "lightskyblue", "Trump"="tomato"))
gg <- gg + labs(x=NULL, y=NULL, 
                title=NULL)
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=24))
gg <- gg + theme(plot.subtitle=element_text(family="Lato Regular", size=17))
gg <- gg + theme(plot.caption=element_text(family="Lato Regular", size=12, color="gray", margin=margin(t=10, r=80)))
gg <- gg + theme(axis.text.x = element_text(family="Lato Regular", size=15))
gg <- gg + theme(legend.position="none")
gg <- gg + theme(axis.line =  element_blank(),
                 axis.text.y =  element_blank(),
                 axis.ticks =  element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 legend.title=element_blank()) 
gg

ggsave(gg, file="output/us_pres_chart.png", width=4, height=4, type="cairo-png")
ggsave(gg, file=paste0("output/archive/us_pres_chart", date_time, ".png"), width=4, height=4, type="cairo-png")


image_background <- image_read("background.png")


trend_logo <- image_read("trend_masthead.png")
pres_map <- image_read("output/us_pres_map.png") %>%
  image_crop("1700x1200+380")

pres_chart <- image_read("output/us_pres_chart.png")

pres_map_edit <- image_read("background.png") %>%
  image_composite(pres_map) %>%
  image_composite(image_scale(pres_chart, "x1000"), offset="+1325+100") %>%
  image_scale("x700") %>%
  image_write(path="output/us_pres_map.png", format="png")

pres_map_edit <- image_read("background.png") %>%
  image_composite(pres_map) %>%
  image_composite(image_scale(pres_chart, "x1000"), offset="+1325+100") %>%
  image_scale("x700") %>%
  image_write(path=paste0("output/archive/us_pres_map", date_time, ".png"), format="png")

source("keys.R")

ftp_url <- paste0("ftp://", ftp_login, "@secure.ctmirror.org/projects.ctmirror.org/content/trend/2016/11/election/social_images/pres_map.png")
ftp_url_archive <-  paste0("ftp://", ftp_login, "@secure.ctmirror.org/projects.ctmirror.org/content/trend/2016/11/election/social_images/archive/us_pres_map", date_time, ".png")

#ftpUpload("output/us_pres_map.png", ftp_url)
ftpUpload(paste0("output/archive/us_pres_map", date_time, ".png"), ftp_url_archive)


if (electoral$Electoral_Votes[1]>=270) {
  cat("Hillary Clinton has reached", electoral$Electoral_Votes[1], "electoral votes and has won the election.
      Donald Trump has so far reached", electoral$Electoral_Votes[2], "but still can't win even if he gets the remaining", leftover_count, ".")
} else if (electoral$Electoral_Votes[2]>=270) {
  cat("Donald Trump has reached", electoral$Electoral_Votes[2], "electoral votes and has won the election.
      Hillary Clinton has so far reached ", electoral$Electoral_Votes[1], "but still can't win even if he gets the remaining", leftover_count, ".")
} else {
  cat("Donald Trump has reached", electoral$Electoral_Votes[2], "electoral votes and Hillary Clinton has so far reached", electoral$Electoral_Votes[1], "electoral votes. 
      There are", leftover_count, "electoral votes still up for grabs.")
}

#setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
#tweet("Trump currently leads in electoral votes but the night is still young bit.ly/2f4jbWP", mediaPath = "output/us_pres_map.png")
