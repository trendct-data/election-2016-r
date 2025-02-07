
y2012 <- read.csv("data/2012_election_results.csv", stringsAsFactors=F)
y2012 <- y2012[1:169,]

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

p2012 <- y2012
p2012$Romney <- round(y2012$Romney/(y2012$Romney+y2012$Obama+y2012$Anderson+ y2012$Johnson)*100,2)
p2012$Obama <- round(y2012$Obama/(y2012$Romney+y2012$Obama+y2012$Anderson+ y2012$Johnson)*100,2)
p2012$Anderson <- round(y2012$Anderson/(y2012$Romney+y2012$Obama+y2012$Anderson+ y2012$Johnson)*100,2)
p2012$Johnson <- round(y2012$Johnson/(y2012$Romney+y2012$Obama+y2012$Anderson+ y2012$Johnson)*100,2)


tot <- left_join(y2012, total_results)
per <- left_join(p2012, pres_results)

per$TCdiff <- per$Trump-per$Clinton
per$ROdiff <- per$Romney-per$Obama

per$COdiff <- per$Clinton-per$Obama
per$TRdiff <- per$Trump-per$Romney

# margins
library(knitr)

tot$vote_diff <- tot$Clinton-tot$Trump
tot <- arrange(tot, vote_diff)
tot <- subset(tot, (vote_diff<6) & (vote_diff > -6))
tot <- subset(tot, winner!="Results not ready")
kable(tot)

# where Clinton lead the most

per_margins <- arrange(per, TCdiff)
per_margins <- subset(per_margins, winner!="Results not ready")

clinton_lead <- head(per_margins, 5)
kable(clinton_lead)

trump_lead <- tail(per_margins, 5)
kable(trump_lead)


## Obama versus Clinton

library(ggalt)

per$Trump[per$Trump == 0.00] <- NA
per$Clinton[per$Clinton == 0.00] <- NA

per <- subset(per, !is.na(per$Clinton))

# start here

per <- arrange(per, Clinton)
per$Town <- factor(per$Town, levels=unique(per$Town))

per$arrange_hc <- ifelse(per$Clinton < 50 & per$Clinton < per$Obama, per$Clinton - 8, 0)
per$arrange_hc <- ifelse(per$Clinton < 50 & per$Clinton > per$Obama, per$Obama - 8, per$arrange_hc)
per$arrange_hc <- ifelse(per$Clinton > 50 & per$Clinton > per$Obama, per$Clinton + 8, per$arrange_hc)
per$arrange_hc <- ifelse(per$Clinton > 50 & per$Clinton < per$Obama, per$Obama + 8, per$arrange_hc)

gg <- ggplot()
gg <- gg + geom_dumbbell(data=per, aes(x=Clinton, xend=Obama, y=Town, group=Town), color="#a3c4dc", size=0.5, point.colour.l="#0e668b")
# gg <- gg + scale_x_continuous(label=percent)
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + scale_x_continuous(limits = c(0, 110))
gg <- gg + theme_bw()
gg <- gg + theme(axis.title = element_text(family = "Lato Black", color="#666666", face="bold", size=6)) 
gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.major.x=element_line())
gg <- gg+ geom_vline(xintercept = 50)
gg <- gg+ geom_vline(xintercept = 20, linetype="dotted", colour="lightgray")
gg <- gg+ geom_vline(xintercept = 40, linetype="dotted", colour="lightgray")
gg <- gg+ geom_vline(xintercept = 60, linetype="dotted", colour="lightgray")
gg <- gg+ geom_vline(xintercept = 80, linetype="dotted", colour="lightgray")

gg <- gg + theme(axis.ticks=element_blank())
#gg <- gg + theme(axis.text = element_text(size = 4))
gg <- gg + labs(title = "Support for Clinton and Obama",
                subtitle= "Percent of votes in 2012 compared to 2016",
                caption="Office of the Secretary of the State \n Andrew Ba Tran/TrendCT.org")
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=19))
gg <- gg + theme(plot.subtitle=element_text(family="Lato Regular", size=17))
gg <- gg + theme(plot.caption=element_text(family="Lato Regular", size=12, color="gray", margin=margin(t=10, r=80)))
gg <- gg + annotate("text", x = 13.2, y = 150, label = "Barack Obama", size=3, colour="gray30")
gg <- gg + annotate("text", x = 12.8, y = 153, label = "Hillary Clinton", size=3, colour="gray30")
gg <- gg + annotate("point", x = 3, y = 150, colour = "#a3c4dc", size = 2) 
gg <- gg + annotate("point", x = 3, y = 153, colour = "#0e668b", size = 2)
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.text.y = element_text(family="Lato Regular", size=12))
gg <- gg + theme(legend.position="none")
gg <- gg + theme(axis.line =  element_blank(),
                 axis.text.y =  element_blank(),
                 axis.ticks =  element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 legend.title=element_blank()) 
gg <- gg + geom_text(data=per, aes(x=arrange_hc, y=Town,  family="Lato", label=Town), size=2)

ggsave(gg, file="output/hillary_obama_1.png", width=6, height=12, type="cairo-png")

ftp_url <- paste0("ftp://", ftp_login, "@secure.ctmirror.org/projects.ctmirror.org/content/trend/2016/11/election/analysis/hillary_obama_1.png")
ftpUpload("output/hillary_obama_1.png", ftp_url)

# OK, alphabetical

per <- arrange(per, desc(as.character(Town)))
per$Town <- factor(per$Town, levels=unique(per$Town))

per$arrange_hc <- ifelse(per$Clinton < 50 & per$Clinton < per$Obama, per$Clinton - 8, 0)
per$arrange_hc <- ifelse(per$Clinton < 50 & per$Clinton > per$Obama, per$Obama - 8, per$arrange_hc)
per$arrange_hc <- ifelse(per$Clinton > 50 & per$Clinton > per$Obama, per$Clinton + 8, per$arrange_hc)
per$arrange_hc <- ifelse(per$Clinton > 50 & per$Clinton < per$Obama, per$Obama + 8, per$arrange_hc)


gg <- ggplot(per, aes(x=Clinton, xend=Obama, y=Town, group=Town))
gg <- gg + geom_dumbbell(color="#a3c4dc", size=0.5, point.colour.l="#0e668b")
# gg <- gg + scale_x_continuous(label=percent)
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + scale_x_continuous(limits = c(0, 110))
gg <- gg + theme_bw()
gg <- gg + theme(axis.title = element_text(family = "Lato Black", color="#666666", face="bold", size=6)) 
gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.major.x=element_line())
gg <- gg+ geom_vline(xintercept = 50)
gg <- gg+ geom_vline(xintercept = 20, linetype="dotted", colour="lightgray")
gg <- gg+ geom_vline(xintercept = 40, linetype="dotted", colour="lightgray")
gg <- gg+ geom_vline(xintercept = 60, linetype="dotted", colour="lightgray")
gg <- gg+ geom_vline(xintercept = 80, linetype="dotted", colour="lightgray")

gg <- gg + theme(axis.ticks=element_blank())
#gg <- gg + theme(axis.text = element_text(size = 4))
gg <- gg + labs(title = "Support for Clinton and Obama",
                subtitle= "Percent of votes in 2012 compared to 2016",
                caption="Office of the Secretary of the State \n Andrew Ba Tran/TrendCT.org")
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=19))
gg <- gg + theme(plot.subtitle=element_text(family="Lato Regular", size=17))
gg <- gg + theme(plot.caption=element_text(family="Lato Regular", size=12, color="gray", margin=margin(t=10, r=80)))

gg <- gg + annotate("text", x = 13.2, y = 150, label = "Barack Obama", size=3, colour="gray30")
gg <- gg + annotate("text", x = 12.8, y = 153, label = "Hillary Clinton", size=3, colour="gray30")
gg <- gg + annotate("point", x = 3, y = 150, colour = "#a3c4dc", size = 2) 
gg <- gg + annotate("point", x = 3, y = 153, colour = "#0e668b", size = 2)
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.text.y = element_text(family="Lato Regular", size=12))
gg <- gg + theme(legend.position="none")
gg <- gg + theme(axis.line =  element_blank(),
                 axis.text.y =  element_blank(),
                 axis.ticks =  element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 legend.title=element_blank()) 
gg <- gg + geom_text(aes(x=arrange_hc, y=Town,  family="Lato", label=Town), size=2)

ggsave(gg, file="output/hillary_obama_2.png", width=6, height=12, type="cairo-png")
ftp_url <- paste0("ftp://", ftp_login, "@secure.ctmirror.org/projects.ctmirror.org/content/trend/2016/11/election/analysis/hillary_obama_2.png")
ftpUpload("output/hillary_obama_2.png", ftp_url)

# Trump versus Romney

# start here

per <- arrange(per, Trump)
per$Town <- factor(per$Town, levels=unique(per$Town))

per$arrange_hc <- ifelse(per$Trump < 50 & per$Trump < per$Romney, per$Trump - 8, 0)
per$arrange_hc <- ifelse(per$Trump < 50 & per$Trump > per$Romney, per$Romney - 8, per$arrange_hc)
per$arrange_hc <- ifelse(per$Trump > 50 & per$Trump > per$Romney, per$Trump + 8, per$arrange_hc)
per$arrange_hc <- ifelse(per$Trump > 50 & per$Trump < per$Romney, per$Romney + 8, per$arrange_hc)

gg <- ggplot()
gg <- gg + geom_dumbbell(data=per, aes(x=Trump, xend=Romney, y=Town, group=Town), color="tomato1", size=0.5, point.colour.l="red4")
# gg <- gg + scale_x_continuous(label=percent)
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + scale_x_continuous(limits = c(0, 110))
gg <- gg + theme_bw()
gg <- gg + theme(axis.title = element_text(family = "Lato Black", color="#666666", face="bold", size=6)) 
gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.major.x=element_line())
gg <- gg+ geom_vline(xintercept = 50)
gg <- gg+ geom_vline(xintercept = 20, linetype="dotted", colour="lightgray")
gg <- gg+ geom_vline(xintercept = 40, linetype="dotted", colour="lightgray")
gg <- gg+ geom_vline(xintercept = 60, linetype="dotted", colour="lightgray")
gg <- gg+ geom_vline(xintercept = 80, linetype="dotted", colour="lightgray")

gg <- gg + theme(axis.ticks=element_blank())
#gg <- gg + theme(axis.text = element_text(size = 4))
gg <- gg + labs(title = "Support for Trump and Romney",
                subtitle= "Percent of votes in 2012 compared to 2016",
                caption="Office of the Secretary of the State \n Andrew Ba Tran/TrendCT.org")
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=19))
gg <- gg + theme(plot.subtitle=element_text(family="Lato Regular", size=17))
gg <- gg + theme(plot.caption=element_text(family="Lato Regular", size=12, color="gray", margin=margin(t=10, r=80)))
gg <- gg + annotate("text", x = 11.8, y = 150, label = "Mitt Romney", size=3, colour="gray30")
gg <- gg + annotate("text", x = 12.8, y = 153, label = "Donald Trump", size=3, colour="gray30")
gg <- gg + annotate("point", x = 3, y = 150, colour = "tomato1", size = 2) 
gg <- gg + annotate("point", x = 3, y = 153, colour = "red4", size = 2)
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.text.y = element_text(family="Lato Regular", size=12))
gg <- gg + theme(legend.position="none")
gg <- gg + theme(axis.line =  element_blank(),
                 axis.text.y =  element_blank(),
                 axis.ticks =  element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 legend.title=element_blank()) 
gg <- gg + geom_text(data=per, aes(x=arrange_hc, y=Town,  family="Lato", label=Town), size=2)

ggsave(gg, file="output/trump_romney_1.png", width=6, height=12, type="cairo-png")

ftp_url <- paste0("ftp://", ftp_login, "@secure.ctmirror.org/projects.ctmirror.org/content/trend/2016/11/election/analysis/trump_romney_1.png")
ftpUpload("output/trump_romney_1.png", ftp_url)

# OK, alphabetical

per <- arrange(per, desc(as.character(Town)))
per$Town <- factor(per$Town, levels=unique(per$Town))

per$arrange_hc <- ifelse(per$Trump < 50 & per$Trump < per$Romney, per$Trump - 8, 0)
per$arrange_hc <- ifelse(per$Trump < 50 & per$Trump > per$Romney, per$Romney - 8, per$arrange_hc)
per$arrange_hc <- ifelse(per$Trump > 50 & per$Trump > per$Romney, per$Trump + 8, per$arrange_hc)
per$arrange_hc <- ifelse(per$Trump > 50 & per$Trump < per$Romney, per$Romney + 8, per$arrange_hc)


gg <- ggplot(per, aes(x=Trump, xend=Romney, y=Town, group=Town))
gg <- gg + geom_dumbbell(color="tomato1", size=0.5, point.colour.l="red4")
# gg <- gg + scale_x_continuous(label=percent)
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + scale_x_continuous(limits = c(0, 110))
gg <- gg + theme_bw()
gg <- gg + theme(axis.title = element_text(family = "Lato Black", color="#666666", face="bold", size=6)) 
gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.major.x=element_line())
gg <- gg+ geom_vline(xintercept = 50)
gg <- gg+ geom_vline(xintercept = 20, linetype="dotted", colour="lightgray")
gg <- gg+ geom_vline(xintercept = 40, linetype="dotted", colour="lightgray")
gg <- gg+ geom_vline(xintercept = 60, linetype="dotted", colour="lightgray")
gg <- gg+ geom_vline(xintercept = 80, linetype="dotted", colour="lightgray")

gg <- gg + theme(axis.ticks=element_blank())
#gg <- gg + theme(axis.text = element_text(size = 4))
gg <- gg + labs(title = "Support for Trump and Romney",
                subtitle= "Percent of votes in 2012 compared to 2016",
                caption="Office of the Secretary of the State \n Andrew Ba Tran/TrendCT.org")
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=19))
gg <- gg + theme(plot.subtitle=element_text(family="Lato Regular", size=17))
gg <- gg + theme(plot.caption=element_text(family="Lato Regular", size=12, color="gray", margin=margin(t=10, r=80)))

gg <- gg + annotate("text", x = 11.8, y = 150, label = "Mitt Romney", size=3, colour="gray30")
gg <- gg + annotate("text", x = 12.8, y = 153, label = "Donald Trump", size=3, colour="gray30")
gg <- gg + annotate("point", x = 3, y = 150, colour = "tomato1", size = 2) 
gg <- gg + annotate("point", x = 3, y = 153, colour = "red4", size = 2)
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.text.y = element_text(family="Lato Regular", size=12))
gg <- gg + theme(legend.position="none")
gg <- gg + theme(axis.line =  element_blank(),
                 axis.text.y =  element_blank(),
                 axis.ticks =  element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 legend.title=element_blank()) 
gg <- gg + geom_text(aes(x=arrange_hc, y=Town,  family="Lato", label=Town), size=2)

ggsave(gg, file="output/trump_romney_2.png", width=6, height=12, type="cairo-png")
ftp_url <- paste0("ftp://", ftp_login, "@secure.ctmirror.org/projects.ctmirror.org/content/trend/2016/11/election/analysis/trump_romney_2.png")
ftpUpload("output/trump_romney_2.png", ftp_url)


# By town type

urban <- read.csv("data/urban_rural.csv", stringsAsFactors=FALSE)
urban <- urban[c("NAME10", "Type")]
colnames(urban) <- c("Town", "Type")

per <- left_join(per, urban)
total_results <- left_join(total_results, urban)

town_type_analysis <-per %>%
   select(Town, Type, Trump, Clinton) %>%
   gather("candidate", "percent", 3:4)

gg <- ggplot(town_type_analysis, aes(x = Type, y = percent, fill = candidate)) 
gg <- gg + geom_boxplot() 
gg <- gg + scale_fill_manual(values= c("Clinton" = "lightskyblue", "Trump"="tomato"))
gg <- gg + labs(x=NULL, y=NULL, 
                title="Presidential support by town type",
                subtitle="Trump got the rural voters in Connecticut while voters in \nurban cities supported Clinton",
                caption="Source: Office of the Secretary of the State \n Andrew Ba Tran/TrendCT.org")
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=24))
gg <- gg + theme(plot.subtitle=element_text(family="Lato Regular", size=17))
gg <- gg + theme(plot.caption=element_text(family="Lato Regular", size=12, color="gray", margin=margin(t=10, r=80)))
gg <- gg + theme(axis.text.x = element_text(family="Lato Regular", size=15))
gg <- gg + theme(axis.line =  element_blank(),
                 axis.ticks =  element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 legend.title=element_blank()) 
gg


# By whiteness of town

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

race_towns_white <- race_towns_long %>%
  filter(race_ethnicity=="White") %>%
  select(town, percent_population)

colnames(race_towns_white) <- c("Town", "Percent_White")

per <- left_join(per, race_towns_white)

per2 <- per %>%
  mutate(Minorities=100-Percent_White) %>%
  select(id, Minorities, Clinton, Trump) %>%
  gather("candidate", "percent", 3:4)
  
gg <- ggplot(per, aes(Percent_White, Trump))
gg <- gg + geom_point()
gg <- gg + labs(x="Percent white population", y="Percent votes for Trump", 
                title="Trump votes compared to white population",
                subtitle="The whiter a town, the higher likelihood they supported Donald Trump.",
                caption="Source: Office of the Secretary of the State \n Andrew Ba Tran/TrendCT.org")
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=24))
gg <- gg + theme(plot.subtitle=element_text(family="Lato Regular", size=17))
gg <- gg + theme(plot.caption=element_text(family="Lato Regular", size=12, color="gray", margin=margin(t=10, r=80)))
gg <- gg + theme(axis.line =  element_blank(),
                 axis.ticks =  element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 legend.title=element_blank()) 
gg <- gg + theme_bw()
gg

per2 <- per %>%
  mutate(Minorities=100-Percent_White) %>%
  select(id, Percent_White, Clinton, Trump) %>%
  gather("candidate", "percent", 3:4)

#%>%
#  filter(percent > 50)
# Swapped?

gg <- ggplot()
gg <- gg + geom_point(data=per2, aes(Percent_White, percent, color=candidate))
gg <- gg + scale_color_manual(values= c("lightskyblue", "tomato"))
gg <- gg + facet_wrap(~candidate)
gg <- gg + scale_x_continuous(limits = c(0, 110))
gg <- gg + scale_y_continuous(limits = c(0, 110))
gg <- gg + labs(x="Percent white population", y="Percent of votes", 
                title="Votes compared to white population",
                subtitle="The larger the white population, the higher the likelihood they supported Donald Trump.",
                caption="Source: Office of the Secretary of the State \n Andrew Ba Tran/TrendCT.org")
gg <- gg + theme_bw(base_family="Lato Regular")
gg <- gg + theme(axis.ticks.y=element_blank())
#gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.key=element_blank())
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=22))
gg <- gg + theme(plot.caption=element_text(face="bold", family="Lato Regular", size=9, color="gray28", margin=margin(t=10, r=80)))
gg <- gg + theme(legend.position="none")
gg <- gg + theme(strip.background=element_blank())
gg <- gg + theme(strip.text.x = element_text(size = 10, colour = "grey5"))

gg
ggsave(gg, file="output/white_population.png", width=8, height=5, type="cairo-png")
ftp_url <- paste0("ftp://", ftp_login, "@secure.ctmirror.org/projects.ctmirror.org/content/trend/2016/11/election/analysis/white_population.png")
ftpUpload("output/white_population.png", ftp_url)

## Income?



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

race_towns_white <- race_towns_long %>%
  filter(race_ethnicity=="White") %>%
  select(town, percent_population)

colnames(race_towns_white) <- c("Town", "Percent_White")

per <- left_join(per, race_towns_white)

per2 <- per %>%
  mutate(Minorities=100-Percent_White) %>%
  select(id, Minorities, Clinton, Trump) %>%
  gather("candidate", "percent", 3:4)

```

## Flip map


per$winner2012 <- ifelse(per$Obama>per$Romney, "Obama", "Romney")
per$flip <- ""
per$flip <- ifelse(per$winner=="Clinton" & per$winner2012=="Obama", "Clinton retained", per$flip)
per$flip <- ifelse(per$winner=="Trump" & per$winner2012=="Romney", "Trump retained", per$flip)
per$flip <- ifelse(per$winner=="Trump" & per$winner2012=="Obama", "Flipped to Trump", per$flip)
per$flip <- ifelse(per$winner=="Clinton" & per$winner2012=="Romney", "Flipped to Clinton", per$flip)

town_shape <- readOGR(dsn="maps", layer="ctgeo")
town_shape_df <- fortify(town_shape, region="NAME10")

names(per)[names(per) == 'Town'] <- 'id'


voters_map <- left_join(town_shape_df,per)

gg <- ggplot(voters_map, aes(long,lat, group=group))
gg <- gg + geom_polygon(aes(fill=flip))
gg <- gg + geom_path(color="white")
gg <- gg + scale_fill_manual(values= c("Clinton retained"="lightskyblue", "Flipped to Clinton" = "navyblue", "Trump retained"="tomato", "Flipped to Trump"="red4"))
gg <- gg + labs(x=NULL, y=NULL, 
                title="Where towns chose or flipped for Clinton or Trump",
                #                title="Presidential results by town",
                #                subtitle="Where candidates are currently leading in votes. Results are unofficial.",
                subtitle="Compared to whether Obama or Romney won in 2012",
                caption="Source: Office of Secretary of the State \nAndrew Ba Tran/TrendCT.org")
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

#ggsave(gg, file="output/pres_map.png", width=8, height=4, type="cairo-png")