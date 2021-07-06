#99519

library(readr)
library(tidyverse)
library(rvest)
library(ggseas)

#library(ggmap)

raw <- read_csv("https://data.wprdc.org/datastore/dump/044f2016-1dfd-4ab0-bc1e-065da05fca2e", col_names = T)

# ok project narrative focusing on if/how pandemic affected crime so from jan last yr to now would be good

df <- subset(raw, INCIDENTTIME >= "2020-01-01" & INCIDENTTIME < "2021-06-01")

#toy <- subset(raw, INCIDENTTIME >= "2021-05-01")

##TODO adjust time frame to df instead of toy

# toy.timefreq <- subset(raw, INCIDENTTIME >= "2019-06-01" & INCIDENTTIME < "2021-06-01") %>% mutate(date = format(INCIDENTTIME, format="%Y-%m-%d")) %>%
#   select(date) %>% group_by(date) %>% summarize(n = n()) %>% mutate(date = as.Date(date))
# ggsdc(toy.timefreq, aes(date, n), frequency = 52, s.window = 7) + geom_line() +
#   labs(title="Seasonal Decomposition of Criminal Offense Incidents",
#        x = "Date", y = "Number of Incidents",
#        subtitle = "June 01 2019 - May 31 2021\nWeekly frequency")

timefreq <- df %>% mutate(date = format(INCIDENTTIME, format="%Y-%m-%d")) %>%
  select(date) %>% group_by(date) %>% summarize(n = n()) %>% mutate(date = as.Date(date))
my.sdc <- ggsdc(timefreq, aes(date, n), frequency = 52, s.window = 7) + geom_line() +
    labs(title="Seasonal Decomposition of Criminal Offense Incidents",
         x = "Date", y = "Number of Incidents",
         subtitle = "Jan 2020 - May 2021\nWeekly frequency")
my.sdc

df %>% select(INCIDENTNEIGHBORHOOD, INCIDENTTIME) %>% 
  group_by(INCIDENTNEIGHBORHOOD) %>% summarize(hoodTotal = n()) -> num.by.hood
num.hood.med = median(num.by.hood$hoodTotal)

#### EDA stuff I guess man whatever
incidents.hist <- ggplot(num.by.hood, aes(x = hoodTotal)) + geom_histogram(binwidth = 500, col = "black", fill = "grey80") +
  theme(panel.background = element_blank(), axis.ticks = element_blank(), panel.grid.major.y = element_line(color = "grey90", size = 0.3)) +
  labs(x = "Total Incidents", title = "Distribution of Total Incidents per Neighborhood", subtitle = "Jan 2020 - May 2021")
incidents.hist

top.hoods <- top_n(num.by.hood, 5, hoodTotal) %>% select(INCIDENTNEIGHBORHOOD) %>% unlist
  
incidents.time <- df %>% select(INCIDENTNEIGHBORHOOD, INCIDENTTIME) %>% 
  mutate(date = format(INCIDENTTIME, format="%Y-%m")) %>% 
  group_by(date, INCIDENTNEIGHBORHOOD) %>% summarize(n = n()) %>% filter(INCIDENTNEIGHBORHOOD %in% top.hoods) %>%
  mutate(date = lubridate::ym(date)) %>%
  ggplot(aes(x = date, y = n, color = INCIDENTNEIGHBORHOOD)) + geom_line(size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = as.Date(c('2020-01-01','2021-05-01'))) +
  scale_color_discrete(breaks = c("Central Business District", "South Side Flats", "Carrick", "East Liberty", "Brookline")) + 
  labs(x = "", y = "", title = "Incidents over Time of\nNeighborhoods with Top 5 Total Incidents", subtitle = "Jan 2020 - May 2021") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.background = element_blank(), panel.grid.major = element_line(color = "grey90", size = 0.3)) +
  guides(colour = guide_legend(nrow=3))
incidents.time
#### Line graph over time by top n neighborhoods
# n (depending on histogram. there's 99 neighborhoods aiya)


##TODO: idk about this method because it leaves out some vehicular and drug related offenses
# e.g. no drug related offenses in that title, 37## is robbery in title 18 but vehicle crash in title 75
# there's a listing of (almost) all of the offenses here at
# https://www.pacodeandbulletin.gov/Display/pacode?file=/secure/pacode/data/204/chapter303/s303.15.html
# ugh... so need to hard-code some of it...
##UPDATE: whatever im just rollin with it


## Defining offense dictionary 
doc <- read_html("https://www.legis.state.pa.us/WU01/LI/LI/CT/HTM/18/18.HTM") %>% html_nodes("a") %>% html_text
mypattern = "(?<=Chapter )(\\d{1,2})\\.\\s+(.+)"
L <- doc %>% regexec(pattern=mypattern, perl=T) %>% regmatches(x=doc,  m=.)
offenses <- L[lapply(L, length)>0] %>% lapply(function(x){x[-1]}) %>% unlist %>% stringr::str_trim(.) %>% matrix(ncol=2, byrow=T) %>% data.frame
colnames(offenses) <- c("Chapter", "Offense")
offenses$Chapter <- as.numeric(offenses$Chapter)
#ok that's the basic stuff. Need to hard-code some
# e.g. 370# is robbery in Title 18, other 37## is vehicle
# 15 isn't in the Title 18 but is vehicle
offenses <- rbind(offenses, data.frame(
  Chapter = c(37, 15),
  Offense = c("Vehicle Law", "Vehicle Law")
))

# I think 13(a)(\d{1,2}) is drug-related. 13 in Title 18 is just some other stuff
offenses[which(offenses$Chapter == 13),] = c(Chapter = 13, Offense = "Drug Act")

myCols = c("HIERARCHY", "INCIDENTTIME", "INCIDENTLOCATION", "INCIDENTNEIGHBORHOOD", "INCIDENTZONE", "INCIDENTHIERARCHYDESC", "OFFENSES", "X", "Y")
df.off <- select(df, myCols)

df.off$offensesSplit <- df.off$OFFENSES %>% strsplit(" / ")
#now have column in data frame of lists that contain the 1+ offenses

col.ret = vector(mode = "list", length = length(df.off$OFFENSES))
for(ii in 1:length(df.off$OFFENSES)){
  #for each row
  vec = df.off$offensesSplit[[ii]]
  n = length(vec)
  #get the list of offenses of that row. could be just 1 offense, could be like 3
  #new column that's pretty much the same but instead of the offenses it's the chapter category
  ret = rep(NA, n)
  for(jj in 1:n){
    #aiieeee the nested for loop
    first.two.digits <- vec[jj] %>% substr(0, 2) %>% as.numeric
    #TODO: so the problem here is that it misses like 900 (not 4-digits) stuff... idk fix later whatever
    if(is.na(first.two.digits)){
      print("skipping non-numeric...")
      #print(df.off$offensesSplit[[ii]][jj])
      #ret[jj] = "Other Offenses"
      #all "Adult 21 or Older" and "1-401 Engaging in Fraudulent Practices"
    } else if(first.two.digits == 37){
      #370_ is robbery
      #else 37__ is vehicle
      third.digit <- vec[jj] %>% substr(2,3) %>% as.numeric
      if(third.digit == 0){
        #robbery
        ret[jj] = "Robbery"
      } else {
        #vehicle
        ret[jj] = "Vehicle Law"
      }
    } else {
      #not 37__ hardcoded stuff
      ind = which(offenses$Chapter == first.two.digits)
      if(length(ind) == 0){
        ret[jj] = "Other Offenses"
      } else {
        #print(offenses$Offense[ind])
        ret[jj] = offenses$Offense[ind]
      }
    }
  }
  #print(ret)
  col.ret[[ii]] = ret
}

#ok so now you can cbind col.ret to the dataframe or whatever
offenses.table <- col.ret %>% unlist %>% table %>% as.data.frame
colnames(offenses.table) <- c("Offense", "Freq")
offenses.table <- offenses.table[order(offenses.table$Freq, decreasing=T),]

offenses.hist <- offenses.table %>% filter(Offense != "Other Offenses") %>% top_n(5, Freq) %>% mutate(Offense = str_wrap(Offense, width = 10)) %>%
  ggplot(aes(x = reorder(Offense, -Freq), y = Freq)) + geom_bar(stat = "identity") +
  theme(panel.background = element_blank(), axis.ticks = element_blank(), panel.grid.major.y = element_line(color = "grey90", size = 0.3)) +
  labs(x = "", y = "", title = "Most Frequent Types of Offense by Category", subtitle = "Note that a single incident can have multiple offenses.")
offenses.hist
##idk what this is I forgot
# tiny.chapters <- tiny$OFFENSES %>% substr(0, 2) %>% as.numeric
# 
# incidents.chapters = rep(NA, 30)
# 
# for(ii in 1:length(tiny.chapters)){
#   inc.ch = tiny.chapters[ii]
#   ind = which(offenses$Chapter == inc.ch)
#   if(length(ind) > 0){
#     incidents.chapters[ii] = offenses$Offense[ind]
#   } else {
#     incidents.chapters[ii] = "Other"
#   }
# }
#  
# tiny$Chapter = incidents.chapters

## TODO: choose only the first offense and do some line graphs or whatever of those

#vector of just the first offenses.
#im assuming the first offense is what is the highest hierarchy offense and stuff in the dataset
#but I cbf to really check and make sure
first.off <- sapply(col.ret, "[[", 1)
first.off.tbl <- table(first.off) %>% as.data.frame
colnames(first.off.tbl) <- c("Offense", "Freq")
first.off.tbl <- first.off.tbl[order(first.off.tbl$Freq, decreasing=T),]

first.off.hist <- first.off.tbl %>% filter(Offense != "Other Offenses" & Offense != "Crime Victims") %>% top_n(5, Freq) %>% mutate(Offense = str_wrap(Offense, width = 10)) %>%
  ggplot(aes(x = reorder(Offense, -Freq), y = Freq)) + geom_bar(stat = "identity") +
  theme(panel.background = element_blank(), axis.ticks = element_blank(), panel.grid.major.y = element_line(color = "grey90", size = 0.3)) +
  labs(x = "", y = "", title = "Most Frequent Type of Offense by Category", subtitle = "First (Highest Hierarchy) Offense Only")
first.off.hist

first.off.df <- cbind(df, first.off) %>% select(INCIDENTTIME, INCIDENTNEIGHBORHOOD, first.off)

top.off.v <- as.vector(top_n(first.off.tbl, 7, Freq)$Offense)
#wtf is the "crime victims" category... its stuff like "found property" "dead on arrival" so I'm just going to ignore it...
top.off.v <- top.off.v[top.off.v != "Other Offenses" & top.off.v != "Crime Victims"]


first.off.time <- first.off.df %>% 
  filter(first.off != "Other Offenses" | first.off != "Crime Victims") %>%
  mutate(date = format(INCIDENTTIME, format="%Y-%m")) %>% 
  group_by(date, first.off) %>% summarize(n = n()) %>% filter(first.off %in% top.off.v) %>%
  mutate(date = lubridate::ym(date)) %>%
  ggplot(aes(x = date, y = n, color = first.off)) + geom_line(size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = as.Date(c('2020-01-01','2021-05-01'))) +
  scale_color_discrete(breaks = top.off.v) + 
  labs(x = "", y = "", title = "Incidents over Time by Category\nTop 5 Categories", subtitle = "Jan 2020 - May 2021") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.background = element_blank(), panel.grid.major = element_line(color = "grey90", size = 0.3)) +
  guides(colour = guide_legend(nrow=3))
first.off.time

# ggsave(filename = "SDC.png", plot = my.sdc)
# ggsave(filename = "Incidents-Histogram.png", plot = incidents.hist)
ggsave(filename = "Incidents-Linegraph.png", plot = incidents.time)
# ggsave(filename = "Offenses-Histogram.png", plot = offenses.hist)
ggsave(filename = "First-Offense-Hist.png", plot = first.off.hist)
ggsave(filename = "First-Offense-Line.png", plot = first.off.time)
