#This file will be about Cowboy Bebop. More specifically, about their bounties.
Cowboy_Bebop_File <- read.csv("...:/.../.../Documents/Data Work/Cowboy Bebop/Cowboy_Bebop_Bounty.csv")
Cowboy_Bebop_File

#Loading Cowboy Bebop Logo File onto R
library(grid)
library(jpeg)
Cowboy_Bebop_Poster <- jpeg::readJPEG("...:/.../.../Documents/Data Work/Cowboy Bebop/Cowboy Bebop Logo.jpg")
library(ggplot2)


#Capture Results
Bounty_Fate_OriginalDF <- data.frame(Fate = Cowboy_Bebop_File$Result)
Bounty_Fate_OriginalDF
Bounty_Fate_CleanedUpDF <- na.omit(Bounty_Fate_OriginalDF)
Bounty_Fate_CleanedUpDF

#Bounty_FateOriginalDF is the unedited data frame directly taken from the csv file
#Bounty_FateCleanedUpDF is the edited data frame. NAs were removed.

Result_Count <- table(Bounty_Fate_CleanedUpDF)
Result_Count

Fate_DF <- data.frame(Fate = Result_Count)
Fate_DF

Bounty_Fate <- ggplot(data = Fate_DF, mapping = aes(Fate.Bounty_Fate_CleanedUpDF, Fate.Freq)) + 
  xlab("Fate") + ylab("Number of Bounties") + ggtitle("Cowboy Bebop: Fate of Every Bounty Hunt \n Done By the Bebop Crew") +
  geom_bar(data = Fate_DF,inherit.aes = TRUE, position = "stack", stat = "identity", col = 'black', fill = 'yellow') + 
  theme(plot.title = element_text(size = 12)) + scale_y_continuous(breaks = seq(0,11,1)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  geom_text(label = Fate_DF$Fate.Freq, vjust = 1, size = 3) + 
  annotation_custom(rasterGrob(Cowboy_Bebop_Poster, width = unit(1,"npc"), height = unit(1,"npc"), interpolate = FALSE),1,5,6,10) +
  annotate("text", x = 0.75, y = 3.0, label = "Created by Iftiar Rana", hjust= -.475, vjust= -1, col="red", cex= 3.5, fontface = "bold", alpha = 1)

Bounty_Fate

#Reward Collected?
Reward_Collected_DF <- data.frame(Reward_Collected = Cowboy_Bebop_File$Reward.Collected.)
Reward_Collected_DF
Reward_Collected_CleanDF <- na.omit(Reward_Collected_DF)
Reward_Collected_CleanDF

Reward_Collect_Count <- table(Reward_Collected_CleanDF)
Reward_Collect_Count

Reward_Collection_DF <- data.frame(Collected_Reward = Reward_Collect_Count)

Reward_Collected <- ggplot(data = Reward_Collection_DF, mapping = aes(Collected_Reward.Reward_Collected_CleanDF, Collected_Reward.Freq)) + 
  xlab("Possibilities") + ylab("Number of Times") + ggtitle("Was The Reward \n On the Bounty Collected?") +
  geom_bar(data = Reward_Collection_DF,inherit.aes = TRUE, position = "stack", stat = "identity", col = 'black', fill = 'yellow') + 
  theme(plot.title = element_text(size = 12)) + scale_y_continuous(breaks = seq(0,20,1)) +
  annotation_custom(rasterGrob(Cowboy_Bebop_Poster, width = unit(1,"npc"), height = unit(1,"npc"), interpolate = FALSE),1.5,2.5,14,17) + 
  annotate("text", x = 1.5, y = 11, label = "Created by Iftiar Rana", hjust= -.475, vjust= -1, col="red", cex= 3.5, fontface = "bold", alpha = 1)

Reward_Collected

#Location
Bounty_Location_DF <- data.frame(Location = Cowboy_Bebop_File$Location)
Bounty_Location_DF <- na.omit(Bounty_Location_DF)
Bounty_Location_DF

LocationCount <- table(Bounty_Location_DF)
LocationCount

LocationCount_DF <- data.frame(Location = LocationCount)
LocationCount_DF

Bounty_Location <- ggplot(data = LocationCount_DF, mapping = aes(Location.Bounty_Location_DF, Location.Freq)) + xlab("Location") + ylab("Number of Bounties There") +
  ggtitle("Cowboy Bebop: Bounty Locations") + geom_bar(data = LocationCount_DF, inherit.aes = TRUE, position = "stack", stat = "identity", col = 'black', fill = 'yellow') +
  scale_y_continuous(breaks = seq(0,11,1)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=.85)) + 
  annotation_custom(rasterGrob(Cowboy_Bebop_Poster, width = unit(1,"npc"), height = unit(1,"npc"), interpolate = FALSE),1,4,4.5,8) + 
  annotate("text", x = 6, y = 6, label = "Created by Iftiar Rana", hjust= -.475, vjust= -1, col="red", cex= 3.5, fontface = "bold", alpha = 1)

Bounty_Location

#Crimes Bounties Done 
Crime_Frame <- data.frame(Crimes = Cowboy_Bebop_File$Crime.s.)
Crime_Frame <- na.omit(Crime_Frame)
Crime_Frame

CrimeCount <- table(Crime_Frame)
CrimeCount

CrimeCount_DF <- data.frame(Crime = CrimeCount)

Crime <- ggplot(data = CrimeCount_DF, mapping = aes(CrimeCount_DF$Crime.Crime_Frame, CrimeCount_DF$Crime.Freq)) + 
  xlab("Crimes Committed") + ylab("Number of Bounties \n Who Committed This Crime") + 
  ggtitle("Crimes That Landed People on The Bounty Hunt") + geom_bar(data = CrimeCount_DF, inherit.aes = TRUE, position = 'stack', stat = 'identity', col = 'black', fill = 'yellow') + 
  scale_y_continuous(breaks = seq(0,7,1)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + annotation_custom(rasterGrob(Cowboy_Bebop_Poster, width = unit(1,"npc"), height = unit(1,"npc"), interpolate = FALSE),0.5,5,2.25,4.25) + 
  annotate("text", x = 9, y = 3, label = "Created by Iftiar Rana", hjust= -.475, vjust= -1, col="red", cex= 3.5, fontface = "bold", alpha = 1)
Crime

#Bounty Worth
Bounty_Worth_DF <- data.frame(Worth = Cowboy_Bebop_File$Reward_Millions)
Bounty_Worth_DF<- Bounty_Worth_DF[(!(Bounty_Worth_DF$Worth=="nill") & !(Bounty_Worth_DF$Worth=="Unknown")),]
library(tibble)
Bounty_Worth_DF <- enframe(Bounty_Worth_DF, name = 'name', value = 'value')
Bounty_Worth_DF
Bounty_Worth_DF$value <- as.numeric(as.character(Bounty_Worth_DF$value))
cats <- table(cut(Bounty_Worth_DF$value, breaks = seq(0, 40, 5)))
fortyplus <- sum(Bounty_Worth_DF$value>40)

dat <- data.frame(ranges=c(names(cats), "40+"),
                  count=c(as.numeric(cats), fortyplus))
ggplot(dat) + geom_bar(aes(x=reorder(ranges,-count), y=count), col = 'black', fill = 'yellow', stat = "identity") + 
  xlab("Bounty Reward (in Milions of Woolongs (???)") + ylab("Number of Bounties") + 
  ggtitle("How Much Was The Bounty Worth?") + scale_y_continuous(breaks = seq(0,13,1)) + 
  annotate("text", x = 1.5, y = 11, label = "Created by Iftiar Rana", hjust= -.475, vjust= -1, col="red", cex= 5, fontface = "bold", alpha = 1) + 
  annotation_custom(rasterGrob(Cowboy_Bebop_Poster, width = unit(1,"npc"), height = unit(1,"npc"), interpolate = FALSE),2,5,5,9) 
