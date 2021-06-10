# Data on lack of diversity in EEB seminar speakers:

# Load packages:
library(tidyverse)
library(googlesheets4)
library(scales)
library(ggpubr)
theme_set(theme_pubr())

# Read in the data:
SeminarData <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/13dwxf62YImVuZAHzNZkeFKsveQF4a3C5ZB3QDL_ZULQ/edit?usp=sharing")
SeminarData$Nonwhite <- SeminarData$Race != 'white'
SeminarData <- unite(SeminarData, Intersection, Nonwhite, Gender, sep = "_", remove = FALSE)
SeminarData$WOC <- SeminarData$Intersection == 'TRUE_Woman'
SeminarData$WOC <-  ifelse(grepl("TRUE_Woman", SeminarData$Intersection), "Woman of color", "Other identity")
SeminarData <- unite(SeminarData, DetailedIntersection, Race, Gender, sep = "_", remove = FALSE)
SeminarData$Old <- SeminarData$Year != '2020_S2'
SeminarData$Race <- factor(SeminarData$Race, levels = c('Black', 'Asian', 'Latinx', 'unsure', 'white'))

# Create a dataframe with percentages for each race by semester:
RaceDataWide <- count(SeminarData, Race, Year) %>% spread(Race, n, fill = 0)
RaceDataWide <- RaceDataWide %>% remove_rownames %>% column_to_rownames(var="Year")
RaceDataWide$Total <- rowSums(RaceDataWide)

BlackRepresentation <- SeminarData
BlackRepresentation$Black <- BlackRepresentation$Race == 'Black'

RaceDataWideTotal <- count(SeminarData, Race) %>% spread(Race, n, fill = 0)
RaceDataWideTotal$Total <- rowSums(RaceDataWideTotal)

# Create a dataframe with percentages for each gender by semester:
GenderDataWide <- count(SeminarData, Gender, Year) %>% spread(Gender, n, fill = 0)
GenderDataWide <- GenderDataWide %>% remove_rownames %>% column_to_rownames(var="Year")
GenderDataWide$Total <- rowSums(GenderDataWide)

GenderDataWideTotal <- count(SeminarData, Gender) %>% spread(Gender, n, fill = 0)
GenderDataWideTotal$Total <- rowSums(GenderDataWideTotal)

# Create a dataframe with percentages for each intersection of identities by semester:
IntersectionDataWide <- count(SeminarData, Intersection, Year) %>% spread(Intersection, n, fill = 0)
IntersectionDataWide <- IntersectionDataWide %>% remove_rownames %>% column_to_rownames(var="Year")
IntersectionDataWide$Total <- rowSums(IntersectionDataWide)

IntersectionDataWideTotal <- count(SeminarData, Intersection) %>% spread(Intersection, n, fill = 0)
IntersectionDataWideTotal$Total <- rowSums(IntersectionDataWideTotal)

# Make some visualizations of the data:
RaceBarGraph <- ggplot(data = SeminarData) + geom_bar(aes(x = Year, fill = factor(Race)), position = "fill") +
  scale_fill_manual(values=c("#C8553D", "#F28F3B", "#FFD5C2", "#CCCCCC", "#588B8b")) + geom_hline(yintercept = 0.60) + geom_text(aes(0, 0.60, label = 'Proportion of US population that is white', vjust = -1, hjust = 0)) + labs(x = "Time", y = "Proportion of speakers", title = "Racial Background of Seminar Speakers") + guides(fill=guide_legend(title=NULL)) + theme(axis.text.x = element_text(angle = 90))
plot(RaceBarGraph)

RaceLineGraph <- ggplot(data = reorder(SeminarData, -Race)) + geom_line(aes(x = Year, group = Race, color = Race), stat = "count")
plot(RaceLineGraph)

BlackBarGraph <- ggplot(data = BlackRepresentation) + geom_bar(aes(x = Year, fill = factor(Black)), position = "fill") + geom_hline(yintercept = 0.134)
plot(BlackBarGraph)

GenderBarGraph <- ggplot(data = SeminarData) + 
  geom_bar(aes(x = Year, fill = factor(Gender)), position = "fill") +
  scale_fill_manual(values=c("#FFD5C2", "#C8553D")) + 
  labs(x = "Time", y = "Proportion of speakers", title = "Proportion of speakers who are women") + 
  guides(fill=guide_legend(title=NULL)) + 
  theme(axis.text.x = element_text(angle = 90))
plot(GenderBarGraph)

IntersectionBarGraph <- ggplot(data = SeminarData) + geom_bar(aes(x = Year, fill = factor(WOC)), position = "fill")  +
  scale_fill_manual(values=c("#CCCCCC", "#588B8b")) + geom_hline(yintercept = 0.18) + labs(x = "Time", y = "Proportion of speakers", title = "Proportion of speakers who are \nwomen of color") + guides(fill=guide_legend(title=NULL)) + theme(axis.text.x = element_text(angle = 90))
plot(IntersectionBarGraph)

# Make a faceted plot of the three datasets we care most about:
HorizontalCombined <-ggarrange(RaceBarGraph, GenderBarGraph, IntersectionBarGraph, ncol = 3, nrow = 1)
plot(HorizontalCombined)
ggsave("SeminarData.png", width = 14, height = 6)

VerticalCombined <- ggarrange(RaceBarGraph, GenderBarGraph, IntersectionBarGraph, ncol = 1, nrow = 3)
plot(VerticalCombined)
