library(dplyr)
library(ggplot2)
library(R.utils)
library(gridExtra)

data <- read.csv("Assessment2/repdata_data_StormData.csv")
dim(data)

data$EVTYPE <- tolower(data$EVTYPE)
data$EVTYPE <- factor(data$EVTYPE)
data$YEAR <- as.numeric(format(as.Date(data$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))

hist(data$YEAR, breaks = 30, xlab = "Year", main = "Events per Year")
dataSlice <- data[data$YEAR >= 1995, ]

dataGrouped <- group_by(dataSlice, EVTYPE)
fatalSum <- summarise(dataGrouped, FATALITIES=sum(FATALITIES))
fatalHead <- head(arrange(fatalSum, desc(FATALITIES)), 12)


injurySum <- summarise(dataGrouped, INJURIES=sum(INJURIES))
injuryHead <- head(arrange(injurySum, desc(INJURIES)), 12)

fatalPlot <- qplot(EVTYPE, data = fatalHead, weight = FATALITIES, geom = "bar", binwidth = 1, fill=I("darkblue")) + 
  scale_y_continuous("Number of Fatalities") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Severe Weather Type") + 
  ggtitle("Total Fatalities by Severe Weather\n Events in the U.S.\n from 1995 - 2011")

injuryPlot <- qplot(EVTYPE, data = injuryHead, weight = INJURIES, geom = "bar", binwidth = 1, fill=I("darkblue")) + 
  scale_y_continuous("Number of Injuries") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Severe Weather Type") + 
  ggtitle("Total Injuries by Severe Weather\n Events in the U.S.\n from 1995 - 2011")

grid.arrange(fatalPlot, injuryPlot, ncol = 2)

rm(data)

expFunc <- function(e) {
  # Transform:
  # h = hundred
  # k = thousand
  # m = million
  # b = billion
  
  if (e %in% c('h', 'H'))
    return(2)
  else if (e %in% c('k', 'K'))
    return(3)
  else if (e %in% c('m', 'M'))
    return(6)
  else if (e %in% c('b', 'B'))
    return(9)
  else if (!is.na(as.numeric(e))) # if a digit
    return(as.numeric(e))
  else if (e %in% c('', '-', '?', '+'))
    return(0)
  else {
    stop("Invalid exponent!")
  }
}

# Compute the economic loss by event type
propDmgExp <- sapply(dataSlice$PROPDMGEXP, FUN=expFunc)
dataSlice$PROPDMG <- dataSlice$PROPDMG * (10 ** propDmgExp)
cropDmgExp <- sapply(dataSlice$CROPDMGEXP, FUN=expFunc)
dataSlice$CROPDMG <- dataSlice$CROPDMG * (10 ** cropDmgExp)
                       
dataGrouped <- group_by(dataSlice, EVTYPE)
propDmgSum <- summarise(dataGrouped, sum=sum(PROPDMG))
cropDmgSum <- summarise(dataGrouped, sum=sum(CROPDMG))
ecoLoss <- summarise(dataGrouped, sumProp=sum(PROPDMG), sumCrop=sum(CROPDMG))

# Across the United States, which types of events have the greatest economic consequences?

# filter out events that caused no economic loss
ecoLoss <- ecoLoss[(ecoLoss$sumProp > 0 | ecoLoss$sumCrop > 0), ]

propDmgSum <- head(arrange(ecoLoss, desc(sumProp)), 15)
cropDmgSum <- head(arrange(ecoLoss, desc(sumCrop)), 15)

p1 <- ggplot(data=propDmgSum, aes(x=reorder(EVTYPE, sumProp), y=log10(sumProp), fill=sumProp )) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("Event type") +
  ylab("Property damage in dollars (log-scale)") +
  theme(legend.position="none")

p2 <- ggplot(data=cropDmgSum, aes(x=reorder(EVTYPE, sumCrop), y=sumCrop, fill=sumCrop)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  xlab("Event type") +
  ylab("Crop damage in dollars (log-scale)") + 
  theme(legend.position="none")

p1
p2
