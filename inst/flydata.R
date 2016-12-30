library(curl)

fly <- read.csv( curl("https://raw.githubusercontent.com/fivethirtyeight/data/master/flying-etiquette-survey/flying-etiquette.csv") ,
                 na.strings=c(""," ","NA"))

names(fly) <- c("ID", "FlightFreq", "DoYouRecline", "Height", "Child18",
                "Seats3_2Arms", "Seats2_1Arm", "WhoControlsWindowShade",
                "RudeToMoveToUnsoldSeat", "RudeToTalkToNeighbor",
                "6hrFlightRudeToLeaveSeat", "RecliningObligationToBehind",
                "RudeToRecline", "EliminateReclining", "RudeToSwitchSeatsForFriends",
                "RudeToSwitchSeatsForFamily", "RudeToWakeNeighborForBathroom",
                "RudeToWakeNeighborForWalk", "RudeToBringBaby",
                "RudeToBringUnrulyChild", "UseElectronicsDuringTakeoff",
                "HaveYouSmoked", "Gender", "Age", "HouseholdIncome", "Education", "Region")

library(ggplot2)
library(ggmosaic)

head(fly)

set.separators(c(":", ";","|"))

ggplot(data = fly) + geom_mosaic(aes(x=product(RudeToRecline), fill=DoYouRecline), na.rm=T)

ggplot(data = fly) + geom_mosaic(aes(x=product(DoYouRecline), fill=RudeToRecline), na.rm=T)


ggplot(data = fly) + geom_mosaic(aes(x=product(Child18), fill=RudeToBringBaby), na.rm=T, offset=0.005)


ggplot(data = fly) + geom_mosaic(aes(x=product(RudeToRecline), fill=DoYouRecline), na.rm=T)+coord_flip()

fly$FlightFreq <- fct_relevel(fly$FlightFreq, c("Every day", "A few times per week" , "A few times per month","Once a month or less",  "Once a year or less" ,"Never"  ) )

ggplot(data = fly) + geom_mosaic(aes(x=product(FlightFreq), fill=RudeToTalkToNeighbor), na.rm=T) +labs(x="Region") + #coord_flip()+
  guides(fill=guide_legend( reverse = TRUE)) +  scale_fill_viridis(discrete=TRUE, option="D", begin=.2, end=.9)

levels(fly$RecliningObligationToBehind) <- c("No", "Yes")
ggplot(data = fly) + geom_mosaic(aes(x=product(RecliningObligationToBehind), fill=RudeToRecline), na.rm=T) +labs(x="Region") + #coord_flip()+
  guides(fill=guide_legend( reverse = TRUE)) +  scale_fill_viridis(discrete=TRUE, option="D", begin=.2, end=.9)

ggplot(data = fly) + geom_mosaic(aes(x=product(RudeToRecline), fill=EliminateReclining), na.rm=T)

ggplot(data = fly) + geom_mosaic(aes(x=product(Age), fill=RudeToBringUnrulyChild),
                                 divider=ddecker(), na.rm=T)

ggplot(data = fly) +
  geom_mosaic(aes(x=product(FlightFreq), fill=DoYouRecline), na.rm=T)

ggplot(data = fly) +
  geom_mosaic(aes(x=product(Height), fill=RudeToRecline), na.rm=T)

fly$HouseholdIncome <- fct_relevel(fly$HouseholdIncome,
                                   c( "$0 - $24,999" ,"$25,000 - $49,999",
                                      "$50,000 - $99,999" ,
                                      "$100,000 - $149,999", "150000"))
levels(fly$HouseholdIncome)[5]<- "150,000 or more"

ggplot(data = fly) +
  geom_mosaic(aes(x=product(HouseholdIncome), fill=FlightFreq), na.rm=T)

ggplot(data = fly) +
  geom_mosaic(aes(x=product(Seats3_2Arms), fill=WhoControlsWindowShade), na.rm=T)
