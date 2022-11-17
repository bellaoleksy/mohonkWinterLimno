## Creating a potential figure for the On Thin Ice proposal
## IAO 2022-10-25

#Run the main script to bring in all data and functions####
source('00_main.R')
library(patchwork)

#annotate panel letters inside plot
panelLetter.normal <- data.frame(
  xpos = c(-Inf),
  ypos =  c(Inf),
  hjustvar = c(-0.5) ,
  vjustvar = c(1.5))



theme_MS <- function () { 
  ggthemes::theme_base(base_size=6) %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="white", colour=NA, size=1.0),
      plot.title=element_text(face="plain",hjust=0.5),
      plot.subtitle = element_text(color="dimgrey", hjust=0, size=6),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_text(size=6),
      axis.text.x = element_text(size=6, face="bold"),
      axis.text.y = element_text(size=6, face="bold"),
      axis.title.x = element_text(size=6, face="bold"),
      axis.title.y = element_text(size=6, face="bold"),
      panel.spacing=grid::unit(0,"lines"),
      axis.ticks.length = unit(0.1, "cm")
    )
}

theme_set(theme_MS())




# What is the correlation between LengthOfStrat_days and MixingAction_gigaJday
plot(AnnualData$IceOutDayofYear, AnnualData$lengthMixedPeriod)
cor.test(AnnualData$IceOutDayofYear, AnnualData$lengthMixedPeriod)


# What is the relationship between spring mixed period length and stratification length?
AnnualData <- AnnualData %>%
  mutate(lengthMixedPeriod=StartOfStratification_Day-IceOutDayofYear) %>%
  filter(lengthMixedPeriod < 100)
  # filter(lengthMixedPeriod<100)
cor.test(AnnualData$lengthMixedPeriod, AnnualData$LengthOfStrat_days)  #it is not significant if you take out the 2020 outlier. grr. 

# What is the relationship between spring mixed period and onset of stratification?
plot(AnnualData$StartOfStratification_Day, AnnualData$lengthMixedPeriod)
cor.test(AnnualData$StartOfStratification_Day, AnnualData$lengthMixedPeriod)

# What is the relationship between mixing action and onset of stratification?
plot(AnnualData$StartOfStratification_Day, AnnualData$MixingAction_gigaJday)
cor.test(AnnualData$StartOfStratification_Day, AnnualData$MixingAction_gigaJday)


# What is the correlation between LengthOfStrat_days and MixingAction_gigaJday
plot(AnnualData$MixingAction_gigaJday, AnnualData$LengthOfStrat_days)
cor.test(AnnualData$MixingAction_gigaJday, AnnualData$LengthOfStrat_days)


# What if we JUST show spring mixed period versus ice out date
# and then show spring mixed period and onset of stratification?
# and then onset of stratification and mixing action?
a<-AnnualData %>%
  ggplot(aes(x=IceOutDayofYear, y=lengthMixedPeriod))+
  geom_point(shape=21,size=1.5,fill="grey50")+
  geom_text(aes(fontface="bold"),
            x=75, y = 10,
            label = "r = -0.64",
            size = 6/.pt)+
  geom_smooth(method="lm",se=F,color="black",size=0.5)+
  labs(x="Ice out (day of year)",
       y="Length of mixed period (days)")+
  theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "lines"),
        axis.title.y = element_text(angle=90))
a

# b<-AnnualData %>%
#   ggplot(aes(x=lengthMixedPeriod, y=StartOfStratification_Day))+
#   geom_point(shape=21,size=1,fill="grey50")+
#   geom_smooth(method="lm",se=F,color="black",size=0.5)+
#   geom_text(aes(fontface="bold"),
#             y=100, x = 42,
#             label = "r = 0.46",
#             size = 10/.pt)+
#   labs(y="Start of\nstrat. (DOY)",
#        x="Length of\nmixed period (days)")+
#   theme(plot.margin=unit(c(0,0,0,0), "lines"))
# c<-AnnualData %>%
#   ggplot(aes(x=StartOfStratification_Day, y=MixingAction_gigaJday))+
#   geom_point(shape=21,size=1,fill="grey50")+
#   geom_text(aes(fontface="bold"),
#             x=110, y = 3.2,
#             label = "r = -0.42",
#             size = 10/.pt)+
#   labs(x="Start of\nstrat. (DOY)",
#        y="Mixing action\n(GJ day)")
# d<-AnnualData %>%
#   ggplot(aes(x=LengthOfStrat_days, y=MixingAction_gigaJday))+
#   geom_point(shape=21,size=1,fill="grey50")+
#   geom_text(aes(fontface="bold"),
#             x=155, y = 4.5,
#             label = "r = 0.71",
#             size = 10/.pt)+
#   labs(x="Length of\nstrat. (days)",
#        y="Mixing action\n(GJ day)")
# (a+b)/(c+d)

a/b + plot_annotation(tag_levels = 'A')

a

ggsave(
  "figures/ProposalFigure3.jpg",
  width = 1.75,
  height = 1.75,
  units = "in",
  dpi = 600
)

