library(dplyr)
library(ggplot2)
library(usmap)
library(fmsb)
library(reshape2)


#load data
incarceration_df <- read.csv("incarceration_trends.csv")
AL_df <- filter(incarceration_df, state == "AL")
AL_filter_df <- filter(AL_df, county_name == "Autauga County" | 
                           county_name == "Barbour County"|
                           county_name == "Bibb County" |
                           county_name == "Blount County" |
                           county_name == "Bullock County")
AL_Autauga_df <- filter(AL_df, county_name == "Autauga County")
pop_2018_1998_df <- filter(AL_Autauga_df , year == 2018 | year == 1998)
total_pop_Autauga_2018 <- filter(AL_Autauga_df, year == 2018)
total_pop_Autauga_1970 <- filter(AL_Autauga_df, year == 1970)

#summary information 
#1.The max total_pop of Autauga County in AL and the year it happened
AL_Autauga_df <- filter(AL_df, county_name == "Autauga County")
max_pop <- max(AL_Autauga_df$total_pop)
max_pop_df <- filter(AL_Autauga_df, AL_Autauga_df$total_pop == max_pop)
max_pop_year <- select(max_pop_df, year, total_pop)

#2.The min total_pop of Autauga County in AL and the year it happened
AL_Autauga_df <- filter(AL_df, county_name == "Autauga County")
min_pop <- min(AL_Autauga_df$total_pop)
min_pop_df <- filter(AL_Autauga_df, AL_Autauga_df$total_pop == min_pop)
min_pop_year <- select(min_pop_df, year, total_pop)

#3. The mean of female_pop and the mean of male_pop in AL
AL_df <- filter(incarceration_df, state == "AL")
mean_female_15to64 <- mean(AL_df$female_pop_15to64)
mean_male_15to64 <- mean(AL_df$male_pop_15to64)  

#4.which county in AL has the most total_pop in the year 2018.
AL_2018_df <- filter(AL_df , year == 2018)
max_2018_pop <- max(AL_2018_df$total_pop)
max_2018_pop_df <- filter(AL_2018_df, AL_2018_df$total_pop == max_2018_pop)
max_pop_county <- select(max_2018_pop_df, total_pop, county_name)

#5. The difference between total_pop in 1970 and 2018 of Autauga County in AL.
total_pop_Autauga_2018 <- filter(AL_Autauga_df, year == 2018)
total_pop_Autauga_1970 <- filter(AL_Autauga_df, year == 1970)
difference <- total_pop_Autauga_2018$total_pop - total_pop_Autauga_1970$total_pop

#plot 1
ggplot(AL_filter_df , aes(x = year,y = total_jail_pop, color = county_name)) + 
  geom_line() +
  ggtitle("Incarceration trends of five county in AL")


#plot 2
pop_2018_1998_df <- filter(AL_Autauga_df , year == 2018 | year == 1998)
selected <- pop_2018_1998_df %>% select(year, aapi_jail_pop, black_jail_pop,
                                        latinx_jail_pop, native_jail_pop,
                                        white_jail_pop)
df <- melt(selected, id.vars="year")
ggplot(df, aes(x=year, y=value, fill=variable))+
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Incarceration rate by race")


#map
incarceration_df <- read.csv("incarceration_trends.csv")
incarceration_2018_df <- filter(incarceration_df, year == 2018)

plot_usmap(data = incarceration_2018_df, values = "total_jail_pop", color = "orange",
           labels = FALSE) + 
  scale_fill_continuous( low = "white", high = "orange",
  name = "population", label = scales::comma)+
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) +
  labs(title = "Incarceration rate in 2018 by County")
  





 




