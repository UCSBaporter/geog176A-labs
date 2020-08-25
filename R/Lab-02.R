# Abigail Porter
# 08-14-2020
# Lab 2

---
  title: "Geography 176A"
author: '[Abigail Porter](https://ucsbaporter.github.io/UCSBaporterW1/first-webpage/index.html)'
subtitle: 'Lab 02: COVID-19 Pandemic'
output:
  html_document:
  theme: journal
word_document: default




install.packages("tidyverse")
install.packages("knitr")
install.packages("readxl")

install.packages("zoo")

library(tidyverse)
library(knitr)
library(readxl)
library(zoo)

url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
home = readr::read_csv(url)
home = read_csv(url)


read_excel = 'data/PopulationEstimates.xls'
pop = readxl::read_excel("data/PopulationEstimates.xls", skip = 2) %>% select(pop2019 = POP_ESTIMATE_2019, fips = FIPStxt)
CA_pop_2019 = pop %>% filter(State == "CA") %>% slice_max(pop2019)


(date, county, state, fips, cases, deaths)

##Covid-19 California Cases

#Step 2
steptwo = home %>% filter(state == "California") %>% group_by(county) %>%
  mutate(newCase = cases - lag(cases)) %>% arrange(-newCase) %>% ungroup()


____
?? %>% group_by  NOT ALL THE COUNTY INFO
?? %>% filter(state == "California") %>% group_by(county) %>%
  summarize(sum_cases = sum(cases)) %>% arrange(-sum_cases)


#Step 3
#Table 1: 5 counties with most cumulative cases
most_cumulative_cases = steptwo %>% filter(date == max(date)) %>% group_by(county) %>%
  summarize(sum_cases = sum(cases)) %>% arrange(-sum_cases) %>%
  ungroup() %>% slice_max(sum_cases, n = 5)

knitr::kable(most_cumulative_cases, caption = c("Cumulative Case Counts: Tops 5 CA counties"),
  col.names = c("County", "Cumulative Cases"))



#Table 2: 5 counties with most new cases
most_new_cases = steptwo %>% filter(date == max(date)) %>% slice_max(newCase, n=5) %>% select (county, newCase)

knitr::kable(most_new_cases, caption = "New Case Counts: Tops 5 CA counties", col.names = c("County", "New Cases"))


____
most_new_cases = steptwo %>% filter(date == max(date)) %>% group_by(county) %>% summarize(newCase, na.rm = TRUE) %>%
  arrange(-newCase) %>% ungroup() %>% head(5)

steptwo %>% filter(state %in% c("California")) %>% group_by(county) %>%
  summarize(sum_cases = sum(cases)) %>% arrange(-sum_cases) %>%
  ungroup() %>% slice_max(sum_cases, n = 5)

steptwo %>% filter(state == "California") %>% group_by(county) %>%
  mutate(newCase = cases - lag(cases)) %>% arrange(-newCase) %>%
  ungroup() %>% slice_max(newCase, n = 5)

home %>% filter(county %in% top_counties_new) %>% group_by(county, date) %>%
  mutate(newCase = cases - lag(cases)) %>% arrange(-newCase) %>%
  ungroup() %>% ggplot(aes(x = date, y = cases, color = county)) +
  geom_line(size = 2) + facet_wrap(~county) + ggthemes::theme_base() +
  theme(legend.position = 'NA') + labs(title = "New Case Counts: Top 5 CA counties",
                                       subtitle = "Data Source: NY-Times", x = "Date", y = "Cases",
                                       caption = "Lab 02")
?? Only LA county (not top 5 counties)

--------------------------------------------------------------------------------------------------------


library(readxl)
PopulationEstimates <- read_excel("data/PopulationEstimates.xls")
View(PopulationEstimates)


----------

#Step 7

read_excel = 'data/PopulationEstimates.xls'

pop = readxl::read_excel("data/PopulationEstimates.xls", skip = 2) %>%
  select(pop2019 = "POP_ESTIMATE_2019", fips = "FIPStxt", state = "State")

CA_pop_covid = right_join(pop, cases, by ="fips") %>% filter(date > max(date)-13) %>%
  group_by(county) %>% mutate(newCase = cases - lag(cases),most_cumulative_cases = (cases/pop2019),
                              most_new_cases = (newCases/pop2019)) %>% ungroup()
#Table 1
pop_most_cumulative_cases = CA_pop_covid %>% filter(date == max(date)) %>% slice_max(cases, n=5) %>%
  select(county, most_cumulative_cases)

knitr::kable(pop_most_cumulative_cases, caption = c("Pop Cumulative Case Counts: Top 5 CA counties"),
             col.names = c("County", "Cumulative Cases with Pop"))

#Table 2

pop_most_new_cases = CA_pop_covid %>% filter(date == max(date)) %>% slice_max(newCases, n=5) %>%
  select(county, most_new_cases) %>% arrange(-most_new_cases)

knitr::kable(pop_most_new_cases, caption = c("Pop New Case Counts: Top 5 CA counties"),
             col.names = c("County", "New Cases with Pop"))

#Table 3: Safe counties

pop_safe_county = CA_pop_covid %>% filter(date == max(date)) %>% slice_min(newCases, n=5) %>%
  select(county, most_new_cases) %>% arrange(-most_new_cases) %>% head(5)

knitr::kable(pop_safe_county, caption = c("Pop Lowest Case Counts: CA counties"),
             col.names = c("County", "Lowest New Cases with Pop"))



--------------
CA_pop_CovidCumulative = pop %>% right_join(pop, most_cumulative_cases, by = "fips") %>%
  group_by(county) %>% mutate(newCase = cases - lag(cases))



## Question 2: Covid-19 New York, California, Louisiana, and Florida

Q2 = home %>%
  filter(state %in% c("New York","California", "Louisiana", "Florida")) %>%
  group_by(state, date) %>% summarise(cases = sum(cases)) %>%
  mutate(newCases = cases - lag(cases),
         roll7 = zoo::rollmean(newCases, 7, fill = NA, align = 'right')) %>% ungroup()

Q2 %>% ggplot(aes(x = date, y = newCases)) + geom_col(aes(y = newCases), col = NA, fill = "#F5B8B5") +
  geom_line(aes(y = roll7), col = "darkred", size = 1) + facet_grid(~state, scale = "free_y") +
  ggthemes::theme_wsj() + theme(legend.position = "right")
labs(title = paste("Daily Cases in NY, CA, LA, FL")) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 10, face = 'bold')) +
  theme(aspect.ratio = .5)

__________ Part 2 doesn't work
Q2b = Q2 %>%  inner_join(p2, by = c("state" = "Area_Name") ) %>%
  group_by(state, date) %>%
  summarise(newCase_pc = sum(newCase_pc), pop2019, .groups = "drop") %>%
  mutate(newCase_pc = cases - lag(cases), newCases / pop2019),
         roll7 = zoo::rollmean(newCase_pc, 7, fill = NA, align = 'right') %>% ungroup()

Q2b %>% ggplot(aes(x = date, y = newCases)) + geom_col(aes(y = newCases), col = NA, fill = "#F5B8B5") +
  geom_line(aes(y = roll7), col = "darkred", size = 1) + facet_grid(~state, scale = "free_y") +
  ggthemes::theme_wsj() + theme(legend.position = "right") +
labs(title = paste("Daily Cases in NY, CA, LA, FL")) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 10, face = 'bold')) +
  theme(aspect.ratio = .5)

******************* Part 2 doesn't work

Q2 %>%  inner_join(p2, by = c("state" = "Area_Name")) %>%
  group_by(state, date) %>% ungroup() %>%
  summarise(newCases = sum(newCases) / (max(pop) /100000) %>% mutate(roll7 = zoo::rollmean(newCases, 7, fill = NA, align = 'right'))

ggplot(aes(x = date, y = roll7)) + geom_col(aes(y = newCases), col = NA, fill = "#F5B8B5") +
  geom_line(aes(y = newCases), col = "green", size = 1) + facet_grid(~state, scale = "free_y") +
  ggthemes::theme_wsj() + theme(legend.position = "right") +
  labs(title = paste("Daily Cases in NY, CA, LA, FL by population (100,000) ")) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 10, face = 'bold')) +
  theme(aspect.ratio = .5)


 *************** Part 2 fails


  ggplot(aes(x = date, y = newCases)) +
  geom_col(aes(y = newCases, col = state))+
  geom_line(aes(col = state)) + geom_line(aes(y = roll7), size = 1)+
  facet_grid(~state) + ggthemes::theme_solarized() + ggthemes::scale_color_gdocs() +
  theme(legend.position = "none") +
  labs(title = "Daily new cases Per Capita by State", y = "Daily New Count Per Capita", x = "Date",
       caption = "Lab 02")

  home %>%
  filter(state %in% c("New York","California", "Louisiana", "Florida")) %>%
  right_join(pop, by = "fips")

Q2_pop = pop %>% filter(State %in% c("NY", "CA", "LA", "FL")) %>%
  group_by(state, date) %>% right_join(Q2b, by "State")

summarise(cases = sum(cases)) %>%
  mutate(newCases = cases - lag(cases),
         roll7 = zoo::rollmean(newCases, 7, fill = NA, align = 'right')) %>% ungroup()

ggplot(data = Q2_pop, aes(x = date)) +
  geom_col(aes(y = cases_per_capita), col = NA, fill = "lightblue") +
  geom_line(aes(y = roll7), col = "blue", size = 1) +
  ggthemes::theme_wsj() +
  labs(title = paste("New Cases Per Capita")) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = 'bold')) +
  facet_grid(~areaname, scales = "free_y")


  group_by(state, date) %>% ungroup() %>%
  mutate(newCases = cases - lag(cases), newCases/ pop2019,
         roll7 = zoo::rollmean(newCases, 7, fill = NA, align = 'right')) %>%
  ungroup() %>% filter(cases > 0)

ggplot(aes(x = date)) + geom_col(aes(y = cases), col = NA, fill = "#F5B8B5") +
  geom_line(aes(y = roll7), col = "darkred", size = 1) +
  ggthemes::theme_wsj() +
  labs(title = paste("Daily Cases in NY, CA, LA, FL")) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 10, face = 'bold')) +
  theme(aspect.ratio = .5)





read_excel = 'data/PopulationEstimates.xls'

pop = readxl::read_excel("data/PopulationEstimates.xls", skip = 2) %>%
  select(pop2019 = POP_ESTIMATE_2019, fips = FIPStxt, State)

CA_pop_2019 = pop %>%
  filter(State == "CA")

Q2 = home %>% right_join(pop, steptwo, by = "fips") %>% filter(state %in% c("New York", "California", "Louisiana", "Florida")) %>%
   group_by(county) %>% mutate(newCase = cases - lag(cases)) %>% ungroup()


  group_by(state, date) %>%
  summarize(totCase = sum(cases)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(newCase = totCase -lag(totCase)) %>%
  mutate(roll7 = zoo::rollmean(newCase, 7, fill = NA, align='right'))

ggplot(data=Q2, aes(x = date, y = value)) +
  geom_line(aes(col = state)) +
  facet_grid(name~state, scale = "free_y") +
  ggthemes::theme_few() +
  labs(title = "Covid-19 New York, California, Louisiana, and Florida",
       subtitle = "Data Source: NY-Times",
       x = "Date",
       y = "Daily Cummulative Count",
       caption = "Daily Exercise 07: Abigail Porter")


*************
Change Q2 to new name + sink new info (review )
Q2 = home %>%
  filter(state %in% c("New York","California", "Louisiana", "Florida")) %>%
  group_by(state, date) %>% summarise(cases = sum(cases)) %>%
  mutate(newCases = cases - lag(cases),
         roll7 = zoo::rollmean(newCases, 7, fill = NA, align = 'right')) %>% ungroup()

Q2 %>% ggplot(aes(x = date, y = newCases)) + geom_col(aes(y = newCases), col = NA, fill = "#F5B8B5") +
  geom_line(aes(y = roll7), col = "darkred", size = 1) + facet_grid(~state, scale = "free_y") +
  ggthemes::theme_wsj() + theme(legend.position = "right")
labs(title = paste("Daily Cases in NY, CA, LA, FL")) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 10, face = 'bold')) +
  theme(aspect.ratio = .5)





LAB NOTES

homes = read_csv('data/landdata-states.csv')

names(homes)

homes %>% group_by(region, Year) %>% summarize(meanHV = mean(Home.Value)) %>% na.omit() %>% ggplot(aes(x = Year, y = meanHV)) + geom_line(aes(color = region)) + labs(title = "Title") + ggthemes::theme_excel


pop = readxl::read_excel("data/PopulationEstimates.xls", skip = 2) %>% select(pop2019 = POP_ESTIMATE_2019, fips = FIPStxt)

covid = read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')

newData = inner_join(pop, covid, by = "fips") %>% group_by(county) %>% mutate(newCase = cases - lag(cases))

names(homes)

homes %>% group_by(region, Year)

ggsave(file = "img/lv-plot.png")












library(tidyverse)
library(readr)

install.packages("readxl")

homes = read_csv('data/landdata-states.csv')

library(readxl)

pop<- read_excel("data/PopulationEstimates.xls", skip = 2) %>% select(pop19 = POP_ESTIMATE_2019, state = State, county = Area_Name)

pop<- read_excel("data/PopulationEstimates.xls", skip = 2) %>% select(pop19 = POP_ESTIMATE_2019, state = State, county = Area_Name, FIPS = FIPStxt) %>% group_by(state) %>% slice_max(pop19, n=1)


summary(pop)
dim(pop)
names(pop)
head(pop)
head(pop)


homes = read_csv('data/landdata-states.csv')

names(homes)

homes %>% group_by(region, Year) %>% summarize(meanHV = mean(Home.Value)) %>% na.omit() %>% ggplot(aes(x = Year, y = meanHV)) + geom_line(aes(color = region)) + labs(title = "Title") + ggthemes::theme_excel



pop = readxl::read_excel("data/PopulationEstimates.xls", skip = 2) %>% select(pop2019 = POP_ESTIMATE_2019, fips = FIPStxt)

covid = read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')

newData = inner_join(pop,covid, by = "fips") %>% group_by(county) %>% mutate(newCase = cases - lag(cases))


names(homes)

homes %>% group_by(region, Year)







pop <- read_excel("data/PopulationEstimates.xls", skip = 2)

p2 = pop %>% select(fips = FIPStxt, state = State, Area_Name, pop2019 = POP_ESTIMATE_2019) %>% group_by(state) %>% slice_max(pop2019, n = 1)

summary(p2)
str(p2)

fivenum(home$Land.Value)

home %>% filter(State %in% c("HI", "CA", "NY")) %>% group_by(State) %>% summarize(minLV = min(Land.Value), meanLV = mean(Land.Value), maxLV = max(Land.Value))
names(home)

pop19 = POP_ESTIMATE_2019, state = State, county = Area_Name, FIPS = FIPStxt) %>% group_by(state) %>% slice_max(pop19, n=1)


summary(pop)
dim(pop)
names(pop)
head(pop)
head(pop)


homes = read_csv('data/landdata-states.csv')

names(homes)

homes %>% group_by(region, Year) %>% summarize(meanHV = mean(Home.Value)) %>% na.omit() %>% ggplot(aes(x = Year, y = meanHV)) + geom_line(aes(color = region)) + labs(title = "Title") + ggthemes::theme_excel



pop = readxl::read_excel("data/PopulationEstimates.xls", skip = 2) %>% select(pop2019 = POP_ESTIMATE_2019, fips = FIPStxt)

covid = read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')

newData = inner_join(pop, covid, by = "fips") %>% group_by(county) %>% mutate(newCase = cases - lag(cases))

names(homes)

homes %>% group_by(region, Year)

ggsave(pl, file = "img/lv-plot.png")


newData = filter(state %in% c('CA')) %>% group_by(county) %>% mutate(newCase = cases - lag(cases))
newData = filter(state.name %in% ('CA'))
