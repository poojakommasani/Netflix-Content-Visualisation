library(dplyr)
library(ggplot2)

alco <- read.csv("C:/Users/HP/Desktop/desktop/Karthik/country_vaccinations.csv", header = TRUE )
summary(alco)




address<- ggplot(alco, 
       aes(x = alco$country, 
           y = alco$people_vaccinated)) +
  geom_boxplot(notch = TRUE, 
               fill = "cornflowerblue", 
               alpha = .7) +
  labs(title = "Salary distribution by rank")

ggplotly(address)
################################################################################################################


# Import required packages
library(tidyverse)
library(maps)
library(rnaturalearth)
# Import dataset from input directory
covid_data_imported <- read_csv("C:/Users/HP/Desktop/desktop/Karthik/country_vaccinations.csv")






# Display first observations
covid_data_total <- covid_data_imported %>%
  group_by(country) %>%
  filter(!is.na(total_vaccinations)) %>%
  summarise(
    total_vaccinations = max(total_vaccinations),
    total_vaccinations_per_hundred = max(total_vaccinations_per_hundred),
    vaccines = first(vaccines),
  ) %>%
  filter(total_vaccinations_per_hundred >= 0.5)

covid_data_fully <- covid_data_imported %>%
  group_by(country) %>%
  filter(!is.na(people_fully_vaccinated)) %>%
  summarise(
    total_vaccinations_per_hundred = max(total_vaccinations_per_hundred),
    people_fully_vaccinated = max(people_fully_vaccinated),
    people_fully_vaccinated_per_hundred = max(people_fully_vaccinated_per_hundred),
  ) %>%
  filter(total_vaccinations_per_hundred >= 0.5) %>%
  select(country, people_fully_vaccinated, people_fully_vaccinated_per_hundred)




head(covid_data_total)
head(covid_data_fully)




options(repr.plot.width=20,repr.plot.height=10)
countries(scale = "medium", returnclass = "sf") %>%
  select(name,formal_en) %>%
  right_join(covid_data_total,by=c("name"="country")) %>%
  ggplot() +
  geom_sf(aes(fill = vaccines, color = vaccines)) +
  theme(legend.position="bottom", panel.background = element_blank())

options(repr.plot.width=12,repr.plot.height=14)
covid_data_total %>%
  group_by(vaccines) %>%
  ggplot(aes(x = vaccines)) +
  geom_bar(aes(fill = country), show.legend = FALSE) +
  labs(x = "Vaccines", y = "Number Of Countries") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




options(repr.plot.width=12,repr.plot.height=16)
covid_data_total %>%
  ggplot(aes(x = total_vaccinations, y = country)) +
  geom_bar(aes(fill = country), position = "identity", stat = "identity", show.legend = FALSE) +
  labs(x = "Total Vaccinations", y = "Country") +
  theme(panel.grid = element_blank())




options(repr.plot.width=16,repr.plot.height=12)
covid_data_fully %>%
  ggplot(aes(x = country, y = people_fully_vaccinated)) +
  geom_bar(aes(fill = people_fully_vaccinated), position = "identity", stat = "identity", show.legend = FALSE) +
  labs(x = "Country", y = "People Fully Vaccinated") +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient(low = "#164313", high = "#61f756")




covid_data_fully %>%
  ggplot(aes(x =country , y = people_fully_vaccinated_per_hundred)) +
  geom_bar(aes(fill = people_fully_vaccinated_per_hundred), position = "identity", stat = "identity", show.legend = FALSE) +
  labs(x = "Country", y = "People Fully Vaccinated Per Hundred") +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
