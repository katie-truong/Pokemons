pokemons_wide <- read.csv("Pokemon.csv")
library(ggplot2)
theme_set(theme_bw())
library(plotly)
library(dplyr)
library(tidyr)
library(scales)
library(reshape2)
library(ggthemes)
library(viridis)
library(gridExtra)

Sys.setenv("plotly_username"="estnnk")
Sys.setenv("plotly_api_key"="34a6ysnxfe")


##### Preliminary steps #####

# Sort into Single or Dual Type
pokemons_wide$Sort <- ifelse(pokemons_wide$Type.2 == "", "Single", "Dual")

# Convert to tbl_df
pokemons_wide <- tbl_df(pokemons_wide)

# Melt Type1 and Type2 into Type
pokemons_long <- gather(pokemons_wide, Category, Type, 
                   Type.1, Type.2, factor_key = T)

# Deselect the empty type 2
pokemons_long <- filter(pokemons_long, Type != "")

# Order by Poke number
pokemons_long <- pokemons_long[order(pokemons_long$X., pokemons_long$Category), ]

names(pokemons_long)[,1] = "Num"

# Arrange
pokemons_long <- select(pokemons_long, X., Name, Type, Category, Sort, Total:Legendary)

##### Basic stats ######

summary(pokemons_wide)


##### Chain analysis #####

# Grouping by type
summaryStats_byType <- group_by(pokemons_long, Type) %>%
                       summarise(Count = n(),
                                 AverageTotal = mean(Total, na.rm = T),
                                 AverageHP = mean(HP, na.rm = T),
                                 AverageAttack = mean(Attack, na.rm = T),
                                 AverageDefense = mean(Defense, na.rm = T),
                                 AverageSpAtk = mean(Sp..Atk, na.rm = T),
                                 AverageSpDef = mean(Sp..Def, na.rm = T),
                                 AverageSpeed = mean(Speed, na.rm = T),
                                 AverageGeneration = mean(Generation, na.rm = T),
                                 LegendaryCount = sum(as.character(Legendary)=="True"))


### Plot COUNT
gg1 <- ggplot(summaryStats_byType, 
              aes(x = reorder(Type, Count), y = Count)) + 
  geom_bar(stat="identity", fill = "dodgerblue3") + 
  geom_text(aes(label = paste0(Count)),
            color = "white", hjust = 1.2, vjust = 0.3) +
  coord_flip() +
  xlab("") +
  guides(fill=FALSE) +
  ggtitle("The number of pokemons in each type") +
  theme(plot.title = element_text(size=20))

## Average Stats per Type of Pokemons
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

averageStats_byType <- select(summaryStats_byType, Type, AverageHP:AverageSpeed)

averageStats_byType_long <- gather(averageStats_byType, Category, Value,
                                    AverageHP:AverageSpeed)

gg2 <- ggplot(averageStats_byType_long, aes(x = reorder(Type, Value), y = Value, fill = Category)) +
  geom_bar(stat='identity') +
  coord_flip() +
  xlab("") +
  ggtitle("Average Stats per Type of Pokemons") +
  scale_fill_manual(values=cbPalette) +
  theme(plot.title = element_text(size=20))


## Does Type 2 matter

# Density plot of total stats
gg3 <- ggplot(pokemons_wide, aes(x=Total)) + 
  geom_density(aes(col = Sort, fill = Sort), alpha=.4) +
  ggtitle("Density plots of Total Stats") +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size=20))

bySort <- group_by(pokemons_long, Sort)
summaryStats_bySort <-  summarise(bySort,
                                  Count = n(),
                                  AverageTotal = mean(Total, na.rm = T),
                                  AverageHP = mean(HP, na.rm = T),
                                  AverageAttack = mean(Attack, na.rm = T),
                                  AverageDefense = mean(Defense, na.rm = T),
                                  AverageSpAtk = mean(Sp..Atk, na.rm = T),
                                  AverageSpDef = mean(Sp..Def, na.rm = T),
                                  AverageSpeed = mean(Speed, na.rm = T),
                                  AverageGeneration = mean(Generation, na.rm = T),
                                  LegendaryCount = sum(as.character(Legendary)=="True"))

averageStats_bySort <- select(summaryStats_bySort, Sort, AverageHP:AverageSpeed)

averageStats_bySort_long <- gather(averageStats_bySort, Category, Value,
                                   AverageHP:AverageSpeed)

### Facet plots

pokemons_long_long <- gather(pokemons_wide, Variable, Value, 
                             HP:Speed, factor_key = T)

pokemons_long_long <- tbl_df(pokemons_long_long)

gg4 <- ggplot(data = pokemons_long_long, aes(x = Value, y = Total, col = Sort)) +
  geom_point(aes(text = paste("Name: ", Name)), alpha = 0.4) +
  geom_smooth() +
  facet_wrap(~Variable) +
  ggtitle("How stats differ between dual and single types") +
  theme(plot.title = element_text(size=20))

p4 <- ggplotly(gg4)

### Sort vs Generation
generationCount <- bySort %>% count(Generation)
                            
gg5 <- ggplot(data = pokemons_wide, aes(x = Generation)) +
  geom_bar(aes(fill = Legendary)) +
  facet_wrap(~Sort) +
  ggtitle("Number of Pokemons in each Generation for Dual and Single Types") +
  theme(plot.title = element_text(size=20)) +
  scale_fill_manual(values = c("navy", "gold")) +
  ylab("Count")
  
gg6 <- ggplot(data = pokemons_long_long, aes(x = Value)) +
  geom_density(aes(fill = Sort), alpha = 0.5) +
  facet_wrap(~Variable) +
  scale_y_continuous(labels = percent) +
  ggtitle("The density of different stats between single and dual types") +
  theme(plot.title = element_text(size=20))

### Lowest vs Highest
lowest <- arrange(pokemons_wide, Total)[1:5,]
highest <- arrange(pokemons_wide, desc(Total))[1:5,]
