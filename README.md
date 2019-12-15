# Stat 184 Final Project

By Leah Miller

**Research topic**: The correlation between health insurance charges and health condition google searches.

# The Data

> For this project I chose to work with data that was related to healthcare in some sense. I used two datasets, the first being an insurance dataset that shows billed charges for Americans dependent on a few factors like region, bmi, and sex. That dataset can be found here: [Insurance Data Link](https://www.kaggle.com/mirichoi0218/insurance)

> The second dataset I chose was a Google dataset that showed searches of health conditions split by location and search. The dataset can be found here: [Google Data Link](https://www.kaggle.com/GoogleNewsLab/health-searches-us-county)

> I was interested in seeing the link between how much a region pays on average vs how much they search for health conditions. My thought was that maybe if a region pays more they would be more likely to search a condition before going to the doctors.

***

Here I read in the CSV Files:
```{r}
insuranceData = read.csv("insurance.csv")
googleData = read.csv("RegionalInterestByConditionOverTime.csv")
```


***

# Exploratory Data Analysis of **Insurance Data**
First, I will explore the insurance data. I start this process by viewing the first 6 records:
```{r}
head(insuranceData, 6)
```
<br>
<br>
__Next I will plot some graphs to see the trends within insurance data:__
```{r}
costbyRegion <- ggplot(insuranceData, aes(x = region, y = charges)) +
  geom_boxplot() +
  geom_boxplot(fill = "#5DADE2", alpha = .7) +
  labs(title = "Charges by Region",
              caption = "This plot shows total medical costs billed by health insurance for each region.") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(hjust = 0))

costbyRegion
```

*Analysis* From this graph I was able to see that generally, the average insurance charges are relatively equal for each region. However, there are some variations in the upper and lower quartiles. It seems that the Southeast has higher chargers for the upper quartile while the southwest has lower charges. 

```{r} 
facetCosts <- ggplot(data=insuranceData,aes(x=age,y=charges))+
  geom_point()+
  geom_smooth() +
  aes(colour=smoker)+
  scale_color_manual(values=c("#AED6F1" , "#21618C"))+
  scale_x_log10()+
  facet_wrap(~region,ncol=4)+
  labs(title = "Charges VS Age (Regional)",
              caption = "This plot shows how charges vary in each region depending on the age of the beneficiary and whether or not they smoke.")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(hjust = 0))

facetCosts
```
*Analysis* From this graph I was able to see the trends in charges for different kinds of people in each region. Specifically, I saw that charges for smokers were much higher and that insurance costs rise as you age, regardless of which region you live in. One notable things I saw in this graph was the difference between smoker costs for the Southeast and Southwest. They are almost inverses of eachother which is odd because the rest of the graphs look pretty similar. 

***

# Exploratory Analysis of **Google Data** 
Next I will explore the google data. I will simply start by viewing the data table:
```{r}
head(googleData,6)
```
 
 ***
 

# Data Cleaning
I quickly realized that this data was formatted in a way that is not useful. Before I begin to explore the data further using plots, first I must extract the most recent search data (2017) using **Select**.
```{r}
CleanGoogleData <-
  googleData %>% 
  select(dma, geoCode, X2017.cancer, X2017.cardiovascular, X2017.depression, X2017.stroke, X2017.rehab, X2017.vaccine, X2017.obesity, X2017.diarrhea)

```


Next I will use regular expressions to extract the State Abbreviation from the location. Then I use mutate to apply the regular expression to all of the addresses. At this point I also drop the "geoCode" column because it is not needed.
```{r}
CleanGoogleData <- CleanGoogleData %>% 
  select(-geoCode) %>% 
  mutate(dma = str_extract(dma, "\\b[A-Z]{2}"))

head(CleanGoogleData, 6)
```


Our data is almost clean. Since all of this data is from 2017 I am going to reformat year into its own column. I also drop the "X2017." at the beginning of each of the conditions.
```{r}
gatheredData <- melt(CleanGoogleData, id.vars = "dma", variable = 'condition')
gatheredData <- gatheredData %>% 
  mutate(year = '2017') %>% 
  mutate(condition = gsub('X2017.','',condition))

head(gatheredData, 6)
```


Now I will group by state so I can do analysis on a state level.

*Note* at this point I realized population may have an effect on searches. I decided to upload a CSV file with state populations so I could find a standarized number of searches based on state population. This new column is called "SearchesPer" and is the number of searches per 100,000 people.
```{r}
populationData = read.csv("population.csv")

StateGoogleData <-
  gatheredData %>% 
  group_by(dma, condition) %>% 
  summarise (total = sum(value))

StateGoogleData <- setnames(StateGoogleData, "dma", "State")


PopulationGoogleData <- merge(StateGoogleData, populationData, by = 'State', type = "full", match = "all")

PopulationGoogleData <- 
  PopulationGoogleData %>% 
  mutate(searchesPer = total/Population *100000)

head(PopulationGoogleData, 6)
```


Next, I group by region. This is to make the comparison to the insurance data easier because it is also grouped by region.

*Note* There are only 4 regions included in Insurance data due to what was surveyed in the study. Due to this, I will only be including those regions for the google data so the comparison is equal. This excludes certain states like Alaska who are in the far north region. They will be labeled with an NA and then ommitted from the dataset.
```{r}
southwest <- c('AZ','CA','NM','NV','OK','TX')
southeast <- c('AL','AR','DE','FL','GA','KY','LA','MD','DC','MS','MC','SC')
northwest <- c('CA', 'CO', 'ID', 'MT', 'ND', 'NE', 'NV', 'OR', 'SD', 'UT', 'WA', 'WY')
northeast <- c('CT','IA','IL','IN','KS','MA','ME','MI','MN','MO','NH','NJ','NY','OH','PA','RI','SD','VA','VT','WI', 'WV')

RegionalGoogleData <-
  PopulationGoogleData %>% 
  mutate(region = ifelse(State %in% southwest, "southwest",
                                     ifelse(State %in% southeast, "southeast",
                                            ifelse(State %in% northwest, "northwest",
                                                   ifelse(State %in% northeast, "northeast", NA)))))

RegionalGoogleData <- na.omit(RegionalGoogleData)
head(RegionalGoogleData)
```


Here is some analysis of the 2017 google data. I begin by creating a few plots.
```{r}
StateSearches <- 
  ggplot(data = RegionalGoogleData, aes(x= condition, y= State)) +
  geom_tile(aes(fill=searchesPer)) +
  scale_fill_gradient2(low='white',mid='#5DADE2',high='black',midpoint=20) + 
  labs(title ="Searches by State (Per 100,000)",x = "Condition", y = "State", fill="Searches Per 100,000")+
  theme(axis.text.x = element_text(angle=45),
        axis.text.y = element_text(size = 5),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(hjust = 0))

StateSearches
```
*Analysis* From this graph I was able to see trends for each condition. For example, cancer, diarrhea, and stroke seem to be googled the most out of any condition. I was also able to see that certain states had way higher rates of searches, specifically Montana.

```{r}
RegionSearches <-
  ggplot(data=RegionalGoogleData,aes(x=condition,y=searchesPer,order=reorder(condition,searchesPer),fill=condition))+
  geom_bar(stat='identity',position='stack', width=.9)+
  scale_fill_brewer(type='seq',palette=1)+
  facet_wrap(~region,ncol=2)+
  labs(title = "Searches by Region (Per 100,000)",
       caption = "This plot shows the variation in number of searches for each region and each condition.")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(hjust = 0))
RegionSearches
```
*Analysis* This graph allowed me to see more variation in the condition searches between each region. In every region, cardiovascualr searches are the least common. In addition, I was able to see that the Southwest had much lower rates of searches compared to the other regions.



***

# Joining the two tables
Here, I quickly clean up both data frames by grouping by region and then joining the two tables.
```{r}
RegionalGoogleData <-
  RegionalGoogleData %>% 
  group_by(region, condition) %>% 
  summarise(total_searches = mean(searchesPer))
  
head(RegionalGoogleData)

insuranceData <-
  insuranceData %>% 
  group_by(region) %>% 
  summarise(total_charges = mean(charges))

head(insuranceData)
```


Next I join the two tables for a final analysis. This will work to answer my main question, is there correlation between average charges and average searches in a region?
```{R}
JoinedTable <- merge(RegionalGoogleData, insuranceData, by = 'region', type = "full", match = "all")

head(JoinedTable)
```


***


# Final Visualization
With my joined table I can check to see if there is correlation between searches and charges.
```{R}
JoinedPlot <- ggplot(JoinedTable, aes(x=total_searches, y=log(total_charges))) +
  geom_point(aes(color = condition, shape = region, size = 14)) +
  labs(title = "Charges VS Searches (Regional)",
              caption = "This plot shows the average charges per 100,000 people and average searches per 100,000 for each region and condition.")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(hjust = 0)) +
  guides(size = FALSE)  
JoinedPlot

```

*Analysis* Unfortunately, I did not find that there was much correlation between these two factors. It seems that even in regions where searches are extremely high, their charges are relatively low. Even though I found no significant correlation between the two, this leads me to believe there may be another factor that could affect the number of searches which I would like to explore eventually.

# Conclusion

Although I did not find any significant correlation regarding my original research question, I still was able to learn a lot through this project. Specifically, I was able to see correlation in searches and charges in varying regions. This leads me to ask further questions like why does the Southwest search health conditions much less than other regions? Is it due to the data or is there another factor causing it? Most importantly, this project allowed me to put my R skills into action with a totally new dataset. I was able to test my skills and use what I learned to answer a question that interested me.




















