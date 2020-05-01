library(tidyverse)
library(readr)

co_income <- read_csv("/Users/monhait/Desktop/r/nichole/data/IncomePoverty_Census_Tracts.csv", col_names = TRUE)

n <- 10

poverty_by_county <- co_income %>% 
  select(County, Percent_Poverty_AllPeople_Income_Below_Pov_Level) %>% 
  group_by(County) %>% 
  summarize(average_poverty = mean(Percent_Poverty_AllPeople_Income_Below_Pov_Level, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(County = as_factor(str_to_title(County)) %>% fct_lump(n = n, w = average_poverty) %>% 
           fct_reorder(average_poverty)) %>% 
  filter(County != "Other") %>%          
  mutate(plot_text = str_glue("County: {County}\nPercent Poverty: {average_poverty}%"))



poverty_by_county %>% 
  
  #Geometries
  ggplot(aes(County, average_poverty)) +
  geom_segment(aes(yend = 0, xend = County)) +
  geom_point() +
  geom_text(aes(label = plot_text),
            hjust = "outward",
            size = 3) +
  
  #Formatting
  scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
  coord_flip() +
  
  #Labels
  labs(title = "Colorado Counties with the Highest Poverty Percentage",
       subtitle = str_glue("Top {n} based on CDPHE Open Data Portal"),
       y = "Avg. Percent of all People Below Poverty Limit (%)",
       x = "County Name") +
  theme_minimal()
