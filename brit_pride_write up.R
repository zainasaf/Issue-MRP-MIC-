library(here)
library(tidyverse)

brit_pride_1 <-  read.csv(here("output", "preds_britishpride_reweighted_small.csv"))





brit_pride_2 <- brit_pride_1 %>%
  mutate(
    proud = very_proud + quite_proud,
    ashamed = very_ashamed + quite_ashamed
  ) %>%
  select(const_name, proud, ashamed, neither, net_score_reweighted)


```
## proprotion who say poud  

summary(brit_pride_2$proud) ## 60 % 

## ashamed  ## 

summary(brit_pride_2$ashamed) ## 14

 

summary(bbc_2$dont_know) #20




## vey proud v somwaht proude - 

summary(brit_pride_1$very_proud) -# 32%
  summary(brit_pride_1$quite_proud)  ## 27%


```


##

negative_net <- brit_pride_1 %>%
  filter(net_score_reweighted < 0) %>%
  select(const_name, net_score_reweighted) %>%
  arrange(net_score_reweighted)

## No constineuncies where think has right wing bias 

nrow(negative_net)
negative_net


greater_40 <- brit_pride_1 %>% 
  filter(net_score_reweighted >= 40) %>%
  select(const_name, net_score_reweighted) %>%
  arrange(net_score_reweighted)

### 244 constituneices where left > right by 40 or more 


nrow(negative_net)
negative_net


region_pride_1 <- brit_pride_1 %>% group_by(region_2) %>%
  summarise(region_avg = mean(net_score_reweighted)) 
## England 39 
## Scotland 25
## Wales 28



greater_15 <- bbc_1 %>% 
  filter(net_score_reweighted >= 15) %>%
  select(const_name, net_score_reweighted) %>%
  arrange(net_score_reweighted)





top_150 <- bbc_1 %>% 
  slice_max(order_by = net_score_reweighted, n = 150, with_ties = FALSE)

bottom_150 <- bbc_1 %>% 
  slice_min(order_by = net_score_reweighted, n = 150, with_ties = FALSE)

top_150_mean <- top_150 %>% 
  summarise(mean_net_score = mean(net_score_reweighted, na.rm = TRUE))

bottom_150_mean <- bottom_150 %>% 
  summarise(mean_net_score = mean(net_score_reweighted, na.rm = TRUE))





# Most proud
most_proud <- brit_pride_1 %>% slice_max(net_score_reweighted, n = 1) %>% select(const_name, net_score_reweighted)

#most ashamed
most_ashamed <- brit_pride_1 %>% slice_min(net_score_reweighted, n = 1) %>% select(const_name, net_score_reweighted)

# Median constituency
median <- brit_pride_1 %>% slice_min(abs(net_score_reweighted - median(net_score_reweighted)), n = 1) %>% select(const_name, net_score_reweighted)



iqr_lower <- quantile(brit_pride_1$net_score_reweighted, 0.25)
iqr_upper <- quantile(brit_pride_1$net_score_reweighted, 0.75)

iqr_lower
