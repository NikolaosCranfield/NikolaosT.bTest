






library(tidyverse)
df1 %>%
  mutate(Season = ifelse(Month %in% 6:10, 'wet season', 'dry season'))