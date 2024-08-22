library(tidyverse)
library(ggplot2)

data <- read_csv('data/fg_bat_data.csv')

data <- 
  data %>%
  group_by(MLBAMID) %>%
  arrange(MLBAMID, Season) %>%
  # Create a column that checks for consecutive seasons
  mutate(season_gap = Season - lag(Season, default = first(Season))) %>% 
  filter(season_gap <= 1) %>% 
  ungroup()

# Calculate the differences for the following variables
data <- data %>%
  group_by(MLBAMID) %>% 
  arrange(MLBAMID, Season) %>%
  mutate(HR_diff = HR - lag(HR),
         R_diff = R - lag(R),
         RBI_diff = RBI - lag(RBI),
         SB_diff = SB - lag(SB),
         wOBA_diff = wOBA - lag(wOBA),
         wRC_plus_diff = `wRC+` - lag(`wRC+`),
         AVG_plus_diff = `AVG+` - lag(`AVG+`),
         BB_percent_plus_diff = `BB%+` - lag(`BB%+`),
         K_percent_plus_diff = (`K%+` - lag(`K%+`)) * -1,
         OBP_plus_diff = `OBP+` - lag(`OBP+`),
         SLG_plus_diff = `SLG+` - lag(`SLG+`),
         ISO_plus_diff = `ISO+` - lag(`ISO+`),
         BABIP_plus_diff = `BABIP+` - lag(`BABIP+`),
         WAR_per_162 = (WAR * 162) / G,
         WAR_per_162_diff = WAR_per_162 - lag(WAR_per_162),
         K = (`K%` * PA),
         BB = (`BB%` * PA),
         TTO_pct = (K + BB + HR) / PA,
         TTO_diff = TTO_pct - lag(TTO_pct)) %>% 
  drop_na(ends_with("plus_diff")) %>% 
  rowwise() %>% 
  mutate(mean_hit_diff = mean(c(wRC_plus_diff:BABIP_plus_diff))) %>% 
  ungroup()

schwarber <- data %>% filter(Name == "Kyle Schwarber")
schwarber %>% filter(Season == 2024) %>% select(-c(3:13))

### Filter the data for Players with similar seasons to Kyle Schwarber
### The requirements are as follow
## 1. Have to be hitting for power (At least 25 home runs, and above average
## SLG+ and ISO+, and not a super high BABIP+)
## 2. Have to walk frequently (BB%+ >= 100)
## 3. Be a bad defender (Def score less than 0), sorry Kyle but the field isn't
## your place.
## 4. Those with very few Intentional Walks. Intentional Walks are way less
## frequent in MLB in 2024 than they have been in the past. (We'll look for
## players with a moderate amount of IBB)

filtered_data <-
  data %>% 
  filter(HR >= 25, wOBA >= 0.365,
         `K%+` >= 115, `OBP+` >= 115, `SLG+` >= 115, `AVG+` <= 105,
         `wRC+` >= 135, `BB%+` >= 175, `BABIP+` <= 105, `ISO+` >= 140, 
         IBB <= 20, Def <= 0) %>%
  arrange(desc(mean_hit_diff)) %>% 
  select(Name, Season, ends_with("plus_diff"), WAR_per_162_diff, TTO_pct, 
         TTO_diff, mean_hit_diff) %>% 
  drop_na()
filtered_data
  
write_csv(filtered_data, "data/filtered_data.csv")

rm(data, filtered_data)

bat <- read_csv('data/fg_bat_22_24.csv')

bat <- bat %>% 
  filter(PA >= 1500) %>% 
  mutate(`1B` = `1B` / PA,
         `K%+` = -`K%+`,
         across(c(`1B`:`BABIP+`, `Barrel%`, `HardHit%`, WPA, wRAA), 
                ~rank(-1 * ., ties.method = "first")
               )
         ) %>% 
  rowwise() %>% 
  mutate(ovr_rank = mean(c(`HR`:wOBA, `wRC+`, `BB%+`:`BABIP+`, `Barrel%`, 
                           `HardHit%`)
                         ),
         hit_rank = mean(c(HR, RBI, wOBA, `wRC+`, `BB%+`, `ISO+`, 
                           `Barrel%`)
                        ) 
         ) %>% 
  arrange(hit_rank)
  
write_csv(bat, 'data/bat_ranks_22_24.csv')
