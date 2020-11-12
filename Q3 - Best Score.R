# This data will be helpful for question 3
library(tidyverse)
library(stringr)
library(rvest)
library(data.table)
poke.data.comb <- read_csv(file.choose())

# Let's make some z-scores:
create.z.scores <- poke.data.comb %>% 
  mutate(z.hp = (HP-mean(HP))/sd(HP)) %>% 
  mutate(z.atk = (Attack-mean(Attack))/sd(Attack)) %>% 
  mutate(z.def = (Defense-mean(Defense))/sd(Defense)) %>% 
  mutate(z.sp.atk = (`Sp. Atk`-mean(`Sp. Atk`))/sd(`Sp. Atk`)) %>% 
  mutate(z.sp.def = (`Sp. Def`-mean(`Sp. Def`))/sd(`Sp. Def`)) %>% 
  mutate(z.spd = (Speed-mean(Speed))/sd(Speed))

# Now, I want to make a column that tells me what variable has the highest z-score for each poke
poke.best.stat <- create.z.scores %>% 
  select(c("z.hp", "z.atk", "z.def", "z.sp.atk", "z.sp.def",  "z.spd")) 

# Now I will find the maximum z-score for each row (Pokemon), and add that as a column with string values to the dataset
list.best.stats <- apply(poke.best.stat,1,which.max) 
list.best.stats <- names(poke.best.stat)[z] 
data.best.stats <- as.data.frame(list.best.stats)
poke.desc.comb.z <- cbind(poke.data.comb, data.best.stats)

# Now I will create some indicator variables that will help me in how I create my graph for question 3
poke.desc.comb.z <- poke.desc.comb.z %>% 
  mutate(best.hp = ifelse(list.best.stats == "z.hp", 1, 0)) %>% 
  mutate(best.atk = ifelse(list.best.stats == "z.atk", 1, 0)) %>% 
  mutate(best.def = ifelse(list.best.stats == "z.def", 1, 0)) %>% 
  mutate(best.sp.atk = ifelse(list.best.stats == "z.sp.atk", 1, 0)) %>% 
  mutate(best.sp.def = ifelse(list.best.stats == "z.sp.def", 1, 0)) %>%   
  mutate(best.spd = ifelse(list.best.stats == "z.spd", 1, 0))

# Save all my hard work
poke.data.comb <- poke.desc.comb.z

write.csv(poke.data.comb, "poke.data.comb.csv")
         