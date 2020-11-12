# This file is for fleshing out the rest of q1
library(tidyverse)
library(stringr)
library(rvest)
library(data.table)

# Read in the descriptions dataset as my written file from "Question 1 - Scraping"
poke.data.descriptions.test <- read_csv(file.choose())
# Read in the no-descriptions dataset as Alex's emailed, untouched csv
poke.data.no.desc.test <- read_csv(file.choose())

# Join the 2
poke.data.combined.desc <- full_join(poke.data.descriptions.test, poke.data.no.desc.test)

# Take only the important columns
poke.data.combined.desc <- poke.data.combined.desc %>% 
  select(c("#", "Name", "Type 1", "Type 2", "Total", "HP", "Attack", "Defense", "Sp. Atk", "Sp. Def", "Speed", "Generation", "Legendary", "url", "desc.1", "desc.2"))

# Replace the missing values for desc.1 and desc.2 with a short error message and a space (for shiny output), respectively
poke.data.combined.desc$desc.1 <- ifelse(is.na(poke.data.combined.desc$desc.1), "The description information for this Pokemon is not readily accessible.", poke.data.combined.desc$desc.1)
poke.data.combined.desc$desc.2 <- ifelse(is.na(poke.data.combined.desc$desc.2), "", poke.data.combined.desc$desc.2)

# They may be out of order; clean them up
poke.data.combined.desc <- poke.data.combined.desc %>% 
  arrange(`#`)

# Save all your hard work
write.csv(poke.data.combined.desc, file = "poke.data.combined.csv")
