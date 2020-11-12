# exam 2
library(tidyverse)
library(stringr)
library(rvest)
library(data.table)

# Unachieved goal: make a different url for the pokemon with "Mega" and other odd cases
# so that I can still scrape a description.

# This might be useful for finding anything other than name: all(sapply(str, grepl, myStr))
# https://stackoverflow.com/questions/30180281/how-can-i-check-if-multiple-strings-exist-in-another-string
# This might also be useful in that, after I use it to rename my data, I can find any names with upper
# letter after the first letter of the name: 
# paste(tolower(substr(poke.data.filt$Name, 1, 1)), substr(poke.data.filt$Name, 2, nchar(poke.data.filt$Name)), sep="")


# I have all the descriptions I am going to be able to get. I can come back to getting the
# more difficult entries later if I have time. If I can come back, I want to look throught the edge cases
# (i.e. the cases where I filter out the "weirdos" and "punctuation") and see if I can find a pattern.
# With enough extra time, I can always enter some of these manually

# Read in Alex's dataset
poke.data <- read_csv(file.choose())

### Locate the start of "Mega"
### mega.loc <- str_locate(poke.data$Name, "Mega")
# Never used

# Try and deal with all the edge cases (weird characters, multiple forms, etc.)
poke.data.filt <- poke.data %>% 
  mutate(mega = str_detect(poke.data$Name,
                           "Mega |Primal |Forme|Cloak|Mode| Rotom|Size|Confined|Unbound| Kyurem|Male|Female")) %>%  #detects weirdos
  mutate(other.char = str_detect(poke.data$Name, regex("[:punct:]"))) #detects punct

# Add the URLs for the non-edge cases
poke.data.filt <- poke.data.filt %>% 
  mutate(url = ifelse(mega == FALSE & other.char == FALSE, 
                      paste("https://bulbapedia.bulbagarden.net/wiki/",
                            poke.data.filt$Name,
                            "_(Pokémon)",
                            sep = ""),
                      NA))

# Test some random URLs
poke.data.filt[sample(nrow(poke.data.filt), 3), 16]

# Narrow down to only valid URLs
valid.url <- poke.data.filt %>% 
  filter(mega == FALSE) %>% 
  filter(other.char == FALSE)

# Break up into human-sized sets that aren't blocked by the website
valid.url.200 <- valid.url %>% 
  filter(`#` < 200)

valid.url.400 <- valid.url %>%
  filter(`#` >= 200 & `#` < 400)

valid.url.600 <- valid.url %>%
  filter(`#` >= 400 & `#` < 600)

valid.url.800 <- valid.url %>%
  filter(`#` >= 600)

# Scrape the two-part descriptions for each of the pokemon with a valid URL
# First part
descriptions.200.1 <- NULL
for(i in 1:length(valid.url.200$url)){
  descriptions.200.1[i] <- valid.url.200$url[i] %>% 
    read_html() %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    .[[1]]
}

descriptions.400.1 <- NULL
for(i in 1:length(valid.url.400$url)){
  descriptions.400.1[i] <- valid.url.400$url[i] %>% 
    read_html() %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    .[[1]]
}

descriptions.600.1 <- NULL
for(i in 1:length(valid.url.600$url)){
  descriptions.600.1[i] <- valid.url.600$url[i] %>% 
    read_html() %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    .[[1]]
}

descriptions.800.1 <- NULL
for(i in 1:length(valid.url.800$url)){
  descriptions.800.1[i] <- valid.url.800$url[i] %>% 
    read_html() %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    .[[1]]
}

# Second part
descriptions.200.2 <- NULL
for(i in 1:length(valid.url.200$url)){
  descriptions.200.2[i] <- valid.url.200$url[i] %>% 
    read_html() %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    .[[2]]
}

descriptions.400.2 <- NULL
for(i in 1:length(valid.url.400$url)){
  descriptions.400.2[i] <- valid.url.400$url[i] %>% 
    read_html() %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    .[[2]]
}

descriptions.600.2 <- NULL
for(i in 1:length(valid.url.600$url)){
  descriptions.600.2[i] <- valid.url.600$url[i] %>% 
    read_html() %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    .[[2]]
}

descriptions.800.2 <- NULL
for(i in 1:length(valid.url.800$url)){
  descriptions.800.2[i] <- valid.url.800$url[i] %>% 
    read_html() %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    .[[2]]
}


# Add the descriptions to each of the subsetted data tables, rename the columns to be able to use rbind
valid.url.200.comb <- cbind(valid.url.200, descriptions.200.1, descriptions.200.2)
colnames(valid.url.200.comb)[17] <- "desc.1"
colnames(valid.url.200.comb)[18] <- "desc.2"
  
valid.url.400.comb <- cbind(valid.url.400, descriptions.400.1, descriptions.400.2)
colnames(valid.url.400.comb)[17] <- "desc.1"
colnames(valid.url.400.comb)[18] <- "desc.2"

valid.url.600.comb <- cbind(valid.url.600, descriptions.600.1, descriptions.600.2)
colnames(valid.url.600.comb)[17] <- "desc.1"
colnames(valid.url.600.comb)[18] <- "desc.2"

valid.url.800.comb <- cbind(valid.url.800, descriptions.800.1, descriptions.800.2)
colnames(valid.url.800.comb)[17] <- "desc.1"
colnames(valid.url.800.comb)[18] <- "desc.2"

# Combine all tables using rbind
valid.url.desc.all <- rbind(valid.url.200.comb, valid.url.400.comb, valid.url.600.comb, valid.url.800.comb)

# Save all your hard work
# write.csv(valid.url.desc.all, file = "descriptions.csv")








