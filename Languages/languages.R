library(readxl)
library(tidyverse)
library(gt)
library(purrr)

# read in data
df <- tibble(read_xlsx("Languages/100 Most Spoken Languages.xlsx")) %>% 
               slice_head(n = 10)

# add text description column
df <- df %>%
  mutate(description =
           c("English is the most spoken language in the world in terms of total speakers, although only the second most common in terms of native speakers.",
             "Mandarin Chinese is the only language of Sino-Tibetan origin to make the top ten, and has the highest number of native speakers.",
             "Hindi is the third most spoken language in the work, and is an official language of India, and Fiji, amongst others.",
             "Spanish is an Indo-European language, spoken by just over half a billion people worldwide.",
             "French is an official language of Belgium, Switzerland, Senegal, alongside France and 25 other countries. It is also of Indo-European origin.",
             "Standard Arabic is the only language of Afro-Asiatic origin in the top ten, and has no native speakers according to www.ethnologue.com.",
             "Bengali is the official and national language of Bangladesh, with 98% of Bangladeshis using Bengali as their first language.",
             "Russian is an official language of only four countries: Belarus, Kazakhstan, Kyrgyzstan and Russia; with over a quarter of a billion total speakers.",
             "Portuguese is spoken by just under a quarter of a billion people, with almost all (around 95%) being native speakers.",
             "Indonesian is the only Austronesian language to make the top ten"))

# format numeric columns
df <- df %>% 
  mutate(
    Total = as.numeric(
      unlist(lapply(
        regmatches(`Total Speakers`, gregexpr("[[:digit:]]+", `Total Speakers`)), function(x) str_flatten(x)))),
    Native = as.numeric(
      unlist(lapply(
        regmatches(`Native Speakers`, gregexpr("[[:digit:]]+", `Native Speakers`)), function(x) str_flatten(x)))))

# replace NA with 0
df <- df %>% 
  mutate(Native = replace_na(Native, 0),
         Nonnative = Total - Native) %>%
  select(Rank, Language, description, Total, Native, `Non-native`)

# function to make plots









