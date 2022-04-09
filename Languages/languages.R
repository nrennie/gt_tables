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
        regmatches(`Total Speakers`, gregexpr("[[:digit:]]+", `Total Speakers`)), 
        function(x) str_flatten(x)))),
    Native = as.numeric(
      unlist(lapply(
        regmatches(`Native Speakers`, gregexpr("[[:digit:]]+", `Native Speakers`)), 
        function(x) str_flatten(x)))))

# replace NA with 0
df <- df %>% 
  mutate(Native = replace_na(Native, 0),
         Nonnative = Total - Native) %>%
  select(Rank, Language, description, Total, Native, Nonnative)

# function to make plots
plot_lang <- function(lang, df){
  # prep data
  p_data <- filter(df, Language == lang) %>%
    select(Native, Nonnative, Total) %>%
    pivot_longer(1:2) %>%
    mutate(x = 1,
           name = factor(name, levels = c("Nonnative", "Native")))
  # limits
  lower <- filter(p_data, name == "Native")$value
  upper <- unique(p_data$Total)
  if ((upper - lower) < 100){
    upper = upper + 50
    lower = lower - 50
  }
  # make plot
  ggplot(data = p_data,
         mapping = aes(x = x, y = value, fill = name)) +
    geom_col() +
    scale_fill_manual(values = c("lightgrey", "#355C7D")) + #355C7D
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, plyr::round_any(max(df$Total), 100, ceiling)),
                       breaks = c(lower, upper),
                       labels = c(filter(p_data, name == "Native")$value, unique(p_data$Total))) +
    theme_minimal() +
    coord_flip() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(colour = "#355C7D", size = 60, face = "bold"))
}

# map plots
all_lang <- df %>%
  pull(Language)
lang_plots <- purrr::map(.x = all_lang, .f = ~plot_lang(.x, df = df))

# final tibble
df <- df %>%
  mutate(plots = lang_plots) %>%
  select(Rank, Language, description, plots)

# make a table
tb <- df %>%
  select(Rank, Language, description) %>%
  mutate(plots = NA) %>%
  gt() 

# add in plots
tb <- tb %>% 
  text_transform(
  locations = cells_body(columns=plots),
  fn = function(x){
    purrr::map(
      df$plots, ggplot_image, height = px(80), aspect_ratio = 4
    )
  }
) 

# add header title, subtitle, source note
tb <- tb %>% 
  tab_header(
    title = "10 Most Spoken Languages",
    subtitle = "Diversity in Data are an initiative centered around diversity, equity & awareness. In December 2021, they released a dataset obtained from www.ethnologue.com detailing the 100 most spoken languages in the world. This table shows the ten most spoken languages in the world. Although English is the most spoken in terms of total speaks, Mandarin Chinese has the highest number of native speakers."
  ) %>%
  tab_source_note(
    source_note = "N. Rennie | Data: data.world/diversityindata"
  )

# edit text styling
tb <- tb %>% 
  tab_style(
    locations = cells_title(groups = 'title'),
    style = list(
      cell_text(
        size = "xx-large",
        weight="bold",
        color='#355C7D'
      ))) %>%
  tab_style(
    style = list(
      cell_text(
        align = "center",
        size='medium',
        color='#355C7D',
        weight="bold")),
    locations = cells_body(Rank)
  ) %>%
  tab_style(
    style = list(
      cell_text(
        align = "center")),
    locations = cells_column_labels(Rank)
  ) %>%
  tab_style(
    style = list(
      cell_text(
        size='large',
        color='#355C7D',
        weight="bold")),
    locations = cells_body(Language)
  ) %>%
  tab_style(
    style = list(
      cell_text(
        align = "left")),
    locations = cells_body(plots)
  ) %>%
  tab_style(
    style = list(
      cell_text(
        align = "left")),
    locations = cells_column_labels(plots)
  ) %>%
  tab_style(
    style = list(
      cell_text(size = 'small',
                align = "center")),
    locations = cells_source_notes()
  )

# edit column names
tb <- tb %>% 
  cols_label(
    Rank = "Rank",
    Language = "Language",
    description = "Description",
    plots = "Number of speakers (millions)"
  )

# edit column widths
tb <- tb %>% 
  cols_width(
    Rank ~ px(100),
    Language ~ px(135),
    description ~ px(400),
    plots ~px(400)
  )

# table width
tb <- tb %>% 
  tab_options(table.width = 1035,
              container.width = 1035)

# colour odd rows
tb <- tb %>% 
  tab_style(
    style = list(cell_fill(color = "#e4edf4")),
    locations = cells_body(rows = seq(1,9,2))
  )

# save table
gtsave(tb,"Languages/languages.png")









