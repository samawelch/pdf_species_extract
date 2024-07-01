
library(pdftools)
library(readr)
library(tidyverse)

# species_list <- read_tsv(file = "NameUsage.tsv", col_select = c(8, 10), n_max = 100000000) |>
#   filter(`col:rank` == "species") |> 
#   select(-2)

# write_csv(x = species_list, file = "species_list.csv")

species_list <- read.csv(file = "species_list.csv")


full_text <- pdf_text(pdf = "Guernsey Red Data Book FULL BOOK_March 2020.pdf")

full_text_pnumber <- tibble(text = full_text, pnumber = 1:195)

genus_list <- species_list |> reframe(genus = str_split_i(col.scientificName, pattern = " ", 1)) |> 
  distinct()
  
genus_vector <- as.vector(genus_list$genus)


word_matches <- full_text_pnumber |> 
  reframe(text_matches = str_extract_all(text, pattern = "[A-Z][a-z]+ [a-z]+", simplify = FALSE),
            pnumber) |> 
  separate_longer_delim(",", cols = text_matches) |> 
  mutate(text_matches = str_remove_all(text_matches, "\"") |> 
           str_remove("c\\(") |> str_remove("\\)") |> trimws()) |> 
  filter(!str_detect(text_matches, "haract")) |> 
  mutate(firstword = str_split_i(text_matches, pattern = " ", 1))

genus_matches <- word_matches |> filter(firstword %in% genus_vector) |> 
  filter(str_detect(text_matches, pattern = "\\b\\w*(um|us|is|a|ii|o|ix|es|eri|ans)\\b")) |> 
  filter(!str_detect(text_matches, pattern = "(Data|Sea|Marine|Bees|buttercup|Species|Common|Here|bees|Small|Spring|and|Red|Tree|roots)")) |> 
  select(-firstword)

write_csv(x = genus_matches, file = "red_data_species_names.csv")
