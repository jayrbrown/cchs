# Functions for working with CCHS Survey results.
library(lazyeval)


healthPalette <- c("#33FF00", "#339900", "#33CC99", "#FFCC00", "#FF0000")
healthPaletteL <- c("#33FF00", "#33D900", "#33B400", "#339D0D", "#33B045", "#33C27D", "#58CC7D", "#A2CC45", "#ECCC0D", "#FF9400", "#FF4A00", "#FF0000")
healthPaletteM <- c("#33FF00", "#33E300", "#33C800", "#33AD00", "#339C0A", "#33AA32", "#33B75B", "#33C584", "#4ECC84", "#84CC5B", "#BACC33", "#F1CC0A", "#FFA300", "#FF6C00", "#FF3600", "#FF0000")


barPalette <- c("#AAAAAA", "#33cc33", "#FFCC00", "#CC0000", "#0000CC")
barPalette2 <- c("#FFCC00", "#CC0000")

fixName <- function(strng) {
  a <- gsub("_dv", "", strng)
  a <- gsub("_", " ", a)
  a <- str_to_title(a)
  return(a)
}

spreadMyGroup <- function(df, a, b) {
  df %>%
    filter(!is.na({{a}}), !is.na({{b}})) %>% 
    group_by({{a}}, {{b}}) %>% 
    tally() %>% 
    spread({{a}}, n)
}

# this should address all needs
parseGroup <- function(df, a, b) {
  df %>%
    # select({{a}}, {{b}}) %>% 
    filter(!is.na({{a}}), !is.na({{b}})) %>% 
    group_by({{a}}, {{b}}) %>% 
    tally() %>% 
    mutate(percent = n/sum(n))
}

parseGroupTri <- function(df, a, b, c) {
  df %>%
    # select({{a}}, {{b}}) %>% 
    filter(!is.na({{a}}), !is.na({{b}}), !is.na({{c}})) %>% 
    group_by({{a}}, {{b}}, {{c}}) %>% 
    tally() %>% 
    mutate(percent = n/sum(n))
}

