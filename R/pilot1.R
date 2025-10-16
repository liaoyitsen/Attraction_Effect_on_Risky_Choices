library(magrittr)
library(dplyr)
library(stringr)

options(dplyr.summarise.inform = FALSE)

pilot1 <- data.table::fread(
  here::here("data/raw_data/attraction_effect_pilot1.csv"),
  header = TRUE, na.strings = '', check.names = TRUE
)[-c(1,2),]%>%
  dplyr::filter(!duplicated(PROLIFIC_PID))%>%
  dplyr::mutate(treatment = dplyr::case_when(treatment == "mutable" ~ "Payoff_deocy",
                                             treatment == "immutable" ~ "Prob_decoy",
                                             T ~ treatment))%>%
  dplyr::mutate(treatment = dplyr::case_when(treatment == "mutable" ~ "Payoff_deocy",
                                             treatment == "immutable" ~ "Prob_decoy",
                                             T ~ treatment))

    