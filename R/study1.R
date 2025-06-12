library(magrittr)
library(dplyr)
library(stringr)

options(dplyr.summarise.inform = FALSE)

study1 <- data.table::fread(here::here("data/raw_data/attraction_effect_study1.csv"),
                                   header = T, na.strings = '')[-c(1,2),]%>%
  dplyr::filter(!is.na(politic))%>%
  dplyr::filter(!duplicated(PROLIFIC_PID))%>%
  tidyr::unite("treatment_1", c(treatment_risky_1, treatment_safe_1), na.rm = T)%>%
  tidyr::unite("treatment_2", c(treatment_risky_2, treatment_safe_2), na.rm = T)%>%
  tidyr::unite("treatment_3", c(treatment_risky_3, treatment_safe_3), na.rm = T)%>%
  tidyr::unite("treatment_4", c(treatment_risky_4, treatment_safe_4), na.rm = T)%>%
  tidyr::unite("treatment_5", c(treatment_risky_5, treatment_safe_5), na.rm = T)%>%
  tidyr::unite("control_1", c(control_risky_1, control_safe_1), na.rm = T)%>%
  tidyr::unite("control_2", c(control_risky_2, control_safe_2), na.rm = T)%>%
  tidyr::unite("control_3", c(control_risky_3, control_safe_3), na.rm = T)%>%
  tidyr::unite("control_4", c(control_risky_4, control_safe_4), na.rm = T)%>%
  tidyr::unite("control_5", c(control_risky_5, control_safe_5), na.rm = T)%>%
  tidyr::pivot_longer(cols = c(treatment_1:control_5), names_to = "trail", values_to = "raw_choice")%>%
  dplyr::group_by(PROLIFIC_PID)%>%
  dplyr::mutate(n_decoy = sum(raw_choice == "decoy"))%>%
  dplyr::filter(n_decoy <= 5)%>%
  tidyr::separate_wider_delim(trail, "_", names = c("treatment", "pair"))%>%
  dplyr::mutate(choice = ifelse((condition == "risk_seeking" & raw_choice == "risky") |
                                  (condition == "risk_averse" & raw_choice == "safe"), "focal", "non_focal"))%>%
  dplyr::rename("duration" = dplyr::starts_with("Duration"))


saveRDS(study1, file = here::here("data/study1.Rds"))
haven::write_dta(study1,
                 path = here::here("data/study1.dta"))


study1_complete <- data.table::fread(here::here("data/raw_data/attraction_effect_study1.csv"),
                            header = T, na.strings = '')[-c(1,2),]%>%
  dplyr::filter(!is.na(politic))%>%
  dplyr::filter(!duplicated(PROLIFIC_PID))%>%
  tidyr::unite("treatment_1", c(treatment_risky_1, treatment_safe_1), na.rm = T)%>%
  tidyr::unite("treatment_2", c(treatment_risky_2, treatment_safe_2), na.rm = T)%>%
  tidyr::unite("treatment_3", c(treatment_risky_3, treatment_safe_3), na.rm = T)%>%
  tidyr::unite("treatment_4", c(treatment_risky_4, treatment_safe_4), na.rm = T)%>%
  tidyr::unite("treatment_5", c(treatment_risky_5, treatment_safe_5), na.rm = T)%>%
  tidyr::unite("control_1", c(control_risky_1, control_safe_1), na.rm = T)%>%
  tidyr::unite("control_2", c(control_risky_2, control_safe_2), na.rm = T)%>%
  tidyr::unite("control_3", c(control_risky_3, control_safe_3), na.rm = T)%>%
  tidyr::unite("control_4", c(control_risky_4, control_safe_4), na.rm = T)%>%
  tidyr::unite("control_5", c(control_risky_5, control_safe_5), na.rm = T)%>%
  tidyr::pivot_longer(cols = c(treatment_1:control_5), names_to = "trail", values_to = "raw_choice")%>%
  dplyr::group_by(PROLIFIC_PID)%>%
  dplyr::mutate(n_decoy = sum(raw_choice == "decoy"))%>%
  tidyr::separate_wider_delim(trail, "_", names = c("treatment", "pair"))%>%
  dplyr::mutate(choice = ifelse((condition == "risk_seeking" & raw_choice == "risky") |
                                  (condition == "risk_averse" & raw_choice == "safe"), "focal", "non_focal"))%>%
  dplyr::rename("duration" = dplyr::starts_with("Duration"))


saveRDS(study1_complete, file = here::here("data/study1_complete.Rds"))
haven::write_dta(study1_complete,
                 path = here::here("data/study1_complete.dta"))

