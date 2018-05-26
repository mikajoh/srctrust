## Mikael Poul Johannesson
## 2018

## Start matter ------------------------------------------------------

library(here)
library(haven)
library(tidyverse)

if (!require(srctrust)) {
  devtools::install_github("mikajoh/srctrust")
}

## Get the raw data --------------------------------------------------

## The Norwegian Citizen Panel (Wave 8).
## See `raw/docs/` for documentation.
## Md5sum: 81849e3f0f8f056ee4f6406a8ac69b40
## tools::md5sum(here("raw", "Norwegian Citizen Panel - wave 8 EN.sav"))
ncp_raw <- read_sav(
  here("raw", "Norwegian Citizen Panel - wave 8 EN.sav")
)

## Tidy experiment data ----------------------------------------------

## `rsp_` denotes respondent-level information.
trust_01 <-
  ncp_raw %>%
  mutate(
    rsp_id = as.numeric(responseid),
    rsp_age = as.numeric(r8P5_1),
    rsp_age_cat = case_when(
      r8P5_2 == 1 ~ "18-29 yrs",
      r8P5_2 == 2 ~ "30-59 yrs",
      r8P5_2 == 3 ~ "60 yrs and above"),
    rsp_gender = case_when(
      r8P1 == 1 ~ "Male",
      r8P1 == 2 ~ "Female"),
    rsp_edu = case_when(
      r8B3_1 %in% 1:2 ~ "Lower or intermediate",
      r8B3_1 == 3     ~ "Higher"),
    rsp_pol_int = case_when(
      r8k1 %in% 1:5 ~ 6 - as.numeric(r8k1)),
    rsp_pol_party = case_when(
      r8k204 == 1 ~ "krf",
      r8k204 == 2 ~ "h",
      r8k204 == 3 ~ "frp",
      r8k204 == 4 ~ "v",
      r8k204 == 5 ~ "sv",
      r8k204 == 6 ~ "sp",
      r8k204 == 7 ~ "mdg",
      r8k204 == 8 ~ "ap",
      r8k204 == 9 ~ "red"),
    rsp_pol_scale = case_when(
      r8k8_1 %in% 1:11 ~ as.numeric(r8k8_1)),
    rsp_pol_side = case_when(
      rsp_pol_scale %in% 1:5  ~ "Left",
      rsp_pol_scale == 6      ~ "Centre",
      rsp_pol_scale %in% 7:11 ~ "Right"),
    rsp_trust = case_when(
      r8k15 %in% 1:10 ~ as.numeric(r8k15)),
    rsp_att_punish = as.numeric(r8straff),
    rsp_att_growth = as.numeric(r8vekst),
    rsp_att_redist = as.numeric(r8storstat),
    rsp_att_selfeffort = as.numeric(r8egeninnsats),
    rsp_att_gayrights = as.numeric(r8parlikhet),
    rsp_att_eu = as.numeric(r8euint),
    rsp_att_imm = as.numeric(r8innvandring),
    rsp_para_interface = case_when(
      r8enhetstype %in% c(1, 3) ~ "pc or generic",
      r8enhetstype == 2         ~ "touch"),
    rsp_para_device = case_when(
      r8mobil == 1 ~ "Did not use smart phone",
      r8mobil == 2 ~ "Used smart phone"),
    ) %>%
  mutate_at(
    vars(matches("rsp_att_")),
    function(x) ifelse(x %in% 1:7, 8 - x, NA)
  )

## The raw experiment data comes in the form of one variable for each
## cell of the conjoint table. The `r8pad2_dimrantext_\\d` variables
## are the left side treatment headers as shown to respondents;
## `srctrust::treat_names()` returns a df which matches these
## headers with the var names we want.
trust_02 <-
  trust_01 %>%
  select(matches("rsp_"), matches("r8pad2")) %>%
  gather(
    row_treat, treat_lab,
    matches("r8pad2_dimrantext_"),
    na.rm = TRUE
  ) %>%
  left_join(treat_names(), by = "treat_lab") %>%
  filter(!is.na(treat)) %>%
  mutate(
    row_treat = gsub("^.*(\\d)$", "\\1", row_treat),
    row_treat = as.numeric(row_treat)
  )

## The `r8pad2_dimranlabel(\\d)_(\\d)` variables shows which value
## label were shown in which \\1 column and \\2 row of the conjoint
## table. `srctrust::val_labs()` returns a df which matches these
## value labels (and prev retrieved treatment names) to new value
## labels in english. Also note that `exp_` denotes experiment-level
## information and `src_` denotes source/choice-level information.
trust_03 <-
  trust_02 %>%
  gather(row_val, val_no, matches("r8pad2_dimranlabel")) %>%
  mutate(
    row = gsub("^r8pad2_dimranlabel(\\d)_(\\d)$", "\\2", row_val),
    row = as.numeric(row),
    src_n = gsub("^r8pad2_dimranlabel(\\d)_(\\d)$", "\\1", row_val),
    src_n = as.numeric(src_n)
  ) %>%
  filter(row == row_treat) %>%
  left_join(val_labs(), by = c("treat", "val_no")) %>%
  select(-matches("row"), -fct_lvl, -treat_lab, -val_no) %>%
  spread(treat, val) %>%
  mutate(
    exp_post = ifelse(r8pad2 %in% 1:2, r8pad2, NA),
    src_post = case_when(
      exp_post == src_n ~ 1,
      exp_post != src_n ~ 0)
  ) %>%
  select(matches("rsp_"), matches("exp_"), matches("src_")) %>%
  filter(!is.na(src_post))

trust <- trust_03

## Write data to file ------------------------------------------------

write.csv(
  x = trust,
  file = here("data", "trust.csv"),
  row.names = FALSE
)
