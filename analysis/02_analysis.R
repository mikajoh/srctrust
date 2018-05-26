## Mikael Poul Johannesson
## 2018

## Start matter ------------------------------------------------------

library(here)
library(haven)
library(tidyverse)

source("labels.R")

## Get the data ------------------------------------------------------

## Prepared data from The Norwegian Citizen Panel (Wave 8).
## Md5sum: 870c9cbcc75a39a60a93297da18d3e20
## tools::md5sum(here("data", "trust.csv"))
trust_raw <- read.csv(
  here("data", "trust.csv"),
  stringsAsFactors = FALSE
)
            

## Main effects ------------------------------------------------------

