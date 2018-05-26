## Mikael Poul Johannesson
## 2018

## Start matter ------------------------------------------------------

library(here)
library(haven)
library(tidyverse)

if (!require(srctrust)) {
  devtools::install_github("mikajoh/srctrust")
}

## Get the data ------------------------------------------------------

## Prepared data from The Norwegian Citizen Panel (Wave 8).
## Md5sum: ffcf95a452733e802f778516f83f6bd8
## tools::md5sum(here("data", "trust.csv"))
trust_raw <- read.csv(
  here("data", "trust.csv"),
  stringsAsFactors = FALSE
)

trust <- 
  trust_raw %>%
  mutate(
    src_adds = lvls_reorder(src_adds, c(3, 2, 1)),
    src_comments = lvls_reorder(src_comments, c(3, 2, 1)),
    src_established = lvls_reorder(src_established, c(2, 3, 1)),
    src_ethics = lvls_reorder(src_ethics, c(3, 1, 2)),
    src_party = lvls_reorder(
      src_party,
      c(6, 8, 10, 9, 1, 4, 2, 5, 3, 7)),
    src_readers = factor(src_readers),
    src_soft = lvls_reorder(src_soft, c(1, 3, 2)),
    src_status = lvls_reorder(src_status, c(2, 3, 1)),
    src_party_matched = lvls_reorder(src_party_matched, c(3, 1, 2))
  )

## Main effects ------------------------------------------------------

res_main <-
  trust %>%
  amce(
    src_post, src_adds, src_comments, src_established, src_ethics,
    src_readers, src_soft, src_status,
    cluster = "rsp_id"
  ) %>%
  add_labels()

fig_main <-
  res_main %>%
  ggplot(
    aes(
      x = estimate, y = value,
      xmin = estimate - (1.96 * std_error),
      xmax = estimate + (1.96 * std_error))) +
  facet_grid(
    treatment ~ .,
    scales = "free_y",
    space = "free_y") +
  geom_errorbarh(height = 0) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  scale_x_continuous(
    limits = c(-.3, .3),
    breaks = round(seq(-.3, .3, .1), 2),
    expand = c(0, 0),
    labels = function(x) x * 100) +
  scale_y_discrete(
    labels = function(x) parse(text = as.character(x))) +
  labs(
    x = "Marginal effect, selecting source (%)",
    y = "Source attributes"
  ) +
  theme_m()
fig_main

ggsave2(fig_main, "fig_main", width = 7, height = 5)

## Matched party effects ---------------------------------------------

res_prty <-
  trust %>%
  amce(src_post, src_party, src_party_matched, cluster = "rsp_id") %>%
  add_labels()

fig_prty <-
  res_prty %>%
  ggplot(
    aes(
      x = estimate, y = value,
      xmin = estimate - (1.96 * std_error),
      xmax = estimate + (1.96 * std_error))) +
  facet_grid(
    treatment ~ .,
    scales = "free_y",
    space = "free_y") +
  geom_errorbarh(height = 0) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  scale_x_continuous(
    limits = c(-.3, .3),
    breaks = round(seq(-.3, .3, .1), 2),
    expand = c(0, 0),
    labels = function(x) x * 100) +
  scale_y_discrete(
    labels = function(x) parse(text = as.character(x))) +
  labs(
    x = "Marginal effect, selecting source (%)",
    y = "Source attributes"
  ) +
  theme_m()
fig_prty

ggsave2(fig_prty, "fig_main_prty", width = 5.5, height = 3)

## Main effects by AGE -----------------------------------------------

res_sub_age <-
  trust %>%
  amce(
    src_post, src_adds, src_comments, src_established, src_ethics,
    src_readers, src_soft, src_status,
    subgroup = "rsp_age_cat",
    cluster = "rsp_id"
  ) %>%
  add_labels()

fig_sub_age <-
  res_sub_age %>%
  ggplot(
    aes(
      x = estimate, y = value,
      xmin = estimate - (1.96 * std_error),
      xmax = estimate + (1.96 * std_error))) +
  facet_grid(
    treatment ~ rsp_age_cat,
    scales = "free_y",
    space = "free_y") +
  geom_errorbarh(height = 0) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  scale_x_continuous(
    limits = c(-.4, .4),
    breaks = round(seq(-.4, .4, .2), 2),
    expand = c(0, 0),
    labels = function(x) x * 100) +
  scale_y_discrete(
    labels = function(x) parse(text = as.character(x))) +
  labs(
    x = "Marginal effect, selecting source (%)",
    y = "Source attributes"
  ) +
  theme_m()
fig_sub_age

ggsave2(fig_sub_age, "fig_sub_age", width = 12, height = 5)

## Main effects by GENDER --------------------------------------------

res_sub_gnd <-
  trust %>%
  amce(
    src_post, src_adds, src_comments, src_established, src_ethics,
    src_readers, src_soft, src_status,
    subgroup = "rsp_gender",
    cluster = "rsp_id"
  )

res_diff_gnd <-
  trust %>%
  mutate(rsp_gender = fct_rev(rsp_gender)) %>%
  amce(
    src_post, src_adds, src_comments, src_established, src_ethics,
    src_readers, src_soft, src_status,
    diff = "rsp_gender",
    cluster = "rsp_id"
  ) %>%
  mutate(rsp_gender = "Difference")

res_sub_gnd <-
  bind_rows(res_sub_gnd, res_diff_gnd) %>%
  add_labels() %>%
  mutate(rsp_gender = lvls_reorder(rsp_gender, c(3, 2, 1)))

fig_sub_gnd <-
  res_sub_gnd %>%
  ggplot(
    aes(
      x = estimate, y = value,
      xmin = estimate - (1.96 * std_error),
      xmax = estimate + (1.96 * std_error))) +
  facet_grid(
    treatment ~ rsp_gender,
    scales = "free_y",
    space = "free_y") +
  geom_errorbarh(height = 0) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  scale_x_continuous(
    limits = c(-.4, .4),
    breaks = round(seq(-.4, .4, .2), 2),
    expand = c(0, 0),
    labels = function(x) x * 100) +
  scale_y_discrete(
    labels = function(x) parse(text = as.character(x))) +
  labs(
    x = "Marginal effect, selecting source (%)",
    y = "Source attributes"
  ) +
  theme_m()
fig_sub_gnd

ggsave2(fig_sub_gnd, "fig_sub_gnd", width = 12, height = 5)

## Main effects by EDUCATION -----------------------------------------

res_sub_edu <-
  trust %>%
  amce(
    src_post, src_adds, src_comments, src_established, src_ethics,
    src_readers, src_soft, src_status,
    subgroup = "rsp_edu",
    cluster = "rsp_id"
  )

res_diff_edu <-
  trust %>%
  amce(
    src_post, src_adds, src_comments, src_established, src_ethics,
    src_readers, src_soft, src_status,
    diff = "rsp_edu",
    cluster = "rsp_id"
  ) %>%
  mutate(rsp_edu = "Difference")

res_sub_edu <-
  bind_rows(res_sub_edu, res_diff_edu) %>%
  add_labels() %>%
  mutate(rsp_edu = lvls_reorder(rsp_edu, c(3, 2, 1)))

fig_sub_edu <-
  res_sub_edu %>%
  ggplot(
    aes(
      x = estimate, y = value,
      xmin = estimate - (1.96 * std_error),
      xmax = estimate + (1.96 * std_error))) +
  facet_grid(
    treatment ~ rsp_edu,
    scales = "free_y",
    space = "free_y") +
  geom_errorbarh(height = 0) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  scale_x_continuous(
    limits = c(-.4, .4),
    breaks = round(seq(-.4, .4, .2), 2),
    expand = c(0, 0),
    labels = function(x) x * 100) +
  scale_y_discrete(
    labels = function(x) parse(text = as.character(x))) +
  labs(
    x = "Marginal effect, selecting source (%)",
    y = "Source attributes"
  ) +
  theme_m()
fig_sub_edu

ggsave2(fig_sub_edu, "fig_sub_edu", width = 12, height = 5)

## Main effects by POLSIDE -------------------------------------------

res_sub_polside <-
  trust %>%
  amce(
    src_post, src_adds, src_comments, src_established, src_ethics,
    src_readers, src_soft, src_status,
    subgroup = "rsp_pol_side",
    cluster = "rsp_id"
  ) %>%
  add_labels() %>%
  mutate(rsp_pol_side = lvls_reorder(rsp_pol_side, c(2, 1, 3)))

fig_sub_polside <-
  res_sub_polside %>%
  ggplot(
    aes(
      x = estimate, y = value,
      xmin = estimate - (1.96 * std_error),
      xmax = estimate + (1.96 * std_error))) +
  facet_grid(
    treatment ~ rsp_pol_side,
    scales = "free_y",
    space = "free_y") +
  geom_errorbarh(height = 0) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  scale_x_continuous(
    limits = c(-.4, .4),
    breaks = round(seq(-.4, .4, .2), 2),
    expand = c(0, 0),
    labels = function(x) x * 100) +
  scale_y_discrete(
    labels = function(x) parse(text = as.character(x))) +
  labs(
    x = "Marginal effect, selecting source (%)",
    y = "Source attributes"
  ) +
  theme_m()
fig_sub_polside


ggsave2(fig_sub_polside, "fig_sub_polside", width = 12, height = 5)
