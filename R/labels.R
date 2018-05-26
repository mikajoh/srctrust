
#' Treatment variable names
#'
#' Treatment variable names matched with the norwegian header labels
#' in the raw data
#' 
#' @importFrom tibble tribble
#' 
#' @export
treat_names <- function() {
  tribble(
    ~treat_lab,                                    ~treat,
    "Reklamefinansiert",                           "src_adds",
    "Sjefredaktøren/eieren støtter åpent partiet", "src_party",
    "Antall brudd på presseetikk det siste året",  "src_ethics",
    "Kommentarfelt",                               "src_comments",
    "Antall lesere",                               "src_readers",
    "Nåværende status",                            "src_status",
    "Andel underholdningsnyheter",                 "src_soft",
    "Ble etablert",                                "src_established"
  )
}

#' Value labels
#'
#' Value labels in english matched with the treatment variable and
#' norwegian labels in the raw data.
#' 
#' @importFrom tibble tribble
#' 
#' @export
val_labs <- function() {
  tribble(
    ~treat,            ~val_no,                                               ~val,                                                     ~fct_lvl,
    "src_status",      "Nettavis (uten papiravis)",                           "Online newpaper",                                        2,
    "src_status",      "Nettsted/blogg",                                      "Blog/website",                                           3,
    "src_status",      "Papiravis og nettavis",                               "Offline and online newspaper",                           1,
    "src_soft",        "En liten andel underholdningsnyheter",                "Some entertainment news",                                2,
    "src_soft",        "Har ikke underholdningsnyheter",                      "No entertainment news",                                  1,
    "src_soft",        "Hovedsakelig underholdningsnyheter",                  "Primarily entertainment news",                           3,
    "src_readers",     "Få lesere",                                           "Few readers",                                            1,
    "src_readers",     "Mange lesere",                                        "Many readers",                                           2,
    "src_party",       "Arbeiderpartiet",                                     "Social Democratic Party",                                4,
    "src_party",       "Fremskrittspartiet",                                  "Progress Party",                                         10,
    "src_party",       "Høyre",                                               "Conservative Party",                                     9,
    "src_party",       "Ingen",                                               "No party",                                               1,
    "src_party",       "Kristelig Folkeparti",                                "Christian Democratic Party",                             7,
    "src_party",       "Miljøpartiet De Grønn",                               "Green Party",                                            6,
    "src_party",       "Miljøpartiet De Grønne",                              "Green Party",                                            6,
    "src_party",       "Rødt",                                                "Red Party",                                              2,
    "src_party",       "Senterpartiet",                                       "Agrarian Party",                                         5,
    "src_party",       "Sosialistisk Venstreparti",                           "Socialist Left Party",                                   3,
    "src_party",       "Venstre",                                             "Liberal Party",                                          8,
    "src_ethics",      "Få brudd",                                            "Few violations",                                         2,
    "src_ethics",      "Ingen brudd",                                         "No violations",                                          1,
    "src_ethics",      "Mange brudd",                                         "Many violations",                                        3,
    "src_established", "Omtrent 100 år siden",                                "About 100 years ago",                                    1,
    "src_established", "Omtrent 50 år siden",                                 "About 50 years ago",                                     2,
    "src_established", "Omtrent fem år siden",                                "About 5 years ago",                                      3,
    "src_comments",    "Har ikke kommentarfelt",                              "Have no comment field",                                  1,
    "src_comments",    "Ja, men journalistene deltar ikke i kommentarfeltet", "Have comment field (without journalists participating)", 2,
    "src_comments",    "Ja, og journalistene deltar i kommentarfeltet",       "Have comment field (with journalists participating)",    3,
    "src_adds",        "Ja, men bruker ikke reklame som ligner på nyheter",   "Have advertisements (without native adv)",               2,
    "src_adds",        "Ja, og bruker reklame som ligner på nyheter",         "Have advertisements (with native adv)",                  3,
    "src_adds",        "Nei",                                                 "Have no advertisements",                                 1
  )
}
