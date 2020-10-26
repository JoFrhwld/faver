#' ae recoding
#'
#' input expects basic vclass coding
#'
#' todo: check for vclass column. Throw error otherwise
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr lead
#' @importFrom stringr str_c
#' @export
phila_ae <- function(df){
  df %>%
    group_by(speakerid, word_n) %>%
    filter(any(vclass == "æ")) %>%
    enrich_syl() %>%
    group_by(speakerid, word_n) %>%
    mutate(post2_phone_word = lead(phone_label, n = 2),
           post3_phone_word = lead(phone_label, n = 3),
           full_trans = str_c(phone_label, collapse = " "),
           new_vclass = case_when(vclass == "æ" &
                                    toupper(word_label) %in% c('AND', "AN'", 'AM', 'AN', 'THAN',
                                                               'RAN', 'SWAM', 'BEGAN', 'CAN', 'FAMILY',
                                                               'FAMILIES', "FAMILY'S", 'JANUARY', 'ANNUAL',
                                                               'ANNE', "ANNE'S", 'ANNIE', "ANNIE'S",
                                                               'JOANNE', 'GAS', 'GASES', 'EXAM', 'EXAMS',
                                                               "EXAM'S", 'ALAS', 'ASPIRIN',
                                                               'CATHOLIC', 'CATHOLICS', 'CAMERA',
                                                               'MATH') ~ "æ*",
                                  vclass == "æ" &
                                    toupper(word_label) %in% c('MAD', 'BAD', 'GLAD', 'MADLY',
                                                               'BADLY', 'GLADLY', 'MADDER',
                                                               'BADDER', 'GLADDER', 'MADDEST',
                                                               'BADDEST', 'GLADDEST', 'MADNESS',
                                                               'GLADNESS', 'BADNESS', 'MADHOUSE') ~ "æh",
                                  vclass == "æ" &
                                    grepl("GRAND", toupper(word_label)) ~ "æh",
                                  vclass == "æ" &
                                    post_phone_word == "S" & post2_phone_word %in% c("P", "T", "K") &
                                    !is.na(post3_phone_word) &
                                    !grepl("*AS.IN[G']$", toupper(word_label)) ~ "æ*",
                                  vclass == "æ" &
                                    post_phone_word == "L" ~ "æ*",
                                  vclass == "æ" &
                                    grepl("AE1 (M |N |S |TH |F )(IH0|AH0) (NG|N|Z)$", full_trans) ~ "æh",
                                  vclass == "æ" &
                                    post_phone_word %in% c('M', 'N', 'S', 'TH', 'F') &
                                    post_syl_part == "coda"~ "æh",
                                  TRUE ~ vclass)) %>%
    incorporate_recoding(df) -> out

  return(out)
}



#' marry fixing
#'
#' input expects basic vclass coding
#'
#' todo: check for vclass column. Throw error otherwise
#'
#' Dramatically incomplete!
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr lead
#' @importFrom stringr str_c
#' @export
phila_marry <- function(df){

  df %>%
    filter(grepl("ARR", toupper(word_label))) %>%
    group_by(speakerid, word_n) %>%
    filter(any(phone_label == "EH1")) %>%
    mutate(new_vclass = case_when(phone_label == "EH1" &
                                    lead(phone_label) == "R" &
                                    grepl("[AEIOU]", lead(phone_label, n = 2)) ~ "æ",
                                  TRUE~vclass))%>%
    incorporate_recoding(df) -> out

  return(out)
}


#' low-back vowels fixing
#'
#' input expects basic vclass coding
#'
#' todo: check for vclass column. Throw error otherwise
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr lead
#' @importFrom dplyr left_join
#' @importFrom stringr str_c
#' @export
phila_lowback <- function(df){

  thought <- c('LAW', 'LAWS', "LAW'S", 'LAWFUL', 'UNLAWFUL', 'DOG', 'DOGS', "DOG'S",
               'DOGGED','ALL', "ALL'S", 'CALL', 'CALLS', "CALL'S", 'CALLING',
               'CALLED', 'FALL', 'FALLS', "FALL'S", 'FALLING',
               'AUDIENCE', 'AUDIENCES', "AUDIENCE'S", 'ON',
               'ONTO', 'GONNA', 'GONE', 'BOSTON', "BOSTON'S",
               'AWFUL', 'AWFULLY', 'AWFULNESS', 'AWKWARD',
               'AWKWARDLY', 'AWKWARDNESS', 'AWESOME', 'AUGUST',
               'COUGH', 'COUGHS', 'COUGHED', 'COUGHING')

  lot <- c('CHOCOLATE', 'CHOCOLATES', "CHOCOLATE'S", 'WALLET', 'WALLETS',
           'WARRANT', 'WARRANTS','WATCH', 'WATCHES', 'WATCHED',
           'WATCHING', 'WANDER', 'WANDERS', 'WANDERED', 'WANDERING',
           'CONNIE', 'CATHOLICISM', 'WANT', 'WANTED', 'PONG', 'GONG',
           'KONG', 'FLORIDA', 'ORANGE', 'HORRIBLE', 'MAJORITY')

  df %>%
    group_by(speakerid, word_n) %>%
    filter(any(grepl("AA", phone_label)) , word_label %in% thought) %>%
    mutate(new_vclass = case_when(vclass == "o" ~ "oh",
                                  TRUE ~ vclass)) -> new_thought

  df %>%
    group_by(speakerid, word_n) %>%
    filter(any(grepl("AO", phone_label)) , word_label %in% lot) %>%
    mutate(new_vclass = case_when(vclass == "oh" ~ "o",
                                  TRUE ~ vclass)) -> new_lot

  bind_rows(new_thought, new_lot) %>%
    incorporate_recoding(df) -> out

  return(out)
}


#' Incorporate Recoding
#'
#' incorporate recoding of new_vclass into vclass
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
incorporate_recoding <- function(recoded, df){
  recoded %>%
    select(speakerid, word_n, phone_n, new_vclass) -> recoded

  df %>%
    left_join(recoded, by = c("speakerid", "phone_n", "word_n")) %>%
    mutate(vclass = case_when(!is.na(new_vclass) & new_vclass != vclass ~ new_vclass,
                              TRUE ~ vclass)) %>%
    select(-new_vclass) -> out

  return(out)
}
