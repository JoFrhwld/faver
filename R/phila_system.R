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
#' @importFrom dplyr left_join
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
                                  TRUE ~ vclass)) ->ae_coding

  ae_coding %>%
    select(speakerid, word_n, phone_n, new_vclass) ->ae_coded


  df %>%
    left_join(ae_coded, by = c("speakerid", "phone_n", "word_n")) %>%
    mutate(vclass = case_when(!is.na(new_vclass) & new_vclass != vclass ~ new_vclass,
                              TRUE ~ vclass)) %>%
    select(-new_vclass) -> out

  return(out)
}
