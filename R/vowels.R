#' arpabet to plotnik
#' @importFrom tibble tribble
#' @export
a2p <- function(){
  tribble(~cmu, ~vclass,
          'AA', 'o',
          'AE', 'æ',
          'AH', 'ʌ',
          'AO', 'oh',
          'AW', 'aw',
          'AY', 'ay',
          'EH', 'e',
          'ER', '*hr',
          'EY', 'ey',
          'IH', 'i',
          'IY', 'iy',
          'OW', 'ow',
          'OY', 'oy',
          'UH', 'u',
          'UW', 'uw'
  )->out
  return(out)
}

#' arpabet to plotnik final
#' @importFrom tibble tribble
#' @export
a2p_final <- function(){
  tribble(~cmu, ~vclass,
          'IY', 'iyF',
          'EY', 'eyF',
          'OW', 'owF')->out
  return(out)
}

#' arpabet to plotnik /r/
#' @importFrom tibble tribble
#' @export
a2p_r <- function(){
  tribble(~cmu, ~vclass,
          'EH', 'e',
          'AE', 'æ',
          'IH', 'iyr',
          'IY', 'iyr',
          'EY', 'eyr',
          'AA', 'ahr',
          'AO', 'owr',
          'OW', 'owr',
          'UH', 'uwr',
          'UW', 'uwr',
          'AH', 'ʌ',
          'AW', 'aw',
          'AY', 'ay',
          'OY', 'oy')->out
  return(out)
}


#' phone features
#' @export
phone_features <- function(){
  structure(list(phone = c("AA", "AE", "AH", "AO", "AW", "AX",
                           "AXR", "AY", "B", "CH", "D", "DH", "DX", "EH", "EL", "EM", "EN",
                           "ER", "EY", "F", "G", "HH", "HV", "IH", "IY", "JH", "K", "L",
                           "M", "N", "NX", "NG", "OW", "OY", "P", "R", "S", "SH", "T", "TH",
                           "UH", "UW", "V", "W", "Y", "Z", "ZH"),
                 vowel = c("+", "+", "+",
                           "+", "+", "+", "+", "+", "-", "-", "-", "-", "-", "+", "+", "+",
                           "+", "+", "+", "-", "-", "-", "-", "+", "+", "-", "-", "-", "-",
                           "-", "-", "-", "+", "+", "-", "-", "-", "-", "-", "-", "+", "+",
                           "-", "-", "-", "-", "-"),
                 vowel_long = c("long", "short", "short",
                                "long", "diphthong", "schwa", "schwa", "diphthong", "0", "0",
                                "0", "0", "0", "short", "short", "short", "short", "a", "diphthong",
                                "0", "0", "0", "0", "short", "long", "0", "0", "0", "0", "0",
                                "0", "0", "diphthong", "diphthong", "0", "0", "0", "0", "0",
                                "0", "short", "long", "0", "0", "0", "0", "0"),
                 vowel_height = c(3,
                                  3, 2, 3, 3, 2, 2, 3, 0, 0, 0, 0, 0, 2, 0, 0, 0, 2, 2, 0, 0, 0,
                                  0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 1, 1, 0,
                                  0, 0, 0, 0),
                 vowel_front = c(3, 1, 2, 3, 2, 2, 2, 2, 0, 0, 0,
                                 0, 0, 1, 0, 0, 0, 2, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,
                                 2, 3, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0),
                 vowel_round = c("-",
                                 "-", "-", "+", "-", "-", "-", "-", "0", "0", "0", "0", "0", "-",
                                 "0", "0", "0", "-", "-", "0", "0", "0", "0", "-", "-", "0", "0",
                                 "0", "0", "0", "0", "0", "+", "+", "0", "0", "0", "0", "0", "0",
                                 "+", "+", "0", "0", "0", "0", "0"),
                 ccons_type = c("0", "0",
                                "0", "0", "0", "0", "rhotic", "0", "stop", "affricate", "stop",
                                "fricative", "stop", "0", "lateral", "nasal", "nasal", "0", "0",
                                "fricative", "stop", "fricative", "fricative", "0", "0", "affricate",
                                "stop", "lateral", "nasal", "nasal", "nasal", "nasal", "0", "0",
                                "stop", "rhotic", "fricative", "fricative", "stop", "fricative",
                                "0", "0", "fricative", "0", "0", "fricative", "fricative"),
                 cons_place = c("0",
                                "0", "0", "0", "0", "0", "a", "0", "labial", "palatal", "alveolar",
                                "dental", "alveolar", "0", "alveolar", "labial", "alveolar",
                                "0", "0", "labiodental", "velar", "glottal", "glottal", "0",
                                "0", "palatal", "v", "alveolar", "labial", "alveolar", "alveolar",
                                "velar", "0", "0", "labial", "alveolar", "a", "palatal", "alveolar",
                                "dental", "0", "0", "labiodental", "labial", "palatal", "alveolar",
                                "palatal"),
                 cons_voiced = c("0", "0", "0", "0", "0", "0", "+",
                                 "0", "+", "-", "+", "+", "+", "0", "+", "+", "+", "0", "0", "-",
                                 "+", "-", "+", "0", "0", "+", "-", "+", "+", "+", "+", "+", "0",
                                 "0", "-", "+", "-", "-", "-", "-", "0", "0", "+", "+", "+", "+",
                                 "+")),
            row.names = c(NA, -47L), class = c("tbl_df", "tbl", "data.frame"
            )) -> out
  return(out)

}


#' basic vclass coding
#' Input must be output of `word_phone_nest`
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @export
basic_vclass <- function(df){

  df %>%
    mutate(cmu = gsub("[0-9]", "", phone_label),
           stress = gsub(".*([0-9])", "\\1", phone_label)) %>%
    left_join(a2p(), by = "cmu") %>%
    mutate(vclass = case_when(cmu == "AH" & stress == 0 ~ "*",
                           vclass %in% c("iy", "ey", "ow") & is.na(post_phone_word) ~ str_c(vclass, "F"),
                           vclass == "ay" & post_phone_word %in% (phone_features() %>%
                                                                 filter(cons_voiced == "-") %>%
                                                                 pull(phone)) ~ "ay0",
                           cmu == "UW" & pre_phone_word %in% (phone_features() %>%
                                                                 filter(cons_place == "alveolar") %>%
                                                                 pull(phone)) ~ "Tuw",
                           cmu %in% c("IY", "IH") & post_phone_word == "R" ~ "iyr",
                           cmu %in% c("EY") & post_phone_word == "R" ~ "eyr",
                           cmu %in% c("AA") & post_phone_word == "R" ~ "ahr",
                           cmu %in% c("AO", "OW") & post_phone_word == "R" ~ "owr",
                           cmu %in% c("UH", "UW") & post_phone_word == "R" ~ "uwr",
                           cmu %in% "AA" & toupper(word_label) %in% c('FATHER', 'FATHER', "FATHER'S",
                                                                      'MA', "MA'S", 'PA', "PA'S",
                                                                      'SPA', 'SPAS', "SPA'S",
                                                                      'CHICAGO', "CHICAGO'S", 'PASTA',
                                                                      'BRA', 'BRAS', "BRA'S", 'UTAH',
                                                                      'TACO', 'TACOS', "TACO'S",
                                                                      'GRANDFATHER', 'GRANDFATHERS',
                                                                      "GRANDFATHER'S", 'CALM', 'CALMER',
                                                                      'CALMEST', 'CALMING', 'CALMED',
                                                                      'CALMS', 'PALM', 'PALMS', 'BALM',
                                                                      'BALMS', 'ALMOND', 'ALMONDS',
                                                                      'LAGER', 'SALAMI', 'NIRVANA',
                                                                      'KARATE', 'AH') ~ "ah",
                           TRUE ~ vclass))%>%
    select(-stress) -> out
  return(out)
}
