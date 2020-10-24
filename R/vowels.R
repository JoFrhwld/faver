#' arpabet to plotnik
#' @importFrom tibble tribble
#' @export
a2p <- function(){
  tribble(~cmu, ~plt,
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
  tribble(~cmu, ~plt,
          'IY', 'iyF',
          'EY', 'eyF',
          'OW', 'owF')->out
  return(out)
}

#' arpabet to plotnik /r/
#' @importFrom tibble tribble
#' @export
a2p_r <- function(){
  tribble(~cmu, ~plt,
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
#' @importFrom tibble tribble
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



