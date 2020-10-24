#' read textgrid
#'
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom tidyr separate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom purrr map_dfr
#' @importFrom rPraat tg.read

read_textgrid <- function(path){
  tg <- tg.read(path)

  tg %>%
    map_dfr(as.data.frame) %>%
    separate(name, into = c("speakerid", "type"), sep = "\\s+?-\\s+?") %>%
    group_by(speakerid, type) %>%
    mutate(n = 1:n()) %>%
    ungroup()-> tg_df

  return(tg_df)
}



#' word nesting
#'
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @importFrom dplyr lead
#' @importFrom dplyr lag
#' @importFrom magrittr %>%
#' @importFrom tidyr fill
#' @export

word_phone_nest <- function(tg_df){

  phones_df <- tg_df %>%
    filter(grepl("phone", type)) %>%
    rename(phone_label = label,
           phone_n = n)

  words_df <- tg_df %>%
    filter(grepl("word", type)) %>%
    rename(word_label = label,
           word_n = n)

  phones_df %>%
    left_join(words_df %>% select(speakerid, t1, word_label, word_n),
              by = c("speakerid", "t1")) %>%
    fill(word_label, word_n) %>%
    group_by(speakerid, word_n) %>%
    mutate(phone_index = 1:n(),
           pre_phone_word = lag(phone_label),
           post_phone_word = lead(phone_label))%>%
    group_by(speakerid) %>%
    mutate(pre_phone_phrase = lag(phone_label),
           post_phone_phrase = lead(phone_label)) %>%
    ungroup() -> structured



  return(structured)
}

#' word data
#' must be output of `word_phone_nest`
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @importFrom dplyr lead
#' @importFrom dplyr lag
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom purrr simplify
#' @export
word_data <- function(df){
  df %>%
    group_by(speakerid, word_n) %>%
    nest() %>%
    mutate(word_label = map(data, ~.x$word_label[1]) %>% simplify(),
           word_trans = map(data, ~str_c(.x$phone_label, collapse = " ")) %>% simplify())%>%
    group_by(speakerid) %>%
    mutate(pre_word = lag(word_label),
           post_word= lead(word_label)) %>%
    ungroup() %>%
    select(speakerid, word_n, pre_word, word_label, post_word, word_trans)->word_data

  return(word_data)
}


#' enrich with syllable structure
#' must be output of `word_phone_nest`
#'
#' @importFrom syllabifyr syllabify
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom dplyr lag
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr safely
#' @export
enrich_syl <- function(df){
  safe_syl <- safely(syllabify)


  df %>%
    group_by(speakerid, word_n) %>%
    nest()  %>%
    mutate(syl_df = map(data, ~safe_syl(.x$phone_label)$result),
           combined = map2(data, syl_df, bind_cols)) %>%
    select(speakerid, word_n, combined) %>%
    unnest(combined) %>%
    select(-phone) %>%
    rename(syl_n = syll,
           syl_part = part,
           syl_stress = stress) %>%
    group_by(speakerid, word_n) %>%
    mutate(pre_syl_part = lag(syl_part),
           post_syl_part = lead(syl_part))->out

  return(out)
}
