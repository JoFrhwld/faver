
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
#' @export
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
