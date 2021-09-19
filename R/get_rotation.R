#' Title
#'
#' @param data a tibble TODO
#' @param time_interval a number TODO
#'
#' @return a tibble TODO
#' @export
#'
#' @examples get_rotation(data) TODO
get_rotation <- function(data, time_interval = 0){
  results <- data %>%
    group_by(Experiment, Participant, Condition, Device, Platform, Trial) %>%
    arrange(Timestamp) %>%
    mutate(if_keep = if_else(Timestamp - lag(Timestamp) - time_interval >= 0, T, F)) %>%
    filter(if_keep) %>%
    mutate(angle = acos(
      (u * lag(u) +  v * lag(v) +  w * lag(w)) / (sqrt(u^2 + v^2 + w^2) * sqrt(lag(u)^2 + lag(v)^2 + lag(w)^2))
    ))%>%
    select(-if_keep)%>%
    ungroup()%>%
    drop_na()

  return(results)

}
