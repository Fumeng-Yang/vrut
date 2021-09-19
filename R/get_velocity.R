

#' get_velocity
#'
#' @param data tibble
#' @param time_interval a number in milliseconds
#'
#' @return a tibble of velocity samples
#' @export
#'
#' @examples get_velocity(data)
#'
get_velocity <- function(data, time_interval = 100){
  results <- data %>%
    group_by(Experiment, Participant, Condition, Device, Platform, Trial) %>%
    arrange(Timestamp) %>%
    mutate(if_keep = if_else(Timestamp - lag(Timestamp) - time_interval >= 0, T, F)) %>%
    filter(if_keep) %>%
    mutate(velocity = sqrt((x - lag(x))^2 + (y - lag(y))^2 + (z - lag(z))^2) / (Timestamp - lag(Timestamp)),
           velocity_x = (x - lag(x))/ (Timestamp - lag(Timestamp)),
           velocity_y = (y - lag(y))/ (Timestamp - lag(Timestamp)),
           velocity_z = (z - lag(z))/ (Timestamp - lag(Timestamp))
           )%>%
    select(-if_keep)%>%
    ungroup()%>%
    drop_na()

  return(results)

}
