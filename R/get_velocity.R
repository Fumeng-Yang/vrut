#' get_velocity TODO
#'
#' @param data a tibble TODO
#' @param time_interval a number in seconds, used when the method is `diff` TODO
#' @param method `diff`, `delta` TODO
#'
#' @return a tibble of velocity samples
#' @export
#'
#' @examples get_velocity(data) TODO
#'
get_velocity <- function(data, time_interval = .1, method = "delta"){

  if(method == "diff"){
    results <- data %>%
      group_by(Experiment, Participant, Condition, Device, Platform, Trial) %>%
      arrange(Timestamp) %>%
      mutate(time = Timestamp / 1000.0,
             if_keep = if_else(time - lag(time) - time_interval >= .1, T, F)) %>%
      filter(if_keep) %>%
      mutate(velocity = sqrt((x - lag(x))^2 + (y - lag(y))^2 + (z - lag(z))^2) / (time - lag(time)),
             velocity_x = (x - lag(x))/ (time - lag(time)),
             velocity_y = (y - lag(y))/ (time - lag(time)),
             velocity_z = (z - lag(z))/ (time - lag(time))
      )%>%
      select(-if_keep, -time)%>%
      ungroup()%>%
      drop_na()

    return(results)
  }

  if(method == "delta"){
    results <- data %>%
      group_by(Experiment, Participant, Condition, Device, Platform, Trial) %>%
      arrange(Timestamp) %>%
      mutate(time = Timestamp / 1000.0,
             velocity = sqrt((x - lag(x, k = 2))^2 + (y - lag(y, k = 2))^2 + (z - lag(z, k = 2))^2) / (time - lag(time, k = 2)),
             velocity_x = (x - lag(x, k = 2))/ (time - lag(time, k = 2)),
             velocity_y = (y - lag(y, k = 2))/ (time - lag(time, k = 2)),
             velocity_z = (z - lag(z, k = 2))/ (time - lag(time, k = 2))
      )%>%
      select(-time)%>%
      ungroup()%>%
      drop_na()


    return(results)
  }

}
