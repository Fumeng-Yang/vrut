#' A local maximum
#'
#' @param data a tibble TODO
#' @param time_interval in seconds TODO
#' @param methods "diff" or "delta" TODO
#'
#' @return a tibble TODO
#' @export
#'
#' @examples get_local_maximum(data) TODO
get_local_maximum <- function(velocities, time_interval = .1, methods = "delta"){

   #d_velocity <- get_velocity(data, time_interval, methods)
   results <- velocities %>%
     #group_by(Experiment, Participant, Condition, Device, Platform, Trial) %>%
     filter(if_else((velocity > lag(velocity)) & (velocity > lead(velocity)), T, F))

   return(results)

}
