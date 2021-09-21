#' estimate the fps/hz of the input data
#'
#' @param data TODO
#'
#' @return a list combining mean_hz, median_hz, and sd_hz TODO
#' @export
#'
#' @examples TODO
estimate_hz <- function(data){
  results <- data %>%
    #group_by(Experiment, Participant, Condition, Device, Platform, Trial) %>%
    arrange(Timestamp)%>%
    mutate(time = Timestamp / 1000,
      time_delta = time - lag(time),
      hz = 1 / time_delta) %>%
    drop_na() %>%
    ungroup()

  return(list("mean_hz" = mean(results$hz), median_hz = median(results$hz), "sd_hz" = sd(results$hz)))
}
