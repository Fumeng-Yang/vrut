#' calculate cumulative moving distances for each experiment, participant, condition, and device
#'
#' @param data a tibble Experiment, Participant, Condition, Trial, Device, x, y, z columns
#'
#' @return a tibble with Experiment, Participant, Condition, Trial, Device, and a moved_distance column
#' @export
#'
#' @examples get_moved_distance(data)
#' todo: refactor using variables, keep other columns
get_moved_distance <- function(data){

   results <- data %>%
    group_by(Experiment, Participant, Condition, Device, Platform, Trial) %>%
    arrange(Timestamp) %>%
    mutate(dist_travelled = sqrt((x - lag(x))^2 + (y - lag(y))^2 + (z - lag(z))^2)) %>%
    summarise(moved_distance = sum(dist_travelled, na.rm = TRUE)) %>%
    ungroup()

   return(results)

}
