#' Title
#'
#' @param data
#' @param hz_method which method to estimate hz
#' @param plot
#' @param h the span passing to loess regression
#'
#' @return
#' @export
#'
#' @examples
find_mould_velocity_threshold <- function(data, hz_method = "median", h = NULL, plot = F){
  #https://github.com/cran/gazepath/blob/master/R/Mould_vel.R

  d_local_max_velocity <- get_local_maximum(data, methods = "delta")$velocity
  d_hz <- estimate_hz(data)

  if(hz_method == "median"){
    Hz <- round(d_hz$`median_hz`)
  }else{
    Hz <- round(d_hz$`mean_hz`)
  }

  message(paste0("estimated Hz is ", Hz))

  vmax <- max(d_local_max_velocity)
  if(length(d_local_max_velocity) < 10){
    return(NA)
    warning("no enough data to estimate a velocity threshold")
  }else{
    thresholds <- seq(min(d_local_max_velocity)+0.00001, max(d_local_max_velocity), length.out = Hz)

    set <- sapply(thresholds, function(x) {length(which(d_local_max_velocity > x))})
    uni <- seq(length(d_local_max_velocity), 0, length.out = Hz)
    gap <- uni - set

    if(is.null(h)){
      if(Hz > 250) {h <- .05}
      else if(Hz > 100) {h <- .1}
      else {h <- .5}
    }

    gap <- predict(loess(gap ~ log(thresholds), span = h))

    # inline local max find
    local_max_gap <- (function(x) which(diff(c(TRUE, diff(x) >= 0, FALSE)) < 0))(gap)
    while (length(local_max_gap) > 1 & h < 1) {
      h <- h + .01
      gap <- predict(loess( (uni - set) ~ log(thresholds), span = h, surface = 'direct', cell = 1))
    }

    if(plot == T) plot_mould(uni, set, gap, thresholds, d_local_max_velocity, Hz)

    if(h != 1) return(thresholds[which.max(gap)]) else return(NA)
  }
}

