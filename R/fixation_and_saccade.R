

#' Title
#'
#' @param velocities
#' @param mould_velocity_threshold
#' @param mould_duration_threshold
#' @param Hz
#'
#' @return
#' @export
#'
#' @examples
fixation_and_saccade <- function(velocities, mould_velocity_threshold, mould_duration_threshold, Hz){
  # https://github.com/cran/gazepath/blob/master/R/fixationANDsaccade.R
  # I really don't understand the code....
  mould_duration_threshold <- mould_duration_threshold * Hz
  fix_classes <- ifelse(velocities > mould_velocity_threshold, 'saccade', 'fixation')
  #print(length(fix_classes[fix_classes == "saccade"]))
  ## Set a minimum saccade duration of 10 ms
  rle <- rle(fix_classes)
  fix_classes[cumsum(rle$lengths)[which(rle$lengths < mould_duration_threshold & rle$values == 'saccade')]] <- 'fixation'

  # while(length(which(rle$lengths < 10 * (Hz/1000) & rle$values == 'saccade')) != 0){
  #   rle <- rle(fixsac)
  #   fixsac[cumsum(rle$lengths)[which(rle$lengths < 10 * (Hz/1000) & rle$values == 'saccade')]] <- 'fixation'
  # }
  # classify <- numeric()
  # for(i in 1:length(rle$values)){
  #   if(is.na(rle$values[i])){
  #     classify <- c(classify, rep(NA, rle$lengths[i]))
  #   } else{
  #     if(rle$values[i] == 'fixation' & rle$lengths[i] >= mould_duration_threshold){
  #       classify <- c(classify, rep('fixation', rle$lengths[i]))
  #     }
  #     if(rle$values[i] == 'fixation' & rle$lengths[i] < mould_duration_threshold){
  #       classify <- c(classify, rep('unknown', rle$lengths[i]))
  #     }
  #     if(rle$values[i] == 'fixation'){
  #       classify <- c(classify, rep('saccade', rle$lengths[i]))
  #     }
  #   }
  # }
  return(fix_classes)
}
