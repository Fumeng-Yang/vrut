#' Title
#'
#' @param data
#' @param method
#' @param velocity_threshold
#'
#' @return
#' @export
#'
#' @examples
classify_fixation_saccade <- function(data, method,
                                      velocity_threshold = .15
                                      ){
  #TODO check the data
  # largely borrowed from https://github.com/cran/gazepath/blob/master/R/GazePath.R

  data_velocity <- get_velocity(data, method = "delta")

   # Identifying Fixations and Saccades in Eye-Tracking Protocols
   if(method == "velocity"){
      velocity_results <-  data_velocity %>%
        mutate(class = if_else(velocity - velocity_threshold >= 0, "saccade", "fixation")) %>%
        group_by(Experiment, Participant, Condition, Device, Platform, Trial) %>%
        mutate(test = (function(x){
          print(x)
          return(x)})(velocity))

      # fixation_counter <- 1
      # fixation_cluster_class <- c(NA)
      # class <- velocity_results$class
      # for(i in 2:length(velocity_results)){
      #   if(class[i] == "fixation"){
      #     if(class[i] != class[i - 1]){
      #       fixation_counter + 1
      #     }
      #     fixation_cluster_class <- rbind(fixation_cluster_class, paste0("class_", fixation_counter))
      #   }else{
      #     fixation_cluster_class <- rbind(fixation_cluster_class, NA)
      #   }
      # }



      return(NA)
   }

   if(method == "dispersion"){

   }

   # Mould estimates velocity threshold per trial and duration thresholds per person
   # (recommended for high, > 250 Hz, samplerate and high quality data)
   if(method == "mould"){

   }

   # MouldDur estimates velocity threshold per trial and uses the duration thresholds
   # specified under ’thres_dur’ (recommended for low, < 250 Hz, samplerate data
   #                             and data of low quality)

   if(method == "mould_duration"){

   }


  # Mould.all estimates velocity threshold and duration threshold for all trials
  # (recommended for high, > 250 Hz, samplerate data and data with short trial times,
  # < 2 seconds)
  if(method == "mould_all"){

  }

  # Mould.allDur estimates one velocity threshold for all trials and uses the duration threshold
  # specified under ’thres_dur’ (recommended for high, > 250 Hz,
  # samplerate data and data with short trial times, < 2 seconds and few trials)
  if(method == "mould_all_duration"){

  }
}
