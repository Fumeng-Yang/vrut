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
        mutate(class = if_else(velocity - velocity_threshold >= 0, "saccade", "fixation"))
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

  velocity_results <- velocity_results %>%
  group_by(Experiment, Participant, Condition, Device, Platform, Trial) %>%
    mutate(fixation_index = (function(classes){
      fixation_count <- 1
      fixation_results <- c()
      for(i in 1:length(classes)){
        if(i == 1){
          if(classes[1] == "fixation")
            fixation_results <- c(paste0("fixation_", fixation_count))
          else
            fixation_results <- c(NA)
        }else if(classes[i] == "fixation" & classes[i] != classes[i-1]){
          fixation_count <- fixation_count + 1
          fixation_results <- rbind(fixation_results, paste0("fixation_", fixation_count))
          #print(fixation_results)
        }else if (classes[i] == "fixation" & classes[i] == classes[i-1]){
          fixation_results <- rbind(fixation_results, paste0("fixation_", fixation_count))
        }else{
          fixation_results <- rbind(fixation_results, NA)
        }
      }
      return(fixation_results)})(class)) %>%
    drop_na() %>%
    summarise(x = mean(x), y = mean(y), z = mean(z), fixation_index=first(fixation_index))

  return(velocity_results)
}
