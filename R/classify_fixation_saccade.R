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
                                      velocity_threshold = .15,
                                      duration_threshold = 1,
                                      dispersion_threshold = .1,
                                      estimated_hz = 10,
                                      raw_output = F,
                                      verbose=T
                                      ){
  #TODO check the data
  # largely borrowed from https://github.com/cran/gazepath/blob/master/R/GazePath.R

  data_velocity <- get_velocity(data, method = "delta")

   # Identifying Fixations and Saccades in Eye-Tracking Protocols
   if(method == "velocity"){
    raw_results <-  data_velocity %>%
        mutate(class = if_else(velocity - velocity_threshold >= 0, "saccade", "fixation"))
   }

   if(method == "dispersion"){
     raw_results <-  data %>%
       #group_by(Experiment, Participant, Condition, Device, Platform, Trial) %>%
       arrange(.,Timestamp)%>%
       mutate(class = (function(x,y,z){
         i_left <- 1
         min_num <- max(1, estimated_hz)
         message(paste0("initial window size ", min_num))
         i_right <- min_num + i_left
         length_tmp <- length(x)
         fixation_list <- rep.int('saccade', times = length_tmp)

         while(i_right <= length_tmp){
           dispersion  <- max(x[i_left:i_right]) - min(x[i_left:i_right]) +
                             max(y[i_left:i_right]) - min(y[i_left:i_right]) +
                             max(z[i_left:i_right]) - min(z[i_left:i_right])

           if(dispersion - dispersion_threshold <= 0){
             fixation_list[i_left:i_right] <- 'fixation'
             i_right <- i_right + 1
           }

           if(dispersion - dispersion_threshold > 0){
             i_left <- i_left + 1
             i_right <- i_left + min_num
           }
         }
         return(fixation_list)
       })(x,y,z))


   }

   # Mould estimates velocity threshold per trial and duration thresholds per person
   # (recommended for high, > 250 Hz, samplerate and high quality data)
   if(method == "mould"){

   }

   # MouldDur estimates velocity threshold per trial and uses the duration thresholds
   # specified under ’thres_dur’ (recommended for low, < 250 Hz, samplerate data
   #                             and data of low quality)

   if(method == "mould_duration"){
    mould_duration_threshold <- duration_threshold
    raw_results <-  data_velocity %>%
      arrange(Timestamp)%>%
      group_by(Experiment, Participant, Condition, Device, Platform, Trial)%>%
      mutate(class = (function(x, y){
          mould_velocity_threshold <- find_mould_velocity_threshold(data.frame(velocity = x, Timestamp = y), Hz = estimated_hz)
          mid_results <- fixation_and_saccade(x, mould_velocity_threshold, mould_duration_threshold, estimated_hz)
        return(mid_results)
      })(velocity, Timestamp))
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

  if(raw_output==F){
   results <- raw_results %>%
    #group_by(Experiment, Participant, Condition, Device, Platform, Trial) %>%
     arrange(Timestamp)%>%
      mutate(fixation_index = (function(classes){
        fixation_count <- 1
        fixation_results <- c()
        for(i in 1:length(classes)){
          if(i == 1){
            if(classes[i] == "fixation")
              fixation_results <- c(paste0("fixation_", fixation_count))
            else
              fixation_results <- c(NA)
          }else if(classes[i] == "fixation" & classes[i] != classes[i-1]){
            fixation_count <- fixation_count + 1
            fixation_results <- rbind(fixation_results, paste0("fixation_", fixation_count))
          }else if (classes[i] == "fixation" & classes[i] == classes[i-1]){
            fixation_results <- rbind(fixation_results, paste0("fixation_", fixation_count))
          }else{
            fixation_results <- rbind(fixation_results, NA)
          }
          #print(fixation_results)
        }
        return(fixation_results)})(class)) %>%
      drop_na() %>%
      #group_by(Experiment, Participant, Condition, Device, Platform, Trial, fixation_index)%>%
      group_by(fixation_index)%>%
      summarise(x = mean(x), y = mean(y), z = mean(z))
    return(results)
  }else{
    return(raw_results)
  }
}
