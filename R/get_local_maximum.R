

#' A local maximumvmaxin PoG speed was defined as a PoG speedthat was greater than that in the immediately preceding and fol-lowing frames. Some of these local maxima were due to saccadeswhereas others were due to fixational eye movements and instru-mental noise. To find the optimum speed threshold for separatingthem, a variable speed threshold was introduced that was allowedto range between the lowest and highest recorded local speedmaxima in the data set in 250 equal steps. The number of stepsused was chosen to reflect the constraints imposed by the tempo-ral resolution of the eye-tracker.The light-gray histogram inFig. 2shows, for one observer view-ing one scene over one block of 223 trials, the frequency of localmaxima in PoG speed exceeding a variable speed threshold thatranged between the lowest and highest recorded local maxima(in this block 37 trials were excluded because of lost tracking). Lo-cal maxima exceeding high values of the threshold were attributedto saccades and those not exceeding low values of the thresholdmainly to fixational eye movements. The much greater numberof the latter was reflected inFig. 2by the sharp peak in the numberexceeding threshold values near zero. The location of the elbow inthe histogram was assumed to provide the speed thresholdvoptthat optimally separated the distribution of saccades from the dis-tribution of fixational eye movements and instrumental noise, thatis, where the probability of misclassification was the least and thesame for each.To locate the elbow, the histogram was compared with a nulldistribution of local speed maxima. This null distribution (Tibshira-ni, Walther, & Hastie, 2001) had the property that values were uni-formly distributed between the lowest and highest recorded localspeed maxima. The dashed line inFig. 2shows for this null distri-bution the frequency of local speed maxima exceeding threshold asa function of threshold (since values were distributed uniformly,this frequency declined linearly). The gap between the frequencyof local speed maxima exceeding threshold and the frequency ofnull-distribution local speed maxima exceeding threshold is actu-ally traced by a dotted curve inFig. 2, but it is largely hidden bythe solid curve, which is explained shortly. The maximum in thisFig. 2.Distribution of local maxima in point-of-gaze speeds. The light-grayhistogram shows the frequency of local speed maxima exceeding a variablethreshold that ranged between the lowest and highest recorded local speed maxima(bin width2.5 deg s1). The dashed line shows the frequency of local speedmaxima exceeding threshold under the null distribution, according to whichmaxima are uniformly distributed. A dotted curve traces the gap between the two,but it is largely hidden by the solid curve, which shows the loess smooth of the gapstatistic (Tibshirani et al., 2001). The vertical line marks the optimum speedthresholdvopt. Data were based on 223 trials by one observer viewing one scene.20M.S. Mould et al./Vision Research 57 (2012) 18–25

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
