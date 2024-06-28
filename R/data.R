#' Approach-Avoidance Task examining approach bias to different foods
#'
#' This data originates from an approach-avoidance task examining approach bias towards food.
#' Participants responded to the stimulus category (food or object) by pulling or pushing a joystick.
#' Instructions were flipped from one block to the next.
#'
#' @docType data
#'
#' @usage data(foodAAT)
#'
#' @format An object of class \code{"data.frame"}.
#' 
#' @details
#' * subjectid: Participant ID.
#' * stimid: Stimulus ID.
#' * is_pull: Whether the trial required an approach response (1) or an avoid response (0).
#' * is_target: Whether the trial featured a food stimulus (1) or an object stimulus (0).
#' * error: Whether the response was incorrect (1) or correct (0).
#' * RT: The response initiation time.
#' * FullRT: The time from stimulus onset to response completion.
#' * trialnum: The trial number.
#' * blocknum: The block number.
#' * palatability: The participant's palatability rating for the stimulus (foods only).
#' * valence: The participant's valence rating for the stimulus.
#' * FCQS_2_craving: The participant's FCQS state food craving score at time of testing.
#' * FCQS_2_hunger: The participant's FCQS state hunger score at time of testing.
#' 
#' @md
#'
#' @keywords datasets
#'
#' @references Lender, A., Meule, A., Rinck, M., Brockmeyer, T., & Blechert, J. (2018). 
#' Measurement of food-related approach–avoidance biases: 
#' Larger biases when food stimuli are task relevant. Appetite, 125, 42–47. 
#' \doi{10.1016/j.appet.2018.01.032}
#' 
#' @source \doi{10.1016/j.appet.2018.01.032}
#'
"foodAAT"

#' Implicit Association Task examining implicit bias towards White and Black people
#'
#' This data originates from the publicly available 
#' implicit association test (IAT) on racial prejudice hosted by Project Implicit.
#' 200 participants were randomly sampled from the full trial-level data 
#' available for participants from 2002 to 2022.
#' We included only those IAT blocks relevant to scoring (3,4,6,7) and 
#' only individuals with full data.
#'
#' @docType data
#'
#' @usage data(raceIAT)
#'
#' @format An object of class \code{"data.frame"}.
#' 
#' @details
#' * session_id: The session id, proxy for participant number.
#' * task_name: Subtype of IAT used.
#' * block_number: IAT block number.
#' * block_pairing_definition: Stimulus pairing displayed in block.
#' * block_trial_number: Trial number within block.
#' * stimulus: Stimulus name.
#' * required_response: The response required from the participant.
#' * latency: Participant's response latency.
#' * error: Whether the response was wrong (\code{TRUE}).
#' * trial_number: Experimentwise trial number.
#' * stimcat: The stimulus category.
#' * respcat: Category of the required response.
#' * blocktype: Either practice block or full IAT block.
#' * congruent: Whether the block was congruent with anti-black bias (\code{TRUE}) or not.
#' * latency2: Response latencies with those for incorrect responses
#' replaced by the block mean plus a penalty.
#' 
#' @md
#'
#' @keywords datasets
#'
#' @references Xu, K., Nosek, B., & Greenwald, A. G. (2014). 
#' Psychology data from the race implicit association test on the project implicit demo website. 
#' Journal of open psychology data, 2(1), e3-e3. 
#' \doi{10.5334/jopd.ac}
#' 
#' @source \href{https://osf.io/y9hiq/}{OSF.io repository}
#'
"raceIAT"

# library(magrittr)
# library(dplyr)
# load("./../alldata.rda")
# foodAAT<-datasets$relfoo
# foodAAT %<>% select(subjectid,stimid,is_pull,is_target,error,RT,FullRT,trialnum,blocknum,
#                     palatability,valence,FCQS_2_craving,FCQS_2_hunger)
# save(foodAAT,file="./data/foodAAT.RData")
