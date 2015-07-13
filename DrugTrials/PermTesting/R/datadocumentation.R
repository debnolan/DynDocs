#' Two populations of data from a trial on calcium's effect on blood pressure
#' 21 participants randomly selected to receive one of two treatments
#' Values are a unique patient's change in blood pressure after treatment
#' Change in BP is difference (in mmHg) before and after trial
#' 
#' A dataset containing the changes in blood pressure for the two populations in the trial
#' 
#' @format A data frame with 11 rows and 2 variables:
#' \describe{
#' \item{calcium}{blood pressure, in mmHg}
#' \item{placebo1}{blood pressure, in mmHg}
#' }
#' @source Cobb Manuscript
"Calcium"

#' Two populations of data from a trial on alcohol's effect on driving
#' Randomly selected to receive alcohol or placebo before testing
#' Values are 20 participants' reaction times in simulated driving situations
#' Reaction times are averages for multiple driving trials
#' 
#' A dataset containing the reaction times for the two populations in the trial
#' 
#' @format A data frame with 10 rows and 2 variables:
#' \describe{
#' \item{alcohol}{time, in seconds}
#' \item{placebo2}{time, in seconds}
#' }
#' @source http://thirteen-01.stat.iastate.edu/wiki/stat430/files?filename=wilcoxon_rank_sum-test.pdf
"Alcohol"

#' Two populations from clinical trial on HIV patients
#' Assesing the effectiveness of a new anti-retroviral therapy
#' HIV patients randomized to receive one of two therapies
#' Therapy1 is standard therapy, whereas therapy2 is new therapy being tested
#' Original data called viral load
#' Measured number of HIV copies per mililiters of blood post-therapy
#' 
#' A dataset containing the reaction times for the two populations in the trial
#' 
#' @format A data frame with 15 rows and 2 variables:
#' \describe{
#' \item{New_Drug}{viral load, HIV copies/mL blood}
#' \item{Standard_Drug}{viral load, HIV copies/mL blood}
#' }
#' @source http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_Nonparametric/mobile_pages/BS704_Nonparametric4.html
"HIV"