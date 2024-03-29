##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: batsmanRunsPredict
# This function uses rpart classiication tree to predict the number of deliveries and the
# runs scored by batsman
#
###########################################################################################
#' @title
#' Predict deliveries to runs scored
#'
#' @description
#' This function  uses a classification tree to predict the number of deliveries required for
#' the batsman to score the runs. It uses the package rpart to perform the classification
#'
#' @usage
#' batsmanRunsPredict(df, name= "A Leg Glance",dateRange)
#'
#' @param df
#' Data frame
#'
#' @param name
#' Name of batsman
#'
#' @param dateRange
#' Date interval to consider
#'
#' @return None
#' @references
#' \url{https://cricsheet.org/}\cr
#' \url{https://gigadom.in/}\cr
#' \url{https://github.com/tvganesh/yorkrData/}
#'
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' \dontrun{
#' #Get the data frame for Kohli
#' kohli <- getBatsmanDetails(team="India",name="Kohli",dir=pathToFile)
#' batsmanRunsVsStrikeRate(kohli,"Kohli",dateRange)
#' }
#'
#' @seealso
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{batsmanRunsVsDeliveries}}\cr
#' \code{\link{batsmanRunsVsStrikeRate}}\cr
#' \code{\link{batsmanRunsPredict}}\cr
#' \code{\link{teamBatsmenPartnershipAllOppnAllMatches}}\cr
#'
#' @export
#'
batsmanRunsPredict <- function(df,name= "A Leg Glance",dateRange){
    batsman = ballsPlayed = runs = rpart =  NULL
    df=df %>% filter(date >= dateRange[1] & date <= dateRange[2])
    b <- select(df,batsman,ballsPlayed,runs)
    names(b) <-c("batsman","deliveries","runs")
    m <-rpart(runs~deliveries,data=b)
    atitle <- paste(name,"- Runs vs Required number of Deliveries")
    rpart.plot(m,main=atitle)

}
