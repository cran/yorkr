##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 24 May 2021
# Function: getSSMBowlingDetails
# This function creates SSM bowling details
#
###########################################################################################
#' @title
#' Gets  the SSM bowling details
#'
#' @description
#' This function creates a single datframe of all SSM bowling
#' @usage
#' getSSMBowlingDetails(dir='.',odir=".")
#'
#' @param dir
#' The input directory
#'
#' @param odir
#' The output directory
#'
#'
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
#'
#' @seealso
#' \code{\link{getIPLBattingDetails}}\cr
#' \code{\link{getIPLBowlingDetails}}\cr
#' \code{\link{getNTBBattingDetails}}\cr
#' \code{\link{getNTBBattingDetails}}\cr
#' @export
#'
getSSMBowlingDetails <- function(dir='.',odir=".") {
    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=totalWickets=NULL
    currDir= getwd()
    teams <-c("Auckland", "Canterbury", "Central Districts",
              "Northern Districts", "Otago", "Wellington")


    # Get all bowling details

    details=df=NULL
    teams1 <- NULL
    for(team in teams){
        print(team)
        tryCatch({
            bowling <- getTeamBowlingDetails(team,dir=dir, save=TRUE,odir=odir)
            teams1 <- c(teams1,team)
        },
        error = function(e) {
            print("No data")

        }
        )
    }
}
