##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 2 May 2020
# Function: getTeamBattingDetails
# This function gets the Batting details of a team against all opposition
#
###########################################################################################
#' @title
#' Get team batting details
#'
#' @description
#' This function  gets the batting details of a team in all matchs against all
#' oppositions. This gets all the details of the batsmen balls faced,4s,6s,strikerate, runs, venue etc.
#' This function is then used for analyses of batsmen. This function calls teamBattingPerfDetails()
#'
#' @usage
#' getTeamBattingDetails(team,dir=".",save=FALSE,odir=".")
#'
#' @param team
#' The team for which batting details is required
#'
#' @param dir
#' The source directory
#'
#' @param odir
#' The output directory to store saved files
#'
#' @param save
#' Whether the data frame needs to be saved as RData or not. It is recommended to set save=TRUE
#' as the data can be used for a lot of analyses of batsmen
#'
#' @return battingDetails
#' The dataframe with the batting details
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
#' @examples
#' \dontrun{
#' a <- getTeamBattingDetails("India",dir="../data", save=TRUE)
#' }
#'
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBowlingDetails}}\cr
#'
#' @export
#'
getTeamBattingDetails <- function(team,dir=".",save=FALSE,odir="."){
    overs=batsman=NULL
    a <- paste(dir,"/","*",team,"*",sep="")
    # Gather team against all ooposition
    fl <- Sys.glob(a)

    battingDetails <- NULL
    for(i in 1:length(fl)){
        # Add try-catch to handle issues
        tryCatch({
            load(fl[i])
            match <- overs
            details <- teamBattingPerfDetails(match,team,includeInfo=TRUE)
            # If the side has not batted details will be NULL. Skip in that case
            if(!is.null(dim(details))){
                battingDetails <- rbind(battingDetails,details)
            }else {
                #print("Empty")

                next
            }
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }

    if(save==TRUE){
        fl <-paste(odir,"/",team,"-BattingDetails.RData",sep="")
        save(battingDetails,file=fl)
    }
    battingDetails <- arrange(battingDetails,batsman,date)
    battingDetails

}
