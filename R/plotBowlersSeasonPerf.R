##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 11 Jun 2022
# Function: plotBowlersSeasonPerf
# This function ranks the  T20  batsmen
#
#
###########################################################################################
#' @title
#' PLot the season performance of  T20 batsmen
#'
#' @description
#' This function creates a single datframe of all T20 batsmen and then ranks them
#' @usage
#' plotBowlersSeasonPerf(dir=".",bowlers, dateRange, type, wicketsVsER,plot=1)
#'
#'
#' @param dir
#' The input directory
#'
#' @param bowlers
#' List of bowlers
#'
#' @param dateRange
#' Date interval to consider
#'
#' @param type
#' T20 format type
#'
#' @param wicketsVsER
#'  Wickets or Economy Rate
#'
#' @param plot
#' Whether to use ggplot2 or ggplotly
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
#' plotBowlersSeasonPerf(dir=".",bowlers, dateRange, type, wicketsVsER,plot=1)
#' }
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'

plotBowlersSeasonPerf <- function(dir=".",bowlers, dateRange,type, wicketsVsER,plot=1) {
    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=totalWickets=year=NULL
    ymd=years=ggplotly=NULL
    wicketPlayerOut=opposition=venue=NULL
    currDir= getwd()

    #Change dir
    setwd(dir)
    bowlingDF<-NULL

    print("*******")
    print(wicketsVsER)
    print(type)
    bowlingDetails <- paste(type,"-BowlingDetails.RData",sep="")
    print(bowlingDetails)
    load(bowlingDetails)


    if(is.null(bowlers))
        return()
    print(length(bowlers))
    print(bowlers)
    print(dim(bowlers))

    df=bowlingDF %>% filter(bowler %in% bowlers)
    print(dim(df))
    year1 = year(dateRange[1])
    year2 = year(dateRange[2])
    range = year2 - year1 + 1

    date1 = ymd(paste(year1,"-01-01",sep=""))
    date2 = ymd(paste(year1,"-12-31",sep=""))


    g <- NULL
    for (x in 1:range){
        # Note: If the date Range is NULL setback to root directory
        tryCatch({

            df1=df %>% filter(date >= date1  & date <= date2)

        },
        warning=function(war)
        {
            print(paste("NULL values: ", war))
        },
        error=function(err)
        {
            # Change to root directory on error
            setwd(currDir)
            cat("Back to root",getwd(),"\n")
        })


        # Compute number of matches played
        a=df1 %>% select(bowler,date) %>% unique()
        b=summarise(group_by(a,bowler),matches=n())

        # Compute wickets
        c <- filter(df1,wicketPlayerOut != "nobody")
        d <- select(c,bowler,wicketPlayerOut,economyRate,date,opposition,venue)
        e <- summarise(group_by(d,bowler,date,economyRate),wickets=length(unique(wicketPlayerOut)))
        f=summarise(group_by(e,bowler), totalWickets=sum(wickets),meanER=mean(economyRate))


        f$year= year(date1)
        g <- rbind(g,f)
        date1 <- date1 + years(1)
        date2 <- date2+  years(1)
        # Reset to currDir
        setwd(currDir)
    }
    plot.title <- paste("Season performance of bowlers")
    if(plot == 1){ #ggplot2
        print("here")
        if(wicketsVsER == "Wickets over Economy rate"){
            print(wicketsVsER)
            h <- select(g,year, bowler, totalWickets)
            ggplot(h,aes(year,totalWickets,colour=bowler)) +geom_line(size=1) +
              xlab("Year") + ylab("Total Wickets")+ scale_x_continuous(breaks=seq(min(h$year),max(h$year),1)) +
                ggtitle(plot.title)
        } else if(wicketsVsER == "Economy rate over Wickets"){
            h <- select(g,year,bowler,  meanER)
            print(h$meanER)
            g <-  ggplot(h,aes(year,meanER,colour=bowler)) +geom_line(size=1) +
                xlab("Year") + ylab("Mean ER")+ scale_x_continuous(breaks=seq(min(h$year),max(h$year),1)) +
                ggtitle(plot.title)

        }

    } else if (plot == 2){ #ggplotly
        print("here1")
        if(wicketsVsER == "Wickets over Economy rate"){
            print(wicketsVsER)
            h <- select(g,year, bowler, totalWickets)
            g <- ggplot(h,aes(year,totalWickets,colour=bowler)) +geom_line(size=1) +
                xlab("Year") + ylab("Total Wickets")  + scale_x_continuous(breaks=seq(min(h$year),max(h$year),1)) +
                ggtitle(plot.title)
            ggplotly(g)
        } else if(wicketsVsER == "Economy rate over Wickets"){
            h <- select(g,year,bowler,  meanER)
            print(h$meanER)
            g <-  ggplot(h,aes(year,meanER,colour=bowler)) +geom_line(size=1) +
                xlab("Year") + ylab("Mean ER") + scale_x_continuous(breaks=seq(min(h$year),max(h$year),1)) +
                ggtitle(plot.title)
            ggplotly(g)
        }

    }

}
