#' Extract spawning stock biomass (ssb) from gmacs run
#'
#' Spawning biomass may be defined as all males or some combination of males and
#' females
#'
#' @param M list object created by read_admb function
#' @return dataframe of spawning biomass
#' @export
#' 
.get_mnage_df <- function(M,biomass=TRUE)
{
    n <- length(M)
    mdf <- NULL
    for (i in 1:n)
    {
        A <- M[[i]]
        df <- data.frame(year = A$EffN_fsh)
        df$Model <- names(M)[i]
        df$type <- "Fishery"
        df$year <- A$EffN_fsh[,1]
        df$obs  <- A$EffN_fsh[,4]
        df$pre  <- A$EffN_fsh[,5]
        df$lb   <- A$EffN_fsh[,7]
        df$ub   <- A$EffN_fsh[,8]
        mdf     <- rbind(mdf, df)

        df <- data.frame(year = A$EffN_bts)
        df$Model <- names(M)[i]
        df$type <- "Bottom trawl survey"
        df$year <- A$EffN_bts[,1]
        df$obs  <- A$EffN_bts[,4]
        df$pre  <- A$EffN_bts[,5]
        df$lb   <- A$EffN_bts[,7]
        df$ub   <- A$EffN_bts[,8]
        mdf     <- rbind(mdf, df)

        df <- data.frame(year = A$EffN_ats)
        df$Model <- names(M)[i]
        df$type <- "Acoustic trawl survey"
        df$year <- A$EffN_ats[,1]
        df$obs  <- A$EffN_ats[,4]
        df$pre  <- A$EffN_ats[,5]
        df$lb   <- A$EffN_ats[,7]
        df$ub   <- A$EffN_ats[,8]
        mdf     <- rbind(mdf, df)
    }
    return(mdf)
}


#' Plot predicted spawning stock biomass (ssb)
#'
#' Spawning biomass may be defined as all males or some combination of males and
#' females
#'
#' @param M List object(s) created by read_admb function
#' @param xlab the x-label of the figure
#' @param ylab the y-label of the figure
#' @param ylim is the upper limit of the figure
#' @param alpha the opacity of the ribbon
#' @return Plot of model estimates of spawning stock biomass 
#' @export
#' 
plot_mnage <- function(M, xlab = "Year", ylab = "Mean age", ylim = NULL, alpha = 0.1,biomass=TRUE)
{
    xlab <- paste0("\n", xlab)
    ylab <- paste0(ylab, "\n")
    
    mdf <- .get_mnage_df(M,biomass=biomass)
    
    p <- ggplot(mdf) + labs(x = xlab, y = ylab)
    
    if (is.null(ylim))
    {
        p <- p + expand_limits(y = 0)
    } else {
        p <- p + ylim(ylim[1], ylim[2])        
    }
    
    if (length(M) == 1)
    {
        p <- p + geom_line(aes(x = year, y = pre)) +geom_point(aes(x=year, y=obs)) + 
            geom_errorbar(aes(x = year, ymax = ub, ymin = lb))
    } else {
        p <- p + geom_line(aes(x = year, y = pre, col = Model),size=1.2) + geom_point(aes(x=year, y=obs)) + 
            geom_errorbar(aes(x = year, ymax = ub, ymin = lb))
    }
    
    #if(!.OVERLAY) 
    p <- p + facet_grid(type ~ .) #+ guides(colour=FALSE)
    print(p + .THEME)
}
