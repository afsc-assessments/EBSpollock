

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
plot_sel <- function(M)
{
  df <- data.frame(cbind(1964:2016,M$sel_fsh[,1:10]) ); names(df) <- c("yr",1:10)
	sdf <- gather(df,age,sel,2:11) %>% filter(yr>1980) %>% mutate(age=as.numeric(age)) #+ arrange(age,yr)
	# sdf <- gather(df,age,sel,2:16) %>% mutate(age=as.numeric(age)) #+ arrange(age,yr)
	p1 <- ggplot(sdf, aes(x=age,y=sel/2+yr,group=yr,fill=yr)) +                  
      geom_ribbon(aes(ymin=yr,ymax=sel/1.3+yr),fill="tan",col="grey60",alpha=.3)  + ylab("") +                      
      xlab("Age") + guides(fill=FALSE,alpha=FALSE,col=FALSE) + .THEME + scale_fill_manual(values=colorRampPalette(rev(brewer.pal(11,"Spectral")))(10),guide="none")
	p1 <- p1 + scale_y_continuous(breaks=seq(1965,2015,5)) + scale_x_continuous(breaks = c(1,3,5,7,9))
	# p1 <- p1 + scale_y_reverse()
	p1
}