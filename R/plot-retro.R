plot_retro <- function(M,main=""){
	M <- retouts
	length(M)
	df <- data.table(M[[1]]$SSB,case=1)
	for (i in 2:length(M))
		df <- rbind(df,data.table(M[[i]]$SSB,case=i))
	names(df) <- c("yr","SSB","SE","lb","ub","case")
	df$case <- as.factor(df$case)
	df$SSB
	df <- filter(df,yr>1977,yr<=2016)
	dt_dc <- dcast(df, yr ~ case, value.var = c("SSB"))
	dt_dc <-data.frame(dt_dc)
	bdft <- dt_dc[,2:12]/dt_dc[,2]
	# for (i in 1:10) bdft[1:(39-i),i+5] <- get(paste0(rn,"_",i))$SSB[14:(52-i),2]
	#for (i in 1:10) bdft[,i+5] <- bdft[,i+5]/bdf$SSB
	for (j in 0:9){
	 for (i in 1:(39-j)){
    icnt <- i+i*j 
    print(df$case[icnt]); #print(j)
		df$RSSB[icnt] <- df$SSB[icnt] / df$SSB[i]
	} 
	} 
	df
	head(df,45 )
	p1 <- ggplot(df,aes(x=yr,y=SSB,col=case)) +geom_line(size=1.3) + scale_x_continuous(limits=c(1995,2017)) + scale_y_continuous(limits=c(0,5e3)) + ylab("Spawning biomass (kt)") + xlab("Year") +  .THEME   + theme(legend.position="none")
	p1
	p2 <- ggplot(df,aes(x=yr,y=SSB/df$SSB[case==1,],col=case)) +geom_line(size=1.3) + scale_x_continuous(limits=c(1995,2017)) + scale_y_continuous(limits=c(0,5e3)) + ylab("Spawning biomass (kt)") + xlab("Year") +  .THEME   + theme(legend.position="none")
	p2
	for (i in 1:10) {
	  tdf <- data.frame(cbind(bdft[1],SSB=bdft[5+i]))[1:(39-i),]
	  names(tdf) <- c("yr", "SSB")
	  p2 <- p2 + geom_line(data=tdf, aes(x=yr,y=SSB),col=i,size=1.5)
	  tdf <- tdf[dim(tdf)[1],]
	  p2 <- p2 + geom_point(data=tdf,aes(x=yr,y=SSB),size=4,col=i)
	}    
	p2 <-p2 +geom_hline(aes(yintercept=1),size=3,linetype="dotted")
	p2 <-p2 +geom_hline(aes(yintercept=1),size=1,col="grey")
	# Mohn's rho
	rc = retro0$SSB[,2]
	ntmp=0
	rho=0
	for (i in 1:15) {
	  dtmp = (get(paste0(rn,"_0"))$SSB )
	  lr   = length(dtmp[,1])
	  ntmp = ntmp+(dtmp[lr,2] -rc[lr])/rc[lr]
	  #rho = rho + (-(ALL[i,2]-ALL[*tsyrs-i,2+i]))/ALL[(j)*tsyrs-i,2]
	  rho  = rho + (-(dtmp[i,2]-rc[i]))/rc[lr]
	  print(paste(i,ntmp/i,rho))
	}    
	if (main=="") p2 = p2+ggtitle(rn)
	if (main!="") p2 = p2+ggtitle(main)
	return(p2)
}