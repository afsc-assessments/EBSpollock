AgeFitsSrv <- function( labrep.file="16.1/For_R.rep" , case_label="2016 assessment") {
  library(PBSmodelling) 
  subtle.color <- "gray40"
  x <- readList(labrep.file)
  names(x)
  ages <- c(1,length(x$pobs_fsh[1,-1]) ) #age range
  obs.data  <- x$pobs_bts[,-1]
  pred.data <- x$phat_bts[,-1]
  years     <- x$pobs_bts[,1]
  x$pobs_bts
  print(years)
  nyears <- length(years)
  ages.list <- ages[1]:ages[2]
  print(ages.list)
  nages <- length(ages.list)
  
  mtmp <- c(ceiling(nyears/3),3)
  par(mfcol=mtmp,oma=c(3.5,4.5,3.5,1),mar=c(0,0,0,0))
  cohort.color <- rainbow(mtmp[1]+2)[-c(1:2)]   #use hideous rainbow colors because they loop more gracefully than rich.colors
  ncolors <- length(cohort.color)
  
  #axis(2,las=1,at=c(0,0.5),col=subtle.color,col.axis=subtle.color,lwd=0.5) 
  #With
  #axis(2,las=1,at=c(0,0.25,0.5),col=subtle.color,col.axis=subtle.color,lwd=0.5)
  ylim <- c(0,1.05*max(obs.data,pred.data))
  for (yr in 1:nyears) { 
    names.arg <- rep("",nages)
    x <- barplot(obs.data[yr,],space=0.2,ylim=ylim,las=1,names.arg=names.arg, cex.names=0.5, xaxs="i",yaxs="i",border=subtle.color,
                 col=cohort.color[1:nages],axes=F,ylab="",xlab="")
    cohort.color <- c(cohort.color[ncolors],cohort.color[-1*ncolors])  #loop around colors
    if (yr %% mtmp[1] == 0) {
      axis(side=1,at=x,lab=ages.list, line=-0.1,col.axis=subtle.color, col=subtle.color,lwd=0,lwd.ticks=0)  #just use for the labels, to allow more control than names.arg
    }
    if (yr <= mtmp[1]) {
      axis(2,las=1,at=c(0,0.2,0.4),col=subtle.color,col.axis=subtle.color,lwd=0.5)
    }
    par(new=T)
    par(xpd=NA)
    plot(x=x,y=pred.data[yr,],ylim=ylim, xlim=par("usr")[1:2], las=1,xaxs="i",yaxs="i",bg="white",col="brown",typ="b",lty=1,pch=19,cex=1.1,axes=F,ylab="",xlab="")
    box(col=subtle.color,lwd=0.5)
    x.pos <- par("usr")[1] + 0.85*diff(par("usr")[1:2])   #par("usr") spits out the current coordinates of the plot window
    y.pos <- par("usr")[3] + 0.75*diff(par("usr")[3:4])   #par("usr") spits out the current coordinates of the plot window
    text(x=x.pos,y=y.pos,years[yr],cex=1.2, col=subtle.color)
    par(xpd=T)
  }
  mtext(side=1,outer=T,"Age",line=2)
  mtext(side=2,outer=T,"Proportion",line=3.2)
  mtext(side=3,outer=T,line=1.2,"EBS pollock survey age composition data")
  mtext(side=3,outer=T,line=0.2,paste("(",case_label,")",sep=""),cex=0.6)
}
