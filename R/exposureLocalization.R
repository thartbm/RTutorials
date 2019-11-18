

t.interval = function(data, variance = var(data, na.rm=TRUE), conf.level = 0.95) {
  
  data <- data[!is.na(data)]
  
  z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
  
  xbar = mean(data)
  sdx = sqrt(variance/length(data))
  
  return(c(xbar - z * sdx, xbar + z * sdx))
  
}

getColors <- function() {
  
  colors <- list()
  
  colors[['blue']]      <- list('s'='#005de4ff', 't'='#005de42f')
  
  colors[['lightblue']] <- list('s'='#0fd2e2ff', 't'='#0fd2e22f')
  
  colors[['yorkred']]   <- list('s'='#e51636ff', 't'='#e516362f')
  
  colors[['orange']]    <- list('s'='#ff8200ff', 't'='#ff82002f')
  
  colors[['purple']]    <- list('s'='#b400e4ff', 't'='#b400e42f')
  
  # colorset[['onlPasS']] <- '#8266f4ff' # violet
  # colorset[['onlPasT']] <- '#8266ff2f'
  
  # colorset[['onlPasS']] <- '#ff6ec7ff' # pink
  # colorset[['onlPasT']] <- '#ff6ec72f'
  
  return(colors)
  
}

plotExposureLocalization <- function(target='inline', remove15=FALSE) {
  
  points=c(15,25,35,45,55,65,75)
  
  # get the data to plot:
  exp <- read.csv('data/exposure_localization.csv', stringsAsFactors = FALSE)
  
  # some columns need to be factors:
  exp$participant   <- factor(exp$participant)
  exp$rotated_b     <- factor(exp$rotated_b)
  exp$passive_b     <- factor(exp$passive_b)
  exp$handangle_deg <- factor(exp$handangle_deg)
  
  if (remove15) {
    
    points=c(25,35,45,55,65,75)
    exp <- exp[-which(exp$handangle_deg == 15),]
    
  }
  
  colors <- getColors()
  
  # get the averages:
  exp.avg <- aggregate(taperror_deg ~ rotated_b + passive_b + handangle_deg, data=exp, FUN=mean)
 
  # get the confidence intervals for polygon areas:
  exp.act <- exp[which(exp$passive_b == 0),]
  exp.CI.act <- matrix(unlist(by(exp.act$taperror_deg, INDICES=c(exp.act$handangle_deg), FUN=t.interval)),nrow=2)
  exp.pas <- exp[which(exp$passive_b == 1),]
  exp.CI.pas <- matrix(unlist(by(exp.pas$taperror_deg, INDICES=c(exp.pas$handangle_deg), FUN=t.interval)),nrow=2)
  
  if (target == 'svg') {
    installed.list <- rownames(installed.packages())
    if ('svglite' %in% installed.list) {
      library('svglite')
      svglite(file='ExposureLocalization.svg', width=7.5, height=3)
    } else {
      cat("Could not generate svg, please install package: 'svglite'\n")
      target='inline'
    }
  }
  
  par(mfrow=c(1,3))
  
  # first we do the before and after stuff for both active and passive localization:
  
  for (passive_b in c(0,1)) {
    
    plot(-1000,-1000, main=c('active','passive')[passive_b+1], xlab='hand angle [°]', ylab='localization error [°]', xlim=c(min(points)-5,max(points)+5), ylim=c(5,-15), axes=F)
    
    for (rotated_b in c(0,1)) {
      
      col <- colors[[c('orange','yorkred','lightblue','blue')[(passive_b*2)+rotated_b+1]]]
      
      exp.cond <- exp[which(exp$passive_b == passive_b & exp$rotated_b == rotated_b),]
      exp.CI <- matrix(unlist(by(exp.cond$taperror_deg, INDICES=c(exp.cond$handangle_deg), FUN=t.interval)),nrow=2)
      
      X <- c(points, rev(points))
      Y <- c(exp.CI[1,],rev(exp.CI[2,]))
      
      polygon(X,Y,border=NA,col=col$t)
      
    }
    
    subplotcolors <- c()
    
    for (rotated_b in c(0,1)) {
      
      col <- colors[[c('orange','yorkred','lightblue','blue')[(passive_b*2)+rotated_b+1]]]
      
      exp.avg.cond <- exp.avg[which(exp.avg$passive_b == passive_b & exp.avg$rotated_b == rotated_b),]
      
      lines(points, exp.avg.cond$taperror_deg, col=col$s)
      
      subplotcolors <- c(subplotcolors, col$s)
      
    }
    
    legend(15,-15,c('aligned','rotated'),col=subplotcolors,lty=c(1,1),lwd=c(1,1),bty='n')
    
    axis(side=1, at=points)
    axis(side=2, at=seq(5,-15,-5))
    
  }
  
  # now the difference scores that we're really interested in
  exp.shift <- aggregate(taperror_deg ~ passive_b + handangle_deg + participant, data = exp, FUN=diff)
  
  exp.shift$taperror_deg <- as.numeric(exp.shift$taperror_deg)
  
  exp.shift.avg <- aggregate(taperror_deg ~ passive_b + handangle_deg, data = exp.shift, FUN=mean, na.rm=TRUE)
  
  plot(-1000,-1000, main='localization shifts', xlab='hand angle [°]', ylab='localization shift [°]', xlim=c(min(points)-5,max(points)+5), ylim=c(5,-15), axes=F)
  
  for (passive_b in c(0,1)) {
    
    col <- colors[[c('yorkred','blue')[passive_b+1]]]
    
    exp.cond <- exp.shift[which(exp.shift$passive_b == passive_b),]
    exp.CI <- matrix(unlist(by(exp.cond$taperror_deg, INDICES=c(exp.cond$handangle_deg), FUN=t.interval)),nrow=2)
    
    X <- c(points, rev(points))
    Y <- c(exp.CI[1,],rev(exp.CI[2,]))
    
    polygon(X,Y,border=NA,col=col$t)
    
  }
  
  subplotcolors <- c()
  
  for (passive_b in c(0,1)) {
    
    col <- colors[[c('yorkred','blue')[passive_b+1]]]
    
    exp.shift.avg.cond <- exp.shift.avg[which(exp.shift.avg$passive_b == passive_b),]
    
    lines(points, exp.shift.avg.cond$taperror_deg, col=col$s)
    
    subplotcolors <- c(subplotcolors, col$s)
    
  }
  
  legend(15,-15,c('active','passive'),col=subplotcolors,lty=c(1,1),lwd=c(1,1),bty='n')
  
  axis(side=1, at=points)
  axis(side=2, at=seq(5,-15,-5))
  
  
  if (target=='svg') {
    dev.off()
  }
  
}