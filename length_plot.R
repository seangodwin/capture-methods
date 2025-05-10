# length_plot.R
# Author: Sean Godwin
# Date: 2025-05-09
# Description: Plots lengths of juvenile salmon across capture methods



### TEst ###

## 0 [LOAD PACKAGES] ------------------------------------------------------------------
library(here)        # for file referencing
library(stringr)     # for str_to_title()
library(PNWColors)   # for colour palette


## 1 [READ IN DATA] ------------------------------------------------------------------
# Remember to change your path
data <- read.csv(here::here("length_data.csv"), header=T, stringsAsFactors=F)


## 2 [SET UP PLOT] -------------------------------------------------------------
# Grab colours
spp.order <- c("pink", "chum", "sockeye", "chinook", "coho")
col.choice <- c(1,4,6,8,10)
col.table <- data.frame(spp=spp.order,
                        col=rev(pnw_palette(name="Sunset2",
                                            n=10, 
                                            type="continuous")[col.choice]))

# Labels and parameters
titles <- c("Beach seine", "Miniature purse seine", "Conventional purse seine", 
            "Microtroll", "Small rope trawl", "Large rope trawl")
method.order <- unique(data$method)
hist.max <- 501
hist.break <- 10
ylab <- seq(0,0.25,0.05)
cex.labs <- 0.85
cex.legend <- 0.8
cex.axis <- cex.title <- 0.7

# Remove the couple data points over max x lim (see figure caption)
data <- data[!(data$length > (hist.max-1)),]


## 3 [PLOT] -------------------------------00-----------------------------------
tiff("length_plot.tiff", width=7, height=7, units="in",
     pointsize=20, res=600, compression="lzw")
par(mfrow=c(3,2), mar=c(1.0,0.5,0.5,0.8), oma=c(2.0,3.1,0,0), 
    tck=-0.03, mgp=c(3,0.5,0), family="sans")
  
  # For each panel
  for(i in 1:length(method.order)) {
    method <- method.order[i]
    spp <- unique(data$spp[data$method==method])
    col <- col.table$col[match(spp, col.table$spp)]
    
    # Plot cumulative histogram (colour = final species in list)
    hist(data$length[data$method==method], xlim=c(0,hist.max), 
         ylim=c(0,max(ylab)*length(data$spp[data$method==method])),
         breaks=seq(1,hist.max,hist.break), 
         col=col.table$col[match(spp,col.table$spp)], freq=T,
         ann=F, xaxt="n", yaxt="n", xaxs="i", yaxs="i", border=F)
    
    # Add individual species (by subtraction)
    for(j in (length(spp)-1):1) {
      hist(data$length[data$method==method & 
            !(data$spp %in% spp[length(spp):(j+1)])], 
           col=col[j], xlim=c(0,hist.max), 
           breaks=seq(1,hist.max,hist.break), freq=T, add=T, border=F)
    }
    
    # X axes
    if(i %in% c(5:6)) {
      axis(1, seq(0,hist.max,100), cex.axis=cex.labs)
      axis(1, at=seq(0,hist.max,50), label=F)
    } else {
      axis(1, at=seq(0,hist.max,50), label=F)
    }
    
    # Y axes
    if(i %in% c(1,3,5)) {
      axis(2, format(ylab, nsmall=2), 
           at=ylab*length(data$spp[data$method==method]), las=1, 
           cex.axis=cex.labs)
    } else {
      axis(2, at=ylab*length(data$spp[data$method==method]), label=F)
    }
    
    
    # Legend
    if(i == 2) {
      legend("right", str_to_title(col.table$spp), fill=col.table$col, 
             bty="n", x.intersp=0.7, cex=cex.legend)
    }
    
    mtext(titles[i], side=3, line=-0.75, cex=cex.title)
  }
  
  
  mtext("Fork length (mm)", side=1, line=0.9, outer=T, cex=cex.axis)
  mtext("Density", side=2, line=2, outer=T, cex=cex.axis+0.05)

dev.off()