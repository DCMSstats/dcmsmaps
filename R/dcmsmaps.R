# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

dcmsmaps <- function(csvfile='',outfile='',mincol='',maxcol='',scale=TRUE,england=FALSE,labels=TRUE,pound=FALSE) {

  # Read in files
  data(NUTS1)
  area2 <- NUTS1
  mydata <- read.csv(csvfile)

  # Add the CSV data to the shape file
  area2@data = data.frame(area2@data, mydata[match(area2@data$NAME, mydata$NAME),])

  # Subset if England only
  if(england==TRUE){
    area2 <-  area2[area2$NATION=='England',]
  }

  # Set pound sign if the data is financial
  pounds=''
  if(pound){
    pounds='£'
  }

  # Convert colours to RGB
  mincol=col2rgb(mincol, alpha = FALSE)
  maxcol=col2rgb(mincol, alpha = FALSE)
  rmin=mincol[1]
  rmax=maxcol[1]
  gmin=mincol[2]
  gmax=maxcol[2]
  bmin=mincol[3]
  bmax=maxcol[3]

  colours=0
  redgrnblu=0

  # Scale the colours to build the colour scale
  for(i in 0:length(area2@data$mapdata)){
    red=rmin+(rmax-rmin)*(area2@data$mapdata[i]-min(area2@data$mapdata))/max(area2@data$mapdata-min(area2@data$mapdata))
    blu=bmin+(bmax-bmin)*(area2@data$mapdata[i]-min(area2@data$mapdata))/max(area2@data$mapdata-min(area2@data$mapdata))
    grn=gmin+(gmax-gmin)*(area2@data$mapdata[i]-min(area2@data$mapdata))/max(area2@data$mapdata-min(area2@data$mapdata))
    colours[i]=rgb(red,grn,blu,maxColorValue=255)
  }

  # Open to EPS (or PNG) to plot to
  setEPS()
  postscript(outfile,width=11.69,height=8.27)
  dev.off()
  pdf(outfile,width=11.69,height=8.27)
  plot(area2,col=colours,border=0)

  fsize=.75

  # Make Key
  if(scale==TRUE){

    if(england==FALSE){

      x=c(600000,700000,700000,600000)
      y=c(620000,620000,630000,630000)

      for(i in 0:89){
        y=y+2500
        red=rmin+(rmax-rmin)*(i/89)
        blu=bmin+(bmax-bmin)*(i/89)
        grn=gmin+(gmax-gmin)*(i/89)
        polygon(x,y,col=rgb(red,grn,blu,maxColorValue=255),border=0)
      }

      text(710000,640000, paste0(pounds,prettyNum(min(area2@data$mapdata),big.mark=',')),adj=0,cex=fsize,col="black")
      text(710000,840000, paste0(pounds,prettyNum(max(area2@data$mapdata),big.mark=',')),adj=0,cex=fsize,col="black")
    }

    if(england==TRUE){
      x=c(50000,150000,150000,50000)
      y=c(420000,420000,430000,430000)

      for(i in 0:89){
        y=y+2500
        red=rmin+(rmax-rmin)*(i/89)
        blu=bmin+(bmax-bmin)*(i/89)
        grn=gmin+(gmax-gmin)*(i/89)
        polygon(x,y,col=rgb(red,grn,blu,maxColorValue=255),border=0)
      }

      text(160000,430000, prettyNum(min(area2@data$mapdata),big.mark=','),adj=0,cex=fsize,col="black")
      text(160000,650000, prettyNum(max(area2@data$mapdata),big.mark=','),adj=0,cex=fsize,col="black")
    }
  }


  #This bit tries to work out if you're entering £thousands, precise amounts in £s
  #or population like figures

  digitsl=0
  if(any(area2@data$mapdata %% 1>0)){digitsl=1}
  if(any(area2@data$mapdata-round(area2@data$mapdata,1)!=0)){digitsl=2}
  if(any(area2@data$mapdata %% 1>0) && pound){digitsl=2}

  # Labels
  if(labels){

    #maincol=rgb(rmin,gmin,bmin,maxColorValue=255)
    maincol="black"

    #East Midlands
    polygon(c(470000,670000),c(370000,370000),border=maincol)
    text(675000,370000,
         paste0(area2@data$NAME[1],"\n",pounds,formatC(area2@data$mapdata[1],big.mark=',',digits=digitsl,format="f")),
         adj=0,cex=fsize,col="black")

    #East of England
    polygon(c(570000,670000),c(270000,270000),border=maincol)
    text(675000,270000,
         paste0(area2@data$NAME[2],"\n",pounds,formatC(area2@data$mapdata[2],big.mark=',',digits=digitsl,format="f")),
         adj=0,cex=fsize,col="black")

    #London
    polygon(c(550000,670000),c(180000,180000),border=maincol)
    text(675000,180000,
         paste0(area2@data$NAME[3],"\n",pounds,formatC(area2@data$mapdata[3],big.mark=',',digits=digitsl,format="f")),
         adj=0,cex=fsize,col="black")

    #North East
    polygon(c(470000,670000),c(440000,440000),border=maincol)
    text(675000,440000,
         paste0(area2@data$NAME[4],"\n",pounds,format(area2@data$mapdata[4],nsmall=1)),
         adj=0,cex=fsize,col="black")

    #North West
    polygon(c(370000,670000),c(510000,510000),border=maincol)
    text(675000,510000,
         paste0(area2@data$NAME[5],"\n",pounds,format(area2@data$mapdata[5],nsmall=1)),
         adj=0,cex=fsize,col="black")

    #South East
    polygon(c(500000,500000),c(130000,50000),border=maincol)
    text(500000,20000,
         paste0(area2@data$NAME[6],"\n",pounds,format(area2@data$mapdata[6],nsmall=1)),
         adj=0,cex=fsize,col="black")

    #South West
    polygon(c(140000,290000),c(115000,115000),border=maincol)
    text(135000,115000,
         paste0(area2@data$NAME[7],"\n",pounds,format(area2@data$mapdata[7],nsmall=1)),
         adj=1,cex=fsize,col="black")

    #West Midlands
    polygon(c(140000,380000),c(310000,310000),border=maincol)
    text(135000,310000,
         paste0(area2@data$NAME[8],"\n",pounds,format(area2@data$mapdata[8],nsmall=1)),
         adj=1,cex=fsize,col="black")

    #Yorkshire & The Humber
    polygon(c(410000,670000),c(580000,580000),border=maincol)
    text(675000,580000,
         paste0(area2@data$NAME[9],"\n",pounds,format(area2@data$mapdata[9],nsmall=1)),
         adj=0,cex=fsize,col="black")

    if(england==FALSE){
      #Wales
      polygon(c(140000,290000),c(220000,220000),border=maincol)
      text(135000,220000,
           paste0(area2@data$NAME[12],"\n",pounds,format(area2@data$mapdata[12],nsmall=1)),
           adj=1,cex=fsize,col="black")

      #Scotland
      polygon(c(250000,670000),c(900000,900000),border=maincol)
      text(675000,900000,
           paste0(area2@data$NAME[11],"\n",pounds,format(area2@data$mapdata[11],nsmall=1)),
           adj=0,cex=fsize,col="black")

      #Northern Ireland
      polygon(c(100000,100000),c(500000,420000),border=maincol)
      text(95000,415000,
           paste0(area2@data$NAME[10],"\n",pounds,format(area2@data$mapdata[10],nsmall=1)),
           adj=1,cex=fsize,col="black")
    }

  }

  dev.off()
  #return(area2)
}
