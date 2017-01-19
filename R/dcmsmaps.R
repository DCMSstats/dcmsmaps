#DCMS Map Maker

##Intro

#This package is used to make maps for DCMS statistical publications. It combines input data
#in a standard format with an Ordnance Survey boundary line shapefile distributed under the
#Open Government License.

dcmsmaps <- function(csvfile='',outfile='',mincol='#3CB43C',maxcol='#5B7DC8',scale=TRUE,england=FALSE,englandwales=FALSE,labels=TRUE,pound=FALSE) {

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

  # Subset if England and Wales only
  if(englandwales==TRUE){
    area2 <-  area2[area2$NATION!='Scotland',]
    area2 <-  area2[area2$NATION!='Northern Ireland',]
  }

  # Set pound sign if the data is financial
  pounds=''
  if(pound){
    pounds='£'
  }

  # Convert colours to RGB
  mincol=col2rgb(mincol, alpha = FALSE)
  maxcol=col2rgb(maxcol, alpha = FALSE)
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
    area2@data$colours[i]=rgb(red,grn,blu,maxColorValue=255)
    colours[i]=rgb(red,grn,blu,maxColorValue=255)
  }

  # Open to EPS to plot to
  setEPS()
  postscript(outfile,width=11.69,height=8.27,colormodel="rgb")
  #Different save method is needed for Windows and Mac
  #if(Sys.info()['sysname']=="Windows"){
  #  dev.off()
  #  dev.new(width=11.69,height=8.27,units="in")
  #}

  #dev.off()
  #pdf(outfile,width=11.69,height=8.27)
  #for(j in 1:length(area2)){
  #  plot(area2[j,],col=area2@data$colours[j],border=0)
  #}

  plot(area2,col=area2@data$colours,border=0)

  fsize=.75


  # Make Key
  if(scale==TRUE){

    if(england==FALSE && englandwales==FALSE){

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

    if(england==TRUE || englandwales==TRUE){
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
         paste0(area2@data$NAME[9],"\n",pounds,formatC(area2@data$mapdata[9],big.mark=',',digits=digitsl,format="f")),
         adj=0,cex=fsize,col="black")

    #North West
    polygon(c(370000,670000),c(510000,510000),border=maincol)
    text(675000,510000,
         paste0(area2@data$NAME[5],"\n",pounds,formatC(area2@data$mapdata[5],big.mark=',',digits=digitsl,format="f")),
         adj=0,cex=fsize,col="black")

    #South East
    polygon(c(500000,500000),c(130000,50000),border=maincol)
    text(500000,20000,
         paste0(area2@data$NAME[6],"\n",pounds,formatC(area2@data$mapdata[6],big.mark=',',digits=digitsl,format="f")),
         adj=0,cex=fsize,col="black")

    #South West
    polygon(c(140000,290000),c(115000,115000),border=maincol)
    text(135000,115000,
         paste0(area2@data$NAME[7],"\n",pounds,formatC(area2@data$mapdata[7],big.mark=',',digits=digitsl,format="f")),
         adj=1,cex=fsize,col="black")

    #West Midlands
    polygon(c(140000,380000),c(310000,310000),border=maincol)
    text(135000,310000,
         paste0(area2@data$NAME[8],"\n",pounds,formatC(area2@data$mapdata[8],big.mark=',',digits=digitsl,format="f")),
         adj=1,cex=fsize,col="black")

    #Yorkshire & The Humber
    polygon(c(410000,670000),c(580000,580000),border=maincol)
    text(675000,580000,
         paste0(area2@data$NAME[4],"\n",pounds,formatC(area2@data$mapdata[4],big.mark=',',digits=digitsl,format="f")),
         adj=0,cex=fsize,col="black")

    if(england==FALSE && englandwales==FALSE){
      #Wales
      polygon(c(140000,290000),c(220000,220000),border=maincol)
      text(135000,220000,
           paste0(area2@data$NAME[12],"\n",pounds,formatC(area2@data$mapdata[12],big.mark=',',digits=digitsl,format="f")),
           adj=1,cex=fsize,col="black")

      #Scotland
      polygon(c(250000,670000),c(900000,900000),border=maincol)
      text(675000,900000,
           paste0(area2@data$NAME[11],"\n",pounds,formatC(area2@data$mapdata[11],big.mark=',',digits=digitsl,format="f")),
           adj=0,cex=fsize,col="black")

      #Northern Ireland
      polygon(c(100000,100000),c(500000,420000),border=maincol)
      text(95000,415000,
           paste0(area2@data$NAME[10],"\n",pounds,formatC(area2@data$mapdata[10],big.mark=',',digits=digitsl,format="f")),
           adj=0,cex=fsize,col="black")
    }

    if(englandwales==TRUE){
      #Wales
      polygon(c(140000,290000),c(220000,220000),border=maincol)
      text(135000,220000,
           paste0(area2@data[area2@data$NATION=='Wales',]$NAME,"\n",pounds,format(area2@data[area2@data$NATION=='Wales',]$mapdata,nsmall=1)),
           adj=1,cex=fsize,col="black")
    }
  }

  #if(Sys.info()['sysname']=="Windows"){
  #  savePlot(filename=outfile,type="eps",device=dev.cur(), restoreConsole=TRUE)
  #}
  dev.off()
  return(area2)
}
