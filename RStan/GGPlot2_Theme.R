
#levels(as.factor(HWS$Club))[1]

x1 = post$dev_high1 # stanfit object interface
x1

x30 = x1[,1:5]



x20 = x1[,1:20]

x24 = x1[,50:56]
#remove(x20)

m20 = melt(x20)
m21  = melt(x21)
m22  = melt(x22)
m23  = melt(x23)
m24  = melt(x24)

m30 = melt(x30)

colnames(m20)[2] = "club"
colnames(m21)[2] = "club"
colnames(m22)[2] = "club"
colnames(m23)[2] = "club"
colnames(m24)[2] = "club"

colnames(m30)[2] = "club"

epl = unique(HWS$Club)[1:20]
league1 = unique(HWS$Club)[21:28]
liga = unique(HWS$Club)[29:40]
bundesliga = unique(HWS$Club)[41:49]
seriea = unique(HWS$Club)[50:56]

leagues = unique(HWS$League)[1:5]
leagues

league1
epl
liga
m20$team = epl[m20$club]
m21$team = league1[m21$club]
m22$team = liga[m22$club]
m23$team = bundesliga[m23$club]
m24$team = seriea[m24$club]

m30$team = leagues[m30$club]
tail(m30)
#======================== facet wrap =========================================

league_plot(m21 = m30, num_col = 2)

league_plot = function(m21, num_col=2, label_x="parameter estimation at league level", hadjust=10){
  
  dhd = ddply(m21, .(team), summarise, int.low = hdi(value)[1], int.high = hdi(value)[2])
  plt1 = ggplot(data=m21, aes(x=value))+geom_density(col="#dddddd", fill="#888888") + facet_wrap(~ team, ncol=num_col) + theme_Posterior
  plt2 = plt1 + geom_segment(data=dhd, aes(x=int.low, xend=int.high, y=0.01*hadjust, yend=0.01*hadjust), size = 2, color="#ffffff") + facet_wrap(~team, ncol=num_col)
  plt3 = plt2 + geom_vline(aes(xintercept=0), linetype=2, col = "#bbbbbb") + facet_wrap(~team, ncol=num_col)                 
  return (plt3 + labs(x = label_x))
}
#=========================================================
 
 

#==========================
dd_plot(m20$value) + facet_wrap(.~ club)
#-------------------------------
p_plot = function(x1, j=1){
      
     return (dd_plot(x1[,j], label=unique(HWS$Club[HWS$xC==j])))
}

#=====================================================
dd_plot = function(x1, label="Delta"){

dat = with(density(x1), data.frame(x,y))
cent = dat$x[dat$y == max(dat$y)]

HDIInterval = hdi(density(x1), credMass = 0.95)
HDIInterval

Hdl = HDIInterval[1]
Hdu = HDIInterval[2]

return(
  
ggplot(data=dat, mapping=aes(x=x,y=y)) + theme_Posterior + 
  
  geom_line() +
  geom_area(mapping = aes(x=x, y = ifelse(x >= Hdl & x <= Hdu, y, 0)), alpha=0.2) +
  geom_vline(xintercept = cent, linetype=2)+
  geom_vline(xintercept = 0, linetype=4, size=1)+  
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(name = label,
                     #expand = c(0,0), # no expansion buffer 
                     breaks = seq(-1, 4, 1),
                     limits=c(min(Hdl - 0.5,0), Hdu + 0.5)
                     )
  )
}

 
 #==================Create Own Theme ======================  
theme_Posterior = theme(
  axis.line.x = element_line(arrow=arrow(length=unit(0.05, "cm")), lineend = "butt"),
  panel.background = element_rect(fill="transparent"), 
  panel.border = element_rect(fill="transparent"),
  plot.background = element_rect(fill = "transparent"),
  #panel.spacing.y = unit(1,"lines"),
  plot.margin = unit(c(1,1,1,1), "cm"), 
  #
  axis.title.y = element_blank(), 
  axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 1, vjust = 0, face = "plain"),
  axis.text.x = element_text(color="grey20",hjust=1, vjust=1, size=8, angle=50, face="plain"), 
  axis.ticks.y = element_blank()
)
#===================================Data and Poisson and Normal curves=======
MAG_Plot <- ggplot(data=HWS, aes(x = yG)) + 
  geom_histogram(
                  breaks=seq(-0.5, 10.5, by =1),   alpha = 0.2, aes(y=..density..) # control y scale
                ) + 
  stat_function(geom="bar" ,alpha=0.8, fun = dpois, args=list(lambda=median(HWS$yG)), xlim=c(0,10))+
  stat_function(fun = dnorm, args = list( mean=mean(HWS$yG, na.rm = TRUE), sd=sd(HWS$yG, na.rm=TRUE))) +

  scale_x_continuous(name = "Most Away Goals at the Season level",
                     breaks = seq(0, 10, 1),
                     limits=c(-0.5, 10)) + 
  
 labs(y="Density") + theme_bw()

MHG_Plot <- 
  ggplot(data=HWS, aes(x = yH)) + 
  geom_histogram(
    breaks=seq(-0.5, 10.5, by =1),   alpha = 0.2, aes(y=..density..) # control y scale
  ) + 
  stat_function(geom="bar" ,alpha=0.8, fun = dpois, args=list(lambda=median(HWS$yH)), xlim=c(0,10))+
  stat_function(fun = dnorm, args = list( mean=mean(HWS$yH, na.rm = TRUE), sd=sd(HWS$yH, na.rm=TRUE))) +
  
  scale_x_continuous(name = "Most Home Goals at the Season level",
                     breaks = seq(0, 10, 1),
                     limits=c(-0.5, 10)) + 
  
  labs(y="Density") + theme_bw()





     #theme_bw(plot.title = element_text(hjust = 0.3)) # center the title
#==============================================================
x1=seq(-3, 3, by=0.1)
x1
y1 = dnorm(x1, 1,1)
y1
df = data.frame(x1, y1)

ggplot(data = df, aes(x=x1, y=y1)) + 
  stat_function( geom = "bar", fun=dpois, args = list(lambda=1), xlim=c(-2,8)) + 
  stat_function( geom="line", fun=dnorm, args = list(1,1)) +
  #geom_text(x=5, y=0.2, label="Line: Normal Distribution", size=3) +  
  #geom_text(x=5, y=0.16, label="Bars: Poisson Distribution", size=3) +
  geom_area(aes(x = ifelse(x1<0, x1, 0)), alpha=0.3)+
  
  scale_x_continuous(name = "Count Value", breaks = seq(-3, 10, 1), limits=c(-3.5, 6)) + #labs(y="Density") + 
  scale_y_continuous(name = "Density / Frequency", breaks = seq(0, 0.4, 0.1), limits=c(0, 0.4)) + #labs(y="Density") +   
  theme_bw()


 


#=======================================multiplot ================================= 
#multiplot(p1, p2, p3, p4, p5, p6, p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20, cols=4)


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
