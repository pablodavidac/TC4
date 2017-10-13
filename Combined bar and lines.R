require(plyr)
require(ggplot2)
require(scales)
require(grid)
require(gtable)


grid.newpage()
p1<-ggplot(data=oysbay, aes(x=Date, weight=Percent, colour=Pop, fill=Pop))+
  geom_bar(binwidth=10, position=position_dodge())+
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))+
  scale_colour_manual(values=c("blue","purple","orange"),labels=c("Dabob","Fidalgo","Oyster Bay"))+
  scale_fill_manual(values=c("blue","purple","orange"),labels=c("Dabob","Fidalgo","Oyster Bay"))+
  labs(x="Sample Date", y="Percent Brooding",title="Percent Brooding vs. Temperature")+
  theme_bw()+
  theme(legend.justification=c(0,1),
        legend.position=c(0,1),
        plot.title=element_text(size=30,vjust=1),
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20))
oysy1edit<-read.csv("TempData/oysY1fixed.csv")
oysy1edit$Date<-as.Date(oysy1edit$Date, "%m/%d/%Y")
oysmintemp<-ddply(oysy1edit,.(Date),summarise,min_temp=min(Temp,na.rm=T))
oysmintemprep<-subset(oysmintemp, Date>="2014-05-01"& Date<="2014-08-07")
p2<-ggplot()+geom_line(data=oysmintemprep,
                       aes(x=Date, y=min_temp), color="red")+
  geom_hline(yintercept=12.5, color="forestgreen")+
  theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_text(size=20,color="red"),
        axis.title.y=element_text(size=20))

g1<-ggplot_gtable(ggplot_build(p1))
g2<-ggplot_gtable(ggplot_build(p2))

pp<-c(subset(g1$layout,name=="panel",se=t:r))
g<-gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]],pp$t,pp$l,pp$b,pp$l)

ia<-which(g2$layout$name=="axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

grid.draw(g)






library(ggplot2)
library(gtable)
library(grid)

grid.newpage()

# two plots
p1 <- ggplot(mtcars, aes(mpg, disp)) + geom_line(colour = "blue") + theme_bw()
p2 <- ggplot(mtcars, aes(mpg, drat)) + geom_line(colour = "red") + theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)



#######################################################################################

Website <- rep(paste("Website",1:3),2)
Year <- c(rep(2013,3),rep(2014,3))
V1 <- c(10,20,50,20,30,70)
V2 <- c(5,15,30,15,30,45)
df <- data.frame(Website,V1,V2)

pg_mean["height"]<-c(1.9,1.2,0.8)

library(gcookbook)
ggplot(pg_mean,aes(x=group, y=weight))+
  geom_bar(stat="identity",fill="lightblue",colour="black")

p<-  ggplot(data=pg_mean,aes(x=group, y=weight))+
  geom_bar(stat="identity")+
  geom_line(data=pg_mean,aes(x=group, y=weight,group=1))
  
######################Grafico combinado lineas y puntos##########################################  
p<-  ggplot(data=pg_mean,aes(x=group, y=height,group=1))+
  geom_bar(data=pg_mean,aes(x=group, y=weight),stat="identity",colour="black", fill="lightblue",width=.7)+
  geom_text(aes(label=as.character(paste0(pg_mean$weight,": ",round(pg_mean$weight*100/sum(pg_mean$weight), digits = 2),"%"))   ),vjust=-6.5,position = position_dodge(0.3),size=4)+
  #scale_fill_brewer(palette = "Pastel1")+
  geom_line()+
  geom_point(size=4)+
  geom_text(aes(label=as.character(paste(pg_mean$height,": ",round(pg_mean$height*100/sum(pg_mean$height), digits = 2),"%"))   ),vjust=1.5)+
  theme_bw()+ theme(axis.title.x=element_blank())+
  ylab("Altura")+
  ggtitle("Edad")
p

###################################################################

p<-  ggplot(data=pg_mean,aes(x=group, y=height,group=1))+
  geom_bar(data=pg_mean,aes(x=group, y=weight),stat="identity",colour="black", fill="steelblue",width=.5)+
  geom_text(aes(label=as.character(paste0(pg_mean$weight,": ",round(pg_mean$weight*100/sum(pg_mean$weight), digits = 2),"%"))   ),vjust=-6.5,position = position_dodge(0.3),size=2.8,color="white")+
  #scale_fill_brewer(palette = "Pastel1")+
  geom_line()+
  geom_point(size=4)+
  geom_text(aes(label=as.character(paste(pg_mean$height,": ",round(pg_mean$height*100/sum(pg_mean$height), digits = 2),"%"))   ),vjust=1.5,color="white",size=2.8)+
  #theme(axis.title.x=element_blank())+
  ylab("Altura")+
  ggtitle("Edad")+
  theme_bw()
p

  




Grade3<-  c("A","B","B","A","B","C","C","D","A","B","C","C","C","D","B","B","D","C","C","D")
Grade6<-  c("A","A","A","B","B","B","B","B","C","C","A","C","C","C","D","D","D","D","D","D")
Cohort<-  table(Grade3, Grade6)
Cohort
library("xtable", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
x<-xtable(Cohort)
print.xtable(x)

res <- knit(prueba.Rnw, encoding = 'UTF-8')
Sweave2knitr('prueba.Rnw', encoding = 'UTF-8')


output <-array(rep(0,32),dim = c(4,8))

library(htmlTable)
htmlTable(output,
          header =  c("", "Buenos",
                            "Malos", "Distribucion", "Malos","","",""),
          rnames = paste(c("1st", "2nd",
                           "3rd", "4th"), "Categoria"),
          
          cgroup = c("Total", "Individuos", "Porcentaje", expression(chi^(2)),"F","D"),
          n.cgroup = c(1,2,2,1,1,1), 
          caption="Basic table with both column spanners (groups) and row groups",
          tfoot="&dagger; A table footer commment")

######################################################################

Base1<-structure(list(Month = c("m11", "m12", "m13", "m14", "m15", "m16"
), variable = structure(c(1L, 1L, 1L, 1L, 1L, 1L), .Label = "Power", class = "factor"), 
value = c(28101696.45, 28606983.44, 30304944, 32583849.36, 
          34791542.82, 40051050.24)), .Names = c("Month", "variable", 
                                                 "value"), row.names = c(NA, -6L), class = "data.frame")

Base2<-structure(list(Month = c("m11", "m12", "m13", "m14", "m15", "m16", 
                                "m11", "m12", "m13", "m14", "m15", "m16"), variable = structure(c(1L, 
                                                                                                  1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("Index1", 
                                                                                                                                                          "Index2"), class = "factor"), value = c(0.044370892419913, 0.0437161234641523, 
                                                                                                                                                                                                  0.0516857394621815, 0.0495793011485982, 0.0506456741259283, 0.0314653057147897, 
                                                                                                                                                                                                  0.0299405579124744, 0.0296145768664101, 0.0269727649059507, 0.0250663815369419, 
                                                                                                                                                                                                  0.0233469715385275, 0.0201801611981898)), .Names = c("Month", 
                                                                                                                                                                                                                                                       "variable", "value"), row.names = c(NA, -12L), class = "data.frame")

double_axis_graph <- function(graf1,graf2){
  graf1 <- graf1
  graf2 <- graf2
  gtable1 <- ggplot_gtable(ggplot_build(graf1))
  gtable2 <- ggplot_gtable(ggplot_build(graf2))
  par <- c(subset(gtable1[['layout']], name=='panel', select=t:r))
  graf <- gtable_add_grob(gtable1, gtable2[['grobs']][[which(gtable2[['layout']][['name']]=='panel')]],
                          par['t'],par['l'],par['b'],par['r'])
  ia <- which(gtable2[['layout']][['name']]=='axis-l')
  ga <- gtable2[['grobs']][[ia]]
  ax <- ga[['children']][[2]]
  ax[['widths']] <- rev(ax[['widths']])
  ax[['grobs']] <- rev(ax[['grobs']])
  ax[['grobs']][[1]][['x']] <- ax[['grobs']][[1]][['x']] - unit(1,'npc') + unit(0.15,'cm')
  graf <- gtable_add_cols(graf, gtable2[['widths']][gtable2[['layout']][ia, ][['l']]], length(graf[['widths']])-1)
  graf <- gtable_add_grob(graf, ax, par['t'], length(graf[['widths']])-1, par['b'])
  return(graf)}

library(ggplot2)
library(scales)
library(gtable)
#Graph 1
g1<-ggplot(Base1, aes(x = Month, y = value, fill = variable)) +
  geom_bar(stat="identity",colour="black",size=1) +
  scale_y_continuous(labels = comma,breaks=pretty_breaks(n=7),
                     limits=c(0,max(Base1$value,na.rm=T))) +
  theme(axis.text.x=element_text(angle=90,colour="grey20",face="bold",size=12),
        axis.text.y=element_text(colour="grey20",face="bold",hjust=1,vjust=0.8,size=15),
        axis.title.x=element_text(colour="grey20",face="bold",size=16),
        axis.title.y=element_text(colour="grey20",face="bold",size=16)) +
  xlab('Month')+ylab('')+ ggtitle("My graph") +
  theme(plot.title = element_text(lineheight=3, face="bold", color="black",size=24)) +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14)) +
  scale_fill_manual(name = "variable", 
                    label = "Power", 
                    values = "#0099FF")
                    #values = "#FF6C91")  
g1
#Graph2
colors=c("red","darkgreen")
g2<-ggplot(Base2, aes(x=Month, y=value, color=variable))+ 
  geom_line(aes(group=variable),size=1.3) +
  geom_point(size=3.8, shape=21, fill="white") + 
  scale_color_manual(values=colors)+ ggtitle("My graph")



g1.1 <- g1 + theme_bw() + theme(legend.position="bottom")
g2.1 <- g2 + theme_bw() + theme(panel.grid=element_blank()) +
  theme(panel.background = element_rect(fill = NA))
plot(double_axis_graph(g1.1,g2.1))

