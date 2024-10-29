##Winter Limno manuscript: map of sites####
## Created 22Oct2024 by David Richardson (DCR), richardsond@newpaltz.edu



#Google API for DCR####
register_google(key="AIzaSyBrzFH7a_1-pidkvPZtonRuiyZSOpuj3OE") #ask DCR for key
has_google_key()

##DCR working directory

#Get average of all locations
Site_locations<-tibble(Lake=c("Mohonk"),lat=c(41.766),long=c(-74.158))

#Read in Mohonk Perimeter####
MohonkPerimeter<-read_csv("data/MohonkLakePerimeter-LatLong.csv")%>%
    separate(WKT,sep=" ",c("A","B","C"))%>% #separates the first column by space
    rename(long=B,lat=C)%>%
    mutate(lat=as.numeric(substr(lat,1,nchar(lat)-1)),
           long=as.numeric(substr(long,2,nchar(long))))

#Site summary
Site_summary<-Site_locations%>%summarize(mean_lat=mean(lat),mean_long=mean(long))
#Function to make circles around a radius
make_circles <- function(centers, radius, nPoints = 100){
  # centers: the data frame of centers with ID
  # radius: radius measured in kilometer
  #
  meanLat <- mean(centers$latitude)
  # length per longitude changes with lattitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius / 111
  circleDF <- data.frame(ID = rep(centers$ID, each = nPoints))
  angle <- seq(0,2*pi,length.out = nPoints)
  
  circleDF$lon <- unlist(lapply(centers$longitude, function(x) x + radiusLon * cos(angle)))
  circleDF$lat <- unlist(lapply(centers$latitude, function(x) x + radiusLat * sin(angle)))
  return(circleDF)
}


# Plot map

# ggmap(map, extent= "device") +
#   geom_rect(data=sb[[1]], aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=z), inherit.aes=F,
#             show.legend = F,  color = "black", fill = sb[[1]]$fill.col) +
#   geom_text(data=sb[[2]], aes(x=xlab, y=ylab, label=text), inherit.aes=F, show.legend = F) 

#install.packages('ggmap')

#Read in state data
state<-map_data('state')  
head(state)

lakes<-map_data('lakes')
head(lakes)

world<-map_data('world')
head(world)

#Can look at data with maps
usa<-map_data('usa')
head(usa)

#Plot the NE states as the base map
base.map<-ggplot()+
  geom_polygon(data=usa,aes(x=long,y=lat,group=group),fill='lightgrey',color='black')+
  
  geom_polygon(data=state[state$region=="new york",],aes(x=long,y=lat,group=group),fill='darkgrey',color='black')+
  #geom_polygon(data=usa,aes(x=long,y=lat,group=group),fill='white',color='black')+
  #geom_polygon(data=lakes[lakes$subregion=="Erie"|lakes$subregion=="Ontario",],aes(x=long,y=lat,group=group),fill='light blue',color='black')+
  geom_point(data=Site_locations, aes(x=long,y=lat),shape=21,size=1.2,color="black",fill="yellow")+ #This is the center of all sites
  #geom_point(aes(x=-74.235885,y=41.725852),shape=21,size=1.4,color="black",fill="yellow")+ #this is the lake sites
  #xlim(min(state[state$region=="new york","long"])-0.2,max(state[state$region=="massachusetts","long"]))+
  #ylim(min(state[state$region=="new york","lat"]),max(state[state$region=="new hampshire","lat"]))+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border= element_rect(colour = "black", fill=NA, linewidth=1),
        panel.background=element_blank(),
        plot.margin=margin(0,0,0,0,"cm"),
        strip.background = element_rect(colour=NA, fill=NA),
        axis.ticks.length = unit(0, "pt")
  )+ 
  labs(x=NULL, y=NULL)

base.map

#Inset
#use style to format the google map, e.g., turn off all labels, roads, parks
style1<-c(feature = "all", element = "labels", visibility = "off")
style2<-c("&style=",feature = "road", element = "all", visibility = "off")
style3<-c("&style=",feature = "poi.park", element = "all", visibility = "off")
#style9<-c("&style=",feature = "water", element = "all", visibility = "on")
style4<-c("&style=",feature = "water", element = "geometry.fill", color = "blue")
style5<-c("&style=",feature = "water", element = "geometry.stroke", color = "red",weight="100")
style6<-c("&style=",feature = "landscape", element = "geometry", color="0xc5f2d6")
style7<-c("&style=",feature = "poi", element = "geometry", color="0xc5f2d6")
style8<-c("&style=",feature = "administrative", element = "geometry", color="green")
style9<-c("&style=",feature = "water", element = "geometry.stroke", weight = "20")
style10<-c("&style=",feature = "all", element = "labels.text", visibility = "off")
#Merge styles together for get_googlemap function
style<-list(style1,style2,style3,style4,style5,style6,style7,style8,style9,style10)
#Pull google map with the correct style
skyLakes = get_googlemap(center = c(lon = Site_locations$long[Site_locations$Lake=="Mohonk"], lat = Site_locations$lat[Site_locations$Lake=="Mohonk"]), zoom = 15,
                         style =style)
# ggmap(skyLakes)

# x<-get_googlemap(center = c(lon = Site_summary$mean_long*1.0006, lat = Site_summary$mean_lat*0.9998), zoom = 13,
#                  style =c(feature = "water", element = "geometry.stroke", visibility = "on",weight=100,color="red"))
# ggmap(x)
#Get bounding box for the map and reformat to dataframe for scalebar
bb <- attr(skyLakes, "bb")
bb2 <- data.frame(long = unlist(bb[c(2, 4)]), lat = unlist(bb[c(1,3)]))

#Centers of the ponds
# Site_locations

#Graph the map plus the scale bar
SkyLakes_map<-ggmap(skyLakes)+
  #geom_label_repel(data=Site_locations,aes(x=long,y=lat,label=Lake),min.segment.length = unit(10, 'lines'),force=1,seed=12,nudge_y=0.01,nudge_x=-0.015,arrow=TRUE)+
  #geom_point(data=Site_locations,aes(x=lat,y=lat),size=1.8,shape=21,color="black",fill="yellow")+
  geom_path(data=MohonkPerimeter,aes(y=lat,x=long),color="darkblue")+
  coord_sf(crs = 4326)+
  scale_y_continuous(breaks=c(41.76,41.77),labels=~as.expression(c(bquote(41.76*degree*N),bquote(41.77*degree*N))),limits=c(41.758,41.772))+
  scale_x_continuous(breaks=c(-74.17,-74.16,-74.15),labels=~as.expression(c(bquote(-74.17*degree*W),bquote(-74.16*degree*W),bquote(-74.15*degree*W))),limits=c(-74.17,-74.147))+
  theme(
    #axis.title.x=element_blank(),
    #axis.title.y=element_blank(),
    panel.border= element_rect(colour = "black", fill=NA, size=1),
    axis.text.y=element_text(angle=90,hjust=0.5)
  )+
  ylab("Latitude")+
  xlab("Longitude")+
  annotation_scale(location = "br", width_hint = 0.5)+
  ggspatial::annotation_north_arrow(location = "br", 
                                    pad_x = unit(0.05, "in"), pad_y = unit(0.2, "in"),
                                    style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"),line_col = "grey20"))

SkyLakes_map
#Test export the map
# ggsave("figures/MohonkWinterLimno_TestMap.jpg",SkyLakes_map,height=4,width=4,dpi=500,units=("in"))



#Export as png with the NYS as inset####
jpeg(file="figures/MohonkWinterLimno_MapWithInset.jpg",w=4,h=3.4,units="in",res=500)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.25*1.7, height = 0.25, x = 0.33, y = 0.84,just=c("center")) #plot area for the inset map
print(SkyLakes_map,vp=v1) 
print(base.map,vp=v2)
dev.off()


#Create ggplot object with inset using cowplot####
gg.composite.map<-ggdraw()+
  draw_plot(SkyLakes_map)+
  draw_plot(base.map,width=0.22*1.5,height=0.22,x=0.17,y=0.535)
# x=0.12,y=0.735
# save(gg.composite.map, file = "output/ggobject.compositeMap.rdata")

#Merge map with Ice Phenology figure####
#Load the gg.MohonkIceTrends object (it gets stored as gg.MohonkIceTrends)
#Can start here so you don't have to recreate the map
# load("output/gg.MohonkIceTrends.rdata")
# load("output/ggobject.compositeMap.rdata")
  #print(gg.MohonkIceTrends)



# #Combine the plots and make the top one a little bigger####
# temp<-plot_grid(gg.composite.map,NULL,gg.MohonkIceTrends,align="v",ncol=1,rel_heights=c(3.4/6.4,-0.02,3.0/6.4),labels=c("a","","b"))
# #Map: 4" wide x 3.4" tall to minimize margins and maintain aspect
# #The ice figure has to be 4x2.4 to maintain aspect ratio (down from 5"x3")
# ggsave(temp,file="figures/MS/Fig1.MapANDIcePhenology_withDates_intermittant.jpg",  width = 4,
#        height = 3.4+3.0,
#        units = "in",
#        dpi = 600)
