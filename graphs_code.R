
#Starting tom -> get the HTML stuff and parse it and create some niceeee freakingggg graphs!!! with GGVIS or plotly. stop the rcharts biz.

# CHECKING IF THIS WORKS
#  
# install_github('rCharts','ramnathv')
# install.packages("stringi")
#install.packages("qdapRegex")

setwd("C:/Users/Sars/Desktop")
library(XML)
library(ggvis)
library(ggplot2)
library(devtools)
library(rCharts)
library(stringi)
library(stringr)
library(qdapRegex)
library(dplyr)



# install.packages("lubridate")
library(lubridate)

# Getting the file
doc.html=htmlParse('C:/Users/Sars/Desktop/8ex2.html')
newdoc<-capture.output(doc.html)

dates<-newdoc[35]
# n_yaxis<-newdoc[36]
graph_data<-newdoc[38]


# Xaxis
#xaxi<-gsub(" [[]","",dates)
# xaxi<-gsub("'","",xaxi)
# new<-strsplit(xaxi, ",")

dates2<-ex_between(dates, "categories: [", "]")
dates2<-gsub("'","",dates2)

dates_final<-strsplit(dates2, ",")


    
#dates_final



cats<-lapply(dates_final, function(x) paste0(gsub("/","-",x),"-01"))
cats2<-unlist(cats)
cats3<-as.Date(cats2)
minuteDataPlot2 <-  as.numeric(as.POSIXct(cats3))

minutes<-minuteDataPlot2*1000

date_final5<-minutes
# 
# 
# to_jsdate <- function(x){
#   as.numeric(as.POSIXct(as.Date(x), origin="1970-01-01")) * 1000
# }
# 
# 
# x <- to_jsdate(as.Date(cats2))
# 
# date_final5<-x

# 
# date_final3<-lapply(dates_final, function(x) dym(x))
# 
# date_final4<-(unlist(date_final3)*1000)




#Template
# nser<-sub(".*THEBEGINNINGWORD", "", strsplit(s, "ENDWORD")[[1]])
# Data
nseries2<-gsub("series: ","",graph_data)
nseries2<-gsub("]\r","",nseries2)

nser<-sub(".*'column',:]", "", strsplit(nseries2, "id:")[[1]])

#nser<-sub(".*id:", "", strsplit(nseries2, "}")[[1]])

#nser<-sub(".*data: ", "", strsplit(graph_data, "}")[[1]])

nser<-nser[-1]
 

names<- ex_between(nser, "name:", ",")
data<- ex_between(nser, "data: [", "]")
    names(data)<-unlist(id)
color<-ex_between(nser, "color: '", "',")
yaxis<-as.numeric(ex_between(nser, "yAxis: ", ","))
type<-ex_between(nser, "type: '", "'")
zIndex<-as.numeric(ex_between(nser, "zIndex: ", ","))
id<-ex_between(nser, "'" , "', z")

####
# names
# data
# color
# yaxis
# type
# zIndex
# id
####


dlist <- list(color, yaxis, type, zIndex)



new_dlist<-do.call(rbind, dlist)

colnames(new_dlist)  <- gsub("'","",unlist(id))

rownames(new_dlist)<- c("color","yaxis", "type", "zIndex")

##
# new_dlist

final_graph_param<-new_dlist
##

# data[1]
tmp <- sapply(data, as.character) 

unique(as.numeric(unlist(strsplit(gsub("[^0-9]", "", unlist(ll)), ""))))

s <- strsplit(as.character(data), ',')
  names(s)<-unlist(id)

output <- matrix(unlist(s), ncol = 4, byrow = TRUE)

u<-t(output)
colnames(u)  <- (unlist(id))

data_final<-u
data_final<-as.data.frame(data_final)

data_final$clu_ind <- ifelse(data_final$cluster == "null","Observed", "Cluster")

# unlist(t(dates_final))

data_final$dates <- unlist(t(dates_final))
data_final

data_final <- transform(data_final, obs_exp = as.numeric(levels(data_final$obs_exp))[data_final$obs_exp],
                        obs = as.numeric(levels(data_final$obs))[data_final$obs],
                        exp = as.numeric(levels(data_final$exp))[data_final$exp]
)

# data_final$dates33 <- date_final4
# 
# data_final$dates_UTC <- date_final5

data_final$dates_UTC2 <- date_final5

data_final$dates_UTC3 <- cats3


## when you come back : take the new data_final and create the "hplot ~" things since you have good vars now)

clusters<-data_final[!(data_final$clu_ind==-0),]

nonclusters<-data_final[(data_final$clu_ind==-0),]

###############################
###TEMPLATE##
# library(rCharts)
# h <- Highcharts$new()
# h$xAxis(categories = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
#                        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
# h$yAxis(list(list(title = list(text = 'Rainfall'))
#              , list(title = list(text = 'Temperature'), opposite = TRUE)
#              , list(title = list(text = 'Sea Pressure'), opposite = TRUE))
# )
# h$series(name = 'Rainfall', type = 'column', color = '#4572A7',
#          data = c(49.9, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5, 216.4, 194.1, 95.6, 54.4))
# h$series(name = 'Temperature', type = 'spline', color = '#89A54E',
#          data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6),
#          yAxis = 1)
# 
# h$series(name = 'Sea-Level Pressure', type = 'spline', color = '#AA4643',
#          data = c(1016, 1016, 1015.9, 1015.5, 1012.3, 1009.5, 1009.6, 1010.2, 1013.1, 1016.9, 1018.2, 1016.7),
#          yAxis = 2)
# h
#########################
#Chart Attempt #3 GREAT
############
library(rCharts)



# 
# h$xAxis(type = 'datetime',
#         startOnTick= F,
#         endOnTick= F
#         #,
#         # tickPixelInterval=800
#         
# )

h <- Highcharts$new()

h$series(name = 'Observed / Expected', type = unlist(final_graph_param[3,4]), color = unlist(final_graph_param[1,4]),
         data = data_final$obs_exp,
         yAxis = unlist(final_graph_param[2,4])
         #,
         # xAxis=data_final$dates_UTC2
         
         )


h<-hPlot(obs ~ dates_UTC2, data = data_final, type = 'column', group = 'clu_ind', name="Number of Cases")

h$colors('rgba(119, 152, 191, .7)', 'rgba(223, 83, 83, .7)')

h$xAxis(type = 'datetime', labels = list(
  format = '{value:%Y-%m}'
))

h$exporting(filename='cluster_graph')
#h$plotOptions("column: { grouping: false }")
h$title(text='Detected Cluster', align='center')
h$tooltip(shared=T, crosshairs=T) 
h$legend(backgroundColor='#F5F5F5', verticalAlign='bottom')
h$plotOptions(colorByPoint=T)
h$yAxis(list(list(title = list(text = 'Number of Cases'))
             , list(title = list(text = 'Observed / Expected'), opposite = TRUE)
))


h

##############################################################


# h$series(name = 'Number of Cases', type = unlist(final_graph_param[3,2]), color = unlist(final_graph_param[1,2]),
#          data = data_final$obs, group = data_final$clu_ind)
# 
# h$series(name = 'Observed / Expected', type = unlist(final_graph_param[3,4]), color = unlist(final_graph_param[1,4]),
#          data = data_final$obs_exp
#          # yAxis = unlist(final_graph_param[2,4])
#          
# )
# 
# h$series(name = 'Expected', type = unlist(final_graph_param[3,3]), color = unlist(final_graph_param[1,3]),
#          data = data_final$exp
#          # yAxis = unlist(final_graph_param[2,3])
#          
# )
# 
# h


h$yAxis(list(list(title = list(text = 'Number of Cases'))
             , list(title = list(text = 'Observed / Expected'), opposite = TRUE)
))

h


h$params$series[[1]]<-hPlot(obs_exp ~ dates_UTC2, data = data_final, type = 'line')

h$params

#### NV.D3



# p2a <- nPlot(obs ~ dates_UTC2, data = data_final, type = 'discreteBarChart')
# p2a
# 
# p2a$chart(color = c('blue','red'))
# p2a$addFilters("clu_ind")
# p2a$set(dom = 'chart2', width = 600)
# p2a

######
# 

# h$plotOptions(
#   pointWidth=0,
#   pointPadding=0,
#   groupPadding=0
# )
# # h$plotOptions(pointPlacement="between")
# h$global("useUTC:true")








########################

class(date_final3)

minuteDataPlot2 <- transform(minuteDataPlot,
                             timestamp2 = as.numeric(as.POSIXct(timestamp))*1000)



minTickInterval: moment.duration(1, 'month').asMiliseconds()

h$plotOptions(column="grouping=F")

h
# h$params$series
str(h)

h$save('d3chart.html', cdn=TRUE)

############
##Chart attempt 3 = trying to do subseting.
###########
h <- Highcharts$new()


#h$xAxis(categories = data_final$dates)

h$xAxis(type = 'datetime', labels = list(
  format = '{value:%Y-%m}'  
))


h$yAxis(list(list(title = list(text = 'Number of Cases'))
             , list(title = list(text = 'Observed / Expected'), opposite = TRUE)
))

# str(h)

h$series(name = 'Number of Cases', type = unlist(final_graph_param[3,2]), color = unlist(final_graph_param[1,2]),
         data = nonclusters$obs)

h$colors('rgba(223, 83, 83, .5)', 'rgba(119, 152, 191, .5)', 'rgba(60, 179, 113, .5)')

h$series(name = 'Observed / Expected', type = unlist(final_graph_param[3,4]), color = unlist(final_graph_param[1,4]),
         data = data_final$obs_exp,
         yAxis = unlist(final_graph_param[2,4]))

h$series(name = 'Expected', type = unlist(final_graph_param[3,3]), color = unlist(final_graph_param[1,3]),
         data = data_final$exp,
         yAxis = unlist(final_graph_param[2,3]))

h

h$series(name = 'Cluster', type = unlist(final_graph_param[3,1]), color = unlist(final_graph_param[1,1]),
         data = as.numeric(levels(clusters$cluster))[clusters$cluster],
         yAxis = unlist(final_graph_param[2,1]))

h$exporting(filename='cluster_graph')
#h$plotOptions("column: { grouping: false }")
h$title(text='Detected Cluster', align='center')
h$tooltip(shared=T, crosshairs=T) 
h$legend(backgroundColor='#F5F5F5', verticalAlign='bottom')
h$plotOptions(colorByPoint=T)
h


##########
#END
####
############
##Chart attempt 2 = good!
###########
h <- Highcharts$new()


h$xAxis(categories = data_final$dates)

h$xAxis(type = 'datetime', labels = list(
  format = '{value:%Y-%m}'  
))


h$yAxis(list(list(title = list(text = 'Number of Cases'))
             , list(title = list(text = 'Observed / Expected'), opposite = TRUE)
             ))
        
# str(h)

h$series(name = 'Number of Cases', type = unlist(final_graph_param[3,2]), color = unlist(final_graph_param[1,2]),
         data = data_final$obs)

h$colors('rgba(223, 83, 83, .5)', 'rgba(119, 152, 191, .5)', 'rgba(60, 179, 113, .5)')

h$series(name = 'Observed / Expected', type = unlist(final_graph_param[3,4]), color = unlist(final_graph_param[1,4]),
         data = data_final$obs_exp,
         yAxis = unlist(final_graph_param[2,4]))

h$series(name = 'Expected', type = unlist(final_graph_param[3,3]), color = unlist(final_graph_param[1,3]),
         data = data_final$exp,
         yAxis = unlist(final_graph_param[2,3]))

h
# 
# h$series(name = 'Cluster', type = unlist(final_graph_param[3,1]), color = unlist(final_graph_param[1,1]),
#          data = as.numeric(levels(clusters$cluster))[clusters$cluster],
#          yAxis = unlist(final_graph_param[2,1]))

h$exporting(filename='cluster_graph')
#h$plotOptions("column: { grouping: false }")
h$title(text='Detected Cluster', align='center')
h$tooltip(shared=T, crosshairs=T) 
h$legend(backgroundColor='#F5F5F5', verticalAlign='bottom')
h$plotOptions(colorByPoint=T)
h

#################
# END
####################

for (i in c(1:length(a$params$series))) {
  a$params$series[[i]]$color <- "#4c4c4c"
  if (a$params$series[[i]]$name == "Female") { a$params$series[[i]]$color <- "#61C250" }
  if (a$params$series[[i]]$name == "Male") { a$params$series[[i]]$color <- "#4B92DB" }
}
a




class(data_final$obs)

h$params$series[[1]]




h <- hPlot(Pulse ~ Height, data = MASS::survey, type = 'scatter', group = 'Sex', radius = 6, group.na = "Not Available")
h$colors('rgba(223, 83, 83, .5)', 'rgba(119, 152, 191, .5)', 'rgba(60, 179, 113, .5)')
h$legend(align = 'right', verticalAlign = 'top', layout = 'vertical')
h$plotOptions(scatter = list(marker = list(symbol = 'circle')))
h$tooltip(formatter = "#! function() { return this.x + ', ' + this.y; } !#")
h

for (i in c(1:length(h$params$series))) {
#  h$params$series[[i]]$color <- "#4c4c4c"
  if (h$params$series[[i]]$name == "Female") { h$params$series[[i]]$color <- "#61C250" }
  #if (h$params$series[[i]]$name == "Male") { h$params$series[[i]]$color <- "#4B92DB" }
}
h
# 
# begindate<-17787
# 
# begin<-gsub('.{2}$', '01', strftime(as.Date(begindate, origin="1970-01-1"),"%Y/%m/%d"))
# 
# 
# begin<-gsub('.{2}$', '01', strftime(as.Date(begindate, origin="1970-01-1"),"%Y/%m/%d"))
# 
# 
# # DATES #
#   lapply(dates_final2, function(x)  strptime(paste(1, x,"%d %m-%Y")))

## se iff you can create a vector from 2006-09 to 2006-12##

  # strptime(paste(1, c("2006-09"),"%d %M-%y"))
  # paste(1, c("2006-09"))
  # 
  # dates_final2<-c("2006-09","2006-10","2006-11","2006-12")
  # 
  # c<-"2006-09"
  # 
  # d <- as.Date(c, format="%M-%Y")
  # d
  # 
  # as.Date(paste('01', c), format='%d %m-%Y')
  # 



##
  
# 1) first transform into the "38830" then do the POSIXct thing
  
  dat = transform(df, Date2 = as.numeric(as.POSIXct(cat)*1000))
  
  h1 &lt;- hPlot(Numbers ~ Date2, data = dat, 
                 group = 'Type', 
             
                 radius=6
  )
  
  h1$xAxis(type = 'datetime', labels = list(
    format = '{value:%Y-%m-%d}'  
  ))
  
###
  
h$params$series

# mass<-plyr::count(MASS::survey, c('Sex', 'Exer'))
# 
# class(mass$freq)
# 
# class(data_final$obs)
# 
# 
# anna.table2 <- data.matrix(data_final)
# 
# anna.table2 <- data.frame(anna.table2)
# 
# class(anna.table2$dates)
# d<-data_final





hPlot(freq ~ Exer, data = plyr::count(MASS::survey, c('Sex', 'Exer')), type = c('column', 'line'), group = 'Sex', radius = 6)

#h$navigation(" buttonOptions: { align: 'right' }")

# 1) add a new variable to "data_final" called "Cluster_indicator"
# 2) THen. set it to "1" for clusters, and "0" for non clusters
# 3) subset data_Final into "non-clusters" and a dataframe of "clusters" only
# 4) then you can have "Expected" series to be just the non-clusters,
# and the "clusters" to be the cluster DF


# h

# hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
# 
# n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, "discreteBarChart"
#             )
# n1$print("chart3")
# n1

h <- rCharts:::Highcharts$new()
h$series(list(
  list(data = rCharts::toJSONArray2(df1, json = FALSE, names = FALSE), 
       name = "Big Reds",
       color = '#FF0000',
       lineWidth = 4,
       marker = list(
         fillColor = '#FFA500', 
         radius = 10)
  ),
  list(data = rCharts::toJSONArray2(df2, json = FALSE, names = FALSE), 
       name = "Small Blues", 
       color = '#0000FF',
       lineWidth = 2,
       marker = list(
         fillColor = '#ADD8E6',
         radius = 6)
  )))
h$xAxis(type = 'datetime')
h$chart(type = "scatter")
h$chart(zoomType = "x")
h

#############
?lapply
aaa
class(dates_final)
class(cat)

strsplit(cat,",")

str(dates_final)

doc.html = htmlTreeParse('8ex2.html', useInternalNodes = T)
doc.html
class(doc.html)


#
#   h1 <- hPlot(x = "Wr.Hnd", y = "NW.Hnd", data = MASS::survey, type = c("line",
#                                                                       "bubble", "scatter"), group = "Clap", size = "Age")
# h1$print("chart5")
# str(h1)
#
# rChart<- rCharts$new()
#
# rChart<-hPlot()
# str(rChart)
#
# readLines(system.file('rChart.html', package = 'rCharts'))
#
# rChart$setLib('libraries/widgets/d3_horizon')
# rChart$setTemplate(script = "libraries/widgets/d3_horizon/layouts/d3_horizon.html")
#
# b <- unlist(strsplit(doc.html,"</span>"))
#
# stri_split_regex(doc.html, "</span>|</p>")
# allTables <- readHTMLTable('8ex2.html')
#
# allTables
# length(allTables)
#
# h1$show('inline')
#
# h1$save('mychart2.html', standalone = TRUE)
#
#
# require(reshape2)
# uspexp <- melt(USPersonalExpenditure)
# names(uspexp)[1:2] = c("category", "year")
# x1 <- xPlot(value ~ year, group = "category", data = uspexp, type = "line-dotted")
# x1$print("chart4")
#
#
dmap <- rCharts::Highcharts$new()
str(dmap)
# #
# # dmap$setTemplate(afterScript=('
# #
# #                               <body style="margin:0;background-color: #fff;">
# #                               <table width="100%" border="0" cellpadding="0" cellspacing="0" bgcolor="#F8FAFA" style="border-bottom: 3px double navy;">
# #                               <tbody><tr>
# #                               <td width="120" align="center" bgcolor="#DBD7DB"><img src="http://www.satscan.org/images/swe2.jpg" alt="&Ouml;stersund map" title="׳tersund map" width="120" height="115" hspace="1" border="0"></td>
# #                               <td align="right" bgcolor="#D4DCE5"><img src="http://www.satscan.org/images/satscan_title2.jpg" alt="SaTScan&#0153; - Software for the spatial, temporal, and space-time scan statistics" title="SaTScan&#0153; - Software for the spatial, temporal, and space-time scan statistics" width="470" height="115"></td>
# #                               <td width="25%" bgcolor="#F8FAFA" align="right"><img src="http://www.satscan.org/images/nyc2.jpg" alt="New York City map" title="New York City map" width="112" height="115" hspace="1" border="0" align="middle"></td>
# #                               </tr></tbody></table>
# #                               <div style="margin:20px;" class="chart-section">
# #                               <div id="chart_1_1" class="highchart-container" style="margin-top:0px;"></div>
# #                               <div class="options">
# #                               <div class="show-chart-options"><a href="#">Show Chart Options</a></div>
# #                               <div class="chart-options">
# #                               <div class="options-table">
# #                               <h4>Chart Options</h4>
# #                               <div class="options-row">
# #                               <label for="title_obs">Title</label>
# #                               <div><input type="text" style="width:95%;" class="title-setter" id="title_obs">
# #                               <p class="help-block">The graph title can be updated simply by editing this text.</p>
# #                               </div>
# #                               </div>
# #                               <div class="options-row">
# #                               <label>Observed Chart Type</label>
# #                               <div>
# #                               <label>
# #                               <input type="radio" name="chart_1_1_obs_series_type" series-type="column" series-id="obs,cluster" checked=checked/>Histogram
# #                               </label>
# #                               <label>
# #                               <input type="radio" name="chart_1_1_obs_series_type" series-type="line" series-id="obs,cluster"/>Line
# #                               </label>
# #                               <p class="help-block">Switch the series type between line and histogram.</p>
# #                               </div>
# #                               </div>
# #                               <div class="options-row">
# #                               <label>Cluster Band</label>
# #                               <div>
# #                               <label>
# #                               <input type="checkbox" name="chart_1_1_cluster_band" start-idx="0" end-idx="3"/>Show Cluster Band
# #                               </label>
# #                               <p class="help-block">Band stretching across the plot area marking cluster interval.</p>
# #                               </div>
# #                               </div>
# #                               <div class="options-row">To zoom a portion of the chart, select and drag mouse within the chart.</div>
# #                               </div>
# #                               <div class="hide-chart-options"><a href="#">Close Chart Options</a></div>
# #                               </div>
# #                               </div>
# #                               </div> '))
# #
# #
# #
# #
# #
# #
dmap <- rCharts::Highcharts$new()
str(dmap)

dmap$setTemplate(chartDiv= "<div id='chart_1_1' class='highchart-container' style='margin-top:0px;'></div>  "   )
dmap$setTemplate(script ="
    (function($){
        $(function () {

 <script type='text/javascript'>
             var charts = {}; 
             $(document).ready(function () { 
                 
                 var chart_1_1 = new Highcharts.Chart({ 
                 chart: { renderTo: 'chart_1_1', zoomType:'x', resetZoomButton: {relativeTo: 'chart', position: {x: -80, y: 10}, theme: {fill: 'white',stroke: 'silver',r: 0,states: {hover: {fill: '#41739D', style: { color: 'white' } } } } }, marginBottom: 110, borderColor: '#888888', plotBackgroundColor: '#e6e7e3', borderRadius: 0, borderWidth: 1, marginRight: 80 }, 
                 title: { text: 'Detected Cluster', align: 'center' }, 
                 exporting: {filename: 'cluster_graph'}, 
                 plotOptions: { column: { grouping: false }}, 
                 tooltip: { crosshairs: true, shared: true, formatter: function(){var is_cluster = false;var has_observed = false;$.each(this.points, function(i, point) {if (point.series.options.id == 'cluster') {is_cluster = true;}if (point.series.options.id == 'obs') {has_observed = true;}});var s = '<b>'+ this.x +'</b>'; if (is_cluster) {s+= '<br/><b>Cluster Point</b>';}$.each(this.points,function(i, point){if (point.series.options.id == 'cluster'){if (!has_observed) {s += '<br/>Observed: '+ point.y;}} else {s += '<br/>'+ point.series.name +': '+ point.y;}});return s;}, }, 
                 legend: { backgroundColor: '#F5F5F5', verticalAlign: 'top', y: 40 }, 
                 xAxis: [{ categories: ['2006/9','2006/10','2006/11','2006/12','2007/1','2007/2','2007/3','2007/4','2007/5','2007/6','2007/7','2007/8','2007/9','2007/10','2007/11','2007/12','2008/1','2008/2','2008/3','2008/4','2008/5','2008/6','2008/7','2008/8','2008/9','2008/10','2008/11','2008/12','2009/1','2009/2','2009/3','2009/4','2009/5','2009/6','2009/7','2009/8','2009/9','2009/10','2009/11','2009/12'], tickmarkPlacement: 'on', labels: { step: 1, rotation: -45, align: 'right' } }], 
                 yAxis: [{ title: { enabled: true, text: 'Number of Cases', style: { fontWeight: 'normal' } }, min: 0 }, { title: { enabled: true, text: 'Observed / Expected', style: { fontWeight: 'normal' } }, min: 0, opposite: true }], 
                 navigation: { buttonOptions: { align: 'right' } }, 
                 series: [{ id: 'cluster', zIndex: 2, type: 'column', name: 'Cluster', color: '#AA4643', yAxis: 0, marker: { enabled: true, symbol: 'circle', radius: 0 }, data: [3,4,7,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null] },{ id: 'obs', zIndex: 1, type: 'column', name: 'Observed', color: '#4572A7', yAxis: 0, marker: { enabled: true, symbol: 'square', radius: 0 }, data: [3,4,7,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] },{ id: 'exp', zIndex: 3, type: 'line', name: 'Expected', color: '#89A54E', yAxis: 0, marker: { enabled: true, symbol: 'triangle', radius: 0 }, data: [0.23,0.28,0.33,0.41,0.42,0.38,0.42,0.40,0.42,0.40,0.42,0.42,0.40,0.42,0.40,0.42,0.42,0.39,0.42,0.40,0.42,0.40,0.42,0.42,0.40,0.42,0.40,0.42,0.42,0.38,0.42,0.40,0.42,0.40,0.42,0.42,0.40,0.42,0.40,0.42] },{ id: 'obs_exp', zIndex: 2, type: 'line', name: 'Observed / Expected', color: '#00FF00', yAxis: 1, marker: { enabled: true, symbol: 'triangle', radius: 0 }, data: [12.84,14.47,21.10,4.89,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] }]
                 }); 
                 charts['chart_1_1'] = chart_1_1;
                 
                 
                 $('.chart-section').each(function() { $(this).find('.title-setter').val(charts[$(this).find('.highchart-container').first().attr('id')].title.textStr); }); 
                 $('.title-setter').keyup(function(){ charts[$(this).parents('.chart-section').find('.highchart-container').first().attr('id')].setTitle({text: $( this ).val()}); }); 
                 $('.show-chart-options a').click(function(event) { event.preventDefault(); $(this).parents('.options').find('.chart-options').show().end().find('.show-chart-options').hide(); }); 
                 $('.hide-chart-options a').click(function(event) { event.preventDefault(); $(this).parents('.options').find('.chart-options').hide().end().find('.show-chart-options').show(); }); 
                 $('.options-row input[type='radio']').click(function(event) { 
                 var series_type = $(this).attr('series-type'); 
                 var chart = charts[$(this).parents('.chart-section').find('.highchart-container').first().attr('id')]; 
                 $.each($(this).attr('series-id').split(','), function(index, value) { chart.get(value).update({type:series_type}, true); }); 
                 }); 
                 $('.options-row input[type='checkbox']').click(function(event) { 
                 var chart = charts[$(this).parents('.chart-section').find('.highchart-container').first().attr('id')]; 
                 if ($(this).is(':checked')) { 
                 chart.xAxis[0].addPlotBand({color: '#FFB3B3', from: $(this).attr('start-idx'), to: $(this).attr('end-idx'), id: 'band', zindex:0}); 
                 chart.xAxis[0].addPlotLine({id:'start',color: '#FF0000', width: 1, value: $(this).attr('start-idx'), zIndex: 5 }); 
                 chart.xAxis[0].addPlotLine({id:'end',color: '#FF0000', width: 1, value: $(this).attr('end-idx'), zIndex: 5 }); 
                 } else { 
                 chart.xAxis[0].removePlotBand('band' ); 
                 chart.xAxis[0].removePlotLine('start'); 
                 chart.xAxis[0].removePlotLine('end'); 
                 } 
                 }); 
                 });   
        });
                 })(jQuery);
                 </script> ")
      
dmap



dmap$save('dmap.html', standalone = TRUE)

###########








mtcars %>% ggvis(~wt, ~mpg) %>% layer_points()
mtcars %>% 
  ggvis(~wt, ~mpg) %>% 
  layer_points(size := 25, shape := "diamond", stroke := "red", fill := NA)


faithful %>% ggvis(~eruptions, fill := "#fff8dc") %>%
  layer_histograms(width = 0.25)



x <- data.frame(USPersonalExpenditure)
colnames(x) <- substr(colnames(x), 2, 5)

# Create chart
a <- rCharts:::Highcharts$new()
a$chart(type = "column")
a$title(text = "US Personal Expenditure")
a$xAxis(categories = rownames(x))
a$yAxis(title = list(text = "Billions of dollars"))
a$data(x)

# Print chart
a$printChart()

#######################
## PLOTLY ATTEMPT
###########################
install.packages("plotly")
library(shiny)
library(plotly)


data_final


ggplot(data_final) +
  geom_bar(aes(x = dates, weight = obs)) +
  geom_line(aes(x = as.numeric(obs_exp), y = obs_exp))


library(plotly)
p <- plot_ly(
  x = data_final$dates_UTC3,
  y = data_final$obs,
  type = "bar",
  color=data_final$cluster
)
p



today <- Sys.Date()
tm <- seq(0, 600, by = 10)
x <- today - tm

add_trace(p, y=data_final$obs_exp,  mode = "lines")


# 
# p <- plot_ly(economics,
#              type = "scatter",       # all "scatter" attributes: https://plot.ly/r/reference/#scatter
#              x = date,               # more about scatter's "x": /r/reference/#scatter-x
#              y = uempmed,            # more about scatter's "y": /r/reference/#scatter-y
#              name = "unemployment",  # more about scatter's "name": /r/reference/#scatter-name
#              marker = list(          
#                color="rgb(16, 32, 77)"     # more about marker's "color" attribute: /r/reference/#scatter-marker-color
#              ))
# 
# p <- add_trace(p,                       # by default, traces are type "scatter"
#                y = fitted((loess(uempmed ~ as.numeric(date)))),  # scatter's "y": /r/reference/#scatter-y
#                line = list(                        # line is a named list, valid keys: /r/reference/#scatter-line
#                  color = "rgb(60, 60, 60)",      # line's "color": /r/reference/#scatter-line-color
#                  dash = "dashed"                 # line's "dash" property: /r/reference/#scatter-line-dash
#                )
# )

library(plotly)


########
# So good, weep
########

ay <- list(
  tickfont = list(color = "black"),
  overlaying = "y",
  side = "right"
)

plot_ly(x = data_final$dates_UTC3, y = data_final$obs, 
        name = "Observed", type="bar", color=data_final$cluster) %>%
  add_trace(x = data_final$dates_UTC3, y = data_final$obs_exp,
            name = "Observed / Expected Ratio", yaxis = "y2", type="scatter") %>%
  layout(title = "Double Y Axis", yaxis2 = ay) %>%
  add_trace(x = data_final$dates_UTC3, y = data_final$exp,
            name = "Expected", type="scatter", color = 'rgb(184, 134, 11)')

#########
#END
##########

library(plotly)

ay <- list(
tickfont = list(color = "black"),
overlaying = "y",
side = "right"
)

x <- list(
  title = "",
  titlefont = f
)
y <- list(
  title = "Number of Cases",
  titlefont = f
)
  
p<-plot_ly(x = data_final$dates_UTC3, y = data_final$obs, 
        name = "Observed", type="bar", color=data_final$clu_ind, marker=list(line=list(color="#E2E2E2")),
        colors = c("dodgerblue3","maroon")) %>%
  add_trace(x = data_final$dates_UTC3, y = data_final$obs_exp,
            name = "Observed / Expected Ratio", yaxis = "y2", type="scatter",
            marker = list( 
              color="rgb(0, 255, 127)")) %>%
  layout(title = "", yaxis2 = ay) %>%
  add_trace(x = data_final$dates_UTC3, y = data_final$exp,
            name = "Expected Cases", type="scatter",marker = list( 
              color="rgb(16, 32, 77)" ))%>%
layout(xaxis = x, yaxis = y)

f <- list(
  family = "sans-serif",
  size = 12,
  color = "#000"
)
l <- list(
  font = f,
  # bgcolor = "#E2E2E2",
  bordercolor = "#FFFF",
  borderwidth = 1,
  title="Legend",
  x = 0, y = 1.3
)

p <-layout(list(font=list(family="arial")), legend = l,  plot_bgcolor="#E2E2E2")

p

