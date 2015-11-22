library(ggplot2)
library(ggmap)
library(reshape2)
library(plyr)
library(RColorBrewer)
options(scipen = 7)

data <- read.csv2('./data/problemos.csv',
                 sep=";",
                 stringsAsFactors=FALSE)
menTvarka <- c("Sau","Vas","Kov","Bal","Geg","Bir","Lie","Rgp","Rgs","Spa","Lap","Grd")

# kintamųjų konvertavimas
data$ID <- as.numeric(data$ID)
data$user_id <- as.factor(data$user_id)
data$metai <- as.Date(data$date)
data$menuo <- format(data$metai,"%b")
data$kada <- format(data$metai,"%Y")

# useriu skaiciaus analize
length(unique(data$user_id)) # is viso useriu
tapply(data$user_id,data$kada,function (x) {length(unique(x))}) # kiek kiekvienais metais unikalių
dataNoDupl <- data[!duplicated(data$user_id),] # isvalom duplicates
tapply(dataNoDupl$user_id,dataNoDupl$kada,length) # kiek kiekvienais metais visiskai nauju
dataRepet <- data[duplicated(data$user_id),] # pakartotinai įvedęs useris
tapply(dataRepet$user_id,dataRepet$kada,length) # kiek useriu įveda antrą kartą

ilgalaikiai <- NULL
res <- NULL
ankstesniUseriai <- factor()
for (metai in unique(data$kada)) {
  # unikalūs konkrečiais metais
  useriai <- unique(data[data$kada==metai,'user_id'])
  tikSiuMetu <- useriai[!(useriai %in% ankstesniUseriai)]
  # patikrinam, ar tų metų unikalūs jau yra kažką veikę anksčiau (ankstesniUseriai)
  res <- tapply(data$user_id,data$kada,function (x) {
    length(intersect(tikSiuMetu,unique(x)))
  })

  # ilgalaikiai <-rbind(ilgalaikiai,res)
  # sujungiam ankstesnius ir dabartinius naudotojus
  ankstesniUseriai <- factor(c(as.character(ankstesniUseriai),as.character(tikSiuMetu)))
  # print(length(ankstesniUseriai))
}

# kiek kiekvienas useris įvedė
resultUsers <- aggregate(data$ID,
                    by=list(data$user_id),
                    FUN=length,
                    simplify=TRUE)
resultUsers$share <- resultUsers$x / sum(resultUsers$x)
resultUsers[order(resultUsers$x),]

# kiek useriu suvede daugiau nei 5 bedas
sum(resultUsers$x>5)
# kiek problemu suvede dazni naudotojai - useriu su daugiau nei 5 bedom
sum(resultUsers$x[resultUsers$x > 5]) / sum(resultUsers$x) 

# kiek pagal dieną įvedė
resultDate <- aggregate(data$ID,
                    by=list(data$metai),
                    FUN=length,
                    simplify=TRUE)
resultDate$menuo <- as.factor(format(resultDate$Group.1,"%b"))
resultDate$metai <- format(resultDate$Group.1,"%Y")
resultDate$savdiena <- weekdays(resultDate$Group.1)
# sutvarkom eiliskuma
resultDate$menuo <- ordered(resultDate$menuo, menTvarka)
resultDate <- resultDate[order(resultDate$metai,resultDate$menuo),]
# apibendrinam
resultDateFinal <- dcast(resultDate, metai ~ menuo,sum,value.var='x')

## grafikasplot
resultDate$metai <- as.factor(resultDate$metai)
savTvarka <- c("Pirmadienis","Antradienis","Trečiadienis","Ketvirtadienis","Penktadienis","Šeštadienis","Sekmadienis")
resultDate$savdiena <- ordered(resultDate$savdiena, savTvarka)
resultDate <- resultDate[order(resultDate$metai,resultDate$menuo),]

grafikas <- ggplot(data=resultDate)
grafikas + aes(resultDate$savdiena,resultDate$x)+geom_bar(width = 0.5, stat='identity')+ facet_grid(metai ~ .)

## paruosimas weeksSinceLaunch
julian(data$metai[which(data$metai==min(data$metai[!is.na(data$metai)]))][1])
data$weeksSinceLaunch <- floor(julian(data$metai,origin=15370)/7)

## skaičiavimas weeksSinceLaunch
resultWeeks <- aggregate(data$ID,
                    by=list(data$weeksSinceLaunch),
                    FUN=length,
                    simplify=TRUE)

## problemos pagal tipa
data$problem_type <- as.factor(data$problem_type)
resultProblems <- aggregate(data$ID,
                         by=list(data$problem_type,data$kada),
                         FUN=length,
                         simplify=TRUE)
dcast(resultProblems,Group.1 ~ Group.2)

## problemu zemelapis
data$coordsX <- as.numeric(data$coordsX)
data$coordsY <- as.numeric(data$coordsY)

# reik ismest nerealias koordinates
data <- data[which(!is.na(data$coordsX)),]
dataClean <- data[data$coordsX > 100000 & data$coordsX < 999999, ]
dataClean <- dataClean[dataClean$coordsY > 3000000 & dataClean$coordsY < 9999999, ]
qplot(dataClean$coordsX,dataClean$coordsY)

write.csv(file="koordinates",dataClean[,c("coordsX","coordsY")],row.names=FALSE)
# http://www.sauliukas.lt/lks/
newCoords <- read.csv('koordinatesConverted',header=F)
dataClean$newX <- newCoords$V2
dataClean$newY <- newCoords$V1
qplot(dataClean$newX,dataClean$newY)

# nustatom maksimumus
xmax <- max(dataClean$newX)
xmin <- 25 #min(dataClean$newX)
ymax <- 54.9 #max(dataClean$newY)
ymin <- min(dataClean$newY)
lon <- c(xmin,xmax)
lat <- c(ymin,ymax)
df <- as.data.frame(cbind(lat,lon))

mapgilbert <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)),
                      zoom = 12, 
                      maptype = "satellite", 
                      scale = 1)
mapnew <- get_map(location = c(lon=25.27954,lat=54.74396),
                  zoom = 16,
                  maptype = 'satellite',
                  scale=1)

dataClean$problem_type <- as.factor(dataClean$problem_type)
dataClean$kada <- as.factor(dataClean$kada)

for (y in unique(dataClean$problem_type)) {
      # y <- unique(dataClean$problem_type)[5]
      tempData <- dataClean[dataClean$problem_type==y,]
#      problemMap <- ggplot(tempData,aes(tempData$newX,tempData$newY)) + stat_binhex()
      problemMap <- ggmap(mapgilbert)
      # problemMap <- problemMap + stat_density2d(data=tempData,
      #                                          aes(tempData$newX,tempData$newY,
      #                                              fill=..count..,
      #                                              alpha=..level..),
      #                                          geom='polygon',
      #                                          contour=TRUE,
      #                                          n=100)
      # problemMap <- problemMap + scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")))
      detailedMap <- ggmap(mapnew) + geom_point(data=tempData,
                                            aes(tempData$newX,tempData$newY),
                                            fill="red", shape=21, alpha=0.8)
       
      problemMap <- problemMap + stat_binhex(data=tempData,
                                             aes(tempData$newX,tempData$newY))
                                 scale_fill_gradient(colours=rev(brewer.pal(7, "Spectral")))
      problemMap <- problemMap + ggtitle(y)
      print(problemMap+facet_wrap(~ kada,nrow(2)))
}  

http://stackoverflow.com/questions/33432168/how-to-output-the-elements-inside-of-each-bins-in-ggplot-stat-binhex
http://ugrad.stat.ubc.ca/R/library/hexbin/html/hexpolygon.html
http://gis.stackexchange.com/questions/88830/overlay-a-spatial-polygon-with-a-grid-and-check-in-which-grid-element-specific-c

http://stackoverflow.com/questions/17801398/counting-species-occurrence-in-a-grid
http://gis.stackexchange.com/questions/48416/aggregating-points-to-grid-using-r/48434#48434
