library(readr)
library(circular)
options(rgl.useNULL = TRUE)
library(Directional)
library(CircStats)
library(movMF)
library(dplyr)
library(ggplot2)
Earth_quack = read_csv("Significant Earthquake Dataset 1900-2023.csv")
Volcano = read_csv("Erupt_data.csv")
Volcano = data.frame(Volcano)
Earth_quack = data.frame(Earth_quack)
head(Earth_quack)
dim(Earth_quack)
Earthquake = Earth_quack[-c(1:1382),]
head(Earthquake)
  
#View(Earthquake)
head(Volcano)
tail(Volcano)
dim(Volcano)

#View(Volcano)

library(sf)
library(dplyr)

Volcano$Longitude = Volcano$longitude
Volcano$Latitude = Volcano$latitude

watson.test(Volcano$latitude, alpha = 0.01 , dist = "vonmises")
watson.test(Volcano$longitude , alpha = 0.01 , dist = "vonmises")
watson.test(Earthquake$Latitude , alpha = 0.01 , dist = "vonmises")
watson.test(Earthquake$Longitude , alpha = 0.01 , dist = "vonmises")

library(CircStats)
watson.two(Volcano$Latitude , Volcano$Longitude , alpha = 0.01)

head(Earth_quack)
head(Volcano)
dim(Earthquake)
dim(Volcano)
head(Volcano)
head(Earthquake)

Volcano = Volcano[c(1 , 2 , 7:9 , 11:15)]
head(Volcano)
Volcano$start_time <- as.POSIXct(paste(Volcano$start_year, Volcano$start_month,
                                       Volcano$start_day, sep = "-"), 
                             format = "%Y-%m-%d")
Volcano$end_time <- as.POSIXct(paste(Volcano$end_year, Volcano$end_month,
                                       Volcano$end_day, sep = "-"), 
                                 format = "%Y-%m-%d")
head(Volcano)
Vol = Volcano[,-c(3:8 )]
head(Vol)

Eq = Earthquake[c(1:6)]

datetime_string <- Eq$Time

# Convert the datetime string to a POSIXlt object
datetime <- strptime(datetime_string, format = "%Y-%m-%d %H:%M:%S")

# Extract date and time components
Eq$date <- as.Date(datetime)
Eq$time <- format(datetime, format = "%H:%M:%S")
Eq = Eq[-c(1)]
dim(Eq)
summary(Eq)
head(Eq)
dim(Vol)
summary(Vol)

library(lubridate)

Vol$start_time <- as.Date(Vol$start_time)

# Extract the year from the Date column
Vol$Year <- year(Vol$start_time)

Vol_count = c()
for (i in 1:121) {
  desired_year <- i+1899
  events_count <- sum(Vol$Year == desired_year)
  Vol_count[[i]] = data.frame(desired_year, events_count)
  Vol_c[i,] = rbind(Vol_count[[i]])
}

plot(density(Vol_c$events_count))

library(geosphere)

# Define the primary point (latitude and longitude)

dataset_list = c()

for (i in 1:1193) {
  
  primary_lat <- Vol[i,]$latitude
  primary_lon <- Vol[i,]$longitude
  
  # Sample dataset (replace this with your own data)
  data <- Eq
  
  # Calculate distances between each point and the primary point
  distances <- distVincentySphere(
    p1 = c(primary_lon, primary_lat),
    p2 = data[, c("Longitude", "Latitude")]
  )
  # Create a new dataset with points within 10 kilometers of the primary point
  threshold_distance_km <- 100
  filtered_data <- data[distances < threshold_distance_km * 1000, ]
  #filtered_data[[i]] <- data[distances < threshold_distance_km * 1000, ]
  # Give each dataset a unique name based on the loop index
  #dataset_name <- paste("filtered_data", i, sep = "_")
  #assign(dataset_name, filtered_data)
  dataset_list[[i]] <- filtered_data
}

result = rbindlist(dataset_list)
r <- unique(result)
dim(r)
summary(r)

r$Date = as.Date(r$date)
r$Year <- year(r$Date)

Eq_count = c()
for (i in 1:116) {
  desired_year <- i+1904
  events_count <- sum(r$Date == desired_year)
  Eq_count[[i]] = data.frame(desired_year, events_count)
  Eq_c[i,] = rbind(Eq_count[[i]])
}

dim(Eq_c)
dim(Vol_c)
Vol_c
Eq_c

plot( density(Eq_c$events_count) , main = "Density plot for Earthquake")
plot(density(Vol_c$events_count), main = "Density plot for Volcano")

hist((Eq_c$events_count), breaks = 10 , main = "Histogram plot for Earthquake")
plot(hist(Vol_c$events_count), main = "Density plot for Volcano")

#############################Time series 

library(tseries)
quake = ts(Eq_c$events_count , start = 1905 , end = 2020)
erup = ts(Vol_c$events_count , start = 1900, end = 2020)
plot(quake)
plot(erup)

Eq_c_t = Eq_c[1:114,]
Eq_c_t
quake_ts = ts(Eq_c_t$events_count , start = 1905 , end = 2018)
Vol_c_t = Vol_c[1:119,]
Vol_c_t
erup_ts = ts(Vol_c_t$events_count , start = 1900, end = 2018)

library(forecast)
arima_model1 <- auto.arima(quake_ts)
forecast_values_1 <- forecast(arima_model1, h = 2)
plot(forecast_values_1)
lines(Eq_c , col = "blue")

arima_model2 <- auto.arima(erup_ts)
forecast_values_2 <- forecast(arima_model2, h =2)
plot(forecast_values_2, col = "red")
lines(Vol_c , col = "blue")

final_list = c()

for (i in 1:1193) {
  primary_data <- data.frame(primary_date = as.Date(Vol[i,]$start_time))
  #primary_data
  # Sample dataset with other parameters (replace with your actual dataset)
  dataset <- dataset_list[[i]]
  
  # Find the nearest date in the dataset for each date in primary_data
  nearest_rows <- lapply( primary_data$primary_date, function(x) {
    #closest_row <- dataset[which.min(abs(as.Date(dataset$date) - x)), ]
    #closest_row <- dataset[which.min(abs(as.Date(dataset$date) - x) <= 365), ]
    dataset$diff_days <- abs(dataset$date - x)
    filtered_data <- dataset[abs(dataset$date - x) <= 365, ]
    # Sort the filtered data by date difference in ascending order
    sorted_data <- filtered_data[order(filtered_data$diff_days), ]
    return(sorted_data[1,])
  })
  
  # Create a new dataframe with the nearest rows
  nearest_data <- do.call(rbind, nearest_rows)
  dataset_name <- paste("final_data", i, sep = "_")
  assign(dataset_name, nearest_data)
  final_list[[i]] <- nearest_data
}



final_list[[1]]

head(Vol)
final_list[[2]]
final_eq = do.call(rbind , final_list)
head(final_eq)
dim(final_eq)
fin_eq = na.omit(final_eq)
dim(fin_eq)

head(fin_eq)
dim(final_eq)

com_vol_eq = data.frame(EQ_place = final_eq$Place, EQ_lat = final_eq$Latitude,
                        EQ_long = final_eq$Longitude , EQ_Depth = final_eq$Depth, 
                        EQ_mag = final_eq$Mag , EQ_date = final_eq$date , EQ_time = final_eq$time,
                        EQ_diff_with_erup_day = final_eq$diff_days, Vol_name = Vol$volcano_name,
                        Vol_lat = Vol$latitude , Vol_long = Vol$longitude , 
                        Vol_strt_date = Vol$start_time, Vol_end_date = Vol$end_time
                        )

head(com_vol_eq)
Combined_final_data = na.omit(com_vol_eq)
head(Combined_final_data)

dim(Combined_final_data)

Combined_final_data$distances <- (distVincentySphere(
  p1 = Combined_final_data[, c("EQ_long", "EQ_lat")],
  p2 = Combined_final_data[, c("Vol_long", "Vol_lat")])/1000
)

head(Combined_final_data)
dim(Combined_final_data)


watson.test(Combined_final_data$EQ_long , alpha = 0.01 , dist = "vonmises")
watson.test(Combined_final_data$EQ_lat , alpha = 0.01 , dist = "vonmises")
watson.two.test(Combined_final_data$EQ_lat , Combined_final_data$EQ_long , alpha = 0.01)

watson.test(Combined_final_data$Vol_lat , alpha = 0.01, dist = "vonmises")
watson.test(Combined_final_data$Vol_long , alpha = 0.01, dist = "vonmises")
watson.two.test(Combined_final_data$Vol_lat , Combined_final_data$Vol_long , alpha = 0.01)

#Combined_final_data$Temp = 30 + 25*(Combined_final_data$EQ_Depth - 3)
#head(Combined_final_data)

date2 = as.Date(Combined_final_data$Vol_strt_date)
date1 = as.Date(Combined_final_data$Vol_end_date)
Combined_final_data$Num_of_days_Vol_erupted =  as.numeric(date1 - date2)

Combined_final_data$Number_of_days_between_EQ_and_Vol = 
  as.numeric(as.Date(Combined_final_data$Vol_strt_date) - as.Date(Combined_final_data$EQ_date))

head(Combined_final_data)
summary(Combined_final_data$Number_of_days_between_EQ_and_Vol)

sum(Combined_final_data$Number_of_days_between_EQ_and_Vol >= 0)
sum(Combined_final_data$Number_of_days_between_EQ_and_Vol < 0)

C_d1<- subset(Combined_final_data, Combined_final_data$Number_of_days_between_EQ_and_Vol > 0)
C_d2 <- subset(Combined_final_data, Combined_final_data$Number_of_days_between_EQ_and_Vol <= 0)

dim(C_d1)
dim(C_d2)

C_d11 = C_d1[1:98,]
C_d12 = C_d1[99:185,]
C_d13 = C_d1[186:215,]

Com1 = C_d11[order(-C_d11$EQ_lat), ]
watson.test(Com1$EQ_lat , alpha = 0.01 , dist = "vonmises")
watson.test(Com1$EQ_long , alpha = 0.01 , dist = "vonmises")
watson.test(Com1$Vol_lat , alpha = 0.01 , dist = "vonmises")
watson.test(Com1$Vol_long, alpha = 0.01 , dist = "vonmises")


Com2 = C_d12[order(-C_d12$EQ_lat), ]
watson.test(Com2$EQ_lat , alpha = 0.01 , dist = "vonmises")
watson.test(Com2$EQ_long , alpha = 0.01 , dist = "vonmises")
watson.test(Com2$Vol_lat , alpha = 0.01 , dist = "vonmises")
watson.test(Com2$Vol_long, alpha = 0.01 , dist = "vonmises")


Com3 = C_d13[order(-C_d13$EQ_lat), ]
watson.test(Com3$EQ_lat , alpha = 0.01 , dist = "vonmises")
watson.test(Com3$EQ_long , alpha = 0.01 , dist = "vonmises")
watson.test(Com3$Vol_lat , alpha = 0.01 , dist = "vonmises")
watson.test(Com3$Vol_long, alpha = 0.01 , dist = "vonmises")


C_d21 = C_d2[1:74,]
C_d22 = C_d2[75:181,]
C_d23 = C_d2[182:248,]
C_d24 = C_d2[249:263,]


Com4 = C_d21[order(-C_d21$EQ_lat), ]
watson.test(Com4$EQ_lat , alpha = 0.01 , dist = "vonmises")
watson.test(Com4$EQ_long , alpha = 0.01 , dist = "vonmises")
watson.test(Com4$Vol_lat , alpha = 0.01 , dist = "vonmises")
watson.test(Com4$Vol_long, alpha = 0.01 , dist = "vonmises")


Com5 = C_d22[order(-C_d22$EQ_lat), ]
watson.test(Com5$EQ_lat , alpha = 0.01 , dist = "vonmises")
watson.test(Com5$EQ_long , alpha = 0.01 , dist = "vonmises")
watson.test(Com5$Vol_lat , alpha = 0.01 , dist = "vonmises")
watson.test(Com5$Vol_long, alpha = 0.01 , dist = "vonmises")


Com6 = C_d23[order(-C_d23$EQ_lat), ]
watson.test(Com6$EQ_lat , alpha = 0.01 , dist = "vonmises")
watson.test(Com6$EQ_long , alpha = 0.01 , dist = "vonmises")
watson.test(Com6$Vol_lat , alpha = 0.01 , dist = "vonmises")
watson.test(Com6$Vol_long, alpha = 0.01 , dist = "vonmises")

Com7 = C_d23[order(-C_d24$EQ_lat), ]
watson.test(Com7$EQ_lat , alpha = 0.01 , dist = "vonmises")
watson.test(Com7$EQ_long , alpha = 0.01 , dist = "vonmises")
watson.test(Com7$Vol_lat , alpha = 0.01 , dist = "vonmises")
watson.test(Com7$Vol_long, alpha = 0.01 , dist = "vonmises")

summary(Com7)


ss = cbind(Combined_final_data$EQ_lat , Combined_final_data$EQ_long)

#max(Combined_final_data$Temp)

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates,map = world_coordinates,
    aes(long, lat , map_id = region),
    color = "black" , fill = "#FFF8DC"
  ) +
  geom_point(
    data = Combined_final_data, 
      aes(Combined_final_data$Vol_long , Combined_final_data$Vol_lat , color = "black",
        size = Combined_final_data$Num_of_days_Vol_erupted, ) , col = "red", shape = 24,
    alpha = 0.7
  ) +
  theme(legend.position = "top")

  
  #geom_point(
   # data = Combined_final_data, 
  #  aes(Combined_final_data$Vol_long , Combined_final_data$Vol_lat , color = "blue"),
   # alpha = 1
  #) 
ggplot() +
  geom_map(
    data = world_coordinates,map = world_coordinates,
    aes(long, lat , map_id = region),
    color = "black" , fill = "#FFF8DC"
  ) +
  geom_point(
    data = Combined_final_data, 
    aes(Combined_final_data$EQ_long , Combined_final_data$EQ_lat , color = "black",
        size = Combined_final_data$EQ_mag, ) , col = "red",
    alpha = 0.4
  ) +
  theme(legend.position = "top")


head(Combined_final_data)

s_eq = cbind(Combined_final_data$EQ_lat , Combined_final_data$EQ_long)
ss_eq = cbind(Combined_final_data$Vol_lat , Combined_final_data$Vol_long)

spher.cor(euclid(s_eq) , euclid(ss_eq))

x= Combined_final_data$EQ_lat
mu = mean.circular(Combined_final_data$EQ_lat)
kappaa = est.kappa(Combined_final_data$EQ_lat)
y <- rvonmises(n=1000, mu, kappaa)
resx <- density(x, bw=25)
resy <- density.circular(y, bw=25)
pp=plot(density.circular(Combined_final_data$EQ_lat , bw = 30) ,points.plot=F, pty = 10, lty=1, lwd=2, col="blue",xlim=c(-1.2,1), ylim=c(-1.1, 1.2), main="Comparison of  estimated sample density\n with fitted Von Mises\n for Earth Quack Latitude data", cex.main=0.25)
lines(resy, points.plot=F, col="red", points.col=2,lwd=2, lty=4, plot.info=pp)

legend("topleft", legend=c("estimated \n kernel density", "von mises density"),
       col=c("blue", "red"), lty=c(1,4),lwd=c(2,2), cex=0.59,
       box.lty=0)

x= Combined_final_data$EQ_long
mu=mean.circular(Combined_final_data$EQ_long)
kappaa=est.kappa(Combined_final_data$EQ_long)
y <- rvonmises(n=1000, mu, kappaa)
resx <- density(x, bw=25)
resy <- density.circular(y, bw=25)
pp=plot(density.circular(Combined_final_data$EQ_long , bw = 30) ,points.plot=F, pty = 10, lty=1, lwd=2, col="blue",xlim=c(-1.2,1), ylim=c(-1.1, 1.2), main="Comparison of  estimated sample density\n with fitted Von Mises\n for Earth Quack Longitude data", cex.main=0.25)
lines(resy, points.plot=F, col="red", points.col=2,lwd=2, lty=4, plot.info=pp)

legend("topleft", legend=c("estimated \n kernel density", "von mises density"),
       col=c("blue", "red"), lty=c(1,4),lwd=c(2,2), cex=0.59,
       box.lty=0)

x= Combined_final_data$Vol_lat
mu = mean.circular(Combined_final_data$Vol_lat)
kappaa = est.kappa(Combined_final_data$Vol_lat)
y <- rvonmises(n=1000, mu, kappaa)
resx <- density(x, bw=25)
resy <- density.circular(y, bw=25)
pp=plot(density.circular(Combined_final_data$Vol_lat , bw = 30) ,points.plot=F, pty = 10, lty=1, lwd=2, col="blue",xlim=c(-1.2,1), ylim=c(-1.1, 1.2), main="Comparison of  estimated sample density\n with fitted Von Mises\n for Volcano Latitude data", cex.main=0.25)
lines(resy, points.plot=F, col="red", points.col=2,lwd=2, lty=4, plot.info=pp)

legend("topleft", legend=c("estimated \n kernel density", "von mises density"),
       col=c("blue", "red"), lty=c(1,4),lwd=c(2,2), cex=0.59,
       box.lty=0)

x= Combined_final_data$Vol_long
mu=mean.circular(Combined_final_data$Vol_long)
kappaa=est.kappa(Combined_final_data$Vol_long)
y <- rvonmises(n=1000, mu, kappaa)
resx <- density(x, bw=25)
resy <- density.circular(y, bw=25)
pp=plot(density.circular(Combined_final_data$Vol_long , bw = 30) ,points.plot=F, pty = 10, lty=1, lwd=2, col="blue",xlim=c(-1.2,1), ylim=c(-1.1, 1.2), main="Comparison of  estimated sample density\n with fitted Von Mises\n for Volcano Longitude data", cex.main=0.25)
lines(resy, points.plot=F, col="red", points.col=2,lwd=2, lty=4, plot.info=pp)

legend("topleft", legend=c("estimated \n kernel density", "von mises density"),
       col=c("blue", "red"), lty=c(1,4),lwd=c(2,2), cex=0.59,
       box.lty=0)

#s = cbind(Earth_quack$Latitude , Earth_quack$Longitude)
#svo = cbind(Volcano$latitude , Volcano$longitude)
set.seed(2023)

rows_with_zeros <- apply(ss_eq, 1, function(row) any(row == 0))
sum(rows_with_zeros)
dim(svo)
vol <- svo[!rows_with_zeros, ]
dim(vol)
# Print the resulting matrix
print(vol)

library(movMF)

# Volcano
EvMFs <- 
  function(K){
    movMF(ss_eq, k = K, control= list(nruns = 20))
  }


#Eq
VvMFs <- 
  function(K){
    movMF(s_eq, k = K, control= list(nruns = 20))
  }
set.seed(123)

#EQ
Vold = lapply(1:12 , VvMFs)
vd = sapply(Vold , BIC)
vd
Vold[[11]]$alpha


set.seed(12345)

#VOL
Esd = lapply(1:12, EvMFs)
gt = sapply(Esd, BIC)
gt

Esd[[10]]$alpha

earthquake_data_partitioned <- Earth_quack %>%
  mutate(partition = case_when(
    Longitude >= 0 ~ "first_half", 
    Longitude >= -180 & Longitude < 0 ~ "Second_half"
  ))


head(earthquake_data_partitioned)

first_half_data <- earthquake_data_partitioned %>%
  filter(partition == "first_half")

second_half_data <- earthquake_data_partitioned %>%
  filter(partition == "Second_half")

first_half_data = data.frame(first_half_data)
head(first_half_data)
dim(first_half_data)

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates,map = world_coordinates,
    aes(long, lat , map_id = region),
    color = "black" , fill = "lightblue"
  ) +
  geom_point(
    data = first_half_data , 
    aes(first_half_data$Longitude , first_half_data$Latitude, color = "red",),
    alpha = 1
  ) +
  theme(legend.position = "top")

dim(second_half_data)


vmf_density_grid = 
  function(u, ngrid = 100) {
    # Translate to (0,180) and (0,360)
    u[,1] <- u[,1] + 90
    u[,2] <- u[,2] + 180
    res <- vmf.kerncontour(u, thumb = "none", den.ret = T, full = T,
                           ngrid = ngrid)
    
    # Translate back to (-90, 90) and (-180, 180) and create a grid of
    # coordinates
    ret <- expand.grid(Lat = res$lat - 90, Long = res$long - 180)
    ret$Density <- c(res$den)
    ret
  }

library(Directional)
options(rgl.useNULL = TRUE) 
vol.dens = vmf_density_grid(ss_eq, ngrid = 300)
eq.dens = vmf_density_grid(s_eq , ngrid = 300)

world <- map_data("world")
v.am_landing <- ggplot() +
  geom_map(data = world, map = world,
           mapping = aes(map_id = region),
           color = "black", fill = "white") +
  geom_point(data = Combined_final_data,
             mapping = aes(x = Combined_final_data$EQ_long, y = Combined_final_data$EQ_lat),
             color = "red", alpha = .5, size = 1, stroke = 0.1) +
  geom_density_2d(data = Combined_final_data,
                  aes(x = Combined_final_data$EQ_long, y = Combined_final_data$EQ_lat),
                  color = "#b266ff", alpha = 2) +
  geom_contour(data = eq.dens, aes(x=Long, y=Lat, z=Density),
               color = "blue") +
  coord_map("mercator")

v.am_landing

v.3d_landing <- ggplot() +
  geom_map(data = world, map = world,
           mapping = aes(map_id = region),
           color = "black", fill = "white") +
  geom_point(data = Combined_final_data,
             mapping = aes(x = Combined_final_data$EQ_long, y = Combined_final_data$EQ_lat),
             color = "red", alpha = .5, size = 1, stroke = 0.1) +
  geom_density_2d(data = Combined_final_data,
                  aes(x = Combined_final_data$EQ_long, y = Combined_final_data$EQ_lat),
                  color = "#b266ff", alpha = 2) +
  geom_contour(data = eq.dens, aes(x=Long, y=Lat, z=Density),
               color = "blue")  +
coord_map("orthographic", orientation = c(0, 110, 0)) +
  scale_x_continuous(breaks = seq(-180, 180, 20)) +
  scale_y_continuous(breaks = seq(-90, 90, 45)) +
  ggtitle("Orthographic Projection of Spherical Density", "Top / Front View") +
  xlab("") +
  ylab("") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.ontop = TRUE,
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid = element_line(color = "black" ),
        panel.background = element_rect(fill = NA))

v.3d_landing


v.am_landing <- ggplot() +
  geom_map(data = world, map = world,
           mapping = aes(map_id = region),
           color = "black", fill = "white") +
  geom_point(data = Combined_final_data,
             mapping = aes(x = Combined_final_data$Vol_long, y = Combined_final_data$Vol_lat),
             color = "red", alpha = .5, size = 1, stroke = 0.1) +
  geom_density_2d(data = Combined_final_data,
                  aes(x = Combined_final_data$Vol_long, y = Combined_final_data$Vol_lat),
                  color = "#b266ff", alpha = 2) +
  geom_contour(data = vol.dens, aes(x=Long, y=Lat, z=Density),
               color = "blue")  +
  coord_map("mercator")

v.am_landing

v.3d_landing <- ggplot() +
  geom_map(data = world, map = world,
           mapping = aes(map_id = region),
           color = "black", fill = "white") +
  geom_point(data = Combined_final_data,
             mapping = aes(x = Combined_final_data$Vol_long, y = Combined_final_data$Vol_lat),
             color = "red", alpha = .5, size = 1, stroke = 0.1) +
  geom_density_2d(data = Combined_final_data,
                  aes(x = Combined_final_data$Vol_long, y = Combined_final_data$Vol_lat),
                  color = "#b266ff", alpha = 2) +
  geom_contour(data = vol.dens, aes(x=Long, y=Lat, z=Density),
               color = "blue")  +
  coord_map("orthographic", orientation = c(20, 110, 0)) +
  scale_x_continuous(breaks = seq(-180, 180, 20)) +
  scale_y_continuous(breaks = seq(-90, 90, 45)) +
  ggtitle("Orthographic Projection of Spherical Density", "Top / Front View") +
  xlab("") +
  ylab("") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.ontop = TRUE,
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid = element_line(color = "black" ),
        panel.background = element_rect(fill = NA))

v.3d_landing



x1 = euclid(s_eq)
fishkent(x1)
x2 = euclid(ss_eq)
fishkent(x2)













pp = function (x,col,col1, ref.line = TRUE )
{
  n <- length(x)
  x <- sort(x%%(2 * pi))
  z <- c(1:n)/(n + 1)
  mu <- circ.mean(x)%%(2 * pi)
  kappa <- est.kappa(x)
  y <- c(1:n)
  for (i in 1:n) {
    y[i] <- pvm(x[i], mu, kappa)
  }
  plot(z, y, xlab = "Von Mises Distribution", ylab = "Empirical Distribution" , col = col , lwd = 0.35)
  if (ref.line) 
    abline(0, 1 , col = col1 , lwd = 3.0)
  data.frame(mu, kappa)
}

ppunif = function (x, ref.line = TRUE, frac = NULL, xlab = "Uniform Distribution", 
                   ylab = "Empirical Distribution", col = NULL, col.inf = NULL, 
                   col.sup = NULL, col1,...) 
{
  x <- na.omit(x)
  if (length(x) == 0) {
    warning("No observations (at least after removing missing values)")
    return(NULL)
  }
  x <- conversion.circular(x, units = "radians", zero = 0, 
                           rotation = "counter", modulo = "2pi")
  attr(x, "class") <- attr(x, "circularp") <- NULL
  y <- sort(x%%(2 * pi))/(2 * pi)
  n <- length(y)
  z <- (1:n)/(n + 1)
  if (is.null(col)) 
    col <- rep(1, n)
  else col <- rep(col, length.out = n)
  if (!is.null(frac)) {
    if (!is.numeric(frac) || (frac < 0 | frac > 1)) {
      stop("'frac' must be in the interval [0,1]")
    }
    f <- round(frac * n)
    if (f) {
      zm <- -1 + ((n - f + 1):n)/(n + 1)
      zp <- 1 + (1:f)/(n + 1)
      ym <- -1 + y[(n - f + 1):n]
      yp <- 1 + y[1:f]
      y <- c(ym, y, yp)
      z <- c(zm, z, zp)
      if (is.null(col.inf)) 
        col.inf <- rep(2, f)
      else col.inf <- rep(col.inf, length.out = f)
      if (is.null(col.sup)) 
        col.sup <- rep(2, f)
      else col.sup <- rep(col.sup, length.out = f)
      col <- c(col.inf, col, col.sup)
    }
  }
  plot.default(z, y, xlab = xlab, ylab = ylab, col = col, ...)
  if (ref.line) {
    abline(0, 1 , col = col1, lwd = 3.0)
    if (!is.null(frac)) {
      abline(h = c(0, 1), lty = 3)
      abline(v = c(0, 1), lty = 3)
    }
  }
}

pp(Volcano$Latitude , col = "red" , col1 = "blue", ref.line = TRUE)
pp(Volcano$Longitude , col = "red" , col1 = "blue", ref.line = TRUE)

ppunif(Volcano$Latitude , col = "red" , col1 = "blue")
ppunif(Volcano$Longitude , col = "red" , col1 = "blue")

pp(Earth_quack$Latitude , col = "red" , col1 = "blue" , ref.line = TRUE)
pp(Earth_quack$Longitude , col = "red" , col1 = "blue" , ref.line = TRUE)
ppunif(Earth_quack$Latitude , col = "red" , col1 = "blue")
ppunif(Earth_quack$Longitude , col = "red" , col1 = "blue")

earthquake_data_partitioned_2 <- first_half_data %>%
  mutate(partition = case_when(
    Latitude >= 0 ~ "first_quarter", 
    Latitude >= -90 & Latitude < 0 ~ "Second_quarter"
  ))


first_quarter_data <- earthquake_data_partitioned_2 %>%
  filter(partition == "first_quarter")

second_quarter_data <- earthquake_data_partitioned_2 %>%
  filter(partition == "Second_quarter")

dim(second_quarter_data)
dim(first_quarter_data)

earthquake_data_partitioned_3 <- first_quarter_data %>%
  mutate(partition = case_when(
    Longitude >= 90 ~ "first_q", 
    Longitude >= 0 & Latitude < 90 ~ "Second_q"
  ))

first_q_data <- earthquake_data_partitioned_3 %>%
  filter(partition == "first_q")

second_q_data <- earthquake_data_partitioned_3 %>%
  filter(partition == "Second_q")

ggplot() +
  geom_map(
    data = world_coordinates,map = world_coordinates,
    aes(long, lat , map_id = region),
    color = "black" , fill = "lightblue"
  ) +
  geom_point(
    data = second_q_data , 
    aes(second_q_data$Longitude , second_q_data$Latitude, color = "red",),
    alpha = 1
  ) +
  theme(legend.position = "top")

dim(second_q_data)

spk = skmeans::skmeans(s , 19)
clus_data = data.frame(Earth_quack$Latitude , Earth_quack$Longitude)
clus_data$cluster = spk$cluster
clus_data


library(dplyr)

cluster_datasets <- split(clus_data, clus_data$cluster)
dim(cluster_datasets[[2]])
watson.test(first_q_data$Longitude , alpha = 0.01 , dist = "vonmises")


num_samples <- 15000

# Randomly sample rows from the matrix
sampled_rows <- sample(1:nrow(Earth_quack), size = num_samples, replace = FALSE)

# Extract the sampled rows
Quack <- Earth_quack[sampled_rows, ]
dim(Quack)
dEq = cbind(Quack$Latitude , Quack$Longitude)
rows_with_zeros <- apply(dEq, 1, function(row) any(row == 0))
sum(rows_with_zeros)
dim(dEq)
vol <- svo[!rows_with_zeros, ]
dim(vol)
# Print the resulting matrix
print(vol)
eq.dens = vmf_density_grid(dEq , ngrid = 300)


head(Earth_quack)
dim(Earth_quack)
head(Earthquake)
dim(Volcano)
head(Earthquake)

datetime <- as.POSIXct(Earthquake$Time, format = "%Y-%m-%dT%H:%M:%OS")

# Extract the date and time components
Earthquake$date <- as.Date(datetime)

head(Earthquake)

head(Volcano)
Vol = Volcano[,-c(1:6,10)]
head(Vol)
Vol$end_date <- as.Date(paste(Vol$end_year, Vol$end_month, Vol$end_day, sep = "-"))
Vol$start_date <- as.Date(paste(Vol$start_year, Vol$start_month, Vol$start_day, sep = "-"))
# Print the updated data frame

head(Vol)

dim(Vol)

                         
# Get the volcano eruption date
volcano_eruption_date <- Vol$start_date


num_earthquakes_before_eruption <- numeric(length(volcano_eruption_date))


for (i in 1:length(volcano_eruption_date)) {
  # Get the current eruption date
  current_eruption_date <- volcano_eruption_date[i]
  
  # Filter earthquakes that occurred before the current eruption date
  earthquakes_before_eruption <- subset(dataset_list[[i]], dataset_list[[i]]$date >=
                                          (current_eruption_date - 365*3) &
                                           dataset_list[[i]]$date 
                                        < current_eruption_date)
  
  # Count the number of earthquakes before the current eruption date
  num_earthquakes_before_eruption[i] <- nrow(earthquakes_before_eruption)
}


num_earthquakes_before_eruption[1]



Vol$days_between = Vol$days_between/365

Vol$num_earthquakes_before_erupt = c(num_earthquakes_before_eruption)
Vol$num_earthquakes_before_erupt
plot(Vol$num_earthquakes_before_erupt, Vol$days_between)


earthquakes_before_eruption_list <- list()

# Define the time window (3 years)
time_window <- 3 * 365  # Assuming approximately 365 days per year

# Iterate through each eruption in the volcano data
for (i in 1:nrow(Vol)) {
  eruption_date <- Vol$start_date[i]
  

  eruption_earthquakes <- data.frame(date = as.Date(character(0)), magnitude = numeric(0))
  

  for (j in 1:length(dataset_list)) {
    earthquake_data <- dataset_list[[j]]
    
    # Extract earthquakes that occurred before the eruption date within the time window
    valid_earthquakes <- earthquake_data[earthquake_data$date >= (eruption_date - time_window) &
                                           earthquake_data$date < eruption_date, ]
    

    eruption_earthquakes <- rbind(eruption_earthquakes, valid_earthquakes)
  }
  

  earthquakes_before_eruption_list[[i]] <- eruption_earthquakes
}



dim(earthquakes_before_eruption_list[[1]])
dim(dataset_list[[2]])
earthquakes_before_eruption_list[[1106]]


library(data.table)
earthq_before3 = rbindlist(earthquakes_before_eruption_list)
head(earthq_before3)

summary(earthq_before3)

dim(Vol)

calculate_distance <- function(lat1, lon1, lat2, lon2) {
  dist <- distVincentySphere(c(lon1, lat1), c(lon2, lat2))
  return(dist)
}


find_nearest_earthquake <- function(volcano_lat, volcano_lon, earthquake_data) {
  
  distances <- sapply(1:nrow(earthquake_data), function(i) {
    dist <- calculate_distance(volcano_lat, volcano_lon, earthquake_data[i, "Latitude"], earthquake_data[i, "Longitude"])
    return(dist)
  })
  
  
  nearest_earthquake_index <- which(distances <= 100000)
  nearest_earthquake <- earthquake_data[nearest_earthquake_index, ]
  
  return(nearest_earthquake)
}


Vol$Latitude = Vol$latitude
Vol$Longitude = Vol$longitude


nearest_earthquakes_list <- list()

for (i in 1:nrow(Vol)) {
  volcano_lat <- Vol[i, "Latitude"]
  volcano_lon <- Vol[i, "Longitude"]
  earthquake_data <- earthquakes_before_eruption_list[[i]]
  
  # Find the nearest earthquake within 100 km
  nearest_earthquake <- find_nearest_earthquake(volcano_lat, volcano_lon, earthquake_data)
  
  nearest_earthquakes_list[[i]] <- nearest_earthquake
}


nearest_earthquakes_list[[2]]
num_earthquakes_before_eruption_list <- list()


for (i in 1:length(volcano_eruption_date)) {
  # Get the current eruption date
  current_eruption_date <- volcano_eruption_date[i]
  
  # Filter earthquakes that occurred before the current eruption date
  earthquakes_before_eruption <- subset(dataset_list[[i]], dataset_list[[i]]$date >=
                                          (current_eruption_date - 365*3) &
                                          dataset_list[[i]]$date 
                                        < current_eruption_date)
  
  # Count the number of earthquakes before the current eruption date
  num_earthquakes_before_eruption_list[[i]] <- earthquakes_before_eruption
}

Sum_mag = c()
for (i in 1:1105) {
  Sum_mag[i] = sum(num_earthquakes_before_eruption_list[[i]]$Mag)
}

num_earthquakes_before_eruption_list[[1000]]
Vol$Sum_Am = Sum_mag



m = c()
for (i in 1:1105) {
  m[i] = mean(num_earthquakes_before_eruption_list[[i]]$Mag)
}

m[is.nan(m)] <- 0

length(m)



m = c()
for (i in 1:1105) {
  m[i] = mean(nearest_earthquakes_list[[i]]$Mag)
}

gm = c()
for (i in 1:1105) {
  gm[i] = exp(mean(log(nearest_earthquakes_list[[i]]$Mag)))
}


head(gm)

Vol$meanMag = m
Vol$gM = gm
Vol1 = na.omit(Vol)

summary(Vol1$gM)

plot(Vol1$gM, Vol1$days_between)
plot(Vol1$meanMag, Vol1$days_between)


world_map <- map_data("world")

datasetee = rbind(dataset_list[[4]])
dim(Vol1)
Vol$d = "Rincon de la Vieja Volcano"
datasetee$d = "Earthquake"

dataset1 = data.frame(Longitude = datasetee$Longitude,
                      Latitude = datasetee$Latitude,
                      Dataset = datasetee$d )

dataset2 = data.frame(Longitude = Vol[c(4),]$longitude, Latitude = Vol[c(4),]$latitude,
                      Dataset = Vol[c(4),]$d)

world_data <- rbind(dataset1, dataset2)
tail(world_data)


world_map <- map_data("world")
northern_map <- subset(world_map, long >-86)
northern_map1 <- subset(northern_map , long < -84)
northern_map2 <- subset(northern_map1 , lat >= 9)
northern_map3 <- subset(northern_map2 , lat <= 12.5)

library(ggforce)
world_plot <- ggplot() +
  geom_polygon(data = northern_map3, aes(x = long, y = lat, group = group), fill = "#FFF8DC", color = "black") +
  geom_point(data = world_data, aes(x = Longitude, y = Latitude, color = Dataset, 
                                    shape = Dataset, size = 3,
                                    fill = Dataset)) +
  geom_circle(data = dataset2, aes(x0 = Longitude, y0 = Latitude, r = 1),
               color = "red", alpha = 5) +
  
  scale_color_manual(values = c("Earthquake" = "blue", "Rincon de la Vieja Volcano" = "red")) +
  scale_shape_manual(values = c("Earthquake" = 1 , "Rincon de la Vieja Volcano" = 24))+
 # scale_size_manual(values = c("Earthquake" = 2 , "Rincon de la Vieja Volcano" = 4))+
  scale_fill_manual(values = c("Earthquake" = "blue","Rincon de la Vieja Volcano" = "red")) +
  theme_minimal() +
  labs(title = "Rincon de la Vieja Volcano and the earthquake arround that")

plot(world_plot)

plot(Vol1$num_earthquakes_before_erupt, Vol1$gM)

fit = lm(Vol1$gM ~ Vol1$meanMag , Vol1)

ggplot(Vol1, aes(x = Vol1$num_earthquakes_before_erupt, y = Vol1$gM)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 5), color = "blue") +
  labs(x = "Number of Earthuqakes", y = "GM of Magnitude") +
  ggtitle("Linear Regression")



fit = lm(Vol1$meanMag ~ poly(Vol1$num_earthquakes_before_erupt, 5))

summary(fit)
plot(Vol1$gM, Vol1$days_between, xlab = "Geometric Mean of Magnitude (Exponential Trans)", 
     ylab = "Eruption Time")
plot(Vol1$meanMag, Vol1$days_between, xlab = "A-M of Magnitude (Exponential Trans)", 
     ylab = "Eruption Time")

                         
three_y = read.csv("C:/Users/prithwihsh/OneDrive/Documents/3_years.csv")
head(three_y)
summary(three_y)
thre_y = three_y[,c(7,8,11,12,14)]
head(thre_y)
dim(three_y)


three_y$Sum_magnitude = Sum_mag



filtered_data = c()
for (i in 0:18) {
  filtered_data[[i]] <- thre_y[thre_y$num_earthquakes_before_erupt == i, ]
}


count_datasets = list()
for (count_value in 0:18) {
  # Extract data for the current count value
  subset_data <- subset(thre_y, thre_y$num_earthquakes_before_erupt == count_value)
  
  # Store the subset in the list using a unique name
  count_datasets[[paste("count", count_value, sep = "_")]] <- subset_data
}


names(count_datasets)


# Access a specific dataset, e.g., count_10
count_0 <- count_datasets[["count_0"]]
count_1 <- count_datasets[["count_1"]]
count_2 <- count_datasets[["count_2"]]
count_3 <- count_datasets[["count_3"]]
count_4 <- count_datasets[["count_4"]]
count_5 <- count_datasets[["count_5"]]
count_6 <- count_datasets[["count_6"]]
count_7 <- count_datasets[["count_7"]]
count_8 <- count_datasets[["count_8"]]
count_9 <- count_datasets[["count_9"]]
count_10 <- count_datasets[["count_10"]]
count_11 <- count_datasets[["count_11"]]
count_12 <- count_datasets[["count_12"]]
count_13 <- count_datasets[["count_13"]]
count_14 <- count_datasets[["count_14"]]
count_15 <- count_datasets[["count_15"]]
count_16 <- count_datasets[["count_16"]]
count_17 <- count_datasets[["count_17"]]
count_18 <- count_datasets[["count_18"]]


count_2$am_mag = count_2$Sum_magnitude/count_2$num_earthquakes_before_erupt
count_3$am_mag = count_3$Sum_magnitude/count_3$num_earthquakes_before_erupt
count_4$am_mag = count_4$Sum_magnitude/count_4$num_earthquakes_before_erupt
count_5$am_mag = count_5$Sum_magnitude/count_5$num_earthquakes_before_erupt
count_6$am_mag = count_6$Sum_magnitude/count_6$num_earthquakes_before_erupt
count_7$am_mag = count_7$Sum_magnitude/count_7$num_earthquakes_before_erupt
count_8$am_mag = count_8$Sum_magnitude/count_8$num_earthquakes_before_erupt
count_9$am_mag = count_9$Sum_magnitude/count_9$num_earthquakes_before_erupt
count_10$am_mag = count_10$Sum_magnitude/count_10$num_earthquakes_before_erupt
count_11$am_mag = count_11$Sum_magnitude/count_11$num_earthquakes_before_erupt
count_12$am_mag = count_12$Sum_magnitude/count_12$num_earthquakes_before_erupt
count_13$am_mag = count_13$Sum_magnitude/count_13$num_earthquakes_before_erupt
count_14$am_mag = count_14$Sum_magnitude/count_14$num_earthquakes_before_erupt
count_15$am_mag = count_15$Sum_magnitude/count_15$num_earthquakes_before_erupt
count_16$am_mag = count_16$Sum_magnitude/count_16$num_earthquakes_before_erupt
count_17$am_mag = count_17$Sum_magnitude/count_17$num_earthquakes_before_erupt
count_18$am_mag = count_18$Sum_magnitude/count_18$num_earthquakes_before_erupt
head(count_5)


absolute_median_0 = 0
absolute_median_1 = median(count_1$Sum_magnitude)
absolute_median_2 = median(count_2$am_mag)
absolute_median_3 = median(count_3$am_mag)
absolute_median_4 = median(count_4$am_mag)
absolute_median_5 = median(count_5$am_mag)
absolute_median_6 = median(count_6$am_mag)
absolute_median_7 = median(count_7$am_mag)
absolute_median_8 = median(count_8$am_mag)
absolute_median_9 = median(count_9$am_mag)
absolute_median_10 = median(count_10$am_mag)
absolute_median_11 = median(count_11$am_mag)
absolute_median_12 = median(count_12$am_mag)
absolute_median_13 = median(count_13$am_mag)
absolute_median_14 = median(count_14$am_mag)
absolute_median_15 = median(count_15$am_mag)
absolute_median_16 = median(count_16$am_mag)
absolute_median_17 = median(count_17$am_mag)
absolute_median_18 = median(count_18$am_mag)
abs_median = c(absolute_median_1,absolute_median_2,absolute_median_3,
             absolute_median_4,absolute_median_5,absolute_median_6,absolute_median_7,
             absolute_median_8,absolute_median_9,absolute_median_10,absolute_median_11,
             absolute_median_12,absolute_median_13,absolute_median_14,absolute_median_15,
             absolute_median_16,absolute_median_17,absolute_median_18)

absolute_mean_0 = 0

absolute_mean_1 = sum(count_1$Sum_magnitude)/length(count_1$Sum_magnitude)
absolute_mean_2 = sum(count_2$am_mag)/length(count_2$Sum_magnitude)
absolute_mean_3 = sum(count_3$am_mag)/length(count_3$Sum_magnitude)
absolute_mean_4 = sum(count_4$am_mag)/length(count_4$Sum_magnitude)
absolute_mean_5 = sum(count_5$am_mag)/length(count_5$Sum_magnitude)
absolute_mean_6 = sum(count_6$am_mag)/length(count_6$Sum_magnitude)
absolute_mean_7 = sum(count_7$am_mag)/length(count_7$Sum_magnitude)
absolute_mean_8 = sum(count_8$am_mag)/length(count_8$Sum_magnitude)
absolute_mean_9 = sum(count_9$am_mag)/length(count_9$Sum_magnitude)
absolute_mean_10 = sum(count_10$am_mag)/length(count_10$Sum_magnitude)
absolute_mean_11 = sum(count_11$am_mag)/length(count_11$Sum_magnitude)
absolute_mean_12 = sum(count_12$am_mag)/length(count_12$Sum_magnitude)
absolute_mean_13 = sum(count_13$am_mag)/length(count_13$Sum_magnitude)
absolute_mean_14 = sum(count_14$am_mag)/length(count_14$Sum_magnitude)
absolute_mean_15 = sum(count_15$am_mag)/length(count_15$Sum_magnitude)
absolute_mean_16 = sum(count_16$am_mag)/length(count_16$Sum_magnitude)
absolute_mean_17 = sum(count_17$am_mag)/length(count_17$Sum_magnitude)
absolute_mean_18 = sum(count_18$am_mag)/length(count_18$Sum_magnitude)


plot(abs_mean)
dur_mean_0 = sum(count_0$days_between)/length(count_0$days_between)
dur_mean_1 = sum(count_1$days_between)/length(count_1$days_between)
dur_mean_2 = sum(count_2$days_between)/length(count_2$days_between)
dur_mean_3 = sum(count_3$days_between)/length(count_3$days_between)
dur_mean_4 = sum(count_4$days_between)/length(count_4$days_between)
dur_mean_5 = sum(count_5$days_between)/length(count_5$days_between)
dur_mean_6 = sum(count_6$days_between)/length(count_6$days_between)
dur_mean_7 = sum(count_7$days_between)/length(count_7$days_between)
dur_mean_8 = sum(count_8$days_between)/length(count_8$days_between)
dur_mean_9 = sum(count_9$days_between)/length(count_9$days_between)
dur_mean_10 = sum(count_10$days_between)/length(count_10$days_between)
dur_mean_11 = sum(count_11$days_between)/length(count_11$days_between)
dur_mean_12 = sum(count_12$days_between)/length(count_12$days_between)
dur_mean_13 = sum(count_13$days_between)/length(count_13$days_between)
dur_mean_14 = sum(count_14$days_between)/length(count_14$days_between)
dur_mean_15 = sum(count_15$days_between)/length(count_15$days_between)
dur_mean_16 = sum(count_16$days_between)/length(count_16$days_between)
dur_mean_17 = sum(count_17$days_between)/length(count_17$days_between)
dur_mean_18 = sum(count_18$days_between)/length(count_18$days_between)





dur_median_0 = median(count_0$days_between)
dur_median_1 = median(count_1$days_between)
dur_median_2 = median(count_2$days_between)
dur_median_3 = median(count_3$days_between)
dur_median_4 = median(count_4$days_between)
dur_median_5 = median(count_5$days_between)
dur_median_6 = median(count_6$days_between)
dur_median_7 = median(count_7$days_between)
dur_median_8 = median(count_8$days_between)
dur_median_9 = median(count_9$days_between)
dur_median_10 = median(count_10$days_between)
dur_median_11 = median(count_11$days_between)
dur_median_12 = median(count_12$days_between)
dur_median_13 = median(count_13$days_between)
dur_median_14 = median(count_14$days_between)
dur_median_15 = median(count_15$days_between)
dur_median_16 = median(count_16$days_between)
dur_median_17 = median(count_17$days_between)
dur_median_18 = median(count_18$days_between)




median_dur = c( dur_median_1,dur_median_2,dur_median_3,
             dur_median_4,dur_median_5,dur_median_6,dur_median_7,
             dur_median_8,dur_median_9,dur_median_10,dur_median_11,
             dur_median_12,dur_median_13,dur_median_14,dur_median_15,
             dur_median_16,dur_median_17,dur_median_18)


plot(log(abs_median), log(median_dur), col = "red")
fit = lm(log(median_dur) ~ poly(log(abs_median), 10 ))
summary(fit)
g = data.frame(abs_median, median_dur)


ggplot(g, aes(x = log(abs_median), y = log(median_dur))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 5), color = "blue") +
  labs(x = "Absolute median of Magnitude of Earthquake", y = "Median Duration of Volcanic Eruption") +
  ggtitle("Polynomial Regression fit")


plot(density(log(count_1$days_between)), xlab = "Volcano Duration Count", main = "Density Plot for Volcano duration for different Earthquake Count")
lines(density(log(count_0$days_between)), col = "green" )
lines(density(log(count_2$days_between)), col = "red")
lines(density(log(count_3$days_between)), col = "blue")
legend("topleft", legend = c("Count 0", "Count 1", "Count 2",
                             "Count 3"#, "Count 4", "Count 5",
                             #"Count 6", "Count 7" #"Count 8",
                             #"Count 9", "Count 10", "Count 11",
                             #"Count 12", "Count 14", "Count 15",
                             #"Count 17"
),
col = c("green", "black", "red","blue"#,"brown","pink",
       # "purple", "#CCFF00"#, "#00CCFF", "#73A3C8","#312271","#F8B539",
        #"#DAFF47",  "yellow", "#00E5FF","#C7A76C"
), lty = 1, cex = 0.8)




plot(density(log(count_4$days_between)), col = "brown",  xlab = "Volcano Duration Count", main = "Density Plot for Volcano duration
     for different Earthquake Count",  xlim = c(-3, 12), ylim = c(0, 0.4))
lines(density(log(count_5$days_between)), col = "pink")
lines(density(log(count_6$days_between)), col = "purple")
lines(density(log(count_7$days_between)), col = "#CCFF00")
legend("topleft", legend = c(#"Count 0", "Count 1", "Count 2",
                             #"Count 3", 
                            "Count 4", "Count 5",
                             "Count 6", "Count 7" #"Count 8",
                             #"Count 9", "Count 10", "Count 11",
                             #"Count 12", "Count 14", "Count 15",
                             #"Count 17"
),
col = c(#"green", "black", "red","blue",
        "brown","pink",
        "purple", "#CCFF00"#, "#00CCFF", "#73A3C8","#312271","#F8B539",
        #"#DAFF47",  "yellow", "#00E5FF","#C7A76C"
), lty = 1, cex = 0.8)







plot(density(log(count_8$days_between)), col = "#00CCFF",  xlab = "Volcano Duration Count", main = "Density Plot for Volcano duration
     for different Earthquake Count",  xlim = c(-3, 12), ylim = c(0, 0.3))
lines(density(log(count_9$days_between)), col = "#73A3C8")
lines(density(log(count_10$days_between)), col = "#312271")
lines(density(log(count_11$days_between)), col = "#F8B539")
legend("topleft", legend = c(#"Count 0", "Count 1", "Count 2",
  #"Count 3", "Count 4", "Count 5",
  #"Count 6", "Count 7", 
  "Count 8",
  "Count 9", "Count 10", "Count 11"
  #"Count 12", "Count 14", "Count 15",
  #"Count 17"
),
col = c(#"green", "black", "red","blue","brown","pink",
  #"purple", "#CCFF00"#
  "#00CCFF", "#73A3C8","#312271","#F8B539"
  #"#DAFF47",  "yellow", "#00E5FF","#C7A76C"
), lty = 1, cex = 0.8)





plot(density(log(count_12$days_between)), col = "#DAFF47" , xlab = "Volcano Duration Count", main = "Density Plot for Volcano duration
     for different Earthquake Count",  xlim = c(-3, 12), ylim = c(0, 1.3))
#lines(density(log(count_13$days_between)), col = "#4BBE9F")
lines(density(log(count_14$days_between)), col = "yellow")
lines(density(log(count_15$days_between)), col = "#00E5FF")
#lines(density(log(count_16$days_between)), col = "#4C00FF")
lines(density(log(count_17$days_between)), col = "#C7A76C")
#lines(density(log(count_18$days_between)), col = "#7F000D")
legend("topleft", legend = c(#"Count 0", "Count 1", "Count 2",
                              #"Count 3", "Count 4", "Count 5",
                              #"Count 6", "Count 7", 
                            #"Count 8",
                              #"Count 9", "Count 10", "Count 11",
                              "Count 12", "Count 14", "Count 15",
                               "Count 17"
                             ),
       col = c(#"green", "black", "red","blue","brown","pink",
               #"purple", "#CCFF00"#
               # "#00CCFF", "#73A3C8","#312271","#F8B539",
               "#DAFF47",  "yellow", "#00E5FF","#C7A76C"
               ), lty = 1, cex = 0.8)
