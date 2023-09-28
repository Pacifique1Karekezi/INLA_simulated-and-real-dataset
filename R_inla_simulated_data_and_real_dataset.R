
##Self study on R INLA ####################
# On a simulated dataset and for a real dataset######
#########################################

##Import libraries###########
library(pixmap)
library (sp)
library(sf)
library(tidyverse)
library(spdep)
library(INLA)
library(ggplot2)

###simulated dataset

N <- 100 # 500, 5000, 25000, 100000
x <- rnorm(N, mean = 6, sd = 2)
y <- rnorm(N, mean = x, sd = 1)
data <- list(x = x, y = y, N = N)

####### Fitting the model#############

res<-inla(y ~ x,
     family = "gaussian",
     data = data,
     control.predictor = list(link = 1)
)

####Plot results##############

library(ggplot2)
alpha <- res$marginals.fixed[[1]]
ggplot(data.frame(inla.smarginal(alpha)), aes(x, y)) +
  geom_line() +
  theme_bw()

alphaa <- res$marginals.fixed[[2]]
ggplot(data.frame(inla.smarginal(alphaa)), aes(x, y)) +
  geom_line() +
  theme_bw()



###########################################
### ANOTHER SIMULATED Dataset#######################
###########################################


x<-rnorm(100,mean=2, sd=2)
y<-rnorm(100, mean= 2,sd =3)

z<-4 + 5* x + 6* y #######The mean of a response varible has to be a linear predictor

data<- list(z=z,x=x,y=y,N=100)

#View(as.data.frame(data))

## fitting the model

rs<- inla(z~x+y, family = "gaussian", data=data,)
summary(rs)

library(gridExtra) ### Library to plot on the same row multiples images
####Distribution for intercept#######

alpha <- rs$marginals.fixed[[1]]
p1<-ggplot(data.frame(inla.smarginal(alpha)), aes(x, y)) +
  geom_line() +
  theme_bw()

#########Distribution for b1#############

alpha <- rs$marginals.fixed[[2]]
p2<-ggplot(data.frame(inla.smarginal(alpha)), aes(x, y)) +
  geom_line() +
  theme_bw()

#########Distribution for b2#############

alpha <- rs$marginals.fixed[[3]]
p3<-ggplot(data.frame(inla.smarginal(alpha)), aes(x, y)) +
  geom_line() +
  theme_bw()
grid.arrange(p1, p2,p3, nrow = 1)

###############################################
######Random effects###########################
#################################################

f.rs <- z ~ f(x, model = "rw1", scale.model = TRUE) +
  f(y, model = "rw1", scale.model = TRUE) ### The formula

f.rss<-inla(f.rs, data =data ) ## Model fitting

summary(f.rss)

## Plot the distribution of the intercept

alpha <- f.rss$marginals.fixed[[1]]
ggplot(data.frame(inla.smarginal(alpha)), aes(x, y)) +
  geom_line() +
  theme_bw()


#######################
#Plotting random effects
############################

t<-f.rss$summary.random$x
plot(t$ID,t$mean)
plot(t$ID,exp(t$mean), type="o", col="red")
#lines(t$ID,exp(t$0.025quant), type="l",col="blue")

###########################################

t<-f.rss$summary.random$y
plot(t$ID,t$mean)
plot(t$ID,exp(t$mean), type="o", col="red")
#lines((t$ID),exp(t$0.025quant), type="l",col="blue"))


################################################
## On a real dataset ##############
######################################################
 ## importing the dataset

setwd("C:/Users/Pacifique Karekezi/Documents/PHD_NM-AIST_ applied_math_computational_skills/Codes/September/Fourth Week/R_INLA on a real_dataset")
datta<-read.csv("clean_data.csv")
dim(datta)
colnames(datta)
cor(datta$malaria,datta$anemia)

##prepare a dataset for joit models

datta1<- datta
datta2<-datta
datta1$r<-1
#datta1$r
datta2$r<-2
#datta2$r

datta<- rbind(datta1, datta2)
dim(datta)

datta$disease <-ifelse(datta$r==1,datta$malaria,datta$anemia)

## Create other variables for the replication########

datta$cage1<-datta$cage
datta$hhage1<-datta$hhage
datta$annual_precipitation_20151<-datta$annual_precipitation_2015
datta$mean_temperature_20151<-datta$mean_temperature_2015
datta$district1<-datta$district

## for spatial models###########
shaperw<-st_read("rwanda.shp") ## reading the shape of Rwanda
##plot(shaperw)

##To create neighbourhoods##############

nb<-poly2nb(shaperw)

###Neighborhoods used in INLA#####

nb2INLA("Rwandagraph",nb)

Rwand.adj<-paste(getwd(),"/Rwandagraph",sep="")

### To create adjacency matrix#########

Amatrix<-inla.read.graph(filename="Rwandagraph")
image(inla.graph2matrix(Amatrix),xlab="",ylab="")

### To create the formula###

formula<- disease~-1+bednet+poorer+richest+sex_male+urban+richer+bednet_all+
  primary+sec_high+urban+f(inla.group(cage),model="rw2",scale.model=T) +
  f(inla.group(cage1),model="rw2",scale.model=T, replicate = r)+
  f(inla.group(annual_precipitation_2015), model = "rw2",scale.model=T)+
  f(inla.group(annual_precipitation_20151),model = "rw2",replicate = r,scale.model = T)+
  f(inla.group(mean_temperature_2015), model = "rw2",scale.model = T)+
  f(inla.group(mean_temperature_20151),model = "rw2",replicate = r,scale.model = T)+
  f(district,model = "besag",graph=Rwand.adj,scale.model = T)+
  f(district1,model = "besag",graph=Rwand.adj,replicate=r,scale.model = T)

### Fitting the model##########

fit.inla<-inla(formula = formula, data = datta,family = "binomial")  

###To see the latent effects###########
fit.inla$summary.fixed

###To see the random  effects###########
##inla.group(cage)##
cage<-fit.inla$summary.random[[1]]
plot(cage$ID,exp(cage$mean), type="l", col="green", lwd=2, xlab="months", ylab="f(Child's age)")
#lines(cage$ID, t(exp(cage[4])), col="blue", lwd=1, ylim=c(exp(-1),exp(5)))
#lines(cage$ID, t(exp(cage[6])), col="blue", lwd=1, ylim=c(exp(-0.5),exp(1.13)))

####cage replicated####
##for malaria
cage1<-fit.inla$summary.random[[2]]
plot(cage1$ID[1:25],exp(cage1$mean[1:25]), type="l", col="green", lwd=2, xlab="months", ylab="f(Child's age)")

###for anemia
plot(cage1$ID[26:50],exp(cage1$mean[26:50]), type="l", col="green", lwd=2, xlab="months", ylab="f(Child's age)")

###Annual precipitation#########

precip<-fit.inla$summary.random[[3]]
plot(precip$ID,exp(precip$mean), type="l", col="green", lwd=2, xlab="mm/day", ylab="f(annual_precip)")

##Annual precipitation#########
#for malaria#

precip<-fit.inla$summary.random[[4]]
plot(precip$ID[1:11],exp(precip$mean[1:11]), type="l", col="green", lwd=2, xlab="mm/day", ylab="f(annual_precip)")
#View(precip)

###for anemia
plot(precip$ID[12:22],exp(precip$mean[12:22]), type="l", col="green", lwd=2, xlab="mm/day", ylab="f(annual_precip)")


####Spatial effects############
library(ggspatial)
library(tidyverse)
library(sf)
theme_set(theme_bw())

district<-fit.inla$summary.random[[7]]

district <- rename(district, district=ID)
#View(district)
map_dist <- inner_join(shaperw,district)

rand <- ggplot(data = map_dist) +
  geom_sf(aes(fill = exp(mean))) #+facet_wrap("random")

rand +  scale_fill_viridis_b(option="magma", direction=-1, 
                             name="Posterior odds ratio",
                             guide=guide_colorbar(
                               direction = "horizontal",
                               barheight = unit(4, units = "mm"),
                               barwidth = unit(100, units = "mm"),
                               title.position = "top",
                               title.hjust=0.5,
                               label.hjust = 0.5
                             ))+
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(legend.position = "bottom") 

#####for malaria#########

districtM<-fit.inla$summary.random[[8]]

districtM <- rename(districtM, district=ID)
districtM<-districtM[1:30,]
#View(district)
map_dist <- inner_join(shaperw,districtM)

rand <- ggplot(data = map_dist) +
  geom_sf(aes(fill = exp(mean))) #+facet_wrap("random")

rand +  scale_fill_viridis_b(option="magma", direction=-1, 
                             name="Posterior odds ratio",
                             guide=guide_colorbar(
                               direction = "horizontal",
                               barheight = unit(4, units = "mm"),
                               barwidth = unit(100, units = "mm"),
                               title.position = "top",
                               title.hjust=0.5,
                               label.hjust = 0.5
                             ))+
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(legend.position = "bottom") 

##For anemia

districtM<-fit.inla$summary.random[[8]]

districtM <- rename(districtM, district=ID)
districtM<-districtM[31:60,]
#View(district)
map_dist <- inner_join(shaperw,districtM)

rand <- ggplot(data = map_dist) +
  geom_sf(aes(fill = exp(mean))) #+facet_wrap("random")

rand +  scale_fill_viridis_b(option="magma", direction=-1, 
                             name="Posterior odds ratio",
                             guide=guide_colorbar(
                               direction = "horizontal",
                               barheight = unit(4, units = "mm"),
                               barwidth = unit(100, units = "mm"),
                               title.position = "top",
                               title.hjust=0.5,
                               label.hjust = 0.5
                             ))+
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(legend.position = "bottom") 


####Done with INLA###############


