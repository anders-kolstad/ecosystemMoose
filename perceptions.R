
# REAL DATA
library(readxl)
library(ggplot2)
dat <- read_excel("M:/Anders L Kolstad/systherb data/EcoS Paper/Survey about perceptions.xlsx")

names(MyData)
names(dat)
set.seed(1237)
dat$random <- rnorm(nrow(dat), 1, 0.02)
dat$Moose_point2 <- dat$Moose*dat$random
dat$Forestry_point2 <- dat$ForestryIntensity*dat$random

dat2 <- na.omit(dat)






polydat <- data.frame(
  id = c(rep(c(1:4), each = 3)),
  x =  c(1,1,4,
         1,1,4,
         9,9,6,
         9,9,6),
  y =   c(9,6,9,
          1,4,1,
          9,6,9,
          1,4,1))

polydat2 <- data.frame(
  x =  c(0,0,10,10),
  y =   c(0,10,10,0)          
          )

MyPlot <- ggplot(data = dat2, aes(y= Moose_point2, x = Forestry_point2))+
  geom_point(size = 5, stroke = 3, shape = c(49:57))+
  geom_errorbarh(aes(xmin =    Forestry_point2 - (Forestry_point2-ForestryIntensityMin),
                     xmax =    Forestry_point2 + (ForestryIntensityMax-Forestry_point2)), 
                 size = 1.2, height = 0, alpha = 1/5)+
  
  geom_errorbar(aes(ymin = Moose_point2 - (Moose_point2-MooseMin),
                    ymax = Moose_point2 + MooseMax-Moose_point2), 
                size = 1.2, width = 0, alpha = 1/5)+
  
  ylab("Moose densities")+
  xlab("Forestry intensity")+
  theme_classic()+
  #geom_hline(yintercept = 5, linetype = "dashed")+
  #geom_vline(xintercept = 5, linetype = "dashed")+
  scale_y_continuous(breaks = c(0,5,10), limits = c(0,10), labels = c("No moose", "Medium", "Very high"))+
  scale_x_continuous(breaks = c(0,5,10), limits = c(0,10), labels = c("No\nforestry", "Medium", "Very\n intensive"))+
  theme(axis.text=element_text(size=13),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
        axis.title = element_text(size=13),
        axis.line=element_blank(),
        axis.ticks = element_blank())+
        #panel.background = element_rect(colour = "black", size=4))+
  geom_polygon(data = polydat2, aes(x = x, y = y),
               alpha=0, colour = "black")+
  geom_polygon(data = polydat, aes(x = x, y = y, group = id),
               fill = "blue", alpha = 1/5, size = 0)+
  
  coord_fixed()


#tiff("M:\\Anders L Kolstad\\systherb data\\EcoS Paper\\tekst\\MS\\perceptions.tif", 
#     height = 15, width = 15, res = 600, units = "cm")
MyPlot
#dev.off()
# Old

library(tibble)
MyData <- tibble(ForestryIntensityMin = c(3, 6, 6, 7, 4, 7, 7, 7),
                   ForestryIntensityMax = c(4, 8, 7, 9, 9, 8, 9, 7),
                   MooseMin =             c(8, 6, 6, 7, 4, 7, 7, 7),
                   MooseMax =             c(10, 8, 9, 10, 8, 9, 10, 10),
                   SubjectID =            c("A", "B", "C", "D", "E", "F", "G", "H"))

MyData$Moose_point <- (MyData$MooseMin+MyData$MooseMax)/2
MyData$Forestry_point <- (MyData$ForestryIntensityMin+MyData$ForestryIntensityMax)/2
MyData$random <- rnorm(nrow(MyData), 1, 0.02)
MyData$Moose_point2 <- MyData$Moose_point*MyData$random
MyData$Forestry_point2 <- MyData$Forestry_point*MyData$random




library(ggplot2)
#devtools::install_github("lionel-/ggstance")
#library(ggstance)






ggplot(data = MyData, aes(y= Moose_point2, x = Forestry_point2))+
  geom_point(size = 3)+
  geom_errorbarh(aes(xmin =    Forestry_point2 - (Forestry_point2-ForestryIntensityMin),
                     xmax =    Forestry_point2 + (ForestryIntensityMax-Forestry_point2)), 
                 size = 1.2, height = 0)+
                 
  geom_errorbar(aes(ymin = Moose_point2 - (Moose_point2-MooseMin),
                    ymax = Moose_point2 + MooseMax-Moose_point2), 
                size = 1.2, width = 0)+
  
  ylab("Moose densities")+
  xlab("Forestry intensity")+
  theme_classic()+
  geom_hline(yintercept = 5, linetype = "dashed")+
  geom_vline(xintercept = 5, linetype = "dashed")+
  scale_y_continuous(breaks = c(0,5,10), limits = c(0,10), labels = c("No moose", "Medium", "Very high"))+
  scale_x_continuous(breaks = c(0,5,10), limits = c(0,10), labels = c("No\nforestry", "Medium", "Very\n intensive"))

ggplot(data = MyData, aes(y= Moose_point2, x = Forestry_point2))+
  #geom_point(size = 3)+
  geom_errorbarh(aes(xmin =    Forestry_point2 - (Forestry_point2-ForestryIntensityMin),
                     xmax =    Forestry_point2 + (ForestryIntensityMax-Forestry_point2)), 
                 size = 1.2, height = 0)+
  
  geom_errorbar(aes(ymin = Moose_point2 - (Moose_point2-MooseMin),
                    ymax = Moose_point2 + MooseMax-Moose_point2), 
                size = 1.2, width = 0)+
  
  ylab("Moose numbers")+
  xlab("Forestry intensity")+
  theme_classic()+
  geom_hline(yintercept = 5, linetype = "dashed")+
  geom_vline(xintercept = 5, linetype = "dashed")+
  scale_y_continuous(breaks = c(0,5,10), limits = c(0,10), labels = c("Low", "Medium", "High"))+
  scale_x_continuous(breaks = c(0,5,10), limits = c(0,10), labels = c("Low", "Medium", "High"))

  



               