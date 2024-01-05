library(ggplot2)
library(dplyr)
library(ggsignif)
setwd("~/desktop/admixture_figures")
##############################################
#load/build data frame
##############################################
#load trait averages (plus structure, calls, etc), instructions on making this file at the bottom of .r
avg_data <- read.csv(file='ind_avg.csv', header=TRUE)
#add log data
avg_data$log_petiole_length <- (log(avg_data$petiole_length))
avg_data$log_blade_length <- (log(avg_data$blade_length))
avg_data$log_blade_width <- (log(avg_data$blade_width))
avg_data$log_blade_area <- (log(avg_data$blade_area))
avg_data$log_blade_perimeter <- (log(avg_data$blade_perimeter))
avg_data$log_blade_circularity <- (log(avg_data$blade_circularity))
avg_data$log_trichomes_ab <- (log(avg_data$trichomes_ab))
avg_data$log_trichomes_ad <- (log(avg_data$trichomes_ad))
avg_data$log_teeth <- (log(avg_data$teeth))
avg_data$log_teeth_by_perimeter <- (log(avg_data$teeth_by_perimeter))
avg_data$log_length_by_width <- (log(avg_data$length_by_width))
#replace log(0)=-inf values with -2.302585 (log(0.1)=-2.302585)
avg_data[avg_data == "-Inf"] <- "-2.302585"
#force some log columns numeric
avg_data$log_teeth <- as.numeric(avg_data$log_teeth)
avg_data$log_trichomes_ab <- as.numeric(avg_data$log_trichomes_ab)
avg_data$log_trichomes_ad <- as.numeric(avg_data$log_trichomes_ad)
#isolate individuals from hybrid swarms for stats
avg_hybs <- subset(avg_data, species %in% c('hybrid'))

##############################################
#plot raw trait values (blade area, length/width, teeth, adaxial trichomes, abaxial trichomes) as points
#in same order as bars from structure plot
##############################################
#put the samples in the same order as the structure plots
#this order matches denovo_hyb_5 with runs from denovo hybrids and parents run separately but plotted together
#make the same kind of plot as above, but prune for samples in strucuture plots and then order by structure
struct_inds <- subset(avg_data, individual %in% c("MKM15","MKM18","MKM17","MKM11","MKM12","MKM13","MKM14",
                                                  "MKM16","MKM20","MKM01","MKM03","MKM09","MKM02","MKM23",
                                                  "MKM24","MKM25","MKM26",
                                                  "Z055","YC05","Z065","Z042","Z073","Z029","Z017","Z011",
                                                  "Z005","YA12","YA07","Z096","Z061","YD10","YA24",
                                                  "YB05","Z018","YB11","YA18","YD03",
                                                  "Z072","Z081","Z036","Z010","Z008",
                                                  "Z004","Z046","Z056","YB09","YA22",
                                                  "YA38","Z001","YA33","Z045","Z006",
                                                  "Z027","YA25","Z063","Z040","Z026",
                                                  "Z094","Z064","Z019","YD13","Z014",
                                                  "Z028","Z044","YB10","Z050","YD08",
                                                  "YA03","Z082","Z024","Z031","Z071",
                                                  "Z059","YA21","Z068","Z074","YD05",
                                                  "Z051","YA14","Z048","YA37","Z069",
                                                  "Z060","Z020","Z032","Z037","Z078",
                                                  "Z016","Z093","YA23","Z013","Z058",
                                                  "Z047","Z052","YA11","Z097","YA13",
                                                  "YA10","YA41","YA29","Z054","Z089",
                                                  "Z090","Z003","Z076","Z023","Z084",
                                                  "YC01","Z079","Z095","Z053","YA17",
                                                  "Z030","YB07","Z041","Z021","YA06",
                                                  "Z043","YB04","Z085","YA26","YA32",
                                                  "Z062","YD04","YD01","Z083","YD16",
                                                  "YC04","Z086","YD18","YA15","YB03",
                                                  "YA19","YB01","Z080","YD11"))
#now put them in the same order as denovo_hyb_5 structure plot                                                
struct_inds <- struct_inds %>% mutate(individual = factor(individual, levels=c("MKM26","MKM24","MKM23","MKM25","MKM02","MKM09","MKM03",
                                                                               "MKM01","MKM20","MKM13","MKM16","MKM17","MKM18","MKM12",
                                                                               "MKM11","MKM14","MKM15","YB03","YD04","YD11","YD01","YC04",
                                                                               "YD16","YD18","YA15","YB01","Z080","YA19","Z086","Z083",
                                                                               "Z062","YA32","YA26","Z085","YB04","Z043","Z021","YA06",
                                                                               "Z041","YB07","Z030","YA17","Z053","Z095","Z079","YC01",
                                                                               "Z084","Z023","Z076","Z003","Z090","Z089","Z054","YA29",
                                                                               "YA41","YA10","YA13","Z097","YA11","Z052","Z047","Z058",
                                                                               "Z013","YA23","Z093","Z016","Z078","Z037","Z032","Z020",
                                                                               "Z060","Z069","YA37","Z048","YA14","Z051","YD05","Z074",
                                                                               "Z068","YA21","Z059","Z071","Z031","Z024","Z082","YA03",
                                                                               "YD08","Z050","YB10","Z044","Z028","Z014","YD13","Z019",
                                                                               "Z064","Z094","Z026","Z063","Z040","YA25","Z006","Z027",
                                                                               "Z045","YA33","Z001","YA38","YA22","YB09","Z056","Z046",
                                                                               "Z004","Z008","Z010","Z036","Z081","Z072","YD03","YB11",
                                                                               "YA18","Z018","YB05","YA24","YD10","Z061","Z096","YA07",
                                                                               "YA12","Z011","Z065","Z029","Z073","Z005","Z042","Z017",
                                                                               "Z055","YC05")))
#plots of raw trait values that match above bars on structure plots
#save each as 10x40 inches
#blade area
ggplot() + geom_point(data=struct_inds, size=8, shape=15, aes(x=individual, y=blade_area, color=species)) +
  scale_color_manual(values=c("lightcyan4","lightpink3","darkgoldenrod3")) +
  labs(x="Individual", y="Blade area (cm^2)") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=20),
        axis.text.x = element_text(size=20, angle=-60, face="bold", hjust=-0.1),
        axis.text.y = element_text(size=30, face="bold"),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
#blade length/width
ggplot() + geom_point(data=struct_inds, size=8, shape=15, aes(x=individual, y=length_by_width, color=species)) +
  scale_color_manual(values=c("lightcyan4","lightpink3","darkgoldenrod3")) +
  labs(x="Individual", y="Blade Length/Width") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20, angle=-60, face="bold", hjust=-0.1),
        axis.text.y = element_text(size=30, face="bold"),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
#teeth
ggplot() + geom_point(data=struct_inds, size=8, shape=15, aes(x=individual, y=teeth, color=species)) +
  scale_color_manual(values=c("lightcyan4","lightpink3","darkgoldenrod3")) +
  labs(x="Individual", y="Marginal Teeth") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20, angle=-60, face="bold", hjust=-0.1),
        axis.text.y = element_text(size=30, face="bold"),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
#trichomes_ab
ggplot() + geom_point(data=struct_inds, size=8, shape=15, aes(x=individual, y=trichomes_ab, color=species)) +
  scale_color_manual(values=c("lightcyan4","lightpink3","darkgoldenrod3")) +
  labs(x="Individual", y="Abaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20, angle=-60, face="bold", hjust=-0.1),
        axis.text.y = element_text(size=30, face="bold"),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
#trichomes_ad
ggplot() + geom_point(data=struct_inds, size=8, shape=15, aes(x=individual, y=trichomes_ad, color=species)) +
  scale_color_manual(values=c("lightcyan4","lightpink3","darkgoldenrod3")) +
  labs(x="Individual", y="Adaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20, angle=-60, face="bold", hjust=-0.1),
        axis.text.y = element_text(size=30, face="bold"),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18)) 

############################################################
#updating structure x trait rergressions with 
#denovo_hyb_5: runs from denovo hybrids and parents run separately but plotted together
#fix levels on $species so that parents plot above hybrids
avg_data <- avg_data %>% mutate(species = factor(species, levels=c("hybrid", "jucundum", "lautum")))
#or if you want hybrids above parents
avg_data <- avg_data %>% mutate(species = factor(species, levels=c("jucundum", "lautum", "hybrid")))
#get linear rergressions to plug into figures from hyrids only
#blade area
regress_juc_blade_area <- lm(avg_hybs$blade_area~avg_hybs$denovo_hyb_5)
summary.lm(regress_juc_blade_area)
#intercept=3.275, slope=20.641  R^2:0.4279  P:4.364e-16 ***
#length/width
regress_juc_lw <- lm(avg_hybs$length_by_width~avg_hybs$denovo_hyb_5)
summary.lm(regress_juc_lw)
#intercept=1.62569, slope=0.05403  R^2:0.001009  P:0.2923
#teeth
regress_juc_teeth <- lm(avg_hybs$teeth~avg_hybs$denovo_hyb_5)
summary.lm(regress_juc_teeth)
#intercept=3.0598,  slope=18.2368  R^2:0.6322  P:<2e-16 ***
#trichab
regress_juc_trichab <- lm(avg_hybs$trichomes_ab~avg_hybs$denovo_hyb_5)
summary.lm(regress_juc_trichab)
#intercept=1.337, slope=112.434  R^2:0.437 P:<2e-16 ***
#trichad
regress_juc_trichad <- lm(avg_hybs$trichomes_ad~avg_hybs$denovo_hyb_5)
summary.lm(regress_juc_trichad)
#intercept=22.463, slope=124.403  R^2:0.3595  P:4.41e-13 ***
#plotting jucundum ancestry by traits, shapes for call by phenotype or by pop locality
#blade area
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=denovo_hyb_5, y=blade_area, fill=locality, shape=assignment), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=3.275, slope=20.641, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y='Blade Area') +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=denovo_hyb_5, y=blade_area, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=3.275, slope=20.641, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y='Blade Area') +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
#blade length/width
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=denovo_hyb_5, y=length_by_width, fill=locality, shape=assignment), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=1.62569, slope=0.05403, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Blade Length/Width") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=denovo_hyb_5, y=length_by_width, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=1.62569, slope=0.05403, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Blade Length/Width") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
#teeth
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=denovo_hyb_5, y=teeth, fill=locality, shape=assignment), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=3.0598,  slope=18.2368, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Marginal Teeth") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=denovo_hyb_5, y=teeth, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=3.0598,  slope=18.2368, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Marginal Teeth") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
#trichab
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=denovo_hyb_5, y=trichomes_ab, fill=locality, shape=assignment), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=1.337, slope=112.434, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Abaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=denovo_hyb_5, y=trichomes_ab, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=1.337, slope=112.434, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Abaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
#trichad
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=denovo_hyb_5, y=trichomes_ad, fill=locality, shape=assignment), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=22.463, slope=124.403, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Adaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=denovo_hyb_5, y=trichomes_ad, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=22.463, slope=124.403, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Adaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))

#put the samples in the same order as the structure plots
#this order matches X12_7_hyb_only (hyb only structure run, but with parent traits on the end) juc, laut, hyb
#still not quite right for hybs that are actually juc or laut, check to structure plot
#this orders by structure plot, but doesn't match all samples
avg_data <- avg_data %>% mutate(individual = factor(individual, levels=c("MKM11","MKM12","MKM13","MKM14","MKM15",
                                                                         "MKM16","MKM17","MKM18","MKM19","MKM20",
                                                                         "MKM21","MKM01","MKM02","MKM03","MKM04",
                                                                         "MKM05","MKM06","MKM07","MKM08","MKM09",
                                                                         "MKM10","MKM22","MKM23","MKM24","MKM25",
                                                                         "MKM26","MKM27","MKM28","MKM29","MKM30",
                                                                         "YC05","Z017","Z029","Z042","Z055",
                                                                         "Z065","Z073","Z005","Z011","YA12",
                                                                         "YA07","Z096","Z061","YD10","YA24",
                                                                         "YB05","Z018","YB11","YA18","YD03",
                                                                         "Z072","Z081","Z036","Z010","Z008",
                                                                         "Z004","Z046","Z056","YB09","YA22",
                                                                         "YA38","Z001","YA33","Z045","Z006",
                                                                         "Z027","YA25","Z063","Z040","Z026",
                                                                         "Z094","Z064","Z019","YD13","Z014",
                                                                         "Z028","Z044","YB10","Z050","YD08",
                                                                         "YA03","Z082","Z024","Z031","Z071",
                                                                         "Z059","YA21","Z068","Z074","YD05",
                                                                         "Z051","YA14","Z048","YA37","Z069",
                                                                         "Z060","Z020","Z032","Z037","Z078",
                                                                         "Z016","Z093","YA23","Z013","Z058",
                                                                         "Z047","Z052","YA11","Z097","YA13",
                                                                         "YA10","YA41","YA29","Z054","Z089",
                                                                         "Z090","Z003","Z076","Z023","Z084",
                                                                         "YC01","Z079","Z095","Z053","YA17",
                                                                         "Z030","YB07","Z041","Z021","YA06",
                                                                         "YB04","Z043","Z085","YA26","YA32",
                                                                         "Z062","YA15","YA19","YB01","YB03",
                                                                         "YC04","YD01","YD04","YD11","YD16",
                                                                         "YD18","Z080","Z083","Z086","YA01",
                                                                         "YA02","YA04","YA09","YA16","YA27",
                                                                         "YA28","YA30","YA34","YA35","YB08",
                                                                         "YB13","YC03","YD02","YD06","YD07",
                                                                         "YD09","YD12","YD14","YD17","YD19",
                                                                         "YD20","Z075","Z077","Z088","Z099")))
#blade area
#save as 10x50 inches for proper spacing
ggplot() + geom_point(data=avg_data, size=8, shape=15, aes(x=individual, y=blade_area, color=species)) +
  scale_color_manual(values=c("lightpink3","darkgoldenrod3","lightcyan4")) +
  labs(x="Individual", y="Blade area (cm^2)") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20, angle=-60, vjust=0.5, hjust=1), 
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
#blade length/width
#save as 10x50 inches for proper spacing
ggplot() + geom_point(data=avg_data, size=8, shape=15, aes(x=individual, y=length_by_width, color=species)) +
  scale_color_manual(values=c("lightpink3","darkgoldenrod3","lightcyan4")) +
  labs(x="Individual", y="Blade Length/Width") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20, angle=-60, vjust=0.5, hjust=1), 
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
#teeth
#save as 10x50 inches for proper spacing
ggplot() + geom_point(data=avg_data, size=8, shape=15, aes(x=individual, y=teeth, color=species)) +
  scale_color_manual(values=c("lightpink3","darkgoldenrod3","lightcyan4")) +
  labs(x="Individual", y="Marginal Teeth") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20, angle=-60, vjust=0.5, hjust=1), 
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
#trichomes_ab
#save as 10x50 inches for proper spacing
ggplot() + geom_point(data=avg_data, size=8, shape=15, aes(x=individual, y=trichomes_ab, color=species)) +
  scale_color_manual(values=c("lightpink3","darkgoldenrod3","lightcyan4")) +
  labs(x="Individual", y="Abaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20, angle=-60, vjust=0.5, hjust=1), 
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
#trichomes_ad
#save as 10x50 inches for proper spacing
ggplot() + geom_point(data=avg_data, size=8, shape=15, aes(x=individual, y=trichomes_ad, color=species)) +
  scale_color_manual(values=c("lightpink3","darkgoldenrod3","lightcyan4")) +
  labs(x="Individual", y="Adaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20, angle=-60, vjust=0.5, hjust=1), 
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
#make the same kind of plot as above, but prune for samples in strucuture plots and then order by structure
struct_inds <- subset(avg_data, individual %in% c("MKM15","MKM18","MKM17","MKM11","MKM12","MKM13","MKM14",
                                                  "MKM16","MKM20","MKM01","MKM03","MKM09","MKM02","MKM23",
                                                  "MKM24","MKM25","MKM26",
                                                  "Z055","YC05","Z065","Z042","Z073","Z029","Z017","Z011",
                                                  "Z005","YA12","YA07","Z096","Z061","YD10","YA24",
                                                  "YB05","Z018","YB11","YA18","YD03",
                                                  "Z072","Z081","Z036","Z010","Z008",
                                                  "Z004","Z046","Z056","YB09","YA22",
                                                  "YA38","Z001","YA33","Z045","Z006",
                                                  "Z027","YA25","Z063","Z040","Z026",
                                                  "Z094","Z064","Z019","YD13","Z014",
                                                  "Z028","Z044","YB10","Z050","YD08",
                                                  "YA03","Z082","Z024","Z031","Z071",
                                                  "Z059","YA21","Z068","Z074","YD05",
                                                  "Z051","YA14","Z048","YA37","Z069",
                                                  "Z060","Z020","Z032","Z037","Z078",
                                                  "Z016","Z093","YA23","Z013","Z058",
                                                  "Z047","Z052","YA11","Z097","YA13",
                                                  "YA10","YA41","YA29","Z054","Z089",
                                                  "Z090","Z003","Z076","Z023","Z084",
                                                  "YC01","Z079","Z095","Z053","YA17",
                                                  "Z030","YB07","Z041","Z021","YA06",
                                                  "Z043","YB04","Z085","YA26","YA32",
                                                  "Z062","YD04","YD01","Z083","YD16",
                                                  "YC04","Z086","YD18","YA15","YB03",
                                                  "YA19","YB01","Z080","YD11"))
#now put them in the same order as structure plot                                                
struct_inds <- struct_inds %>% mutate(individual = factor(individual, levels=c("MKM15","MKM18","MKM17","MKM11","MKM12","MKM13","MKM14",
                                                  "MKM16","MKM20","MKM01","MKM03","MKM09","MKM02","MKM23",
                                                  "MKM24","MKM25","MKM26",
                                                  "Z055","YC05","Z065","Z042","Z073","Z029","Z017","Z011",
                                                  "Z005","YA12","YA07","Z096","Z061","YD10","YA24",
                                                  "YB05","Z018","YB11","YA18","YD03",
                                                  "Z072","Z081","Z036","Z010","Z008",
                                                  "Z004","Z046","Z056","YB09","YA22",
                                                  "YA38","Z001","YA33","Z045","Z006",
                                                  "Z027","YA25","Z063","Z040","Z026",
                                                  "Z094","Z064","Z019","YD13","Z014",
                                                  "Z028","Z044","YB10","Z050","YD08",
                                                  "YA03","Z082","Z024","Z031","Z071",
                                                  "Z059","YA21","Z068","Z074","YD05",
                                                  "Z051","YA14","Z048","YA37","Z069",
                                                  "Z060","Z020","Z032","Z037","Z078",
                                                  "Z016","Z093","YA23","Z013","Z058",
                                                  "Z047","Z052","YA11","Z097","YA13",
                                                  "YA10","YA41","YA29","Z054","Z089",
                                                  "Z090","Z003","Z076","Z023","Z084",
                                                  "YC01","Z079","Z095","Z053","YA17",
                                                  "Z030","YB07","Z041","Z021","YA06",
                                                  "Z043","YB04","Z085","YA26","YA32",
                                                  "Z062","YD04","YD01","Z083","YD16",
                                                  "YC04","Z086","YD18","YA15","YB03",
                                                  "YA19","YB01","Z080","YD11")))
#blade area
#save as 10x50 inches for proper spacing
#10x38 is tight but still legible, maybe make text tiny bit smaller and bold?
ggplot() + geom_point(data=struct_inds, size=8, shape=15, aes(x=individual, y=blade_area, color=species)) +
  scale_color_manual(values=c("lightcyan4","lightpink3","darkgoldenrod3")) +
  labs(x="Individual", y="Blade area (cm^2)") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20, angle=-60, vjust=0.5, hjust=1), 
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
#blade length/width
#save as 10x50 inches for proper spacing
ggplot() + geom_point(data=struct_inds, size=8, shape=15, aes(x=individual, y=length_by_width, color=species)) +
  scale_color_manual(values=c("lightcyan4","lightpink3","darkgoldenrod3")) +
  labs(x="Individual", y="Blade Length/Width") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20, angle=-60, vjust=0.5, hjust=1), 
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
#teeth
#save as 10x50 inches for proper spacing
ggplot() + geom_point(data=struct_inds, size=8, shape=15, aes(x=individual, y=teeth, color=species)) +
  scale_color_manual(values=c("lightcyan4","lightpink3","darkgoldenrod3")) +
  labs(x="Individual", y="Marginal Teeth") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20, angle=-60, vjust=0.5, hjust=1), 
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
#trichomes_ab
#save as 10x50 inches for proper spacing
ggplot() + geom_point(data=struct_inds, size=8, shape=15, aes(x=individual, y=trichomes_ab, color=species)) +
  scale_color_manual(values=c("lightcyan4","lightpink3","darkgoldenrod3")) +
  labs(x="Individual", y="Abaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20, angle=-60, vjust=0.5, hjust=1), 
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
#trichomes_ad
#save as 10x50 inches for proper spacing
ggplot() + geom_point(data=struct_inds, size=8, shape=15, aes(x=individual, y=trichomes_ad, color=species)) +
  scale_color_manual(values=c("lightcyan4","lightpink3","darkgoldenrod3")) +
  labs(x="Individual", y="Adaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20, angle=-60, vjust=0.5, hjust=1), 
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))

                                                  
##############################################
#supplement plots: boxplots for juc/laut/hybrids with significance (done)
#supplement plots: trait by trait plots 
#supplement plots: boxplots for hybrid traits comparing trichome classes
#supplement plots:ancestry by trichome density coded by trichome branching class
##############################################                                                  
#subset relevant data to check t-tests
avg_hybs <- subset(avg_data, species %in% c('hybrid'))
avg_juc <- subset(avg_data, species %in% c('jucundum'))
avg_laut <- subset(avg_data, species %in% c('lautum'))
#within hybrids subset trichome classes (stellate trichomes present = 1)
#allows comparison of traits for stellate vs. not stellate
ablam_stellate <- subset(avg_hybs, ablam_stellate %in% c('1'))
ablam_not_stellate <- subset(avg_hybs, ablam_stellate %in% c('0'))
adlam_stellate <- subset(avg_hybs, adlam_stellate %in% c('1'))
adlam_not_stellate <- subset(avg_hybs, adlam_stellate %in% c('0'))
abvein_stellate <- subset(avg_hybs, abvein_stellate %in% c('1'))
abvein_not_stellate <- subset(avg_hybs, abvein_stellate %in% c('0'))
margin_stellate <- subset(avg_hybs, margin_stellate %in% c('1'))
margin_not_stellate <- subset(avg_hybs, margin_stellate %in% c('0'))
#for trichome class comparisons, make new data frame of hybrids removing samples that don't have trichome classes
# combine *_stellate and *_not stellate from above (removes NAs) and convert 0/1 to not_stellate/stellate
boxplot_ablam_stellate <- rbind(ablam_stellate, ablam_not_stellate)
boxplot_ablam_stellate$ablam_stellate <- factor(boxplot_ablam_stellate$ablam_stellate, labels=c('not_stellate','stellate'))
boxplot_ablam_stellate$adlam_stellate <- factor(boxplot_ablam_stellate$adlam_stellate, labels=c('not_stellate','stellate'))

#stellate trichome classes by trichome density in hybrids 

#save boxplot figures as 10 by 8 inches landscape for now
#boxplots: abaxial trichome density by abaxial stellate/not stellate
ggplot(data=boxplot_ablam_stellate, aes(x=ablam_stellate, y=trichomes_ab)) + 
  geom_boxplot(aes(fill=as.factor(ablam_stellate))) +
  geom_signif(comparisons = list(c("not_stellate", "stellate") ), 
              test="t.test", map_signif_level=TRUE, textsize=6, y_position = c(250)) +
  scale_fill_manual(values=c("lightcyan4", "lightcyan4")) +
  labs(x="Abaxial Trichomes", y="Abaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_text(size=30), 
        axis.text.x = element_text(size=20), 
        axis.text.y = element_text(size=30),
        legend.position = "none")

#boxplots: adaxial trichome density by adaxial stellate/not stellate
ggplot(data=boxplot_ablam_stellate, aes(x=adlam_stellate, y=trichomes_ad)) + 
  geom_boxplot(aes(fill=as.factor(adlam_stellate))) +
  geom_signif(comparisons = list(c("not_stellate", "stellate") ), 
              test="t.test", map_signif_level=TRUE, textsize=6, y_position = c(310)) +
  scale_fill_manual(values=c("lightcyan4", "lightcyan4")) +
  labs(x="Adaxial Trichomes", y="Adaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_text(size=30), 
        axis.text.x = element_text(size=20), 
        axis.text.y = element_text(size=30),
        legend.position = "none")

#fix levels on $species so that parents plot above hybrids
avg_data <- avg_data %>% mutate(species = factor(species, levels=c("hybrid", "jucundum", "lautum")))

#saving plots at 10 x 8 landscapes
#scatter plots: blade area by teeth shaded by abaxial not stellate/stellate
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=blade_area, y=teeth, fill=locality, shape=as.factor(ablam_stellate), size=8)) + 
  scale_shape_manual(values=c(21,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  labs(x="Blade Area", y='Marginal Teeth') +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.position = "none")

#scatter plots: blade area by teeth shaded by adaxial not stellate/stellate
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=blade_area, y=teeth, fill=locality, shape=as.factor(adlam_stellate), size=8)) + 
  scale_shape_manual(values=c(21,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  labs(x="Blade Area", y='Marginal Teeth') +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.position = "none")

#scatter plots: abaxial/adaxial trichome density shaded by abaxial not stellate/stellate
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=trichomes_ab, y=trichomes_ad, fill=locality, shape=as.factor(ablam_stellate), size=8)) + 
  scale_shape_manual(values=c(21,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  labs(x="Abaxial Trichome Density", y='Adaxial Trichome Denisty') +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.position = "none")

#scatter plots: abaxial/adaxial trichome density shaded by adaxial not stellate/stellate
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=trichomes_ab, y=trichomes_ad, fill=locality, shape=as.factor(adlam_stellate), size=8)) + 
  scale_shape_manual(values=c(21,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  labs(x="Abaxial Trichome Density", y='Adaxial Trichome Denisty') +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.position = "none")



#t.test between blade areas, check to match significance assessed by geom_signif(test="t.test")
#default t.test is Welch's t-test/Welch two sample t-test
#p-value is less than 0.05, *. less than 0.01, **. less than 0.001, ***
#blade area
t.test(avg_hybs$blade_area, avg_juc$blade_area) #p-value = 9.483e-06 ***
t.test(avg_hybs$blade_area, avg_laut$blade_area) #p-value = p-value = 0.005454 **
t.test(avg_laut$blade_area, avg_juc$blade_area) #p-value = 6.31e-06 ***

#reorder samples so species match order in figure 2
avg_data <- avg_data %>% mutate(species = factor(species, levels=c("lautum", "jucundum", "hybrid")))

#save boxplot figures as 6 by 7 inches landscape for now
#blade area comparisons for lautum, jucundum, hybrids (all significantly different)
ggplot(data=avg_data, aes(x=species, y=blade_area)) + 
  geom_boxplot(aes(fill=as.factor(species))) +
  geom_signif(comparisons = list(c("lautum", "hybrid"), c("jucundum", "lautum"), c("jucundum", "hybrid") ), 
              test="t.test", map_signif_level=TRUE, textsize=6, y_position = c(128, 118, 108)) +
  scale_fill_manual(values=c("darkgoldenrod1", "lightpink3", "lightcyan4")) +
  labs(x="Species", y= bquote('Blade Area'~(cm^2))) +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_text(size=30), 
        axis.text.x = element_text(size=20), 
        axis.text.y = element_text(size=30))

#blade length/width comparisons for lautum, jucundum, hybrids (none significantly different)
ggplot(data=avg_data, aes(x=species, y=length_by_width)) + 
  geom_boxplot(aes(fill=as.factor(species))) +
  geom_signif(comparisons = list(c("lautum", "hybrid"), c("jucundum", "lautum"), c("jucundum", "hybrid") ), 
              test="t.test", map_signif_level=TRUE, textsize=6, y_position = c(2.3, 2.5, 2.6)) +
  scale_fill_manual(values=c("darkgoldenrod1", "lightpink3", "lightcyan4")) +
  labs(x="Species", y="Blade Length/Width") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_text(size=30), 
        axis.text.x = element_text(size=20), 
        axis.text.y = element_text(size=30))

#teeth comparisons for lautum, jucundum, hybrids (all significantly different)
#YA41 is missing for teeth and trichome measurements but has blade area and structure
ggplot(data=avg_data, aes(x=species, y=teeth)) + 
  geom_boxplot(aes(fill=as.factor(species))) +
  geom_signif(comparisons = list(c("lautum", "hybrid"), c("jucundum", "lautum"), c("jucundum", "hybrid") ), 
              test="t.test", map_signif_level=TRUE, textsize=6, y_position = c(39, 43, 46)) +
  scale_fill_manual(values=c("darkgoldenrod1", "lightpink3", "lightcyan4")) +
  labs(x="Species", y="Marginal Teeth") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_text(size=30), 
        axis.text.x = element_text(size=20), 
        axis.text.y = element_text(size=30))

#abaxial trichome density comparisons for lautum, jucundum, hybrids (all significantly different)
#YA41 is missing for teeth and trichome measurements but has blade area and structure
ggplot(data=avg_data, aes(x=species, y=trichomes_ab)) + 
  geom_boxplot(aes(fill=as.factor(species))) +
  geom_signif(comparisons = list(c("lautum", "hybrid"), c("jucundum", "lautum"), c("jucundum", "hybrid") ), 
              test="t.test", map_signif_level=TRUE, textsize=6, y_position = c(235, 257, 270)) +
  scale_fill_manual(values=c("darkgoldenrod1", "lightpink3", "lightcyan4")) +
  labs(x="Species", y="Abaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_text(size=30), 
        axis.text.x = element_text(size=20), 
        axis.text.y = element_text(size=30))

#adaxial trichome density comparisons for lautum, jucundum, hybrids (all significantly different)
#YA41 is missing for teeth and trichome measurements but has blade area and structure
ggplot(data=avg_data, aes(x=species, y=trichomes_ad)) + 
  geom_boxplot(aes(fill=as.factor(species))) +
  geom_signif(comparisons = list(c("lautum", "hybrid"), c("jucundum", "lautum"), c("jucundum", "hybrid") ), 
              test="t.test", map_signif_level=TRUE, textsize=6, y_position = c(305, 330, 345)) +
  scale_fill_manual(values=c("darkgoldenrod1", "lightpink3", "lightcyan4")) +
  labs(x="Species", y="Adaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_text(size=30), 
        axis.text.x = element_text(size=20), 
        axis.text.y = element_text(size=30))

#trait by trait plots: blade area, teeth, trichab, trichad
#fix levels on $species so that parents plot above hybrids
avg_data <- avg_data %>% mutate(species = factor(species, levels=c("hybrid", "jucundum", "lautum")))

#save boxplot figures as 10 by 8 inches landscape for now
#blade area by teeth
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=blade_area, y=teeth, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
#  geom_abline(intercept=3.275, slope=20.641, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Blade Area", y='Marginal Teeth') +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))

#blade area by abaxial trichomes
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=blade_area, y=trichomes_ab, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  #  geom_abline(intercept=3.275, slope=20.641, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Blade Area", y='Abaxial Trichomes') +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))

#blade area by adaxial trichomes
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=blade_area, y=trichomes_ad, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  #  geom_abline(intercept=3.275, slope=20.641, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Blade Area", y='Adaxial Trichomes') +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))

#abaxial by adaxial trichomes
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=trichomes_ab, y=trichomes_ad, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  #  geom_abline(intercept=3.275, slope=20.641, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Abaxial Trichomes", y='Adaxial Trichomes') +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))

#teeth by abaxial trichomes
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=teeth, y=trichomes_ab, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  #  geom_abline(intercept=3.275, slope=20.641, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Marginal Teeth", y='Abaxial Trichomes') +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))

#teeth by adaxial trichomes
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=teeth, y=trichomes_ad, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  #  geom_abline(intercept=3.275, slope=20.641, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Marginal Teeth", y='Adaxial Trichomes') +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(face="bold", size=20), axis.text.y = element_text(face="bold", size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))


##############################################
#plot log trait values(blade area, length/width, teeth, adaxial trichomes, abaxial trichomes) by ancestry 
#point color by assignment and shape by region
##############################################
#fix levels on $species so that parents plot above hybrids
avg_data <- avg_data %>% mutate(species = factor(species, levels=c("hybrid", "jucundum", "lautum")))
#or if you want hybrids above parents
avg_data <- avg_data %>% mutate(species = factor(species, levels=c("jucundum", "lautum", "hybrid")))
#compute regressions
#blade area
regress_juc_blade_area <- lm(avg_hybs$log_blade_area~avg_hybs$X12_7_hyb_only)
summary.lm(regress_juc_blade_area)
#intercept=1.58917, slope=1.52362  R^2:0.5733  P:<2e-16 ***
#length/width
regress_juc_lw <- lm(avg_hybs$log_length_by_width~avg_hybs$X12_7_hyb_only)
summary.lm(regress_juc_lw)
#intercept=0.47951, slope=0.03399  R^2:0.002065  P:0.267
#teeth
regress_juc_teeth <- lm(avg_hybs$log_teeth~avg_hybs$X12_7_hyb_only)
summary.lm(regress_juc_teeth)
#intercept=1.0868,  slope=2.2079  R^2:0.5305  P:<2e-16 ***
#trichab
regress_juc_trichab <- lm(avg_hybs$log_trichomes_ab~avg_hybs$X12_7_hyb_only)
summary.lm(regress_juc_trichab)
#intercept=0.5815, slope=4.6589  R^2:0.4611 P:<2e-16 ***
#trichad
regress_juc_trichad <- lm(avg_hybs$log_trichomes_ad~avg_hybs$X12_7_hyb_only)
summary.lm(regress_juc_trichad)
#intercept=1.2177, slope=4.5344  R^2:0.3865  P:3.50e-14 ***

#####correlations between 'jucundum' ancestry and traits, hybrids and parents from separate structure runs, k=2 mincov=30
#####color by locality, different shades of same pallete for hybrids/juc/laut, shapes for assigment, 
#####trendline for hybrids only, juc:"lightpink3" laut:"darkgoldenrod1" hybrid:"lightcyan4" 
#blade area
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=log_blade_area, fill=locality, shape=assignment), size=8, alpha=0.8) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=1.58917, slope=1.52362, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y=bquote('Log Blade Area'~(cm^2))) +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
#blade length/width
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=log_length_by_width, fill=locality, shape=assignment), size=8, alpha=0.8) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=0.47951, slope=0.03399, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Log Blade Length/Width") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
#teeth
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=log_teeth, fill=locality, shape=assignment), size=8, alpha=0.8) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=1.0868,  slope=2.2079, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Log Marginal Teeth") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
#trichab
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=log_trichomes_ab, fill=locality, shape=assignment), size=8, alpha=0.8) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=0.5815, slope=4.6589, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Log Abaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
#trichad
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=log_trichomes_ad, fill=locality, shape=assignment), size=8, alpha=0.8) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=1.2177, slope=4.5344, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Log Adaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
######same plots as above but traits are not log transformed
#first, regressions
#blade area
reg_blade_area <- lm(avg_hybs$blade_area~avg_hybs$X12_7_hyb_only)
summary.lm(reg_blade_area)
#intercept=3.276, slope=20.641  R^2:0.4279  P:4.36e-16 ***
#length/width
reg_lw <- lm(avg_hybs$length_by_width~avg_hybs$X12_7_hyb_only)
summary.lm(reg_lw)
#intercept=1.62576, slope=0.05386  R^2:0.0009511  P:0.294
#teeth
reg_teeth <- lm(avg_hybs$teeth~avg_hybs$X12_7_hyb_only)
summary.lm(reg_teeth)
#intercept=3.0608,  slope=18.2375  R^2:0.6322  P:< 2e-16 ***
#trichab
reg_trichab <- lm(avg_hybs$trichomes_ab~avg_hybs$X12_7_hyb_only)
summary.lm(reg_trichab)
#intercept=1.341, slope=112.445  R^2:0.4371 P:<2e-16 ***
#trichad
reg_trichad <- lm(avg_hybs$trichomes_ad~avg_hybs$X12_7_hyb_only)
summary.lm(reg_trichad)
#intercept=22.469, slope=124.412  R^2:0.3595  P:4.38e-13 ***
#blade area
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=blade_area, fill=locality, shape=assignment), size=8, alpha=0.8) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=3.276, slope=20.641, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y='Blade Area') +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=blade_area, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=3.276, slope=20.641, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y='Blade Area') +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
#blade length/width
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=length_by_width, fill=locality, shape=assignment), size=8, alpha=0.8) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=1.62576, slope=0.05386, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Blade Length/Width") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=length_by_width, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=1.62576, slope=0.05386, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Blade Length/Width") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
#teeth
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=teeth, fill=locality, shape=assignment), size=8, alpha=0.8) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=3.0608,  slope=18.2375, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Marginal Teeth") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=teeth, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=3.0608,  slope=18.2375, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Marginal Teeth") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
#trichab
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=trichomes_ab, fill=locality, shape=assignment), size=8, alpha=0.8) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=1.341, slope=112.445, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Abaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=trichomes_ab, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=1.341, slope=112.445, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Abaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
#trichad
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=trichomes_ad, fill=locality, shape=assignment), size=8, alpha=0.8) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=22.469, slope=124.412, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Adaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=trichomes_ad, fill=locality, shape=species), size=8, alpha=0.9) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=22.469, slope=124.412, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Adaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
#just to check colors
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=trichomes_ad, color=locality), size=8, alpha=0.9) + 
  scale_color_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=22.469, slope=124.412, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Adaxial Trichomes") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))

#correlations between 'jucundum' ancestry and traits, hybrids and parents from separate structure runs, k=2 mincov=30
#####color by locality, different shades of same pallete for hybrids/juc/laut, shapes for assigment
#juc:"lightpink3" laut:"darkgoldenrod1" hybrid:"lightcyan4" 
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=log_blade_area, fill=locality, shape=assignment), size=8, alpha=0.8) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  labs(x="Percent 'jucundum' Ancestry", y="Log Blade Area (cm^2)") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
#####above, but with trend line for hybrids only
#compute regression
regress_juc_blade_area <- lm(avg_hybs$log_blade_area~avg_hybs$X12_7_hyb_only)
summary.lm(regress_juc_blade_area)
#intercept:1.58866  slope:1.53113  R^2:0.5736  P:<2e-16 ***
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=log_blade_area, fill=locality, shape=assignment), size=8, alpha=0.8) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","darkgoldenrod1","indianred","lightcyan4","lightsteelblue3")) +
  geom_abline(intercept=1.58866, slope=1.53113, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Log Blade Area (cm^2)") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))
#####what I might prefer
ggplot(avg_data %>% arrange(species)) +
  geom_point(aes(x=X12_7_hyb_only, y=log_blade_area, fill=species, shape=species), size=8, alpha=0.8) + 
  scale_shape_manual(values=c(22,24,21)) +
  scale_fill_manual(values=c("lightpink3","darkgoldenrod3","lightcyan4")) +
  geom_abline(intercept=1.58866, slope=1.53113, size=4, color="midnightblue", linetype="solid") + 
  labs(x="Percent 'jucundum' Ancestry", y="Log Blade Area (cm^2)") +
  theme(panel.background=element_rect(fill = "gray96"), axis.title.x = element_text(face="bold", size=20), 
        axis.title.y = element_text(face="bold", size=20), 
        axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
        legend.title = element_text(face="bold", size=20), 
        legend.text = element_text(size=18))



##############################################
#making the averages dataframe used above from the raw data
##############################################
#organize data into better format
#I put copies of the original data files into the admixture_figures directory for ease
#then checked good_measurement=false measurements, replaced with nas if actually weird measurement
#also changed individual names from original files to match rad file names from strucutre
#found that all z18 leaves were named z19 overall, but individual leaves were named z18 -> fixed, z18 now correct
#load files
parents <- read.csv(file='jucundum_lautum_parent_measurements.csv', header=TRUE)
hybrids <- read.csv(file='admixture_measurement_master.csv', header=TRUE)
#coerce some columns to numeric
parents$trichomes_ab <- as.numeric(parents$trichomes_ab)
parents$trichomes_ad <- as.numeric(parents$trichomes_ad)
parents$teeth <- as.numeric(parents$teeth)
hybrids$trichomes_ab <- as.numeric(hybrids$trichomes_ab)
hybrids$trichomes_ad <- as.numeric(hybrids$trichomes_ad)
hybrids$teeth <- as.numeric(hybrids$teeth)
#add some transformed measurements
#divide teeth by perimeter
parents$teeth_by_perimeter <- parents$teeth / parents$blade_perimeter
hybrids$teeth_by_perimeter <- hybrids$teeth / hybrids$blade_perimeter
#divide length by width
parents$length_by_width <- parents$blade_length / parents$blade_width
hybrids$length_by_width <- hybrids$blade_length / hybrids$blade_width
#combine parents and hybrids
all_leaves <- rbind(hybrids, parents)
#get averages for all individuals
ind_avg <- all_leaves %>% group_by(species,locality,individual) %>% summarise(petiole_length=mean(petiole_length, na.rm=TRUE),
                                                                                     blade_length=mean(blade_length, na.rm=TRUE),
                                                                                     blade_width=mean(blade_width, na.rm=TRUE),
                                                                                     blade_area=mean(blade_area, na.rm=TRUE),
                                                                                     blade_perimeter=mean(blade_perimeter, na.rm=TRUE),
                                                                                     blade_circularity=mean(blade_circularity, na.rm=TRUE),
                                                                                     trichomes_ab=mean(trichomes_ab, na.rm=TRUE),
                                                                                     trichomes_ad=mean(trichomes_ad, na.rm=TRUE),
                                                                                     teeth=mean(teeth, na.rm=TRUE),
                                                                                     teeth_by_perimeter=mean(teeth_by_perimeter, na.rm=TRUE),
                                                                                     length_by_width=mean(length_by_width, na.rm=TRUE))
#ind_avg should be good going forward to structure and gemma traits, export to admixture_figures directory
#write.csv(ind_avg, "/Users/morganmoeglein/Desktop/admixture_figures/ind_avg.csv", row.names = FALSE)









