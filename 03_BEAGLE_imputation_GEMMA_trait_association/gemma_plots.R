library(ggplot2)
library(qqman)
library(tidyr)
library(dplyr)
library(stringr)
library(reshape2) 
library(tidyverse)

setwd("~/desktop/Mexico_admixture/gemma_output")

##################################
#make plots for composite figures 
#load files
#output for impute_20 (blade area)
gemma_ba <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/phenotype.assoc_rename_contig.qqman20.txt')
#output for impute_20 (length/width)
gemma_low <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/low.assoc_rename_contig.qqman20.txt')
#output for impute_20 (teeth)
gemma_teeth <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/teeth.assoc_rename_contig.qqman20.txt')
#output for impute_20 (abaxial trichomes)
gemma_trichab <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/trichab.assoc_rename_contig.qqman20.txt')
#output for impute_20 (adaxial trichomes)
gemma_trichad <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/trichad.assoc_rename_contig.qqman20.txt')
#trichome branching results
#output for impute_20 (abaxial lamina trichome branching)
gemma_ab_branch <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/binary_trait_gemma/ab_branch_phenotype.assoc_rename_contig.qqman20.txt')
#output for impute_20 (adaxial lamina trichome branching)
gemma_ad_branch <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/binary_trait_gemma/ad_branch_phenotype.assoc_rename_contig.qqman20.txt')
#output for impute_20 (margin trichome branching)
gemma_margin_branch <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/binary_trait_gemma/margin_branch_phenotype.assoc_rename_contig.qqman20.txt')
#output for impute_20 (vein trichome branching)
gemma_vein_branch <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/binary_trait_gemma/vein_branch_phenotype.assoc_rename_contig.qqman20.txt')


#sort data frames by number of snps per scaffold, highest to lowest
gemma_ba_sort <- gemma_ba %>% add_count(CHR, sort = TRUE)
#after sorting, rename CHR to ogCHR, make new CHR where scaffolds are sequential (1,2,3, etc.)
gemma_ba_sort <- gemma_ba_sort %>% rename(ogCHR = CHR)
gemma_ba_sort$CHR <- as.numeric(fct_rev(as.factor(gemma_ba_sort$n)))
#and the other traits
gemma_low_sort <- gemma_low %>% add_count(CHR, sort = TRUE)
gemma_low_sort <- gemma_low_sort %>% rename(ogCHR = CHR)
gemma_low_sort$CHR <- as.numeric(fct_rev(as.factor(gemma_low_sort$n)))
gemma_teeth_sort <- gemma_teeth %>% add_count(CHR, sort = TRUE)
gemma_teeth_sort <- gemma_teeth_sort %>% rename(ogCHR = CHR)
gemma_teeth_sort$CHR <- as.numeric(fct_rev(as.factor(gemma_teeth_sort$n)))
gemma_trichab_sort <- gemma_trichab %>% add_count(CHR, sort = TRUE)
gemma_trichab_sort <- gemma_trichab_sort %>% rename(ogCHR = CHR)
gemma_trichab_sort$CHR <- as.numeric(fct_rev(as.factor(gemma_trichab_sort$n)))
gemma_trichad_sort <- gemma_trichad %>% add_count(CHR, sort = TRUE)
gemma_trichad_sort <- gemma_trichad_sort %>% rename(ogCHR = CHR)
gemma_trichad_sort$CHR <- as.numeric(fct_rev(as.factor(gemma_trichad_sort$n)))
#binary traits sort
gemma_ab_branch_sort <- gemma_ab_branch %>% add_count(CHR, sort = TRUE)
gemma_ab_branch_sort <- gemma_ab_branch_sort %>% rename(ogCHR = CHR)
gemma_ab_branch_sort$CHR <- as.numeric(fct_rev(as.factor(gemma_ab_branch_sort$n)))
gemma_ad_branch_sort <- gemma_ad_branch %>% add_count(CHR, sort = TRUE)
gemma_ad_branch_sort <- gemma_ad_branch_sort %>% rename(ogCHR = CHR)
gemma_ad_branch_sort$CHR <- as.numeric(fct_rev(as.factor(gemma_ad_branch_sort$n)))
gemma_margin_branch_sort <- gemma_margin_branch %>% add_count(CHR, sort = TRUE)
gemma_margin_branch_sort <- gemma_margin_branch_sort %>% rename(ogCHR = CHR)
gemma_margin_branch_sort$CHR <- as.numeric(fct_rev(as.factor(gemma_margin_branch_sort$n)))
gemma_vein_branch_sort <- gemma_vein_branch %>% add_count(CHR, sort = TRUE)
gemma_vein_branch_sort <- gemma_vein_branch_sort %>% rename(ogCHR = CHR)
gemma_vein_branch_sort$CHR <- as.numeric(fct_rev(as.factor(gemma_vein_branch_sort$n)))

#manhattans for all traits, just dots: save these as 7 x 30 inches
manhattan(gemma_ba_sort, main = "Blade Area", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
manhattan(gemma_low_sort, main = "Blade Length/Blade Width", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
manhattan(gemma_teeth_sort, main = "Marginal Leaf Teeth", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
manhattan(gemma_trichab_sort, main = "Abaxial Trichome Density", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
manhattan(gemma_trichad_sort, main = "Adaxial Trichome Density", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
manhattan(gemma_ab_branch_sort, main = "Stellate Trichomes on Abaxial Lamina", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
manhattan(gemma_ad_branch_sort, main = "Stellate Trichomes on Adaxial Lamina", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
manhattan(gemma_margin_branch_sort, main = "Stellate Trichomes on Leaf Margins", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
manhattan(gemma_vein_branch_sort, main = "Stellate Trichomes on Veins", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
#manhattans for all traits, annotated dots: 
#suggestive line is annotatePval = 0.00001 (1 x 10^-5), genome wide significance line is 0.00000005
manhattan(gemma_ba_sort, main = "Blade Area", annotatePval = 0.00001, ylim = c(0, 15), 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
manhattan(gemma_low_sort, main = "Blade Length/Blade Width", annotatePval = 0.00001, ylim = c(0, 15), 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
manhattan(gemma_teeth_sort, main = "Marginal Leaf Teeth", annotatePval = 0.00001, ylim = c(0, 15), 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
manhattan(gemma_trichab_sort, main = "Abaxial Trichome Density", annotatePval = 0.00001, ylim = c(0, 15), 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
manhattan(gemma_trichad_sort, main = "Adaxial Trichome Density", annotatePval = 0.00001, ylim = c(0, 15), 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))

###can't anotate branch plots for some reason, had to modify sliightly
manhattan(gemma_ab_branch_sort, main = "Stellate Trichomes on Abaxial Lamina", annotatePval = 0.00001)
manhattan(gemma_ad_branch_sort, main = "Stellate Trichomes on Adaxial Lamina", annotatePval = 0.00001)
manhattan(gemma_margin_branch_sort, main = "Stellate Trichomes on Leaf Margins", annotatePval = 0.00001)
manhattan(gemma_vein_branch_sort, main = "Stellate Trichomes on Veins", annotatePval = 0.00001)
#same as above but with unsorted scaffolds to get better idea of scaffold names
manhattan(gemma_ba, main = "Blade Area", annotatePval = 0.00001, ylim = c(0, 15), 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
manhattan(gemma_low, main = "Blade Length/Blade Width", annotatePval = 0.00001, ylim = c(0, 15), 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))
manhattan(gemma_teeth, main = "Marginal Leaf Teeth", annotatePval = 0.00001, ylim = c(0, 15), 
          cex.axis = 2, col = c("coral4", "lavenderblush4"))

#plotting multiple traits together/on top of each other, save as landscape 7x30 like others
#Blade area (bottom), teeth (middle), and length/width (top): "#682C37", "#F6955E", "#7887A4"
manhattan(gemma_ba_sort, main = "Blade Area", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("#682C37", "#956B73")) 
par(new=TRUE)
manhattan(gemma_teeth_sort, main = "Marginal Leaf Teeth", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("#F6955E", "#F8B48E"))
par(new=TRUE)
manhattan(gemma_low_sort, main = "Blade Length/Blade Width", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("#7887A4", "#A0ABBF"))

#abaxial trichome density (bottom) and adaxial trichome density (top): "#466D53", "#A79CA5"
manhattan(gemma_trichab_sort, main = "Abaxial Trichome Density", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("#857C84", "#A79CA5")) 
par(new=TRUE)
manhattan(gemma_trichad_sort, main = "Adaxial Trichome Density", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("#466D53", "#7D9886"))

#stellate trichomes: abaxial blade, adaxial blade, margin, veins -> brown, yellow, navy, 
manhattan(gemma_ad_branch_sort, main = "Stellate Trichomes on Adaxial Lamina", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("#E39B38", "#EBB973"))
par(new=TRUE)
manhattan(gemma_vein_branch_sort, main = "Stellate Trichomes on Veins", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("#A12A19", "#BD695E"))
par(new=TRUE)
manhattan(gemma_ab_branch_sort, main = "Stellate Trichomes on Abaxial Lamina", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("#787066", "#A0978C"))
par(new=TRUE)
manhattan(gemma_margin_branch_sort, main = "Stellate Trichomes on Leaf Margins", ylim = c(0, 15), cex = 5, 
          cex.axis = 2, col = c("#1F304A", "#626E80"))

##################################################
#now more investigation of significant snps
#for each trait, take snps above "suggestive" line (significance less than 0.00001)
sig_ba <- subset(gemma_ba, P <= 0.00001)
#then make new column for trait so we can combine trait analyses later
sig_ba$trait <- 'ba'
#for all traits (except low with no sig snps)
sig_teeth <- subset(gemma_teeth, P <= 0.00001)
sig_teeth$trait <- 'teeth'
sig_trichab <- subset(gemma_trichab, P <= 0.00001)
sig_trichab$trait <- 'trichab'
sig_trichad <- subset(gemma_trichad, P <= 0.00001)
sig_trichad$trait <- 'trichad'
#put all the sig snps together
sig_traits <- rbind(sig_ba, sig_teeth, sig_trichab, sig_trichad)
#make new data frame of snps that are significant for more than one trait (repeat value for SNP in sig_traits)
sig_overlap <- sig_traits %>% group_by(SNP) %>% filter(n() > 1)

#read in allele information for significant snps
snps_rah <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/hybrids_impute_20_hetaltref.tsv')
snps_ra <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/hybrids_impute_20_altref.tsv')
#read in trait data
traits <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/gemma_traits.tsv')
snps_rah_m <- snps_rah[,-c(1,2,4:9)]
snps_rah_t = setNames(data.frame(t(snps_rah_m[,-1])), snps_rah_m[,1])
snps_ra_m <- snps_ra[,-c(1,2,4:9)]
snps_ra_t = setNames(data.frame(t(snps_ra_m[,-1])), snps_ra_m[,1])
snp_trait_rah <- cbind(traits, snps_rah_t)
snp_trait_ra <- cbind(traits, snps_ra_t)

##################
#plotting traits for significant snps
#for ba, snp of strongest effect loc67757_pos12
ggplot(snp_trait_ra, aes(x=loc67757_pos12, y=ba)) + 
  geom_boxplot()
#for dissertation figure: blade area
ggplot(data=snp_trait_ra, aes(x=loc67757_pos12, y=ba)) + 
  geom_boxplot(fill = 'lightskyblue4') +
  ggtitle("Blade Area (cm ^2)") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
#        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=20))
#blade area with swarms plotted separately
ggplot(data=snp_trait_ra, aes(x=loc67757_pos12, y=ba, fill=swarm)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("lightcyan3","lightcyan4")) +
  ggtitle("Blade Area (cm ^2)") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        #        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=20))

ggplot(snp_trait_ra, aes(x=loc67757_pos12, y=teeth)) + 
  geom_boxplot()
ggplot(snp_trait_ra, aes(x=loc67757_pos12, y=trichab)) + 
  geom_boxplot()
ggplot(snp_trait_ra, aes(x=loc67757_pos12, y=trichad)) + 
  geom_boxplot()
#for teeth, snp of strongest effect loc9204_pos147
ggplot(snp_trait_ra, aes(x=loc9204_pos147, y=ba)) + 
  geom_boxplot()
ggplot(snp_trait_ra, aes(x=loc9204_pos147, y=teeth)) + 
  geom_boxplot()

#for dissertation figure: teeth
ggplot(data=snp_trait_ra, aes(x=loc9204_pos147, y=teeth)) + 
  geom_boxplot(fill = 'lightskyblue4') +
  ggtitle("Marginal Teeth") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        #        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=20))
#teeth with swarms plotted separately
ggplot(data=snp_trait_ra, aes(x=loc9204_pos147, y=teeth, fill=swarm)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("lightcyan3","lightcyan4")) +
  ggtitle("Marginal Teeth") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        #        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=20))

ggplot(snp_trait_ra, aes(x=loc9204_pos147, y=trichab)) + 
  geom_boxplot()
ggplot(snp_trait_ra, aes(x=loc9204_pos147, y=trichad)) + 
  geom_boxplot()
#for trichab, snp of strongest effect loc119342_pos200
ggplot(snp_trait_ra, aes(x=loc119342_pos200, y=ba)) + 
  geom_boxplot()
ggplot(snp_trait_ra, aes(x=loc119342_pos200, y=teeth)) + 
  geom_boxplot()
ggplot(snp_trait_ra, aes(x=loc119342_pos200, y=trichab)) + 
  geom_boxplot()

#for dissertation figure: trichab
ggplot(data=snp_trait_ra, aes(x=loc119342_pos200, y=trichab)) + 
  geom_boxplot(fill = 'lightskyblue4') +
  ggtitle("Abaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        #        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=20))
#trichab with swarms plotted separately
ggplot(data=snp_trait_ra, aes(x=loc119342_pos200, y=trichab, fill=swarm)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("lightcyan3","lightcyan4")) +
  ggtitle("Abaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        #        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=20))

ggplot(snp_trait_ra, aes(x=loc119342_pos200, y=trichad)) + 
  geom_boxplot()
#for trichad, snp of strongest effect loc71590_pos40
ggplot(snp_trait_ra, aes(x=loc71590_pos40, y=ba)) + 
  geom_boxplot()
ggplot(snp_trait_ra, aes(x=loc71590_pos40, y=teeth)) + 
  geom_boxplot()
ggplot(snp_trait_ra, aes(x=loc71590_pos40, y=trichab)) + 
  geom_boxplot()
ggplot(snp_trait_ra, aes(x=loc71590_pos40, y=trichad)) + 
  geom_boxplot()

#for dissertation figure: trichad
ggplot(data=snp_trait_ra, aes(x=loc71590_pos40, y=trichad)) + 
  geom_boxplot(fill = 'lightskyblue4') +
  ggtitle("Adaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        #        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=20))
#trichad with swarms plotted separately
ggplot(data=snp_trait_ra, aes(x=loc71590_pos40, y=trichad, fill=swarm)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("lightcyan3","lightcyan4")) +
  ggtitle("Adaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        #        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=20))

#boxplots with reference allele on left and alternative allele on right
#read in allele information for significant snps
snps_rah <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/hybrids_impute_20_hetaltref.tsv')
snps_ra <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/hybrids_impute_20_altref.tsv')
#read in trait data
traits <- read.delim(file='~/desktop/Mexico_admixture/gemma_output/gemma_traits.tsv')
#recast snps_* and remove rows so that it will be compatible with traits
snps_rah_m <- snps_rah[,-c(1,2,4:9)]
snps_rah_t = setNames(data.frame(t(snps_rah_m[,-1])), snps_rah_m[,1])
snps_ra_m <- snps_ra[,-c(1,2,4:9)]
snps_ra_t = setNames(data.frame(t(snps_ra_m[,-1])), snps_ra_m[,1])
#combine snps and traits 
snp_trait_rah <- cbind(traits, snps_rah_t)
snp_trait_ra <- cbind(traits, snps_ra_t)

#############################
### trait changes for significant loci
## save all as 3x8
#blade area
# loc67757_pos12 (snp1)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc67757_pos12 = factor(loc67757_pos12, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc67757_pos12, y=ba)) + 
  geom_boxplot(fill = "#72874E") +
#  ggtitle("Blade Area (cm ^2)") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc82358_pos140 (snp2)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc82358_pos140 = factor(loc82358_pos140, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc82358_pos140, y=ba)) + 
  geom_boxplot(fill = "#72874E") +
  #  ggtitle("Blade Area (cm ^2)") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc82358_pos175 (snp3)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc82358_pos175 = factor(loc82358_pos175, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc82358_pos175, y=ba)) + 
  geom_boxplot(fill = "#72874E") +
  #  ggtitle("Blade Area (cm ^2)") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc26647_pos166 (snp4)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc26647_pos166 = factor(loc26647_pos166, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc26647_pos166, y=ba)) + 
  geom_boxplot(fill = "#72874E") +
  #  ggtitle("Blade Area (cm ^2)") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc81775_pos55 (snp5)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc81775_pos55 = factor(loc81775_pos55, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc81775_pos55, y=ba)) + 
  geom_boxplot(fill = "#72874E") +
  #  ggtitle("Blade Area (cm ^2)") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
#adding a couple more snps as top 3 are shared by the same two: loc100383_pos96 and loc95800_pos200
# loc100383_pos96 (snp6)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc100383_pos96 = factor(loc100383_pos96, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc100383_pos96, y=ba)) + 
  geom_boxplot(fill = "#72874E") +
  #  ggtitle("Blade Area (cm ^2)") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc95800_pos200 (snp7)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc95800_pos200 = factor(loc95800_pos200, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc95800_pos200, y=ba)) + 
  geom_boxplot(fill = "#72874E") +
  #  ggtitle("Blade Area (cm ^2)") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
#teeth
# loc9204_pos147 (snp1)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc9204_pos147 = factor(loc9204_pos147, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc9204_pos147, y=teeth)) + 
  geom_boxplot(fill = "#476F84") +
#  ggtitle("Teeth") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc125360_pos80 (snp2)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc125360_pos80 = factor(loc125360_pos80, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc125360_pos80, y=teeth)) + 
  geom_boxplot(fill = "#476F84") +
  #  ggtitle("Teeth") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc88589_pos166 (snp3)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc88589_pos166 = factor(loc88589_pos166, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc88589_pos166, y=teeth)) + 
  geom_boxplot(fill = "#476F84") +
  #  ggtitle("Teeth") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc36106_pos11 (snp4)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc36106_pos11 = factor(loc36106_pos11, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc36106_pos11, y=teeth)) + 
  geom_boxplot(fill = "#476F84") +
  #  ggtitle("Teeth") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc68029_pos67 (snp5)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc68029_pos67 = factor(loc68029_pos67, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc68029_pos67, y=teeth)) + 
  geom_boxplot(fill = "#476F84") +
  #  ggtitle("Teeth") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))

#trichab
#how many individuals with alt allele? what sample_name
which(snp_trait_ra$loc119342_pos200 == "alt") #(5 individuals),  ->51  74  90  93 106
which(snp_trait_ra$loc71590_pos40 == "alt") #(19 individuals),  ->3  12  46  51  53  54  55  59  60  62  64  66  67  71  75  95  99 106 113
which(snp_trait_ra$loc78935_pos39 == "alt") #(3 individuals),  -> 51 74 99
which(snp_trait_ra$loc114008_pos58 == "alt") #(8 individuals),  -> 24  26  51  54  60  74  75 106
which(snp_trait_ra$loc91337_pos23 == "alt") #(4 individuals),  -> 51 55 74 75
# loc119342_pos200 (snp1)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc119342_pos200 = factor(loc119342_pos200, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc119342_pos200, y=trichab)) + 
  geom_boxplot(fill = "#8D7F99") +
#  ggtitle("Abaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc71590_pos40 (snp2)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc71590_pos40 = factor(loc71590_pos40, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc71590_pos40, y=trichab)) + 
  geom_boxplot(fill = "#8D7F99") +
  #  ggtitle("Abaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc78935_pos39 (snp3)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc78935_pos39 = factor(loc78935_pos39, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc78935_pos39, y=trichab)) + 
  geom_boxplot(fill = "#8D7F99") +
  #  ggtitle("Abaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc114008_pos58 (snp4)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc114008_pos58 = factor(loc114008_pos58, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc114008_pos58, y=trichab)) + 
  geom_boxplot(fill = "#8D7F99") +
  #  ggtitle("Abaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc91337_pos23 (snp5)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc91337_pos23 = factor(loc91337_pos23, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc91337_pos23, y=trichab)) + 
  geom_boxplot(fill = "#8D7F99") +
  #  ggtitle("Abaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))

#trichad: top 5 snps and effect size as boxplots -> loc71590_pos40,loc15949_pos141,loc121914_pos96,loc130375_pos101,loc17612_pos122
#how many individuals with alt allele? what sample_name
which(snp_trait_ra$loc71590_pos40 == "alt") #(19 individuals), 3  12  46  51  53  54  55  59  60  62  64  66  67  71  75  95  99 106 113 ->
which(snp_trait_ra$loc15949_pos141 == "alt") #(3 individuals), 61 62 99 ->
which(snp_trait_ra$loc121914_pos96 == "alt") #(3 individuals), 79 95 99 -> 
which(snp_trait_ra$loc130375_pos101 == "alt") #(5 individuals), 46 53 59 95 99 ->
which(snp_trait_ra$loc17612_pos122 == "alt") #(2 individuals), 95 99 -> 
# loc71590_pos40 (snp1)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc71590_pos40 = factor(loc71590_pos40, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc71590_pos40, y=trichad)) + 
  geom_boxplot(fill = 'lightcyan3') +
  #ggtitle("Adaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc15949_pos141 (snp2)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc15949_pos141 = factor(loc15949_pos141, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc15949_pos141, y=trichad)) + 
  geom_boxplot(fill = 'lightcyan3') +
  #ggtitle("Adaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc121914_pos96 (snp3)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc121914_pos96 = factor(loc121914_pos96, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc121914_pos96, y=trichad)) + 
  geom_boxplot(fill = 'lightcyan3') +
  #ggtitle("Adaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc130375_pos101 (snp4)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc130375_pos101 = factor(loc130375_pos101, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc130375_pos101, y=trichad)) + 
  geom_boxplot(fill = 'lightcyan3') +
  #ggtitle("Adaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))
# loc17612_pos122 (snp5)
#reorder levels so ref is on left and alt on right
snp_trait_ra <- snp_trait_ra %>% mutate(loc17612_pos122 = factor(loc17612_pos122, levels=c("ref","alt")))
ggplot(data=snp_trait_ra, aes(x=loc17612_pos122, y=trichad)) + 
  geom_boxplot(fill = 'lightcyan3') +
  #ggtitle("Adaxial Trichome Density") +
  theme(panel.background=element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=30))



