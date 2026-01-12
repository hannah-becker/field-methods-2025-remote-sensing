# requirements ----
library(hsdar) #install from source
library(tidyverse)
library(ggplot2)
library(asdreader)

# data ----
list <- list.files("./data/hyperspectral", pattern = "*.asd", full.names = TRUE)

asd <- read.ASD(list, type = "reflectance")


# this is the stuff I was working on in the field, keeping it in on principle but I think there are more elegant solutions, skip ahead for those ----

#Calluna vulgaris plot 1 spectra are in asd[51:65] and plot 2 spectra are in asd[11:30]
#edit: there were more, see below

png("./viz/Calluna_all_spectra")
plot(asd, FUN = 11:30, col = "orange") #plot2
plot(asd, FUN = 51:65, col = "darkgreen", new = FALSE) #plot1
dev.off()
#medians
png("./viz/Calluna_median_spectra")
plot(asd[11:30], FUN = median, col = "orange", lwd = 2) #plot2
plot(asd[51:65], FUN = median, col = "darkgreen", new = FALSE, lwd = 2) #plot1
dev.off()


#ndvi und ndwi nur für Calluna

ndvi_1 <- vegindex(asd[51:65], "NDVI")
ndvi_2 <- vegindex(asd[11:30], "NDVI")
ndwi_1 <- vegindex(asd[51:65], "NDWI")
ndwi_2 <- vegindex(asd[11:30], "NDWI")
index <- data.frame(ndvi = c(ndvi_1, ndvi_2), ndwi = c(ndwi_1, ndwi_2), plot = c(rep("dry", 15), rep("wet", 20)))

png("./viz/ndwi_Calluna.png")
ggplot(index, aes(x = plot, y = ndwi))+
  geom_boxplot()+
  scale_color_viridis_d()+
  theme_classic()
dev.off()

#derivations

deriv_1 <- derivative.speclib(asd, m = 1)
deriv_2 <- derivative.speclib(asd, m = 2)

png("./viz/Calluna_deriv1_median_spectra")
plot(deriv_1[51:65], FUN = median, col = "darkgreen", lwd = 2)
plot(deriv_1[11:30], FUN = median, col = "orange", new = FALSE, lwd = 2)
dev.off()

# ============================================ #

# continue here ----

#I think we should extract the spectra for prettier plots though bc the hsdar plotting options are so far pretty inflexible

deriv_1 <- derivative.speclib(asd, m = 1) #first derivative

data <- data.frame(asd@spectra@spectra_ma, deriv_1@spectra@spectra_ma)
colnames(data) <- c(asd@wavelength, paste0("deriv_", deriv_1@wavelength))
data$file <- c(paste0("G2_", 1:95))
data$plot <- c(rep(2, 45), rep(1, 45), rep(2, 5))  
#plot 1 was drier, plot 2 more wet
x <- c()
for (i in c("Veronica_officinalis", "Empetrum_nigrum", "Calluna_vulgaris", "Calluna_vulgaris", "Calluna_vulgaris", "Calluna_vulgaris", "Erica_tetralix", "Erica_tetralix", "Sphagnum_sp", "Festuca_ovina", "Calluna_vulgaris", "Calluna_vulgaris", "Calluna_vulgaris", "Galium_saxatile", "Calluna_vulgaris", "Molinia_caerulea", "Moos", "Calluna_vulgaris", "Eleocharis_sp")) {
  x <- c(x, rep(i, 5))
}
data$species <- x

#add derivs
spectra <- pivot_longer(data, !c(file, plot, species), names_to = "wavelength", values_to = "reflectance")
spectra$wavelength <- as.numeric(spectra$wavelength)
spectra$plot <- as.factor(spectra$plot)
spectra$reflectance <- spectra$reflectance * 100

write.csv(data, "./data/hyperspectral_combined.csv")

#for plot 2
subset(spectra, species == "Calluna_vulgaris" & plot == "2") %>% 
  ggplot(., aes(x = wavelength, y = reflectance))+
  geom_line(stat = "summary", fun = "mean", color = "darkgreen")+ #plots mean of all files
  theme_classic()+
  ylab("reflectance [%]")+
  xlab("wavelength [nm]")+
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(limits = c(NA, 40)) 
ggsave("Calluna_mean_plot2.png", path = "./visualizations", height = 700, width = 1500, units = "px")

#for plot 1
subset(spectra, species == "Calluna_vulgaris" & plot == "1") %>% 
  ggplot(., aes(x = wavelength, y = reflectance))+
  geom_line(stat = "summary", fun = "mean", color = "orange")+
  theme_classic()+
  ylab("reflectance [%]")+
  xlab("wavelength [nm]")+
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(limits = c(NA, 40))
ggsave("Calluna_mean_plot1.png", path = "./visualizations", height = 700, width = 1500, units = "px")

#visualize both together
#set colors
cols <- c("darkgreen", "orange")
names(cols) <- c("2", "1")

subset(spectra, species == "Calluna_vulgaris") %>%
  ggplot(., aes(x = wavelength, y = reflectance, color = plot))+
  geom_line(stat = "summary", fun = "mean")+
  theme_classic()+
  scale_color_manual(values = cols, labels = c("dry", "wet"))+
  theme(legend.position = c(0.9, 0.8), legend.key.size = unit(1, "char"))+
  ylab("reflectance [%]")+
  xlab("wavelength [nm]")+
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(limits = c(NA, 40))
ggsave("Calluna_mean_comparison.png", path = "./visualizations", height = 700, width = 1500, units = "px")
