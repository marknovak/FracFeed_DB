#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load saved database
load('../tmp/tmp_DB/FracFeed_Data_Clean.Rdata')
load('../tmp/FracFeed_Data_FactorLevels.Rdata')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ldat = dat[, c('Latitude',
               'Longitude',
               'Taxon.group',
               'Year',
               'Ecosystem')]

ldat <- subset(ldat, !is.na(ldat$Latitude) & !is.na(ldat$Longitude))

ldat$Taxon.group <- factor(ldat$Taxon.group,
                           levels = TaxonGroupLevels)
ldat$Ecosystem <- factor(ldat$Ecosystem, 
                         levels = EcosystemLevels)

ldat$Decade <- factor(ldat$Year - ldat$Year %% 10)
                
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
world <- ne_countries(returnclass = "sf")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# By Taxon group and Ecosystem
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p = ggplot() +
  geom_sf(data = world,
          fill = 'white', 
          color = NA) +
  coord_sf(expand = FALSE) +
  labs(x = "",
       y = "", 
       title = paste(nrow(ldat), 'of', nrow(dat), 'surveys have Lat-Long information')) +
  geom_point(data = ldat, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = Taxon.group,
                 shape = Ecosystem), 
             size = 1) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  theme(panel.background = element_rect(fill = "grey", 
                                        colour = "white"),
        legend.key.size = unit(0.3, 'cm'),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        plot.title = element_text(size = 8),
        plot.margin = grid::unit(c(-15,0,-25,0), "mm"),
        axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave("../Figs/Map_global_TaxonGroupEcosystem.png", 
       plot = p,
       width = 6, height = 3,
       device = 'png')

#~~~~~~~~~~~
# By Decade
#~~~~~~~~~~~
p = ggplot() +
  geom_sf(data = world,
          fill = 'white', 
          color = NA) +
  coord_sf(expand = FALSE) +
  labs(x = "",
       y = "", 
       title = paste(nrow(ldat), 'of', nrow(dat), 'surveys have Lat-Long information')) +
  geom_point(data = ldat, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = Decade), 
             size = 1) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  theme(panel.background = element_rect(fill = "grey", 
                                        colour = "white"),
        legend.key.size = unit(0.3, 'cm'),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        plot.title = element_text(size = 8),
        plot.margin = grid::unit(c(-15,0,-25,0), "mm"),
        axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave("../Figs/Map_global_Decade.png", 
       plot = p,
       width = 6, height = 3,
       device = 'png')

###############################################################################
###############################################################################
###############################################################################
