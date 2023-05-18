#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load saved database
load('../tmp/FracFeed_Data_Clean.Rdata')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fill.color <- 'steelblue'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Taxon groups
tab <- data.frame(sort(table(dat$Taxon.group,
                             dnn = 'Taxon')))

p <- ggplot(tab,
            aes(x = Taxon, y = Freq)) +
  geom_bar(stat = "identity",
           fill = fill.color) +
  geom_text(label = tab$Freq,
            hjust = -0.2) +
  coord_flip()

ggsave('../Figs/Freq_TaxonGroups.png',
       p,
       device = 'png')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ecosystems
tab <- data.frame(sort(table(dat$Ecosystem,
                             dnn = 'Ecosystem')))

p <- ggplot(tab,
            aes(x = Ecosystem, y = Freq)) +
  geom_bar(stat = "identity",
           fill = fill.color) +
  geom_text(label = tab$Freq,
            hjust = -0.2) +
  coord_flip()

ggsave('../Figs/Freq_Ecosystem.png',
       p,
       device = 'png')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sample year
p <- ggplot(dat,
            aes(x = Year)) +
  geom_histogram(binwidth = 1,
                 fill = fill.color,
                 color = 'black') +
  scale_x_continuous(breaks = seq(1850, 2040, by = 10)) +
  xlab('Survey year') + 
  theme_bw()
ggsave('../Figs/Freq_SurveyYear.png',
       p,
       width = 6,
       height = 4,
       device = 'png')


###########################################################################
###########################################################################
###########################################################################