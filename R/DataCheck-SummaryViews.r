#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load saved database
load('../tmp/tmp_DB/FracFeed_Data_Clean.Rdata')

options(warn = -1) # Turn warnings off

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
  expand_limits(y = c(0, 1.05*max(tab$Freq))) +
  theme(axis.title.y = element_blank()) +
  ylab('Count') +
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
  expand_limits(y = c(0, 1.05*max(tab$Freq))) +
  theme(axis.title.y = element_blank()) +
  ylab('Count') +
  coord_flip()

ggsave('../Figs/Freq_Ecosystem.png',
       p,
       height = 3,
       device = 'png')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Survey year
p <- ggplot(dat,
            aes(x = Year)) +
  geom_histogram(binwidth = 1,
                 fill = fill.color,
                 color = 'black') +
  scale_x_continuous(breaks = seq(1850, 2040, by = 10)) +
  xlab('Survey year') + 
  scale_y_continuous(expand=c(0, 2)) +
  theme_bw() +
  ylab('Count')

ggsave('../Figs/Freq_SurveyYear.png',
       p,
       width = 6,
       height = 4,
       device = 'png')

options(warn = 0) # Turn warnings back on

###########################################################################
###########################################################################
###########################################################################