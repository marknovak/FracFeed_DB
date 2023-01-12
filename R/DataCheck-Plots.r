#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preliminary data exploration plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(plyr)
source('library/multiple_plots.r')
source('library/violin_data_summary.r')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load saved database
load('../tmp/FracFeed_Data_Clean.Rdata')

# Load the factor levels (to maintain orders)
load('../tmp/FracFeed_FactorLevels.Rdata')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

alpha <- 0.3 # alpha level for colours

##################
# Data exploration
##################

pdf('../Figs/FracFeed-DataSummaryPlots.pdf',
    height = 11,
    width = 8)
op <-
  par(
    mfrow = c(4, 2),
    tcl = -0.3,
    mgp = c(2, 0.4, 0),
    cex = 0.7,
    yaxs = 'i',
    cex.lab = 1.3
  )

# Count of surveys by name
tab <- sort(table(dat$First.entry.Name))
barplot(tab,
        las = 2,
        ylab = 'Surveys',
        log = 'y')
legend('topleft', legend = paste('n = ', sum(tab)), bty = 'n')

# Count of citations by name
tab <-
  apply(table(dat$First.entry.Name, dat$Citation), 1, function(x) {
    sum(x > 0)
  })
barplot(sort(tab), las = 2, ylab = 'Citations')
legend('topleft', legend = paste('n = ', sum(tab)), bty = 'n')

# Count of Taxon groups
taxCnt <- tab <- sort(table(dat$Taxon.group))
barplot(tab,
        las = 2,
        ylab = 'Frequency',
        log = 'y')

# Count of Taxon groups by species
tab <- table(dat$Consumer.identity, dat$Taxon.group)
tab <- tab[, order(apply(tab, 2, sum))]
predCnt <- apply(tab, 2, function(x) {
  sum(x > 0)
})
b <- barplot(tab,
             las = 2,
             ylab = 'Frequency',
             ylim = c(0, 1.1 * max(taxCnt)))
text(b, taxCnt + 20, paste0('(', predCnt, ')'), cex = 0.7)
legend('topleft',
       legend = paste0(sum(predCnt), ' taxa'),
       bty = 'n')

# Count of ecosystems
tab <- table(factor(dat$Ecosystem,
                    levels = EcosystemLevels),
             useNA = 'ifany')
barplot(tab,
        las = 2,
        ylab = 'Frequency',
        log = 'y')

# Count of SpaceTime.replicate types
tab <- rev(table(factor(
  dat$SpaceTime.replicate,
  levels = rev(SpaceTimeLevels)
),
useNA = 'ifany'))
barplot(tab, las = 2, ylab = 'Frequency')

# Count of Spatial averaging scales
tab <- rev(table(factor(
  dat$Space.averaging,
  levels = rev(SpaceAvgLevels)
), useNA = 'ifany'))
barplot(tab, las = 2, ylab = 'Frequency')

# Count of Temporal averaging scales
tab <- table(factor(dat$Time.averaging,
                    levels = TimeAvgLevels),
             useNA = 'ifany')
barplot(tab, las = 2, ylab = 'Frequency')

# Histogram of citation year
cite.yrs <-
  as.numeric(substring(sapply(strsplit(dat$Citation, "_"), "[", 2), 1, 4))
rng <- range(cite.yrs, na.rm = TRUE)
yrs <- seq(rng[1] - 1, rng[2] + 2, 1) + 0.5
h <- hist(cite.yrs, plot = FALSE, breaks = yrs)
plot(
  h,
  axes = FALSE,
  xlab = 'Citation year',
  col = adjustcolor('grey', alpha),
  main = ''
)
axis(2)
axis(1, at = h$mids[h$mids %% 10 == 0], labels = h$mids[h$mids %% 10 == 0])

# Histogram of sampling year
rng <- range(dat$Year, na.rm = TRUE)
yrs <- seq(rng[1] - 1, rng[2] + 2, 1) + 0.5
h <- hist(dat$Year, plot = FALSE, breaks = yrs)
plot(
  h,
  axes = FALSE,
  xlab = 'Sample year',
  col = adjustcolor('grey', alpha),
  main = ''
)
axis(2)
axis(1, at = h$mids[h$mids %% 10 == 0], 
     labels = h$mids[h$mids %% 10 == 0])

# Histogram of latitude
h <- hist(dat$Latitude, breaks = seq(-90, 90, 5), plot = FALSE)
plot(
  h,
  axes = FALSE,
  xlab = 'Latitude',
  col = adjustcolor('grey', alpha),
  main = ''
)
labs <- c('90S', abs(seq(-80, 80, 10)), '90N')
axis(2)
axis(1, at = seq(-90, 90, 10), labels = labs)

# Histogram of longitude
h <- hist(dat$Longitude,
          breaks = seq(-180, 180, 10),
          plot = FALSE)
plot(
  h,
  axes = FALSE,
  xlab = 'Longitude',
  col = adjustcolor('grey', alpha),
  main = ''
)
labs <- c('180W', abs(seq(-140, 140, 40)), '180E')
axis(2)
axis(1, at = seq(-180, 180, 40), labels = labs)

# Histogram of sampling month - Should split by Northern/Southern hemisphere  - see below
mnths <- seq(0.5, 12.5, 1)
h <- hist(dat$Month, plot = FALSE, breaks = mnths)
plot(
  h,
  axes = FALSE,
  xlab = 'Month',
  col = adjustcolor('grey', alpha),
  main = ''
)
axis(2)
axis(1, at = h$mids, labels = h$mids)

# Histogram of sample sizes per survey
h <- hist(log10(dat$Total.stomachs.count.given),
          plot = FALSE,
          breaks = 50)
plot(
  h,
  axes = FALSE,
  xlab = 'Individuals sampled',
  col = adjustcolor('grey', alpha),
  main = ''
)
axis(2)
axis(1, at = h$breaks, labels = round(10 ^ (h$breaks), 2))
# The high frequency of 50 samples is real,
# though almost entirely due to Menge 1972 who explicitly states
# that 50 individuals were surveyed each time.
# table(subset(dat, dat$Total.stomachs.count.given > 48 &
#                   dat$Total.stomachs.count.given < 52)$Citation)

# Histogram of diet richness per survey
h <- hist(log10(dat$Diet.richness.minimum),
          plot = FALSE,
          breaks = 20)
plot(
  h,
  axes = FALSE,
  xlab = 'Diet richness',
  col = adjustcolor('grey', alpha),
  main = ''
)
ax1.at <- pretty(h$breaks)
axis(2)
axis(1, at = ax1.at, labels = round(10 ^ ax1.at, 1))

# Histogram of body mass
h <- hist(log10(dat$mass_g),
          plot = FALSE,
          breaks = 20)
plot(
  h,
  axes = FALSE,
  xlab = 'Body mass',
  col = adjustcolor('grey', alpha),
  main = ''
)
ax1.at <- pretty(h$breaks)
axis(2)
axis(1, at = ax1.at, labels = round(10 ^ ax1.at, 1))

# Histogram of Percent feeding
h <- hist(dat$Percent.feeding, plot = FALSE, breaks = 50)
plot(
  h,
  axes = FALSE,
  xlab = 'Percent feeding',
  col = adjustcolor('grey', alpha),
  main = ''
)
axis(2)
axis(1)

# Compare given and calculated Percent Feeding
op2 <- par(pty = 's', xaxs = 'i')
plot(
  dat$Percent.feeding,
  dat$Percent.feeding.given,
  xlab = 'Calculated % feeding',
  ylab = 'Given % feeding',
  pch = 21,
  bg = adjustcolor('grey', alpha)
)
par(op2)
par(yaxs = 'r')

# Compare given and calculated Percent Feeding
op2 <- par(pty = 's', xaxs = 'i')
plot(
  dat$Percent.feeding,
  dat$Feeding.stomachs.count / dat$Total.stomachs.count.given,
  xlab = 'Calculated % feeding',
  ylab = 'Feeding.count / Total.count',
  pch = 21,
  bg = adjustcolor('grey', alpha)
)
par(op2)
par(yaxs = 'r')

# Diet richness vs. Individuals surveys
plot(
  dat$Diet.richness.minimum ~ dat$Total.stomachs.count.given,
  log = 'x',
  pch = 21,
  bg = adjustcolor('grey', alpha),
  xlab = 'Individuals sampled',
  ylab = 'Minimmum diet richness'
)

# Percent feeding vs. Individuals surveyed
plot(
  dat$Percent.feeding ~ dat$Total.stomachs.count.given,
  log = 'x',
  pch = 21,
  bg = adjustcolor('grey', alpha),
  xlab = 'Individuals sampled',
  ylab = 'Percent feeding'
)

# Percent feeding vs. Diet richness
plot(
  dat$Percent.feeding ~ dat$Diet.richness.min,
  log = 'x',
  pch = 21,
  bg = adjustcolor('grey', alpha),
  xlab = 'Minimum diet richness',
  ylab = 'Percent feeding'
)

# Percent feeding vs. Latitude
plot(
  dat$Percent.feeding ~ dat$Latitude,
  pch = 21,
  bg = adjustcolor('grey', alpha),
  xlab = 'Latitude',
  ylab = 'Percent feeding'
)

# Percent feeding vs. Year
plot(
  dat$Percent.feeding ~ dat$Year,
  pch = 21,
  bg = adjustcolor('grey', alpha),
  xlab = 'Year',
  ylab = 'Percent feeding'
)

# Percent feeding vs. Hour
plot(
  dat$Percent.feeding ~ dat$Hour,
  pch = 21,
  bg = adjustcolor('grey', alpha),
  xlab = 'Hour',
  ylab = 'Percent feeding',
  axes = FALSE
)
axis(2)
axis(1, seq(0, 24, 2))
box(lwd = 1)

# Percent feeding vs. Month split by hemisphere
datN <- subset(dat, Latitude > 0)
datS <- subset(dat, Latitude < 0)
plot(
  datN$Percent.feeding ~ jitter(datN$Month),
  pch = 21,
  bg = adjustcolor('grey', alpha),
  xlab = 'Month',
  ylab = 'Percent feeding',
  axes = FALSE,
  main = "Northern"
)
axis(2)
axis(1, at = 1:12)
box(lwd = 1)

plot(
  datS$Percent.feeding ~ jitter(datS$Month),
  pch = 21,
  bg = adjustcolor('grey', alpha),
  xlab = 'Month',
  ylab = 'Percent feeding',
  axes = FALSE,
  main = "Southern"
)
axis(2)
axis(1, at = 1:12)
box(lwd = 1)

# Percent feeding vs. Daylength
plot(
  dat$DayLength,
  dat$Percent.feeding,
  xlab = 'Day length',
  ylab = 'Percent feeding',
  pch = 21,
  bg = adjustcolor('grey', alpha)
)

# Daylength vs. Days since winter solstice
plot(
  dat$tWinterSolstice,
  dat$DayLength,
  xlab = 'Days post winter solstice',
  ylab = 'Day length',
  pch = 21,
  bg = adjustcolor('grey', alpha)
)

# Percent feeding vs. Days since winter solstice
plot(
  dat$tWinterSolstice,
  dat$Percent.feeding,
  xlab = 'Days since winter solstice',
  ylab = 'Percent feeding',
  pch = 21,
  bg = adjustcolor('grey', alpha)
)

# Evaluate potential methodological biases over time
decades <- seq(1920, 2020, 10)
lab.decades <- decades[-length(decades)]
# By Feeding Data Type
p1 <- ggplot(dat) +
  aes(
    x = cut(
      Year,
      breaks = decades,
      include.lowest = TRUE,
      right = FALSE,
      labels = lab.decades
    ),
    fill = factor(Feeding.data.type)
  ) +
  geom_bar(position = 'fill') +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  facet_grid(rows = vars(Taxon.group)) +
  labs(x = "", y = "Proportion", fill = "Observation type") +
  theme(strip.text.y = element_text(angle = 0))

# By Rate-limiting-step
p2 <- ggplot(dat) +
  aes(
    x = cut(
      Year,
      breaks = decades,
      include.lowest = TRUE,
      right = FALSE,
      labels = lab.decades
    ),
    fill = factor(Rate.limiting.step)
  ) +
  geom_bar(position = 'fill') +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  facet_grid(rows = vars(Taxon.group)) +
  labs(x = "", y = "Proportion", fill = "Rate\nlimiting\nstep?") +
  theme(strip.text.y = element_text(angle = 0))

# By Survey type
p3 <- ggplot(dat) +
  aes(
    x = cut(
      Year,
      breaks = decades,
      include.lowest = TRUE,
      right = FALSE,
      labels = lab.decades
    ),
    fill = factor(SpaceTime.replicate)
  ) +
  geom_bar(position = 'fill') +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  facet_grid(rows = vars(Taxon.group)) +
  labs(x = "", y = "Proportion", fill = "Survey type") +
  theme(strip.text.y = element_text(angle = 0))

multiplot(p1, p2, p3, rows = 3)

# Percent feeding vs. Time-averaging
p1 <-
  ggplot(dat,
         aes(
           factor(Time.averaging, levels = TimeAvgLevels),
           Percent.feeding
         )) +
  geom_violin(scale = "width") + labs(x = '')

# Percent feeding vs. Spatial-averaging
p2 <-  ggplot(dat,
              aes(
                factor(Space.averaging, levels = SpaceAvgLevels),
                Percent.feeding
              )) +
  geom_violin(scale = "width") + labs(x = '')

# Percent feeding vs. Taxon group
PercFeed.Taxon <-
  ddply(
    dat,
    .(Taxon.group),
    summarize,
    Mean = mean(Percent.feeding),
    SD = sd(Percent.feeding)
  )
PercFeed.Taxon <- PercFeed.Taxon[order(PercFeed.Taxon$Mean),]
TaxonGroupLevels <- PercFeed.Taxon$Taxon.group
p3 <- ggplot(dat,
             aes(
               factor(Taxon.group, levels = TaxonGroupLevels),
               Percent.feeding
             )) +
  geom_violin(scale = "width") + labs(x = '')

# Percent feeding vs. Ecosystem
PercFeed.Ecosystem <-
  ddply(
    dat,
    .(Ecosystem),
    summarize,
    Mean = mean(Percent.feeding),
    SD = sd(Percent.feeding)
  )
PercFeed.Ecosystem <-
  PercFeed.Ecosystem[order(PercFeed.Ecosystem$Mean),]
EcosystemLevels <- PercFeed.Ecosystem$Ecosystem
p4 <- ggplot(dat,
             aes(factor(Ecosystem, levels = EcosystemLevels), Percent.feeding)) +
  geom_violin(scale = "width") + labs(x = '')

multiplot(p1, p2, p3, p4, rows = 4)


# Diet richness vs. Individuals surveys
p1 <- ggplot(dat) +
  geom_point(
    aes(x = Total.stomachs.count.given,
        y = Diet.richness.minimum, color = Taxon.group),
    alpha = alpha
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_log10()

# Percent feeding vs. Individuals surveyed
p2 <- ggplot(dat) +
  geom_point(aes(x = Total.stomachs.count.given,
                 y = Percent.feeding,
                 color = Taxon.group),
             alpha = alpha) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_log10()

# Percent feeding vs. Diet richness
p3 <- ggplot(dat) +
  geom_point(aes(x = Diet.richness.minimum,
                 y = Percent.feeding,
                 color = Taxon.group),
             alpha = alpha) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_log10()

# Percent feeding vs. Latitude
p4 <- ggplot(dat) +
  geom_point(aes(x = Latitude,
                 y = Percent.feeding,
                 color = Taxon.group), alpha = alpha) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Percent feeding vs. Year
p5 <- ggplot(dat) +
  geom_point(aes(x = Year,
                 y = Percent.feeding,
                 color = Taxon.group), alpha = alpha) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Percent feeding vs. Hour
p6 <- ggplot(dat) +
  geom_point(aes(x = Hour,
                 y = Percent.feeding,
                 color = Taxon.group), alpha = alpha) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Percent feeding vs. Month split by hemisphere
datN <- subset(dat, Latitude > 0)
datS <- subset(dat, Latitude < 0)
p7 <- ggplot(datN) +
  geom_point(aes(x = Month,
                 y = Percent.feeding,
                 color = Taxon.group), alpha = alpha) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())

p8 <- ggplot(datS) +
  geom_point(aes(x = Month,
                 y = Percent.feeding,
                 color = Taxon.group), alpha = alpha) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())

# Percent feedings vs. Daylength
p9 <- ggplot(dat) +
  geom_point(aes(x = DayLength,
                 y = Percent.feeding,
                 color = Taxon.group),
             alpha = alpha) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Percent feeding vs. Days since winter solstice
p10 <- ggplot(dat) +
  geom_point(aes(x = tWinterSolstice,
                 y = Percent.feeding,
                 color = Taxon.group),
             alpha = alpha) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())

multiplot(p1, p2, p3)
multiplot(p4, p5, p6)
multiplot(p7, p8, p9)
multiplot(p10, p10, p10)


dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
