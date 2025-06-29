#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to import and error-check Google doc sheets
# and generated calculated variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~
# Define some functions
#~~~~~~~~~~~~~~~~~~~~~~
# Are all elements non-NA
NoNA <- function(x) {
  !any(is.na(x))
}
# Are all elements NA
AllNA <- function(x) {
  all(is.na(x))
}
# ISOdate and ISOdatetime only work for specific combinations, so create own
coll <-
  function(x, sel, sep = '-') {
    if (length(sel) > 0) {
      apply(x, 1, paste0, collapse = sep)
    }
  }

#######################
# Import from GoogleDoc
#######################
# Specify the format (data type) of each column in the data spreadsheets
colTypes <- 'llccccncccnnnnncncccccnnnnnnc'

dat1 <-
  read_sheet(
    'https://docs.google.com/spreadsheets/d/16zumXsmbAU-MbBgDmiTjNX8m9YxmbHRUAtSm2xNOJ4I/edit?usp=sharing',
    sheet = 'Data1',
    col_types = colTypes
  )

dat2 <-
  read_sheet(
    'https://docs.google.com/spreadsheets/d/16zumXsmbAU-MbBgDmiTjNX8m9YxmbHRUAtSm2xNOJ4I/edit?usp=sharing',
    sheet = 'Data2',
    col_types = colTypes
  )

dat3 <-
  read_sheet(
    'https://docs.google.com/spreadsheets/d/16zumXsmbAU-MbBgDmiTjNX8m9YxmbHRUAtSm2xNOJ4I/edit?usp=sharing',
    sheet = 'Data3',
    col_types = colTypes
  )

dat4 <-
  read_sheet(
    'https://docs.google.com/spreadsheets/d/16zumXsmbAU-MbBgDmiTjNX8m9YxmbHRUAtSm2xNOJ4I/edit?usp=sharing',
    sheet = 'Data4',
    col_types = colTypes
  )

dat5 <-
  read_sheet(
    'https://docs.google.com/spreadsheets/d/16zumXsmbAU-MbBgDmiTjNX8m9YxmbHRUAtSm2xNOJ4I/edit?usp=sharing',
    sheet = 'Data5',
    col_types = colTypes
  )

message(
  "Google sheets imported."
)

#####################
# Combine data sheets
#####################
dat <- dat1
if (nrow(dat2) > 0) {
  dat <- rbind(dat, dat2)
}
if (nrow(dat3) > 0) {
  dat <- rbind(dat, dat3)
}
if (nrow(dat4) > 0) {
  dat <- rbind(dat, dat4)
}
if (nrow(dat5) > 0) {
  dat <- rbind(dat, dat5)
}
dat <- as.data.frame(dat) # convert from tibble to data.frame

###########################
# Removed unchecked entries
###########################
dat <- subset(dat, Checked == 'TRUE')

######################################################
# Standardize variables relating to "fraction feeding"
######################################################
# Convert given count of empty to count of not empty (=feeding)
dat$Feeding.stomachs.count <-
  dat$Total.stomachs.count.given - dat$Empty.stomachs.count.given

# Convert given counts to percent feeding
dat$Percent.feeding <-
  100 * dat$Feeding.stomachs.count / dat$Total.stomachs.count.given

# Use given percent feeding only if no counts are given
# (i.e. if just-calculated Percent.feeding is empty)
dat$Percent.feeding[is.na(dat$Percent.feeding)] <-
  dat$Percent.feeding.given[is.na(dat$Percent.feeding)]

# Convert given percent feeding to counts if no counts are given
# (rounding to nearest integer)
# This leaves rows with no Total Stomach count as NAs
sel <- which(is.na(dat$Feeding.stomachs.count) &
               !is.na(dat$Percent.feeding.given))
dat$Feeding.stomachs.count[sel] <-
  round(dat$Percent.feeding.given[sel] / 100 *
          dat$Total.stomachs.count.given[sel], 0)

# Round non-integer Total Stomach counts that were calculated upon data entry 
# to integer value
dat$Total.stomachs.count.given <- round(dat$Total.stomachs.count.given, 0)

####################
# Basic error checks
####################
#~~~~~~~~~~~~~~~~~~~~
# Initiate error file
#~~~~~~~~~~~~~~~~~~~~
ErrFile <- '../tmp/ErrorReports/ImportErrors.txt'
sink(file = ErrFile)
print("The following are errors associated with Google doc meta-analysis data.")
sink()
err.cnt <- 0


Check.consult <- which(dat$Consult.needed == TRUE)
if (length(Check.consult) > 0) {
  warn <- 'Some entries are indicated as needing QAQC consultation.\n'
  warning(warn, immediate. = TRUE)
  sink(file = ErrFile, append = TRUE)
  print(warn)
  print(Check.consult)
  err.cnt <- err.cnt + 1
  print("")
  print("")
  sink()
}

Check.lat <- which(!is.numeric(dat$Latitude))
if (length(Check.lat) > 0) {
  warn <- 'There are problems with the following lat-longs.\n'
  warning(warn, immediate. = TRUE)
  sink(file = ErrFile, append = TRUE)
  print(warn)
  print(dat[Check.lat, ])
  err.cnt <- err.cnt + 1
  print("")
  print("")
  sink()
}

Check.long <- which(!is.numeric(dat$Longitude))
if (length(Check.long) > 0) {
  warn <- 'There are problems with the following lat-longs.\n'
  warning(warn, immediate. = TRUE)
  sink(file = ErrFile, append = TRUE)
  print(warn)
  print(dat[Check.long, ])
  err.cnt <- err.cnt + 1
  print("")
  print("")
  sink()
}

err.nopf <- which(is.na(dat$Percent.feeding) == TRUE &
                    is.na(dat$Percent.feeding.given) == TRUE)
if (length(err.nopf) > 0) {
  warn <-
    'These records have neither given nor calculated percent feeding.'
  warning(warn, immediate. = TRUE)
  sink(file = ErrFile, append = TRUE)
  print(warn)
  print(dat[err.nopf, c(
    'Citation',
    'Consumer.identity',
    'Percent.feeding',
    'Percent.feeding.given',
    'Total.stomachs.count.given',
    'Empty.stomachs.count.given',
    'Feeding.stomachs.count'
  )])
  err.cnt <- err.cnt + 1
  print("")
  print("")
  sink()
}

err.spf <- which(dat$Percent.feeding.given > 0 &
                   dat$Percent.feeding.given < 1 &
                   dat$Citation != 'Gil_2007') # ignoring appropriately
if (length(err.spf) > 0) {
  warn <-
    'These records have given percent feeding 0 < % < 1, so confirm.'
  warning(warn, immediate. = TRUE)
  sink(file = ErrFile, append = TRUE)
  print(warn)
  print(dat[err.spf, c(
    'Citation',
    'Consumer.identity',
    'Percent.feeding',
    'Percent.feeding.given',
    'Total.stomachs.count.given',
    'Empty.stomachs.count.given',
    'Feeding.stomachs.count'
  )])
  err.cnt <- err.cnt + 1
  print("")
  print("")
  sink()
}

err.pf <- which(dat$Percent.feeding < 0 | dat$Percent.feeding > 100)
if (length(err.pf) > 0) {
  warn <-
    'These records have percent feeding < 0% or > 100%'
  warning(warn, immediate. = TRUE)
  sink(file = ErrFile, append = TRUE)
  print(warn)
  print(dat[err.pf, c('Citation',
                      'Consumer.identity',
                      'Percent.feeding')])
  err.cnt <- err.cnt + 1
  print("")
  print("")
  sink()
}

err.sc <- which(dat$Total.stomachs.count < dat$Empty.stomachs.count)
if (length(err.sc) > 0) {
  warn <-
    'There are more empty stomachs given than the
      total number that was counted.\n'
  warning(warn, immediate. = TRUE)
  sink(file = ErrFile, append = TRUE)
  print(warn)
  print(dat[err.sc, c('Citation',
                      'Consumer.identity',
                      'Total.stomachs.count.given',
                      'Empty.stomachs.count.given')])
  err.cnt <- err.cnt + 1
  print("")
  print("")
  sink()
}

err.pfmatch <-
  which(abs(dat$Percent.feeding - dat$Percent.feeding.given) > 0.6)
if (length(err.pfmatch) > 0) {
  warn <-
    'The calculated and citation-given estimates of percent
      feeding differ by more than 0.6%.\n'
  warning(warn, immediate. = TRUE)
  sink(file = ErrFile, append = TRUE)
  print(warn)
  print(dat[err.pfmatch, c('Citation',
                           'Consumer.identity',
                           'Percent.feeding',
                           'Percent.feeding.given')])
  err.cnt <- err.cnt + 1
  print("")
  print("")
  sink()
}

err.dr <-
  which(dat$Diet.richness.minimum <= 0 & dat$Percent.feeding > 0)
if (length(err.dr) > 0) {
  warn <-
    'These records have non-positive diet richness despite feeding individuals.\n'
  warning(warn, immediate. = TRUE)
  sink(file = ErrFile, append = TRUE)
  print(warn)
  print(dat[err.dr, c('Citation',
                      'Consumer.identity',
                      'Diet.richness.minimum')])
  err.cnt <- err.cnt + 1
  print("")
  print("")
  sink()
}


tab <- table(dat$Consumer.identity, dat$Taxon.group)
tab <- apply(tab > 0, 1, sum)
tab <- tab[tab > 1]
err.ct <-
  which(dat$Consumer.identity %in% names(tab))
err.ct.dat <- dat[err.ct, c('Citation',
                            'Consumer.identity',
                            'Taxon.group')]
err.ct.dat <- err.ct.dat[order(err.ct.dat$Consumer.identity),]
if (length(err.ct) > 0) {
  warn <-
    'These taxa have multiple taxon groups assigned to them.\n'
  warning(warn, immediate. = TRUE)
  sink(file = ErrFile, append = TRUE)
  print(warn)
  print(dat[err.ct, c('Citation',
                      'Consumer.identity',
                      'Taxon.group')])
  err.cnt <- err.cnt + 1
  print("")
  print("")
  sink()
}

tab <- table(dat$Consumer.identity, dat$Ecosystem)
tab <- apply(tab > 0, 1, sum)
tab <- tab[tab > 1]
# Don't report confirmed multi-ecosystem species
ok <- read.csv('../OtherData/MultiEcosystemTaxa.txt',
               header = FALSE)[,1]
tab <- tab[!(names(tab) %in% ok)]
err.me <-
  which(dat$Consumer.identity %in% names(tab))
err.me.dat <- dat[err.me, c('Citation',
                            'Consumer.identity',
                            'Ecosystem')]
err.me.dat <- err.me.dat[order(err.me.dat$Consumer.identity),]
if (length(err.me) > 0) {
  warn <-
    'These taxa have multiple ecosystems assigned to them.\n'
  warning(warn, immediate. = TRUE)
  sink(file = ErrFile, append = TRUE)
  print(warn)
  print(dat[err.me, c('Citation',
                      'Consumer.identity',
                      'Ecosystem')])
  err.cnt <- err.cnt + 1
  print("")
  print("")
  sink()
}

#~~~~~~~~~~~~~~~~~
# Close error file
#~~~~~~~~~~~~~~~~~
if (sink.number() > 0) {
  sink()
}
if (err.cnt == 0) {
  # Delete if no errors occurred
  unlink('../ErrorReports/ImportErrors.txt')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##################################
# Clean up consumer identity names
##################################
dat$Consumer.identity <-
  gsub("[[:punct:]]", "_", dat$Consumer.identity)

# Remove '_sp_...' from names of taxa identified to genus only
dat$Consumer.identity <- gsub("_sp_.*", "", dat$Consumer.identity)

######################################################
# Create derived variables with shortened column names
######################################################
dat$Space.averaging <-
  sub("1000m (1km)", '1000m', dat$Space.averaging)
dat$SpaceTime.replicate[dat$SpaceTime.replicate == 'NA'] <-
  'Singleton'
dat$SpaceTime.replicate[dat$SpaceTime.replicate == 'SpaceTime'] <-
  'Space'

#############################################################
# Factor levels allowed and order in which they are to appear
#############################################################
TaxonGroupLevels <-
  c(
    'Ctenophores',
    'Cnidarians',
    'Annelids',
    'Chaetognaths',
    'Echinoderms',
    'Molluscs',
    'Arthropods',
    'Fish',
    'Reptiles',
    'Amphibians',
    'Birds',
    'Mammals'
  )

EndoEctoLevels <- c('Ectotherm', 
                    'Endotherm')

SpaceAvgLevels <-
  c(NA, 
    '1m', 
    '10m', 
    '100m', 
    '1000m', 
    '10km', 
    '100km', 
    '1000km')

TimeAvgLevels <-
  c(NA,
    'Minutes',
    'Hours',
    'Days',
    'Months',
    'Years',
    'Decades')

SpaceTimeLevels <-
  c(NA,
    'Singleton',
    'Space',
    'Time',
    'TimeSeries',
    'MultipleTimeSeries')

EcosystemLevels <-
  c(NA, 
    'Marine', 
    'Lentic', 
    'Lotic', 
    'Terrestrial')

DietResolutionLevels <-
  c(NA,
    'Kingdom',
    'Phylum',
    'Subphylum',
    'Class',
    'Subclass',
    'Order',
    'Suborder',
    'Infraorder',
    'Family',
    'Genus',
    'Species')

PopnSplitLevels <-
  c('Whole population', 
    'Sub-population', 
    'Individual')

FeedDataTypeLevels <- c('Gut content (lethal)',
                        'Gut content (non-lethal)',
                        'Direct observation')

######################################

save(
  TaxonGroupLevels,
  EndoEctoLevels,
  SpaceAvgLevels,
  TimeAvgLevels,
  SpaceTimeLevels,
  EcosystemLevels,
  DietResolutionLevels,
  PopnSplitLevels,
  FeedDataTypeLevels,
  file = '../tmp/FracFeed_Data_FactorLevels.Rdata'
)

######################################
# Check to ensure all are represented
######################################
if (any(unique(dat$Taxon.group) %in% TaxonGroupLevels == FALSE)) {
  warning('Not all taxon groups represented')
}
if (any(unique(dat$Space.averaging) %in% SpaceAvgLevels == FALSE)) {
  warning('Not all space-averaging levels represented')
}
if (any(unique(dat$Time.averaging) %in% TimeAvgLevels == FALSE)) {
  warning('Not all time-averaging levels represented')
}
if (any(unique(dat$SpaceTime.replicate) %in% SpaceTimeLevels == FALSE)) {
  warning('Not all SpaceTime replicate types represented')
}
if (any(unique(dat$Ecosystem) %in% EcosystemLevels == FALSE)) {
  warning('Not all Ecosystem types represented')
}
if (any(unique(dat$Feeding.data.type) %in% FeedDataTypeLevels == FALSE)) {
  warning('Not all Feeding types represented')
}

##############################################
# Classify taxa as endo- or ectotherm
# using Taxon Group Levels (specified above).
# This gets all birds and mammals
##############################################
EndoEcto <-
  data.frame(Taxon.group = TaxonGroupLevels,
             EndoEcto = c(rep('Ectotherm', 10),
                          rep('Endotherm', 2)))
dat <- merge(dat, EndoEcto, all.x = TRUE)

genera <- sub('_.*','', dat$Consumer.identity)

EndoGenera <- read.csv('../OtherData/EndothermicGenera.txt',
                       header = FALSE)[,1]
sel <- which(genera %in% EndoGenera)
dat$EndoEcto[sel] <- 'Endotherm'

#########################################################
# Convert date-time columns into a single column
# Note: lubridate will convert Year-only to January 1st,
#       Month-only to 1st of month, etc.
#########################################################
dat$DateTime <- dat$Year

tdat1 <- cbind(dat$Year, dat$Month)
tdat2 <- cbind(dat$Day, dat$Hour, dat$Minutes, dat$Seconds)
sel <- which(apply(tdat1, 1, NoNA) == TRUE &
               apply(tdat2, 1, AllNA) == TRUE)

# Assume "month" date is mid-month
dat$DateTime[sel] <-
  coll(cbind(tdat1[sel, 1:2], rep(15, length(sel))),
       sel, sep = '-')

tdat1 <- cbind(dat$Year, dat$Month, dat$Day)
tdat2 <- cbind(dat$Hour, dat$Minutes, dat$Seconds)
sel <- which(apply(tdat1, 1, NoNA) == TRUE &
               apply(tdat2, 1, AllNA) == TRUE)
dat$DateTime[sel] <- coll(tdat1[sel, 1:3], sel, sep = '-')

tdat1 <- cbind(dat$Year, dat$Month, dat$Day, dat$Hour)
tdat2 <- cbind(dat$Minutes, dat$Seconds)
sel <- which(apply(tdat1, 1, NoNA) == TRUE &
               apply(tdat2, 1, AllNA) == TRUE)
dat$DateTime[sel] <-
  coll(cbind(coll(tdat1[sel, 1:3], sel, sep = '-'),
             tdat1[sel, 4]), sel, sep = ' ')

tdat1 <- cbind(dat$Year, dat$Month, dat$Day, dat$Hour, dat$Minutes)
tdat2 <- cbind(dat$Seconds)
sel <- which(apply(tdat1, 1, NoNA) == TRUE &
               apply(tdat2, 1, AllNA) == TRUE)
dat$DateTime[sel] <-
  coll(cbind(coll(tdat1[sel, 1:3], sel, sep = '-'),
             coll(tdat1[sel, 4:5], sel, sep = ':')),
       sel, sep = ' ')

tdat1 <- cbind(dat$Year,
               dat$Month,
               dat$Day,
               dat$Hour,
               dat$Minutes,
               dat$Seconds)
sel <- which(apply(tdat1, 1, NoNA) == TRUE)
dat$DateTime[sel] <-
  coll(cbind(coll(tdat1[sel, 1:3], sel, sep = '-'),
             coll(tdat1[sel, 4:6], sel, sep = ':')),
       sel, sep = ' ')

dat$DateTime <-
  parse_date_time(dat$DateTime,
                  orders = c("Y", "Ym", "Ymd", "Ymd H", "Ymd H:M", "Ymd H:M:S"))

#################################################
# Determine daylength by latitude and day of year
# using month-midpoint for Month and removing
# Year-only entries
#################################################
dat$DayLength <-
  geosphere::daylength(dat$Latitude, as.Date(dat$DateTime))
dat$DayLength[which(is.na(dat$Month))] <- NA

##########################################################
# Approximate days since northern/southern winter solstice
# Dec/June 20,21,22 or 23 in N/S
##########################################################
dat$tWinterSolstice <- NA
sub <- dat$Latitude > 0 & !is.na(dat$Latitude)
dat$tWinterSolstice[sub] <- yday(dat$DateTime[sub]) + 10
sub <- dat$Latitude < 0 & !is.na(dat$Latitude)
dat$tWinterSolstice[sub] <-
  ifelse(yday(dat$DateTime[sub]) >= 182,
         yday(dat$DateTime[sub]) - 182 + 10,
         yday(dat$DateTime[sub]) + 10 + 182)
dat$tWinterSolstice[which(is.na(dat$Month))] <- NA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##############
# Save to file
##############
save(dat, file = '../tmp/tmp_DB/FracFeed_Data_Imported.Rdata')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# system("say -v Victoria I have finished importing and processing the database")

if (err.cnt > 0) {
  system(
    "say -v Samantha Your database contains potential errors that may need fixing! I have not, repeat, not removed these errors."
  )
  stop()
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~