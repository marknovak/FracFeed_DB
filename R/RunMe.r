############################################################
############################################################
# Set working directory to location of this RunMe.r script
############################################################
############################################################
rm(list = ls())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Match only taxa not already previously matched
matchNewTaxa <- TRUE
# Prompt for each taxon (TRUE) or just unmatched (FALSE)
promptNext <- FALSE
# Rather than prompting for unmatched, select the taxon with
# the highest matching score.
select_max_score <- TRUE

# Regenerate body mass compilations
regBMcompilations <- FALSE

# CAREFUL HERE: 
# Erase previously saved matches and start over
matchAllTaxa <- FALSE

#########################
# Load necessary packages
#########################
# For 'DataPrep-Generate.r' and 'DataPrep-MetaCite.r'
library(googlesheets4) # to read in Google spreadsheet
library(geosphere) # for daylength()
library(lubridate) # for yday() and other functions

# For citations
library(rmarkdown)

# For 'DataPrep-TaxonClean.r'
library(rotl)

# For 'DataPrep-CompileBodyMass.r'
library(plyr)
library(devtools)
library(stringr)
library(rdataretriever)

# For 'DataCheck-SummaryViews.r' and 'DataCheck-Plots.r'
library(ggplot2)

# For 'DataCheck-Maps.r'
library(sf)
library(rnaturalearth)

############################################################
# Import from GoogleSheets and generate calculated variables
############################################################
source('DataPrep-Generate.r')

# load('../tmp/FracFeed_Data_Imported.Rdata')

######################################################
# Import, generate, and export meta-data and citations
######################################################
source('DataPrep-MetaCite.r')

#############################################################
# Merge in validated species names and ottIDs from TaxonClean
#############################################################
source('DataPrep-TaxonClean.r') # creates 'taxa'

dat <- merge(dat,
             taxa[, c('search_string', 'unique_name', 'ott_id')],
             by.x = 'Consumer.identity',
             by.y = 'search_string',
             all.x = TRUE)

colnames(dat) <- sub('Consumer.identity','Consumer.identity.orig', colnames(dat))
colnames(dat) <- sub('unique_name','Consumer.identity', colnames(dat))

# Pull back names of INCERTAE_SEDIS taxa that weren't in the ToL
dat$Consumer.identity[is.na(dat$Consumer.identity)] <- 
  dat$Consumer.identity.orig[is.na(dat$Consumer.identity)]

#########################
# Merge in body mass data 
#########################
source('DataPrep-CompileBodyMass.r')

adat <- read.csv(file = '../tmp/BodyMass/FracFeed_BodyMass.csv')

dat <- merge(dat,
             adat[, c('taxon', 'mass_g')],
             by.x = 'Consumer.identity',
             by.y = 'taxon',
             all.x = TRUE)

####################################################
####################################################
save(dat, 
     file = '../tmp/tmp_DB/FracFeed_Data_Clean.Rdata')
####################################################
####################################################

####################################################
# Count surveys having various covariates
####################################################
source('DataCheck-VarCombnCounts.r')

####################################################
# Plot various views of the data to check for errors
####################################################
source('DataCheck-Plots.r')

####################################################
# Print some summary views for repository README
####################################################
source('DataCheck-SummaryViews.r')

####################################################
# Print some maps for repository README
####################################################
source('DataCheck-Maps.r')

#########################################
# Convert to factor levels and abbreviate
# Convert to ordinal scale where appropriate
#########################################
load('../tmp/FracFeed_Data_FactorLevels.Rdata')

fdatc <- data.frame(Cite = factor(dat$Citation),
                   ConID = factor(dat$Consumer.identity),
                   ConIDorig = dat$Consumer.identity.orig,
                   ott_id = dat$ott_id)

fdatc$TG <- factor(dat$Taxon.group, 
                  levels = TaxonGroupLevels)
fdatc$EE <- factor(dat$EndoEcto, 
                 levels = EndoEctoLevels)
fdatc$BM <- dat$mass_g
fdatc$BMlog <- log10(dat$mass_g)
fdatc$PS <- factor(dat$Survey.population.split, 
                  levels = PopnSplitLevels)

fdatc$TSc <- dat$Total.stomachs.count.given
fdatc$ESc <- dat$Empty.stomachs.count.given
fdatc$FSc <- dat$Feeding.stomachs.count
fdatc$fF <- round(dat$Percent.feeding / 100, 4)

fdatc$DR <- dat$Diet.richness.minimum
fdatc$DRlog <- log10(dat$Diet.richness.minimum + 1)
fdatc$Drc <- factor(dat$Diet.resolution.coarsest, 
                              levels = DietResolutionLevels)
fdatc$Drf <- factor(dat$Diet.resolution.finest, 
                              levels = DietResolutionLevels)

fdatc$Eco <- factor(dat$Ecosystem, 
                   levels = EcosystemLevels)
fdatc$Lat <- dat$Latitude
fdatc$Long <- dat$Longitude

fdatc$Yr <- dat$Year
fdatc$DT <- dat$DateTime
fdatc$DL <- dat$DayLength
fdatc$tWS <- dat$tWinterSolstice

fdatc$ST <- factor(dat$SpaceTime.replicate, 
                  levels = SpaceTimeLevels)

fdatc$FD <- factor(dat$Feeding.data.type, 
                  levels = FeedDataTypeLevels)

fdatc$SA <- factor(dat$Space.averaging, 
                  levels = SpaceAvgLevels)
fdatc$SAlog <- as.numeric(factor(dat$Space.averaging, 
                                levels = SpaceAvgLevels)) - 1

fdatc$TA <- factor(dat$Time.averaging, levels = TimeAvgLevels)
secL <- c(NA, 1 / 3600, 1 / 60, 1, 24, 730, 8760, 87600) # as a function of hours
for (i in 1:length(TimeAvgLevels)) {
  fdatc$TAnum[fdatc$TA == TimeAvgLevels[i]] <- secL[i]
}
fdatc$TAlog <- log10(fdatc$TAnum)

###########################################
# Restrict to non-redundant focal variables
###########################################
vars <-
  c(
    'Cite',
    'ConIDorig',
    'ConID',
    'ott_id',
    'TSc',
    'FSc',
    'fF',
    'TG',
    'EE',
    'BM',
    'BMlog',
    'Eco',
    'Lat',
    'Long',
    'DR',
    'DRlog',
    'Drf',
    'Drc',
    'TA',
    'TAlog',
    'SA',
    'SAlog',
    'Yr',
    'tWS',
    'DT',
    'FD'
  )
fdatc <- fdatc[, vars]

##############################################################################

save(fdatc, 
     file = '../tmp/FracFeed_Data.Rdata')

write.csv(
  fdatc,
  '../tmp/FracFeed_Data.csv',
  row.names = FALSE
)

# Summary stats
SummFile <- '../tmp/SummaryStats.txt'
sink(file = SummFile)
print(paste("The database includes a total of",
            nrow(fdatc), "surveys of at least",
            sum(fdatc$TSc, na.rm = TRUE), "individuals from ",
            length(unique(fdatc$ConID)), "taxa from",
            length(unique(fdatc$Cite)), "studies."))
print(paste("Date range:", 
            paste(range(fdatc$Yr, na.rm = TRUE), collapse = ' to ')))
print(paste("Latitude range:", 
            paste(range(fdatc$Lat, na.rm = TRUE), collapse = ' to ')))
sink()

message("
\n******************************************************************
******************************************************************
The data have been imported and processed.
They have been placed into the 'tmp' directory.
If these data are 'good to use', 
  manually move or copy them into the repository's main directory.
******************************************************************
******************************************************************")
        
##############################################################################
##############################################################################
##############################################################################