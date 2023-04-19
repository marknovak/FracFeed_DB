# Code to grab species body sizes from various databases
########################################################
# AnAge http://genomics.senescence.info/species/
# install.packages('rYoutheria')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wd <- getwd()
setwd('../OtherData/BodyMass/')
library(plyr)
library(devtools)
require(googlesheets4) # to read in Google spreadsheet
library(stringr)
library(rdataretriever)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Retrieve from dataretriever databases?
DataRetrieve <- FALSE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Geometric mean
gmean <- function(x){
  exp(mean(log(x[!is.infinite(x)]),na.rm=TRUE))
  }
##########################################################################
##########################################################################
##########################################################################
# Data compilation fom DataRetriever
#####################################
# http://retriever.readthedocs.io/en/latest/index.html
# Need to install Python and Retriever first to work

# Contains VertNet within it, but use RVertNet package instead
# install_github("ropensci/rdataretriever")

# # List the datasets available via the Retriever
# rdataretriever::datasets()

# # Install the Gentry forest transects dataset into csv files in your working directory
# rdataretriever::install('mammal-life-hist', 'csv')
#
# # Download the raw Gentry dataset files without any processing to the
# # subdirectory named data
# rdataretriever::download('mammal-life-hist', './data/')

if (DataRetrieve) {
  # Install and load a dataset as a list
  mlh = rdataretriever::fetch('mammal-life-hist')
  mlh <- mlh$species[, 1:5]
  mlh$taxon <- paste(mlh$genus, mlh$species)
  mlh <- mlh[, c('taxon', 'mass_g')]
  mlh <- mlh[which(!is.na(mlh$mass_g) & mlh$mass_g > 0), ]
  mlh <-
    ddply(mlh,
          .(taxon),
          summarise,
          mass_g = gmean(mass_g),
          n = length(mass_g))
  mlh$source <- 'rdataretriever-mammal-life-hist'
  
  bir = rdataretriever::fetch('bird-size')
  bir <- bir$species[, c('species_name', 'm_mass')]
  colnames(bir) <- c('taxon', 'mass_g')
  bir <- bir[which(!is.na(bir$mass_g) & bir$mass_g > 0), ]
  bir <-
    ddply(bir,
          .(taxon),
          summarise,
          mass_g = gmean(mass_g),
          n = length(mass_g))
  bir$source <- 'rdataretriever-bird-size'
  
  ver = rdataretriever::fetch('home-ranges')
  ver <- ver$home_ranges
  ver$taxon <- paste(ver$genus, ver$species)
  ver <- ver[, c('taxon', 'mean_mass_g')]
  colnames(ver) <- c('taxon', 'mass_g')
  ver <- ver[which(!is.na(ver$mass_g) & ver$mass_g > 0), ]
  ver <-
    ddply(ver,
          .(taxon),
          summarise,
          mass_g = gmean(mass_g),
          n = length(mass_g))
  ver$source <- 'rdataretriever-home-ranges'
  
  ppb = rdataretriever::fetch('predator-prey-body-ratio')
  ppb <- ppb$bodysizes
  ppb <-
    ppb[which(ppb$taxonomy_consumer != '' & ppb$taxonomy_resource != ''), ]
  ppb1 <- ppb[, c('taxonomy_consumer', 'mean_mass_g_consumer')]
  ppb2 <- ppb[, c('taxonomy_resource', 'mean_mass_g_resource')]
  colnames(ppb1) <- colnames(ppb2) <- c('taxon', 'mass_g')
  ppb <- rbind(ppb1, ppb2)
  ppb <- ppb[which(!is.na(ppb$mass_g) & ppb$mass_g > 0), ]
  ppb <-
    ddply(ppb,
          .(taxon),
          summarise,
          mass_g = gmean(mass_g),
          n = length(mass_g))
  ppb$source <- 'rdataretriever-predator-prey-body-ratio'
  
  pan = rdataretriever::fetch('pantheria')
  pan <- pan$species
  pan <- pan[, c('msw05_binomial', 'adultbodymass_g')]
  colnames(pan) <- c('taxon', 'mass_g')
  pan <- pan[which(!is.na(pan$mass_g) & pan$mass_g > 0), ]
  pan <-
    ddply(pan,
          .(taxon),
          summarise,
          mass_g = gmean(mass_g),
          n = length(mass_g))
  pan$source <- 'rdataretriever-pantheria'
  
  # amn = rdataretriever::fetch('amniote-life-hist') # takes a long time to download
  # amn<-amn$main
  # save(amn,file='BodyMass_amniote-life-hist.Rdata')
  load(file = 'BodyMass_amniote-life-hist.Rdata')
  amn <- subset(amn, trait == 'adult_body_mass_g')
  amn <- amn[!is.na(amn$trait_value), ]
  amn$taxon <- paste(amn$genus, amn$species)
  amn <- amn[, c('taxon', 'trait_value')]
  colnames(amn) <- c('taxon', 'mass_g')
  amn <- amn[which(!is.na(amn$mass_g) & amn$mass_g > 0), ]
  amn <-
    ddply(amn,
          .(taxon),
          summarise,
          mass_g = gmean(mass_g),
          n = length(mass_g))
  amn$source <- 'rdataretriever-amniote-life-hist'
  
  sdd = rdataretriever::fetch('socean-diet-data')
  sdd <- sdd$diet
  sdd1 <- sdd[, c('predator_name', 'predator_mass_mean')]
  sdd2 <- sdd[, c('prey_name', 'prey_mass_mean')]
  colnames(sdd1) <- colnames(sdd2) <- c('taxon', 'mass_g')
  sdd <- rbind(sdd1, sdd2)
  sdd <- sdd[which(!is.na(sdd$mass_g) & sdd$mass_g > 0), ]
  sdd <-
    ddply(sdd,
          .(taxon),
          summarise,
          mass_g = gmean(mass_g),
          n = length(mass_g))
  sdd$source <- 'rdataretriever-socean-diet-data'
  
  # vra = rdataretriever::fetch('vertnet-amphibians') # takes a VERY long time to download
  # vra<-vra$amphibians
  # vra<-vra[!is.na(vra$massing),]
  # vra<-vra[,c('scientificname','massing')]
  # colnames(vra)<-c('taxon','mass_g')
  # vra<-vra[which(!is.na(vra$mass_g)&vra$mass_g>0),]
  # vra<-ddply(vra,.(taxon),summarise,mass_g=gmean(mass_g),n=length(mass_g))
  # vra$source<-'rdataretriever-vertnet-amphibians'
  # save(vra,file='BodyMass_vertnet-amphibians.Rdata')
  load(file = 'BodyMass_vertnet-amphibians.Rdata')
  
  # vrr = rdataretriever::fetch('vertnet-reptiles')
  # vrr<-vrr$reptiles
  # vrr<-vrr[!is.na(vrr$massing),]
  # vrr<-vrr[,c('scientificname','massing')]
  # colnames(vrr)<-c('taxon','mass_g')
  # vrr<-vrr[which(!is.na(vrr$mass_g)&vrr$mass_g>0),]
  # vrr<-ddply(vrr,.(taxon),summarise,mass_g=gmean(mass_g),n=length(mass_g))
  # vrr$source<-'rdataretriever-vertnet-reptiles'
  # save(vrr,file='BodyMass_vertnet-reptiles.Rdata')
  load(file = 'BodyMass_vertnet-reptiles.Rdata')
  
  # nem = rdataretriever::fetch('nematode-traits') # downlaod failed
  # ppm = rdataretriever::fetch('predator-prey-size-marine') # download failed
  # ~~~~~~~~~~~~~~~~~~~~~~~~
  adat <- rbind(mlh, bir, ver, ppb, pan, amn, sdd, vra, vrr)
  adat <- FixNames(adat)
  adat <- adat[which(!is.na(adat$mass_g) & adat$mass_g > 0), ]
  adat <-
    ddply(
      adat,
      .(taxon),
      summarise,
      mass_g = gmean(mass_g),
      gen_time_days = NA,
      n = sum(n),
      source = paste(unique(source), collapse = '_')
    )
  adat <- adat[!adat$taxon == 0, ]
  DR <- adat
  save(DR, file = '../OtherData/BodyMass_DataRetrieverAll.Rdata')
}
##########################################################################
# devtools::install_github("BiologicalRecordsCentre/rYoutheria")
# library('rYoutheria')
# rY <- getMeasurementData(measurementType = 'Body Mass',silent = TRUE)

##########################################################################
# Encyclopedia of Life TraitBank
# install_github("ropensci/reol")
# library(reol)
# install_github("ropensci/traits")
# library(traits)
# install.packages('rjson')
# library(rjson)
#
# # rEOL and TraitBank can only be accessed via species names,
# # so use names from OpenTree contstructed tree.
# load(file='Output/Phylo/Tree_comb.Rdata')
#
# # Following code pulled from:
# # https://github.com/lukejharmon/traitathon/blob/master/EOLtraithack/mammalTraits.R
# # Requires functions pasted into 'FracFeed-Functions.r'
#
# dir.create('EOL_Traits')
# setwd('EOL_Traits')
#
# # These steps will take a while!
# EOLids <- MatchTaxatoEOLID(tree$tip.label)
# EOLids <- EOLids[!is.na(EOLids$eolPageNumbers),]
# OTL1 <- DownloadEOLtraits(EOLids[1:384,3], to.file=TRUE) # An error occurred for file 385
# OTL2 <- DownloadEOLtraits(EOLids[386:628,3], to.file=TRUE) # An error occurred for file 629
# OTL3 <- DownloadEOLtraits(EOLids[630:667,3], to.file=TRUE) # An error occurred for file 668
# OTL4 <- DownloadEOLtraits(EOLids[669:nrow(EOLids),3], to.file=TRUE)
#
# files <- RemoveNAFiles(list.files(pattern="eol"))
#
# #You can see which traits are available
# WhichTraits(files)
#
# #Then you can compile data from each of the files for whichever trait is available.  We chose to examine body mass. Then the trait will need a little finagling.
# adat <- GetData("body mass", files, chatty=TRUE)
# adat <- adat[which(adat[,5] == "adult"),]  #only want adult animals
# adat[,1] <- sapply(adat[,1], FirstTwo) #species names have authors attached
# adat[which(adat[,4] == "kg"),][,3] <- as.numeric(adat[which(adat[,4] == "kg"),][,3])*1000 #convert kg to g
#
# colnames(adat)[c(1,3)]<-c('taxon','mass_g')
# adat<-adat[,c('taxon','mass_g')]
# adat<-ddply(adat,.(taxon),summarise, mass_g=gmean(mass_g), gen_time_days=NA,n=sum(n))
# adat$source<-'EOL_TraitBank'
# TB<-adat
# save(TB,file='BodyMass_EOLTraitBank.Rdata')
#
# setwd('R')
# # unlink('EOL_Traits')

##################################
##########################################################################
# Brown, J. H., C. A. S. Hall, and R. M. Sibly. 2018.
# Equal fitness paradigm explained by a trade-off between generation time and energy production rate.
# Nature Ecology & Evolution 2:262-268.
##################################
dat1 <-
  read.csv('Brown_etal_2018/41559_2017_430_MOESM3_ESM.csv')
dat1$taxon <- paste(dat1$Genus, dat1$Species)
dat1 <-
  dat1[, c('taxon', 'Dry.mass.g', 'Uncorrected.mortality.rate.y.1')]
colnames(dat1) <- c('taxon', 'mass_g', 'gen_time_yrs')
dat1$gen_time_yrs <- 1 / dat1$gen_time_yrs
dat1 <-
  ddply(
    dat1,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = round(gmean(gen_time_yrs) * 365, 3),
    n = length(mass_g)
  )

dat2 <-
  read.csv('Brown_etal_2018/41559_2017_430_MOESM2_ESM.csv')
dat2$taxon <- gsub('.*: (.*)', '\\1', dat2$Taxon2)
dat2 <- dat2[, c('taxon', 'Body.mass.g')]
colnames(dat2) <- c('taxon', 'mass_g')
dat2 <-
  ddply(
    dat2,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = NA,
    n = length(mass_g)
  )

adat <- rbind(dat1, dat2)
adat <- FixNames(adat)
adat <-
  ddply(
    adat,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = gmean(gen_time_days),
    n = sum(n)
  )
adat$source <- 'Brown_etal_2018'
BR <- adat
save(BR, file = 'BodyMass_Brown_etal_2018.Rdata')


##########################################################################
# F. A. Smith, S. K. Lyons, S. K. M. Ernest, K. E. Jones, D. M. Kaufman, T. Dayan, P. A. Marquet, J. H. Brown, and J. P. Haskell. Body mass of late quaternary mammals (v.10.2). Ecology, 84(12):3403–3403, 2019/08/01 2003.
##################################
adat <- read.csv('Smith_etal_2003/MOMv10.2.csv')
adat <- adat[, c('Genus', 'Species', 'Combined.Mass..g.')]
adat$taxon <- paste(adat$Genus, adat$Species, sep = '_')
colnames(adat)[3] <- 'mass_g'
adat <- adat[, c('taxon', 'mass_g')]
adat <- FixNames(adat)
adat <- adat[which(adat$mass_g != -999), ]
adat <-
  ddply(adat,
        .(taxon),
        summarise,
        mass_g = gmean(mass_g),
        n = length(mass_g))
adat$source <- 'Smith_2003'
MM <- adat
save(MM, file = 'BodyMass_Smith_2003.Rdata')


##########################################################################
# Anderson, D. M., and J. F. Gillooly. 2017.
# Physiological constraints on long-term population cycles: a broad-scale view.
# Evolutionary Ecology Research 18:693-707.
##################################
adat <-
  read.csv('AndersonGillooly_2017/AndersonGillooly_2017_EER_data.csv')
adat <- adat[, c('Species', 'Mass_g', 'Gen_Time_days')]
colnames(adat) <- c('taxon', 'mass_g', 'gen_time_days')
adat <- FixNames(adat)
adat <-
  ddply(
    adat,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = gmean(gen_time_days),
    n = length(mass_g)
  )
adat$source <- 'AndersonGillooly_2017'
AG <- adat
save(AG, file = 'BodyMass_AndersonGillooly_2017.Rdata')



##########################################################################
# Simon Jennings, John K. Pinnegar, Nicholas V. C. Polunin, Karema J. Warr
# Linking size-based and trophic analyses of benthic community structure
# MEPS Vol. 226: 77–85, 2002
##################################
adat <- read.csv('Jennings_2002/Jennings_2002.csv')
adat <- adat[, c('Species', 'Mean_mass_g')]
colnames(adat) <- c('taxon', 'mass_g')
adat <- FixNames(adat)
adat <-
  ddply(
    adat,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = NA,
    n = length(mass_g)
  )
adat$source <- 'Jennings_2002'
JE <- adat
save(JE, file = 'BodyMass_Jennings_2002.Rdata')

##########################################################################
# Lislevand, T., J. Figuerola, and T. Székely. 2007.
# AVIAN BODY SIZES IN RELATION TO FECUNDITY, MATING SYSTEM, DISPLAY BEHAVIOR, AND RESOURCE SHARING.
# Ecology 88:1605-1605.
##################################
adat <-
  read.table(
    'Lislevand_etal_2007/avian_ssd_jan07.txt',
    sep = '\t',
    header = TRUE
  )
adat <- adat[, c('Species_name', 'M_mass', 'F_mass', 'unsexed_mass')]
colnames(adat)[1] <- c('taxon')
adat[which(adat == '-999', arr.ind = TRUE)] <- NA
adat$mass_g <- apply(adat[, -1], 1, mean, na.rm = TRUE)
adat <- adat[, c('taxon', 'mass_g')]
adat <- FixNames(adat)
adat <-
  ddply(
    adat,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = NA,
    n = length(mass_g)
  )
adat$source <- 'Lislevand_etal_2007'
LI <- adat
save(LI, file = 'BodyMass_Lislevand_etal_2007.Rdata')

##########################################################################
# Eklöf, J., Å. Austin, U. Bergström, S. Donadi, B. D. H. K. Eriksson, J. Hansen, and G. Sundblad. 2017.
# Size matters: relationships between body size and body mass of common coastal, aquatic invertebrates in the Baltic Sea.
# PeerJ 5:e2906.
adat <-
  read.csv('Eklof_etal_2017/Eklof_etal_2017.csv',
           header = TRUE)
adat <- adat[, c('Taxa', 'DW')]
colnames(adat) <- c('taxon', 'mass_g')
adat <- FixNames(adat)
adat <-
  ddply(
    adat,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = NA,
    n = length(mass_g)
  )
adat$source <- 'Eklof_etal_2017'
EK <- adat
save(EK, file = 'BodyMass_Eklof_etal_2017.Rdata')

##########################################################################
# Feldman, A., N. Sabath, R. A. Pyron, I. Mayrose, and S. Meiri. 2016. Body sizes and diversification rates of lizards, snakes, amphisbaenians and the tuatara. Global Ecology and Biogeography 25:187-197.
adat <-
  read.csv('Feldman_etal_2016/Appendix S1 - Lepidosaur body sizes.csv',
           header = TRUE)
adat <- adat[, c('binomial', 'mass..g.')]
colnames(adat) <- c('taxon', 'mass_g')
adat <- FixNames(adat)
adat <-
  ddply(
    adat,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = NA,
    n = length(mass_g)
  )
adat$source <- 'Feldman_etal_2016'
FE <- adat
save(FE, file = 'BodyMass_Feldman_etal_2016.Rdata')

##########################################################################
# Killen, S. S., D. S. Glazier, E. L. Rezende, T. D. Clark, D. Atkinson, A. S. T. Willener, and L. G. Halsey. 2016. Ecological Influences and Morphological Correlates of Resting and Maximal Metabolic Rates across Teleost Fish Species. The American Naturalist 187:592-606.
adat <-
  read.csv('Killen_etal_2016/TableS1.csv', header = TRUE)
adat <- adat[, c('species', 'MMRmass')]
colnames(adat) <- c('taxon', 'mass_g')
adat <- FixNames(adat)
adat <-
  ddply(
    adat,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = NA,
    n = length(mass_g)
  )
adat$source <- 'Killen_etal_2016'
KI <- adat
save(KI, file = 'BodyMass_Killen_etal_2016.Rdata')

##########################################################################
# Tucker, M. A., and T. L. Rogers. 2014. Examining predator–prey body size, trophic level and body mass across marine and terrestrial mammals. Proceedings of the Royal Society B: Biological Sciences 281.
adat <-
  read.csv('Tucker_etal_2014a/TrophicLevel_Appendix1.csv',
           header = TRUE)
adat <- adat[, c('Taxon', 'Mass..log10.kg.')]
colnames(adat) <- c('taxon', 'mass_g')
adat$mass_g <-
  (10 ^ adat$mass_g) * 1000 # convert natural scale and then to grams
adat <- FixNames(adat)
adat <-
  ddply(
    adat,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = NA,
    n = length(mass_g)
  )
adat$source <- 'Tucker_etal_2014a'
TU1 <- adat
save(TU1, file = 'BodyMass_Tucker_etal_2014a.Rdata')

##########################################################################
# Tucker, M. A., T. J. Ord, and T. L. Rogers. 2014. Evolutionary predictors of mammalian home range size: body mass, diet and the environment. Global Ecology and Biogeography 23:1105-1114.
adat <-
  read.csv('Tucker_etal_2014b/Tucker_etal_2014b.csv',
           header = TRUE)
adat <- adat[, c('Taxon', 'log10.Mass..kg.')]
colnames(adat) <- c('taxon', 'mass_g')
adat$mass_g <-
  (10 ^ adat$mass_g) * 1000 # convert natural scale and then to grams
adat <- FixNames(adat)
adat <-
  ddply(
    adat,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = NA,
    n = length(mass_g)
  )
adat$source <- 'Tucker_etal_2014b'
TU2 <- adat
save(TU2, file = 'BodyMass_Tucker_etal_2014b.Rdata')

##########################################################################
# Hirt, M. R., W. Jetz, B. C. Rall, and U. Brose. 2017. A general scaling law reveals why the largest animals are not the fastest. Nature Ecology & Evolution 1:1116-1122.
adat <-
  read.csv('Hirt_etal_2017/Hirt_etal_2017.csv', header = TRUE)
adat <- adat[, c('species', 'body.mass..kg.')]
colnames(adat) <- c('taxon', 'mass_g')
adat$mass_g <- adat$mass_g * 1000 # convert to grams
adat <- FixNames(adat)
adat <-
  ddply(
    adat,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = NA,
    n = length(mass_g)
  )
adat$source <- 'Hirt_etal_2017'
HI <- adat
save(HI, file = 'BodyMass_Hirt_etal_2017.Rdata')

##########################################################################
# Gillooly, J. F., J. P. Gomez, E. V. Mavrodiev, Y. Rong, and E. S. McLamore. 2016. Body mass scaling of passive oxygen diffusion in endotherms and ectotherms. Proceedings of the National Academy of Sciences 113:5340-5345.
adat <-
  read.csv('Gillooly_etal_2016/Gillooly_etal_2016.csv',
           header = TRUE)
adat$taxon <- paste(adat$Genus, adat$Species)
adat <- adat[, c('taxon', 'M')]
colnames(adat) <- c('taxon', 'mass_g')
adat <- FixNames(adat)
adat <-
  ddply(
    adat,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = NA,
    n = length(mass_g)
  )
adat$source <- 'Gillooly_etal_2016'
GI <- adat
save(GI, file = 'BodyMass_Gillooly_etal_2016.Rdata')

##########################################################################
# Quaardvark from ADW https://animaldiversity.ummz.umich.edu/quaardvark/
adat <-
  read.csv('Quaardvark/report-201802270108.csv',
           header = TRUE)
adat <- adat[, c(1:5, 8:10, 16)]
adat[which(adat == 0, arr.ind = TRUE)] <- NA
nNAdat <- !is.na(adat)
dat1 <-
  adat[which(nNAdat[, 2] == TRUE), c(1, 2)]
colnames(dat1) <- c('taxon', 'mass_g')
dat2 <-
  adat[which(nNAdat[, 2] == FALSE &
              nNAdat[, 3] == TRUE), c(1, 3)]
colnames(dat2) <- c('taxon', 'mass_g')
dat12 <- rbind(dat1, dat2)
dat3 <-
  adat[which(nNAdat[, 8] == TRUE), c(1, 8)]
colnames(dat3) <- c('taxon', 'gen_time_days')
dat4 <-
  adat[which(nNAdat[, 8] == FALSE &
              nNAdat[, 9] == TRUE), c(1, 9)]
colnames(dat4) <- c('taxon', 'gen_time_days')
dat34 <- rbind(dat3, dat4)
dat34$gen_time_days <- dat34$gen_time_days * 365 # convert years to days
adat <- merge(dat12, dat34, all = TRUE)
adat <- FixNames(adat)
adat <-
  ddply(
    adat,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = gmean(gen_time_days),
    n = length(mass_g)
  )
adat$source <- 'Quaardvark'
AA <- adat
save(AA, file = 'BodyMass_Quaardvark.Rdata')
##########################################################################
# AnAge - Tacutu, R., Craig, T., Budovsky, A., Wuttke, D., Lehmann, G., Taranukha, D., Costa, J., Fraifeld, V. E., de Magalhaes, J. P. (2013) "Human Ageing Genomic Resources: Integrated databases and tools for the biology and genetics of ageing." Nucleic Acids Research 41(D1):D1027-D1033
adat <- read.csv('AnAge/AnAge_data.csv', header = TRUE)
adat$taxon <- paste(adat$Genus, adat$Species)
adat <-
  adat[, c('taxon',
          'Adult.weight..g.',
          'Body.mass..g.',
          'Maximum.longevity..yrs.')]
colnames(adat) <- c('taxon', 'a.mass_g', 'mass_g', 'MaxLong')
adat$mass_g[is.na(adat$mass_g) &
             !is.na(adat$a.mass_g)] <-
  adat$a.mass_g[is.na(adat$mass_g) & !is.na(adat$a.mass_g)]
adat <- FixNames(adat)
adat <-
  ddply(
    adat,
    .(taxon),
    summarise,
    mass_g = gmean(mass_g),
    gen_time_days = gmean(MaxLong),
    n = length(mass_g)
  )
adat$source <- 'AnAge'
AN <- adat[!is.na(adat$mass_g) | !is.na(adat$gen_time_days), ]
save(AN, file = 'BodyMass_AnAge.Rdata')

##########################################################################
##########################################################################
# Combine databases, given ordered preference:
##############################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load(file = 'BodyMass_Brown_etal_2018.Rdata')
load(file = 'BodyMass_Smith_2003.Rdata')
load(file = 'BodyMass_AndersonGillooly_2017.Rdata')
load(file = 'BodyMass_Gillooly_etal_2016.Rdata')
load(file = 'BodyMass_Jennings_2002.Rdata')
load(file = 'BodyMass_Lislevand_etal_2007.Rdata')
load(file = 'BodyMass_Killen_etal_2016.Rdata')
load(file = 'BodyMass_Feldman_etal_2016.Rdata')
load(file = 'BodyMass_Tucker_etal_2014a.Rdata')
load(file = 'BodyMass_Tucker_etal_2014b.Rdata')
load(file = 'BodyMass_Hirt_etal_2017.Rdata')
load(file = 'BodyMass_Eklof_etal_2017.Rdata')
load(file = 'BodyMass_Quaardvark.Rdata')
load(file = 'BodyMass_AnAge.Rdata')
load(file = 'BodyMass_DataRetrieverAll.Rdata')

adat <- BR
adat <- merge(adat, MM[!MM$taxon %in% adat$taxon, ], all = TRUE)
  nrow(adat)
adat <- merge(adat, AG[!AG$taxon %in% adat$taxon, ], all = TRUE)
  nrow(adat)
adat <- merge(adat, GI[!GI$taxon %in% adat$taxon, ], all = TRUE)
  nrow(adat)
adat <- merge(adat, JE[!JE$taxon %in% adat$taxon, ], all = TRUE)
  nrow(adat)
adat <- merge(adat, LI[!LI$taxon %in% adat$taxon, ], all = TRUE)
  nrow(adat)
adat <- merge(adat, KI[!KI$taxon %in% adat$taxon, ], all = TRUE)
  nrow(adat)
adat <- merge(adat, FE[!FE$taxon %in% adat$taxon, ], all = TRUE)
  nrow(adat)
adat <- merge(adat, TU1[!TU1$taxon %in% adat$taxon, ], all = TRUE)
  nrow(adat)
adat <- merge(adat, TU2[!TU2$taxon %in% adat$taxon, ], all = TRUE)
  nrow(adat)
adat <- merge(adat, HI[!HI$taxon %in% adat$taxon, ], all = TRUE)
  nrow(adat)
adat <- merge(adat, EK[!EK$taxon %in% adat$taxon, ], all = TRUE)
  nrow(adat)
adat <- merge(adat, AA[!AA$taxon %in% adat$taxon, ], all = TRUE)
  nrow(adat)
adat <- merge(adat, AN[!AN$taxon %in% adat$taxon, ], all = TRUE)
  nrow(adat)
adat <- merge(adat, DR[!DR$taxon %in% adat$taxon, ], all = TRUE)
  nrow(adat)

adat$mass_g[is.nan(adat$mass_g)] <- NA
adat$gen_time_days[is.nan(adat$gen_time_days)] <- NA
nrow(adat)
DBs <- adat

##########################################################################
# Export and have lab fill in as many of the rest that are needed
#################################################################
setwd(wd)

Cons <- unique(dat[, c('Taxon.group', 'Consumer.identity')])

Cons <- Cons[Cons$Consumer.identity %!in% unique(DBs$taxon), ]
Cons$mass_g <- NA
Cons$gen_time_days <- NA
Cons$mass_Source <- NA
Cons$gen_Source <- NA

write.csv(Cons, 
          file = '../tmp/BodyMass/BodyMass_Needs.csv', 
          row.names = FALSE)

# Paste the above into a Google Doc to have lab add data

##########################################################
# Bring in additional body sizes that the lab has collated
DBs$source_gen <- 'NA'
DBs$source_gen[!is.na(DBs$gen_time_days)] <-
  DBs$source[!is.na(DBs$gen_time_days)]
colnames(DBs)[which(colnames(DBs) == 'source')] <- 'source_mass'

ddat <-
  read_sheet("https://docs.google.com/spreadsheets/d/1_TzVFXjcUrDBGHbpRuLh3NwYIF1I8AucsJh8heIFulY/edit?usp=sharing",
             sheet = 'BM_data',
             col_types = 'cnncc')

ddat <- ddat[which(!is.na(ddat$mass_g) | !is.na(ddat$gen_time_days)), ]
DBs <- DBs[DBs$taxon %!in% ddat$taxon, ]

adat <- merge(ddat, DBs, all = TRUE)

write.csv(adat, 
          file = '../tmp/BodyMass/FracFeed_BodyMass.csv',
          row.names = FALSE)

#######################################################################
#######################################################################
#######################################################################
