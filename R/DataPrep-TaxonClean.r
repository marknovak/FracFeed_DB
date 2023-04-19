##########################################################################
# Match (and correct) consumers in FracFeed data to Tree of Life.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########################################################################
# This code is iterative in that you'll have to repeat a few steps 
# by jumping back up in the code a few times.
##########################################################################
options(warn = 1) # to issue warnings immediately
##########################################################################
# install.packages(c("curl", "httr"))
# library(devtools)
# Tree of Life (https://cran.r-project.org/web/packages/rotl/vignettes/how-to-use-rotl.html)
# devtools::install_github("ropensci/rotl@fix-101", force=FALSE) # rotl on CRAN can't handle a large number of taxa at the same time, so use this fix.
# source("https://install-github.me/ropensci/rotl")
library(rotl) 

#######################
# Define some functions
#######################
# Which elements are not in the vector
'%!in%' <- function(x, y){
  ! ('%in%'(x, y))
}

# Capitalize first letter of taxon names
firstup <-
  function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }

# Fix taxon names
FixNames <- function(dat) {
  dat$taxon <- gsub(' ', '_', dat$taxon)
  dat$taxon <- firstup(dat$taxon)
  dat <- subset(dat, taxon != '')
  dat$taxon <- word(dat$taxon, 1, 2, sep = '_') # Remove subspecies names
  return(dat)
}

###########################################################################
ConsIdent <- sort(unique(dat$Consumer.identity))

#######################################################################
# Problem taxa identified during prior attempts to match to Tree of Life
#######################################################################
warning(
  'INCERTAE_SEDIS detected.  These species should be periodically checked in the ToL to see if their status has changed.'
)
# See below for source of names
rem <-
  c(
    # "Centropomus_pectinatus",
    # "Chromis_chrysura",
    # "Platycephalus_speculator",
    # "Psellogrammus_kennedyi",
    "Astropecten_platyacanthus",
    "Hemigrammus_arriba"
  )
print(rem)

test <- tnrs_match_names(rem, context_name = "Animals")

ConsIdent <- ConsIdent[ConsIdent %!in% rem]

###################################
# Identify taxa in the Tree of Life
###################################
taxa <- tnrs_match_names(ConsIdent, context_name = "Animals")

write.csv(taxa, 
          file = '../tmp/TaxonClean/Taxa.csv',
          row.names = FALSE)
warning("\nFinished matching taxa to the tree of life.\nYou should now check for instances of, and select among, multiple matches.")

##################
# Resolve problems
##################
# foc_tax <- tnrs_match_names("Pterostichus_melanaria")
# foc_ins <- inspect(foc_tax,taxon_name = 'Feronia'); foc_ins
# taxonomy_taxon_info(foc_ins$ott_id[2])
# sort(taxonomy_subtree(foc_ins$ott_id[2])$tip_label)
# taxa[is.na(taxa[,1]),]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following taxa have multiple matches (often synonyms) that need to be corrected
# This has to be done manually (mostly using WORMS, ReptileBase, Fishbase).
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
multmatch <- taxa[taxa$number_matches > 1, ]
if(nrow(multmatch) > 0){
  warning(paste0(nrow(multmatch),
                ' (', round(nrow(multmatch) / nrow(taxa) * 100, 1), '%) ',
                'of the taxa have multiple matches in the Tree of Life.'))
}
multmatch$chosen.ott_id <- multmatch$chosen.name <- NA
multmatch$ott_id <- as.numeric(multmatch$ott_id)

if (matchAllTaxa) {
  write.csv(multmatch, 
            file = '../tmp/TaxonClean/Taxa_MultipleMatches.csv',
            row.names = FALSE)
}

if (!matchNewTaxa){
  warning('You have decided not to select among multiple matches, instead relying on prior selections.  This will be a problem if there are new taxa in the database.')
}
  
if (matchNewTaxa | matchAllTaxa) {
  prior.multmatch <-
    read.csv('../tmp/TaxonClean/Taxa_MultipleMatches.csv')
  if(nrow(prior.multmatch) > 1){
    prior.multmatch <- subset(prior.multmatch, !is.na(prior.multmatch$chosen.ott_id))
  }
  for (i in 1:nrow(multmatch)) {
    # If a prior match has been assigned....
    if (multmatch$search_string[i] %in% prior.multmatch$search_string) {
      sel_ottid <-
        prior.multmatch$chosen.ott_id[
          which(prior.multmatch$search_string == multmatch$search_string[i])]
      if (!is.na(sel_ottid)) {
        multmatch$chosen.ott_id[i] <- sel_ottid
        taxa <- update(taxa,
                 ott_id = multmatch$ott_id[i],
                 new_ott_id = multmatch$chosen.ott_id[i])
      }
    }
    
    # If no prior match has been assigned...
    if (multmatch$search_string[i] %!in% prior.multmatch$search_string |
        isTRUE(is.na(
          prior.multmatch$chosen.ott_id[
            which(prior.multmatch$search_string == multmatch$search_string[i])]))) {
      insp <- inspect(taxa, taxon_name = multmatch$search_string[i])
      
      if (length(unique(insp$ott_id)) == 1) {
        multmatch$chosen.ott_id[i] <- insp$ott_id[1]
      }
      
      if (length(unique(insp$ott_id)) > 1) {
        # sometimes multiple rows actually have the same ott_id, 
        # so skip these since it won't change anything
        inf <- array(NA, dim = c(40, nrow(insp)))
        colnames(inf) <- insp$ott_id
        for (r in 1:nrow(insp)) {
          foc_ottid <- insp$ott_id[r]
          tlin <-
            tax_lineage(taxonomy_taxon_info(as.numeric(foc_ottid), 
                                            include_lineage = TRUE))[[1]][2]
          inf[1:nrow(tlin), r] <- rev(tlin[, 1])
        }
        inf <- inf[-which(apply(is.na(inf), 1, sum) == ncol(inf)), ]
        print(inf)
        print(insp)
        sel_tax <-
          as.numeric(
            readline(prompt = 
 "Which row (i.e. taxon) do you want to choose to replace the existing one? ")
          )
        if (sel_tax > nrow(insp) |
            !is.numeric(sel_tax)) {
          sel_tax <-
            as.numeric(
              readline(prompt = 
"Try again. Which row (i.e. taxon) do you want to choose to replace the existing one? ")
            )
        }
        multmatch$chosen.ott_id[i] <- insp$ott_id[sel_tax]
      }
      
      taxa <-
        update(taxa,
               ott_id = multmatch$ott_id[i],
               new_ott_id = multmatch$chosen.ott_id[i])
      print(paste(i, ' of ', nrow(multmatch), ' completed.'))
      
      if (promptNext) {
        yn <- readline(prompt = "Next taxon? y/n ")
      }else{
        yn <- 'y'
      }
      if (yn != 'y' & yn !='n') {
        yn <- readline(prompt = "Next taxon? y/n ")
      }
      if (yn == 'n') {
        break
      }
      write.csv(multmatch, '../tmp/TaxonClean/Taxa_MultipleMatches.csv',
                row.names = FALSE)
    }
  }
  write.csv(multmatch, '../tmp/TaxonClean/Taxa_MultipleMatches.csv',
            row.names = FALSE)
  write.csv(taxa, file = '../tmp/TaxonClean/Taxa_Matched.csv',
            row.names = FALSE)
  print('All done matching.')
}

# ~~~~~~~~~~~~~~~~~~~~~
# Inspect flagged taxa
# ~~~~~~~~~~~~~~~~~~~~~
# Uncertain taxonomic positions cause problems, so remove upstream in DataPrep script
unique(taxa$flags)
taxa[which(taxa$flags == 'INCERTAE_SEDIS_INHERITED'), ]
taxa[which(taxa$flags == 'INCERTAE_SEDIS'), ]

#####################
# Hardwire name fixes (some rotl names have extra text)
#####################
taxa$unique_name[grep('Gadus morhua', taxa$unique_name)] <-
  'Gadus morhua'


taxa$search_string <- firstup(taxa$search_string)
taxa$unique_name <- gsub(' ', '_', taxa$unique_name)

#################################################
# Check to ensure there are no duplicates in taxa
#################################################
a <- taxa[duplicated(taxa[, c('unique_name', 'ott_id')]), ]
b <- taxa[duplicated(taxa[, c('unique_name', 'ott_id')], 
                     fromLast = TRUE), ]
Dups <- merge(a[, c(1, 2, 4)], b[, c(1, 2, 4)], all = TRUE)
# All "duplicates" have different search_strings,
# meaning these are species with synonymous names in the original data.
if (nrow(Dups) > 0) {
  warning('\nThere are duplicate species due to synonyms that may need to be fixed in the original data.')
  print(Dups[order(Dups$unique_name), ])
}

message(paste0('\nThere are a total of ', 
               length(unique(taxa$unique_name)),
               ' unique and identified taxa in the database.'))

#################################################

write.csv(taxa, 
          file = '../tmp/TaxonClean/Taxa_Matched.csv',
          row.names = FALSE)

##########################################################################
##########################################################################
##########################################################################