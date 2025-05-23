##########################################################################
# Match (and correct) consumers in FracFeed data to Tree of Life
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########################################################################
message("
    This taxon-clearning code is iterative in that you'll have to repeat 
    a few steps by jumping back up in the code a few times.
")
##########################################################################
options(warn = 1) # to issue warnings immediately
##########################################################################

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
  dat$taxon <- gsub(' ', '_', iconv(dat$taxon, from = "ISO-8859-1", to = "UTF-8"))
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
# See below for source of names
rem <-
  c(
  )

if(length(rem>0)){
  message('
      INCERTAE_SEDIS detected.  
      These species should be periodically checked in the ToL 
      to see if their status has changed.
  ')
  print(rem)
  
  test <- tnrs_match_names(rem, context_name = "Animals")
  
  message('
          Unmatched species have been removed.
  ')
  ConsIdent <- ConsIdent[ConsIdent %!in% rem]
}
###################################
# Identify taxa in the Tree of Life
###################################
ConsIdent <- firstup(sub('_', ' ', ConsIdent))
taxa <- tnrs_match_names(ConsIdent, context_name = "Animals")

write.csv(taxa, 
          file = '../tmp/TaxonClean/Taxa.csv',
          row.names = FALSE)
message("
    Finished matching taxa to the tree of life.
    You should now check for instances of, and select among, multiple matches.
")

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
  message(paste0(nrow(multmatch),
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
  message('
  You have decided not to select among multiple matches, 
            instead relying on prior selections.
            This will be a problem if there are new taxa in the database.
  ')
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
        
        if(select_max_score){
          sel_tax <- which.max(insp$score)
        }else{
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

impmatch <- subset(taxa, score < 1)
if(nrow(impmatch > 0)){
  write.csv(impmatch, file = '../tmp/TaxonClean/Taxa_ApproxMatches.csv',
            row.names = FALSE)
  message(paste0(nrow(impmatch),
                 ' (', round(nrow(impmatch) / nrow(taxa) * 100, 1), '%) ',
                 'of the taxa have approximate matches in the Tree of Life.'))
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
taxa$unique_name[grep('Physeter catodon', taxa$unique_name)] <- 
  'Physeter macrocephalus'
taxa$unique_name[grep('Oncorhynchus mykiss', taxa$unique_name)] <- 
  'Oncorhynchus mykiss'


taxa$search_string <- firstup(taxa$search_string)
taxa$unique_name <- gsub(' ', '_', taxa$unique_name)
taxa$search_string <- gsub(' ', '_', taxa$search_string)

#################################################
# Check to ensure there are no duplicates in taxa
#################################################
a <- taxa[duplicated(taxa[, c('unique_name', 'ott_id')]), ]
b <- taxa[duplicated(taxa[, c('unique_name', 'ott_id')], 
                     fromLast = TRUE), ]
Dups <- merge(a[, c(1, 2, 4)], b[, c(1, 2, 4)], all = TRUE)
# All "duplicates" have different search_strings,
# meaning these are species with synonymous names in the original data.
TE <- '../tmp/ErrorReports/TaxonErrors.txt'
if (file.exists(TE)) {
  file.remove(TE)
}
if (nrow(Dups) > 0) {
  warn <-
    'There are duplicate species due to synonyms that may need to be fixed in the original data..\n'
  warning(warn, immediate. = TRUE)
  sink(file = TE)
    print(Dups[order(Dups$unique_name), ])
  sink()
}

message(paste0('\nThere are a total of ', 
               length(unique(taxa$unique_name)),
               ' taxa in the database.'))

#################################################

write.csv(taxa, 
          file = '../tmp/TaxonClean/Taxa_Matched.csv',
          row.names = FALSE)

##########################################################################
##########################################################################
##########################################################################