############################################################################
# Import and process meta-data and citation information
############################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta-data from GoogleSheet
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
meta <-
  read_sheet(
    'https://docs.google.com/spreadsheets/d/16zumXsmbAU-MbBgDmiTjNX8m9YxmbHRUAtSm2xNOJ4I/edit?usp=sharing',
    sheet = 'Meta-data',
    col_types = 'lcccc')
    
pub.meta <- subset(meta, 
                   meta$VariableName != 'Checked' & 
                     meta$VariableName != 'Consult.needed' &
                     meta$VariableName != 'First.entry.Name' &
                     meta$VariableAbbreviation != '-'
                     )

# Reorder rows to match order of columns in data file
all.present <-
  all(pub.meta$VariableAbbreviation %in% colnames(fdatc) &
      colnames(fdatc) %in% pub.meta$VariableAbbreviation  )
if(!all.present){
  warning("Data columns and meta-data variables don't match.")
}

pub.meta <-
  pub.meta[match(colnames(fdatc), pub.meta$VariableAbbreviation),
           c('VariableAbbreviation',
             'VariableName',
             'VariableDescription',
             'VariableValues',
             'GeneratedByScripts')]

write.csv(pub.meta, 
          file = '../tmp/FracFeed_Data_Metadata.csv', 
          row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~
# FracFeed citations
#~~~~~~~~~~~~~~~~~~~
cite <-
  read_sheet(
    'https://docs.google.com/spreadsheets/d/16zumXsmbAU-MbBgDmiTjNX8m9YxmbHRUAtSm2xNOJ4I/edit?usp=sharing',
    sheet = 'Citations',
    col_types = 'lccclc')

pub.cite <- cite[order(cite$CiteID),]
pub.cite <- pub.cite[, c('CiteID',
                         'Bibcite',
                         'Citation')]

write.csv(pub.cite, 
          file = '../tmp/FracFeed_Citations.csv', 
          row.names = FALSE)

yaml <- c("---",
    "bibliography: FracFeed_Citations.bib",
    "nocite: '@*'",
    "...",
    "# Data sources"
)

writeLines(yaml, "../Bib/FracFeed_Citations.md")

pandoc_convert(input = '../Bib/FracFeed_Citations.md', 
               to = 'markdown_phpextra',
               output = '../Bib/README.md',
               citeproc = TRUE
               )
file.remove("../Bib/FracFeed_Citations.md")

#~~~~~~~~~~~~~~~~~~~~
# Body mass citations
#~~~~~~~~~~~~~~~~~~~~
# Citations are owned by TaxonBodyMass_DB; read its committed CSV and filter
# to sources that actually contributed body mass to FracFeed taxa.
tbm_cite_path <- file.path('..', '..', 'TaxonBodyMass_DB', 'output',
                           'TaxonBodyMass_Citations.csv')
dcite <- read.csv(tbm_cite_path, stringsAsFactors = FALSE)

fracfeed_sources <- adat[adat$taxon %in% dat$Consumer.identity, 'source_mass']
used_sources <- unique(unlist(strsplit(fracfeed_sources, '-')))
pub.dcite <- dcite[is.na(dcite$CiteID) | dcite$CiteID %in% used_sources, ]
pub.dcite <- pub.dcite[order(pub.dcite$CiteID), ]

write.csv(pub.dcite,
          '../tmp/FracFeed_Citations_BodyMass.csv',
          row.names = FALSE)

# Body mass bib: filter TaxonBodyMass_DB's bib to entries used in FracFeed taxa
tbm_bib_src <- file.path('..', '..', 'TaxonBodyMass_DB', 'Bib',
                         'TaxonBodyMass_Citations.bib')
bib_lines  <- readLines(tbm_bib_src)
entry_keys <- sub('^@\\w+\\{([^,]+),.*', '\\1',
                  bib_lines[grepl('^@', bib_lines)], perl = TRUE)
bib_text <- paste(bib_lines, collapse = '\n')
parts    <- strsplit(bib_text, '\n\n(?=@)', perl = TRUE)[[1]]
header   <- parts[!grepl('^@', trimws(parts))]
entries  <- parts[ grepl('^@', trimws(parts))]
keep_keys <- pub.dcite$Bibcite
matched   <- entries[entry_keys %in% keep_keys]
writeLines(paste(c(header, matched), collapse = '\n\n'),
           '../Bib/FracFeed_Citations_BodyMass.bib')

###############################################################################
###############################################################################
###############################################################################