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
                     meta$VariableName != 'First.entry.Name')

write.csv(pub.meta, 
          file = '../tmp_DB/FracFeed_Metadata.csv', 
          row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~
# FracFeed citations
#~~~~~~~~~~~~~~~~~~~
cite <-
  read_sheet(
    'https://docs.google.com/spreadsheets/d/16zumXsmbAU-MbBgDmiTjNX8m9YxmbHRUAtSm2xNOJ4I/edit?usp=sharing',
    sheet = 'Citations',
    col_types = 'lccclc')

pub.cite <- cite[order(cite$CitationID),]
pub.cite <- pub.cite[, c('CitationID',
                         'Bibcite',
                         'Citation')]

write.csv(pub.cite, 
          file = '../tmp_DB/FracFeed_Citations.csv', 
          row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~~
# Body mass citations
#~~~~~~~~~~~~~~~~~~~~
dcite <-
  read_sheet("https://docs.google.com/spreadsheets/d/1_TzVFXjcUrDBGHbpRuLh3NwYIF1I8AucsJh8heIFulY/edit?usp=sharing",
    sheet = 'BM_citations',
    col_types = 'ccc')

pub.dcite <- dcite[order(dcite$CitationID), ]

write.csv(pub.dcite,
          '../tmp_DB/FracFeed_Citations_BodyMass.csv',
          row.names = FALSE)

###############################################################################
###############################################################################
###############################################################################