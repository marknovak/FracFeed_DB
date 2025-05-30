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
dcite <-
  read_sheet("https://docs.google.com/spreadsheets/d/1_TzVFXjcUrDBGHbpRuLh3NwYIF1I8AucsJh8heIFulY/edit?usp=sharing",
    sheet = 'BM_citations',
    col_types = 'ccc')

pub.dcite <- dcite[order(dcite$CiteID), ]

write.csv(pub.dcite,
          '../tmp/FracFeed_Citations_BodyMass.csv',
          row.names = FALSE)

###############################################################################
###############################################################################
###############################################################################