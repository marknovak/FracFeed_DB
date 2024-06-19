# FracFeed Database
This main directory contains the files associated with the complete, curated, and quality-controlled *FracFeed* database.  Please use these files (rather than those contained in */tmp*) for your own analyses.

* *FracFeed_Data.csv* or *FracFeed_Data.Rdata*- the full, curated compilation of fraction-feeding and covariate data
* *FracFeed_Data_Metadata.csv* - meta-data for the *FracFeed_Data*
* *FracFeed_Citations.csv* - source citations for the *FracFeed_Data*
* *FracFeed_Citations_BodyMass.csv* - source citations for body mass data

Taxonomic standardization uses the [Open Tree of Life](https://opentreeoflife.github.io/) through the [rotl package](https://cran.r-project.org/web/packages/rotl/index.html).

### Sub-directories:
* [Bib](Bib) - *Bibtex* bibliographic files for the source citations
* [Figs](Figs) - Summary views of the data (shown below)
* [OtherData](OtherData) - Original auxiliary data sources
* [R](R) - Code for curating the raw data
* [tmp](tmp) - Temporary files created during data curation

## Funding
The compilation of this database began with the support of a National Science Foundation award [DEB-1353827](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1353827&HistoricalAwards=false).

## Please contribute data!
If you have or know of published predator diet surveys for which information with which to calculate the fraction of (non-)feeding individuals is available, please send us the associated citation ([mark.novak@oregonstate.edu](mark.novak@oregonstate.edu)).  Feel free to also suggest additional co-variates to start collecting information on.

## Summary views
![Global map by Taxon Group and Ecosystem](Figs/Map_global_TaxonGroupEcosystem.png)
![Global map by Survey Decade](Figs/Map_global_Decade.png)
![Frequency distribution by Taxon Group](Figs/Freq_TaxonGroups.png)
![Frequency distribution by Ecosystem](Figs/Freq_Ecosystem.png)
![Frequency distribution by Survey Year](Figs/Freq_SurveyYear.png)
