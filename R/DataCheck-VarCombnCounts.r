###############################################
# Count data set sizes by variable combinations
###############################################
load('../tmp/tmp_DB/FracFeed_Data_Clean.Rdata')

vars <- c(
  'First.entry.Name',
  'Citation',
  'Taxon.group',
  'EndoEcto',
  'Total.stomachs.count.given',
  'Time.averaging',
  'Space.averaging',
  'mass_g',
  'Diet.richness.minimum',
  'Year',
  'Month',
  'Ecosystem',
  'Latitude',
  'Feeding.data.type'
)

tdat <- dat[, vars]

# Do not consider the following compilation studies from which 
# we won't be able to extract more information
tdat <- subset(tdat, 
               tdat$Citation!='Arrington_2002' &
                 tdat$Citation!='Huey_2001')

##################################################################
# Start a fresh directory
unlink("../tmp/VarCombnCounts", recursive = TRUE)
dir.create("../tmp/VarCombnCounts")

##################################################################

UniVarCnt <- apply(!is.na(tdat), 2, sum)
MVarCnt <- count(!is.na(tdat))
MVarCnt <- MVarCnt[order(MVarCnt$freq, decreasing = TRUE),]
MVarCnt <- MVarCnt[, c(ncol(MVarCnt),
                       order(apply(MVarCnt[,-ncol(MVarCnt)], 2, sum),
                             decreasing = TRUE))]
colnames(MVarCnt) <- sub('x.', '', colnames(MVarCnt))

dropCols <- grep('TRUE', apply(MVarCnt[,-1], 2, all)) + 1
MVarCnt.Out <- MVarCnt[, -dropCols]

write.table(MVarCnt.Out,
            '../tmp/VarCombnCounts/aaaVarCombnCounts.csv',
            sep = ',',
            row.names = FALSE)

# Which studies should be re-examine to try fill-in variables?
MVarCnt2 <- MVarCnt[MVarCnt$freq > 50 ,-1]
tdat <- tdat[, sort(names(tdat))]
MVarCnt2 <- MVarCnt2[, sort(names(MVarCnt2))]

for (foc in 2:nrow(MVarCnt2)) {
  temp <- tdat[which(apply(!is.na(tdat), 1, 
                    function(x) {sum(x == MVarCnt2[foc,]) }) == ncol(MVarCnt2)),]
  temp <- temp[order(temp$First.entry.Name, temp$Citation),]
  write.csv(temp,
              paste0('../tmp/VarCombnCounts/Row', foc, '.csv'),
              row.names = FALSE)
}

message("
Inspect the contents of 'tmp/VarCombnCount/' to see if there are
surveys for which additional covariate information could be obtained.
")

##################################################################
# Given that consumer body mass information is most commonly missing variable,
# count the number of surveys for the consumers with missing body mass
# to identify most valuable consumers.

tdat <- dat[is.na(dat$mass_g), 
            c('Taxon.group', 'Consumer.identity', 'mass_g')]
tdats <- data.frame(table(tdat$Consumer.identity,
               dnn = c('Consumer.identity')))
tdatx <- unique(merge(tdat, tdats))
tdatx <- tdatx[order(tdatx$Freq, decreasing = TRUE),]
write.table(tdatx,
          '../tmp/BodyMass/BodyMass_Needs.csv',
          sep = ',',
          row.names = FALSE)
          
message("
Inspect 'tmp/BodyMass/BodyMass_Needs.csv' to see which
taxa of 'high value' (those that have many surveys) need 
body mass estimates.
")

###############################################################################
###############################################################################
###############################################################################
