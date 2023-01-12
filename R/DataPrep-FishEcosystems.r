library(rfishbase) # ecosystem() to fill in ecosystem gaps

fish.sel <-
  which(dat$Taxon.group == 'Fish' & 
          is.na(dat$Ecosystem) &
          grepl("[[:punct:]]", dat$Consumer.identity) )  # Skip genus-only taxa
fish <- dat[fish.sel, ]
fish$Consumer.identity <- gsub("[[:punct:]]", " ", fish$Consumer.identity)
EcoC <- c('Lotic', 'Lentic', 'Freshwater', 'Marine')
Eco <- c('River', 'Lake', 'Marine', 'Sea', 'Bay', 'Gulf', 'Lagoon')
AllEco <- dim(0)
for (f in 1:length(fish$Consumer.identity)) {
  eco <- rfishbase::ecosystem(fish$Consumer.identity[f])$EcosystemType
  AllEco <- c(AllEco, eco)
  teco <- which(!is.na(charmatch(Eco, eco)))
  teco[teco > 3] <- 4
  teco <- sort(unique(teco))
  if (length(teco) == 0 |
      length(teco) > 2) {
    fish$Ecosystem[f] <- NA
  }
  if (length(teco) == 1) {
    if (teco == 1) {
      fish$Ecosystem[f] <- EcoC[1]
    }
    if (teco == 2) {
      fish$Ecosystem[f] <- EcoC[2]
    }
    if (teco == 4) {
      fish$Ecosystem[f] <- EcoC[4]
    }
  }
  if (length(teco) == 2) {
    if (teco[1] == 1 & teco[2] == 2) {
      fish$Ecosystem[f] <- EcoC[3]
    }
  }
  print(paste(f, 'of ', length(fish$Consumer.identity), 'completed.'))
}

fish$Consumer.identity <- gsub(" ", ".", fish$Consumer.identity)
print("New fishes matched to their ecosystems.")

newEcos <- fish[!is.na(fish$Ecosystem),c('Consumer.identity','Ecosystem')]
write.csv(newEcos, file = '../tmp/FishEcosystems/newFishEcosystems.csv', row.names = FALSE)
