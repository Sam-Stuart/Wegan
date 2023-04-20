# Prepare the clinical trait data list  
traitData <- read.csv("./rawData-WGCNA/ClinicalTraits.csv") 

head(traitData)

# Remove variables we don't need 
allTraits <- traitData[, -c(31, 16)] 
allTraits <- allTraits[, c(2, 11:36)]

allTraits <- allTraits %>% column_to_rownames("Mice") 

save(allTraits, file = "./clinicalTrait_example.RData")  

