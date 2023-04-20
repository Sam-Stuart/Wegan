femData <- utils::read.csv(file = "./rawData-WGCNA/LiverFemale3600.csv")   

femData_trimmed <- femData %>% 
    dplyr::select(-c(2:8)) %>% 
    column_to_rownames("substanceBXH") 

# Original mSet obejct 
mSetObj_example <- list(dataSet = list(orig = femData_trimmed)) 

save(mSetObj_example, file = "mSet_example.RData")
