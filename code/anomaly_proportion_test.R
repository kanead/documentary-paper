##################################################################################
# Documentary analysis
# Code compares the proportion of anomalies for the Planet Earth 1 species
# to the proportion of anomalies seen for the Planet Earth 2 species
##################################################################################

#' PE1 had 8 anomalies out of 121 species
#' PE2 had 46 anomalies out of 112 species
prop.test(x = c(8, 46), n = c(121, 112), correct = FALSE)
