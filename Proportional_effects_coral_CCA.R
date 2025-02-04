library(plotrix)

##############################
##Temperature
####
#temp from control

library(plotrix)
setwd("/Users/steevecomeau/Documents/Article/Published/Review_reef_budget/Data_for_Chris_P/")

z=read.table("responses and slopes_Oct_coral_Temp_SD.csv", header=T, sep=";",dec=",", na.string="NA", fill=TRUE)
head(z)
library(plyr)
z2=read.table("ensemble_model_combined_mean_sst.csv", header=T, sep=",",dec="." ,na.string="NA", fill=TRUE)

mean_CV <- tapply(z$CV, z$Study, mean)
weight=1/mean_CV


######################
##2021-2040
#######################

###SSP119_2021_2040

propor_SSP119_2021_2040_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2021_2040_ssp119_ensemble)) {

models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
res_temp_cont=ldply(models_temp_cont, coef)
res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2021_2040_ssp119_ensemble-z2$historical_1995_2014_ensemble)[i]
propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)

gg= summary(propor_temp_proj*100)

propor_SSP119_2021_2040_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)

}

###ssp126_2021_2040

propor_ssp126_2021_2040_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2021_2040_ssp126_ensemble)) {
  
  models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
  res_temp_cont=ldply(models_temp_cont, coef)
  res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
  res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2021_2040_ssp126_ensemble-z2$historical_1995_2014_ensemble)[i]
  propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)
  
  gg= summary(propor_temp_proj*100)
  
  propor_ssp126_2021_2040_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)
  
}

###ssp245_2021_2040

propor_ssp245_2021_2040_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2021_2040_ssp245_ensemble)) {
  
  models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
  res_temp_cont=ldply(models_temp_cont, coef)
  res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
  res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2021_2040_ssp245_ensemble-z2$historical_1995_2014_ensemble)[i]
  propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)
  
  gg= summary(propor_temp_proj*100)
  
  propor_ssp245_2021_2040_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)
  
}


###ssp370_2021_2040

propor_ssp370_2021_2040_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2021_2040_ssp370_ensemble)) {
  
  models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
  res_temp_cont=ldply(models_temp_cont, coef)
  res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
  res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2021_2040_ssp370_ensemble-z2$historical_1995_2014_ensemble)[i]
  propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)
  
  gg= summary(propor_temp_proj*100)
  
  propor_ssp370_2021_2040_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)
  
}

###ssp585_2021_2040

propor_ssp585_2021_2040_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2021_2040_ssp585_ensemble)) {
  
  models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
  res_temp_cont=ldply(models_temp_cont, coef)
  res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
  res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2021_2040_ssp585_ensemble-z2$historical_1995_2014_ensemble)[i]
  propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)
  
  gg= summary(propor_temp_proj*100)
  
  propor_ssp585_2021_2040_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)
  
}


######################################
### 2041-2060
####################################

###SSP119_2041_2060

propor_SSP119_2041_2060_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2041_2060_ssp119_ensemble)) {
  
  models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
  res_temp_cont=ldply(models_temp_cont, coef)
  res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
  res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2041_2060_ssp119_ensemble-z2$historical_1995_2014_ensemble)[i]
  propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)
  
  gg= summary(propor_temp_proj*100)
  
  propor_SSP119_2041_2060_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)
  
}

###ssp126_2041_2060

propor_ssp126_2041_2060_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2041_2060_ssp126_ensemble)) {
  
  models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
  res_temp_cont=ldply(models_temp_cont, coef)
  res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
  res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2041_2060_ssp126_ensemble-z2$historical_1995_2014_ensemble)[i]
  propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)
  
  gg= summary(propor_temp_proj*100)
  
  propor_ssp126_2041_2060_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)
  
}

###ssp245_2041_2060

propor_ssp245_2041_2060_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2041_2060_ssp245_ensemble)) {
  
  models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
  res_temp_cont=ldply(models_temp_cont, coef)
  res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
  res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2041_2060_ssp245_ensemble-z2$historical_1995_2014_ensemble)[i]
  propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)
  
  gg= summary(propor_temp_proj*100)
  
  propor_ssp245_2041_2060_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)
  
}


###ssp370_2041_2060

propor_ssp370_2041_2060_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2041_2060_ssp370_ensemble)) {
  
  models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
  res_temp_cont=ldply(models_temp_cont, coef)
  res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
  res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2041_2060_ssp370_ensemble-z2$historical_1995_2014_ensemble)[i]
  propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)
  
  gg= summary(propor_temp_proj*100)
  
  propor_ssp370_2041_2060_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)
  
}

###ssp585_2041_2060

propor_ssp585_2041_2060_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2041_2060_ssp585_ensemble)) {
  
  models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
  res_temp_cont=ldply(models_temp_cont, coef)
  res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
  res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2041_2060_ssp585_ensemble-z2$historical_1995_2014_ensemble)[i]
  propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)
  
  gg= summary(propor_temp_proj*100)
  
  propor_ssp585_2041_2060_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)
  
}


######################################
### 2081-2100
####################################

###SSP119_2081_2100

propor_SSP119_2081_2100_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2081_2100_ssp119_ensemble)) {
  
  models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
  res_temp_cont=ldply(models_temp_cont, coef)
  res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
  res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2081_2100_ssp119_ensemble-z2$historical_1995_2014_ensemble)[i]
  propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)
  
  gg= summary(propor_temp_proj*100)
  
  propor_SSP119_2081_2100_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)
  
}

###ssp126_2081_2100

propor_ssp126_2081_2100_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2081_2100_ssp126_ensemble)) {
  
  models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
  res_temp_cont=ldply(models_temp_cont, coef)
  res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
  res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2081_2100_ssp126_ensemble-z2$historical_1995_2014_ensemble)[i]
  propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)
  
  gg= summary(propor_temp_proj*100)
  
  propor_ssp126_2081_2100_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)
  
}

###ssp245_2081_2100

propor_ssp245_2081_2100_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2081_2100_ssp245_ensemble)) {
  
  models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
  res_temp_cont=ldply(models_temp_cont, coef)
  res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
  res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2081_2100_ssp245_ensemble-z2$historical_1995_2014_ensemble)[i]
  propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)
  
  gg= summary(propor_temp_proj*100)
  
  propor_ssp245_2081_2100_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)
  
}


###ssp370_2081_2100

propor_ssp370_2081_2100_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2081_2100_ssp370_ensemble)) {
  
  models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
  res_temp_cont=ldply(models_temp_cont, coef)
  res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
  res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2081_2100_ssp370_ensemble-z2$historical_1995_2014_ensemble)[i]
  propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)
  
  gg= summary(propor_temp_proj*100)
  
  propor_ssp370_2081_2100_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)
  
}

###ssp585_2081_2100

propor_ssp585_2081_2100_weighted<-0
sd_prop<-0


for(i in 1:length(z2$future_2081_2100_ssp585_ensemble)) {
  
  models_temp_cont <- dlply(z, "Study", function(df) lm(Response ~ temp_from_control, data = df))
  res_temp_cont=ldply(models_temp_cont, coef)
  res_temp_0 = res_temp_cont[,2]+ res_temp_cont[,3]*0
  res_temp_proj = res_temp_cont[,2]+ res_temp_cont[,3]*(z2$future_2081_2100_ssp585_ensemble-z2$historical_1995_2014_ensemble)[i]
  propor_temp_proj=(res_temp_proj-res_temp_0)/abs(res_temp_0)
  
  gg= summary(propor_temp_proj*100)
  
  propor_ssp585_2081_2100_weighted[i]= sum(propor_temp_proj*weight)/sum(weight)
  
}


Results_temp_proj= cbind(z2, propor_SSP119_2021_2040_weighted, propor_ssp126_2021_2040_weighted, propor_ssp245_2021_2040_weighted, propor_ssp370_2021_2040_weighted
                         , propor_ssp585_2021_2040_weighted, propor_SSP119_2041_2060_weighted, propor_ssp126_2041_2060_weighted, propor_ssp245_2041_2060_weighted, propor_ssp370_2041_2060_weighted,
                         propor_ssp585_2041_2060_weighted, propor_SSP119_2081_2100_weighted, propor_ssp126_2081_2100_weighted, propor_ssp245_2081_2100_weighted, propor_ssp370_2081_2100_weighted, propor_ssp585_2081_2100_weighted)


write.csv(Results_temp_proj, "Calcif_effects_corals_Temp.csv")









##################################################################
#Interaction Corals

###############################################################################@



z=read.table("Corals_response_pH_OCT_20.csv", header=T, sep=";", dec=".", na.string="NA", fill=TRUE)

z2=read.table("ensemble_model_combined_pH.csv", header=T, sep=";",dec="." ,na.string="NA", fill=TRUE)

z3=read.table("ensemble_model_combined_mean_sst.csv", header=T, sep=";",dec="." ,na.string="NA", fill=TRUE)



mean_CV <- tapply(z$CV, z$Study, mean)
weight=1/mean_CV


models_comb <- dlply(z, "Study", function(df) lm(Response ~ pH_dev*temp_from_control, data = df))
res_comb=ldply(models_comb, coef)
res_comb=na.omit(res_comb)


######################
##2021-2040
#######################

###SSP119_2021_2040


# Initialize the proportional change vector for each site
propor_SSP119_2021_2040_comb <- numeric(length(z3$future_2021_2040_ssp119_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp119_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp119_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2021_2040_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp119_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_SSP119_2021_2040_comb[j] <- propor_SSP119_2021_2040_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_SSP119_2021_2040_comb <- propor_SSP119_2021_2040_comb / sum(weight)


propor_SSP119_2021_2040_comb


###ssp126_2021_2040

# Initialize the proportional change vector for each site
propor_ssp126_2021_2040_comb <- numeric(length(z3$future_2021_2040_ssp126_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp126_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp126_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2021_2040_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp126_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp126_2021_2040_comb[j] <- propor_ssp126_2021_2040_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp126_2021_2040_comb <- propor_ssp126_2021_2040_comb / sum(weight)


propor_ssp126_2021_2040_comb

###ssp245_2021_2040

# Initialize the proportional change vector for each site
propor_ssp245_2021_2040_comb <- numeric(length(z3$future_2021_2040_ssp245_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp245_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp245_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2021_2040_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp245_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp245_2021_2040_comb[j] <- propor_ssp245_2021_2040_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp245_2021_2040_comb <- propor_ssp245_2021_2040_comb / sum(weight)


propor_ssp245_2021_2040_comb

###ssp370_2021_2040

# Initialize the proportional change vector for each site
propor_ssp370_2021_2040_comb <- numeric(length(z3$future_2021_2040_ssp370_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp370_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp370_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2021_2040_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp370_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp370_2021_2040_comb[j] <- propor_ssp370_2021_2040_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp370_2021_2040_comb <- propor_ssp370_2021_2040_comb / sum(weight)


propor_ssp370_2021_2040_comb

###ssp585_2021_2040

# Initialize the proportional change vector for each site
propor_ssp585_2021_2040_comb <- numeric(length(z3$future_2021_2040_ssp585_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp585_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp585_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2021_2040_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp585_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp585_2021_2040_comb[j] <- propor_ssp585_2021_2040_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp585_2021_2040_comb <- propor_ssp585_2021_2040_comb / sum(weight)


propor_ssp585_2021_2040_comb

######################################
### 2041-2060
####################################

###SSP119_2041_2060


# Initialize the proportional change vector for each site
propor_SSP119_2041_2060_comb <- numeric(length(z3$future_2041_2060_ssp119_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp119_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp119_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2041_2060_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp119_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_SSP119_2041_2060_comb[j] <- propor_SSP119_2041_2060_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_SSP119_2041_2060_comb <- propor_SSP119_2041_2060_comb / sum(weight)


propor_SSP119_2041_2060_comb


###ssp126_2041_2060

# Initialize the proportional change vector for each site
propor_ssp126_2041_2060_comb <- numeric(length(z3$future_2041_2060_ssp126_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp126_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp126_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2041_2060_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp126_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp126_2041_2060_comb[j] <- propor_ssp126_2041_2060_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp126_2041_2060_comb <- propor_ssp126_2041_2060_comb / sum(weight)


propor_ssp126_2041_2060_comb

###ssp245_2041_2060

# Initialize the proportional change vector for each site
propor_ssp245_2041_2060_comb <- numeric(length(z3$future_2041_2060_ssp245_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp245_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp245_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2041_2060_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp245_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp245_2041_2060_comb[j] <- propor_ssp245_2041_2060_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp245_2041_2060_comb <- propor_ssp245_2041_2060_comb / sum(weight)


propor_ssp245_2041_2060_comb

###ssp370_2041_2060

# Initialize the proportional change vector for each site
propor_ssp370_2041_2060_comb <- numeric(length(z3$future_2041_2060_ssp370_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp370_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp370_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2041_2060_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp370_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp370_2041_2060_comb[j] <- propor_ssp370_2041_2060_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp370_2041_2060_comb <- propor_ssp370_2041_2060_comb / sum(weight)


propor_ssp370_2041_2060_comb

###ssp585_2041_2060

# Initialize the proportional change vector for each site
propor_ssp585_2041_2060_comb <- numeric(length(z3$future_2041_2060_ssp585_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp585_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp585_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2041_2060_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp585_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp585_2041_2060_comb[j] <- propor_ssp585_2041_2060_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp585_2041_2060_comb <- propor_ssp585_2041_2060_comb / sum(weight)


propor_ssp585_2041_2060_comb




######################################
### 2081-2100
####################################


###SSP119_2081_2100



# Initialize the proportional change vector for each site
propor_SSP119_2081_2100_comb <- numeric(length(z3$future_2081_2100_ssp119_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp119_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp119_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2081_2100_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp119_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_SSP119_2081_2100_comb[j] <- propor_SSP119_2081_2100_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_SSP119_2081_2100_comb <- propor_SSP119_2081_2100_comb / sum(weight)


propor_SSP119_2081_2100_comb


###ssp126_2081_2100

# Initialize the proportional change vector for each site
propor_ssp126_2081_2100_comb <- numeric(length(z3$future_2081_2100_ssp126_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp126_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp126_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2081_2100_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp126_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp126_2081_2100_comb[j] <- propor_ssp126_2081_2100_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp126_2081_2100_comb <- propor_ssp126_2081_2100_comb / sum(weight)


propor_ssp126_2081_2100_comb

###ssp245_2081_2100

# Initialize the proportional change vector for each site
propor_ssp245_2081_2100_comb <- numeric(length(z3$future_2081_2100_ssp245_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp245_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp245_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2081_2100_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp245_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp245_2081_2100_comb[j] <- propor_ssp245_2081_2100_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp245_2081_2100_comb <- propor_ssp245_2081_2100_comb / sum(weight)


propor_ssp245_2081_2100_comb

###ssp370_2081_2100

# Initialize the proportional change vector for each site
propor_ssp370_2081_2100_comb <- numeric(length(z3$future_2081_2100_ssp370_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp370_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp370_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2081_2100_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp370_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp370_2081_2100_comb[j] <- propor_ssp370_2081_2100_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp370_2081_2100_comb <- propor_ssp370_2081_2100_comb / sum(weight)


propor_ssp370_2081_2100_comb

###ssp585_2081_2100

# Initialize the proportional change vector for each site
propor_ssp585_2081_2100_comb <- numeric(length(z3$future_2081_2100_ssp585_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb)) {  # Loop through each study (res_comb rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp585_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb[i, 2] + 
      res_comb[i, 3] * (z2$ssp585_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb[i, 4] * (z3$future_2081_2100_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb[i, 5] * (z2$ssp585_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp585_2081_2100_comb[j] <- propor_ssp585_2081_2100_comb[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp585_2081_2100_comb <- propor_ssp585_2081_2100_comb / sum(weight)


propor_ssp585_2081_2100_comb


Results_combination_coral_proj= cbind(z2[,(1:5)],propor_SSP119_2021_2040_comb, propor_ssp126_2021_2040_comb, propor_ssp245_2021_2040_comb, propor_ssp370_2021_2040_comb
                         , propor_ssp585_2021_2040_comb, propor_SSP119_2041_2060_comb, propor_ssp126_2041_2060_comb, propor_ssp245_2041_2060_comb, propor_ssp370_2041_2060_comb,
                         propor_ssp585_2041_2060_comb, propor_SSP119_2081_2100_comb, propor_ssp126_2081_2100_comb, propor_ssp245_2081_2100_comb, propor_ssp370_2081_2100_comb, propor_ssp585_2081_2100_comb)


write.csv(Results_combination_coral_proj, "Calcif_effects_corals_Combination.csv")












##################################################################
#Interaction Corallines

###############################################################################@



z_cca=read.table("Coralline_interaction_response_pH_OCT_20.csv", header=T, sep=";", dec=".", na.string="NA", fill=TRUE)

z2=read.table("ensemble_model_combined_pH.csv", header=T, sep=";",dec="." ,na.string="NA", fill=TRUE)

z3=read.table("ensemble_model_combined_mean_sst.csv", header=T, sep=";",dec="." ,na.string="NA", fill=TRUE)



mean_CV <- tapply(z_cca$CV, z_cca$Study, mean)
weight=1/mean_CV


models_comb_cca <- dlply(z_cca, "Study", function(df) lm(Response ~ delta_pH*temp_from_control, data = df))
res_comb_cca=ldply(models_comb_cca, coef)



######################
##2021-2040
#######################

###SSP119_2021_2040


# Initialize the proportional change vector for each site
propor_SSP119_2021_2040_comb_cca <- numeric(length(z3$future_2021_2040_ssp119_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp119_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp119_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2021_2040_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp119_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_SSP119_2021_2040_comb_cca[j] <- propor_SSP119_2021_2040_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_SSP119_2021_2040_comb_cca <- propor_SSP119_2021_2040_comb_cca / sum(weight)


propor_SSP119_2021_2040_comb_cca


###ssp126_2021_2040

# Initialize the proportional change vector for each site
propor_ssp126_2021_2040_comb_cca <- numeric(length(z3$future_2021_2040_ssp126_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp126_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp126_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2021_2040_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp126_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp126_2021_2040_comb_cca[j] <- propor_ssp126_2021_2040_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp126_2021_2040_comb_cca <- propor_ssp126_2021_2040_comb_cca / sum(weight)


propor_ssp126_2021_2040_comb_cca

###ssp245_2021_2040

# Initialize the proportional change vector for each site
propor_ssp245_2021_2040_comb_cca <- numeric(length(z3$future_2021_2040_ssp245_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp245_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp245_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2021_2040_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp245_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp245_2021_2040_comb_cca[j] <- propor_ssp245_2021_2040_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp245_2021_2040_comb_cca <- propor_ssp245_2021_2040_comb_cca / sum(weight)


propor_ssp245_2021_2040_comb_cca

###ssp370_2021_2040

# Initialize the proportional change vector for each site
propor_ssp370_2021_2040_comb_cca <- numeric(length(z3$future_2021_2040_ssp370_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp370_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp370_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2021_2040_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp370_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp370_2021_2040_comb_cca[j] <- propor_ssp370_2021_2040_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp370_2021_2040_comb_cca <- propor_ssp370_2021_2040_comb_cca / sum(weight)


propor_ssp370_2021_2040_comb_cca

###ssp585_2021_2040

# Initialize the proportional change vector for each site
propor_ssp585_2021_2040_comb_cca <- numeric(length(z3$future_2021_2040_ssp585_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp585_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp585_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2021_2040_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp585_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp585_2021_2040_comb_cca[j] <- propor_ssp585_2021_2040_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp585_2021_2040_comb_cca <- propor_ssp585_2021_2040_comb_cca / sum(weight)


propor_ssp585_2021_2040_comb_cca

######################################
### 2041-2060
####################################

###SSP119_2041_2060


# Initialize the proportional change vector for each site
propor_SSP119_2041_2060_comb_cca <- numeric(length(z3$future_2041_2060_ssp119_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp119_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp119_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2041_2060_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp119_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_SSP119_2041_2060_comb_cca[j] <- propor_SSP119_2041_2060_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_SSP119_2041_2060_comb_cca <- propor_SSP119_2041_2060_comb_cca / sum(weight)


propor_SSP119_2041_2060_comb_cca


###ssp126_2041_2060

# Initialize the proportional change vector for each site
propor_ssp126_2041_2060_comb_cca <- numeric(length(z3$future_2041_2060_ssp126_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp126_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp126_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2041_2060_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp126_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp126_2041_2060_comb_cca[j] <- propor_ssp126_2041_2060_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp126_2041_2060_comb_cca <- propor_ssp126_2041_2060_comb_cca / sum(weight)


propor_ssp126_2041_2060_comb_cca

###ssp245_2041_2060

# Initialize the proportional change vector for each site
propor_ssp245_2041_2060_comb_cca <- numeric(length(z3$future_2041_2060_ssp245_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp245_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp245_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2041_2060_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp245_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp245_2041_2060_comb_cca[j] <- propor_ssp245_2041_2060_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp245_2041_2060_comb_cca <- propor_ssp245_2041_2060_comb_cca / sum(weight)


propor_ssp245_2041_2060_comb_cca

###ssp370_2041_2060

# Initialize the proportional change vector for each site
propor_ssp370_2041_2060_comb_cca <- numeric(length(z3$future_2041_2060_ssp370_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp370_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp370_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2041_2060_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp370_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp370_2041_2060_comb_cca[j] <- propor_ssp370_2041_2060_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp370_2041_2060_comb_cca <- propor_ssp370_2041_2060_comb_cca / sum(weight)


propor_ssp370_2041_2060_comb_cca

###ssp585_2041_2060

# Initialize the proportional change vector for each site
propor_ssp585_2041_2060_comb_cca <- numeric(length(z3$future_2041_2060_ssp585_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp585_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp585_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2041_2060_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp585_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp585_2041_2060_comb_cca[j] <- propor_ssp585_2041_2060_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp585_2041_2060_comb_cca <- propor_ssp585_2041_2060_comb_cca / sum(weight)


propor_ssp585_2041_2060_comb_cca




######################################
### 2081-2100
####################################


###SSP119_2081_2100



# Initialize the proportional change vector for each site
propor_SSP119_2081_2100_comb_cca <- numeric(length(z3$future_2081_2100_ssp119_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp119_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp119_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2081_2100_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp119_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_SSP119_2081_2100_comb_cca[j] <- propor_SSP119_2081_2100_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_SSP119_2081_2100_comb_cca <- propor_SSP119_2081_2100_comb_cca / sum(weight)


propor_SSP119_2081_2100_comb_cca


###ssp126_2081_2100

# Initialize the proportional change vector for each site
propor_ssp126_2081_2100_comb_cca <- numeric(length(z3$future_2081_2100_ssp126_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp126_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp126_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2081_2100_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp126_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp126_2081_2100_comb_cca[j] <- propor_ssp126_2081_2100_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp126_2081_2100_comb_cca <- propor_ssp126_2081_2100_comb_cca / sum(weight)


propor_ssp126_2081_2100_comb_cca

###ssp245_2081_2100

# Initialize the proportional change vector for each site
propor_ssp245_2081_2100_comb_cca <- numeric(length(z3$future_2081_2100_ssp245_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp245_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp245_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2081_2100_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp245_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp245_2081_2100_comb_cca[j] <- propor_ssp245_2081_2100_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp245_2081_2100_comb_cca <- propor_ssp245_2081_2100_comb_cca / sum(weight)


propor_ssp245_2081_2100_comb_cca

###ssp370_2081_2100

# Initialize the proportional change vector for each site
propor_ssp370_2081_2100_comb_cca <- numeric(length(z3$future_2081_2100_ssp370_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp370_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp370_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2081_2100_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp370_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp370_2081_2100_comb_cca[j] <- propor_ssp370_2081_2100_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp370_2081_2100_comb_cca <- propor_ssp370_2081_2100_comb_cca / sum(weight)


propor_ssp370_2081_2100_comb_cca

###ssp585_2081_2100

# Initialize the proportional change vector for each site
propor_ssp585_2081_2100_comb_cca <- numeric(length(z3$future_2081_2100_ssp585_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_cca)) {  # Loop through each study (res_comb_cca rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp585_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_cca[i, 2]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_cca[i, 2] + 
      res_comb_cca[i, 3] * (z2$ssp585_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 4] * (z3$future_2081_2100_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_cca[i, 5] * (z2$ssp585_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / abs(res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp585_2081_2100_comb_cca[j] <- propor_ssp585_2081_2100_comb_cca[j] + (propor_proj * weight[i])
  }
}

# Normalize by the sum of the weights
propor_ssp585_2081_2100_comb_cca <- propor_ssp585_2081_2100_comb_cca / sum(weight)


propor_ssp585_2081_2100_comb_cca


Results_combination_corallines_proj= cbind(z2[,(1:5)],propor_SSP119_2021_2040_comb_cca, propor_ssp126_2021_2040_comb_cca, propor_ssp245_2021_2040_comb_cca, propor_ssp370_2021_2040_comb_cca
                                      , propor_ssp585_2021_2040_comb_cca, propor_SSP119_2041_2060_comb_cca, propor_ssp126_2041_2060_comb_cca, propor_ssp245_2041_2060_comb_cca, propor_ssp370_2041_2060_comb_cca,
                                      propor_ssp585_2041_2060_comb_cca, propor_SSP119_2081_2100_comb_cca, propor_ssp126_2081_2100_comb_cca, propor_ssp245_2081_2100_comb_cca, propor_ssp370_2081_2100_comb_cca, propor_ssp585_2081_2100_comb_cca)


write.csv(Results_combination_corallines_proj, "Calcif_effects_corallines_Combination.csv")







##########################################################################################
###
##
#BIOEROSION - Interaction
#############################################################################


##Slopes calculations

# Compile lit data into slopes and intercepts



fang2013_bioerosion = c(2.23,	2.52,	3.14,	4.56)
fang2013_pH = c(0, -0.08, -0.21, -0.36)
fang2013_temp = c(0,	1.4, 2.9, 4.7)
model_fang2013 = lm(fang2013_bioerosion ~ fang2013_pH * fang2013_temp)
fang2013_pH_slope = coef(model_fang2013)[2]
fang2013_temp_slope = coef(model_fang2013)[3]
fang2013_temp_pH_slope = coef(model_fang2013)[4]
fang2013_intercept = coef(model_fang2013)[1]



# Reyes-Nivia et al 2014 (including both light and dark)
reyesnivia2014_light_bioerosion = c(0.23,	0.21,	0.23,	0.28)
reyesnivia2014_pH = c(0, -0.1, -0.2, -0.4)
reyesnivia2014_temp = c(0,0.9, 2.7, 4.7)
model_reyesnivia2014_light = lm(reyesnivia2014_light_bioerosion ~ reyesnivia2014_pH * reyesnivia2014_temp)
reyesnivia2014_light_pH_slope = coef(model_reyesnivia2014_light)[2]
reyesnivia2014_light_temp_slope = coef(model_reyesnivia2014_light)[3]
reyesnivia2014_light_intercept = coef(model_reyesnivia2014_light)[1]
reyesnivia2014_light_temp_pH_slope = coef(model_reyesnivia2014_light)[4]

reyesnivia2014_dark_bioerosion = c(0.5,	0.54,	0.77,	0.78)
model_reyesnivia2014_dark = lm(reyesnivia2014_dark_bioerosion ~ reyesnivia2014_pH * reyesnivia2014_temp)
reyesnivia2014_dark_pH_slope = coef(model_reyesnivia2014_dark)[2]
reyesnivia2014_dark_temp_slope = coef(model_reyesnivia2014_dark)[3]
reyesnivia2014_dark_intercept = coef(model_reyesnivia2014_dark)[1]
reyesnivia2014_dark_temp_pH_slope = coef(model_reyesnivia2014_dark)[4]

reyesnivia2013_cuneata_bioerosion = c(8.04,	10.65,	11.76)
reyesnivia2013_pH = c(0, -0.19, -0.38)
reyesnivia2013_temp = c(0,2,4)
model_reyesnivia2013_cuneata = lm(reyesnivia2013_cuneata_bioerosion ~ reyesnivia2013_pH * reyesnivia2013_temp)
reyesnivia2013_cuneata_pH_slope = coef(model_reyesnivia2013_cuneata)[2]
reyesnivia2013_cuneata_temp_slope = coef(model_reyesnivia2013_cuneata)[3]
reyesnivia2013_cuneata_intercept = coef(model_reyesnivia2013_cuneata)[1]
reyesnivia2013_cuneata_temp_pH_slope = coef(model_reyesnivia2013_cuneata)[4]

reyesnivia2013_cylindrica_bioerosion = c(12.67,	17.21,	23.96)
model_reyesnivia2013_cylindrica = lm(reyesnivia2013_cylindrica_bioerosion ~ reyesnivia2013_pH * reyesnivia2013_temp)
reyesnivia2013_cylindrica_pH_slope = coef(model_reyesnivia2013_cylindrica)[2]
reyesnivia2013_cylindrica_temp_slope = coef(model_reyesnivia2013_cylindrica)[3]
reyesnivia2013_cylindrica_intercept = coef(model_reyesnivia2013_cylindrica)[1]
reyesnivia2013_cylindrica_temp_pH_slope = coef(model_reyesnivia2013_cuneata)[4]

# Stubler 2015 with and without sponges
stubler2015_sponges_bioerosion = c(0.01,	0.02,	0.05,	0.02,	0.11,	0.04)
stubler2015_pH = c(0, -0.36, -0.56, 0, -0.37, -0.53)
stubler2015_temp = c(0,0,0, 1,1,1)
model_stubler2015_sponges = lm(stubler2015_sponges_bioerosion ~ stubler2015_pH * stubler2015_temp)
stubler2015_sponges_pH_slope = coef(model_stubler2015_sponges)[2]
stubler2015_sponges_temp_slope = coef(model_stubler2015_sponges)[3]
stubler2015_sponges_intercept = coef(model_stubler2015_sponges)[1]
stubler2015_sponges_temp_pH_slope = coef(model_stubler2015_sponges)[4]



achlatis2017_bioerosion = c(1, 1, 0.8, 0.8)
achlatis2017_pH = c(0, -0.23, 0, -0.26)
achlatis2017_temp = c(0,0,3,3)
model_achlatis2017 = lm(achlatis2017_bioerosion ~ achlatis2017_pH * achlatis2017_temp)
achlatis2017_pH_slope = coef(model_achlatis2017)[2]
achlatis2017_temp_slope = coef(model_achlatis2017)[3]
achlatis2017_intercept = coef(model_achlatis2017)[1]
achlatis2017_temp_pH_slope = coef(model_achlatis2017)[4]


Slope_temp = c(achlatis2017_temp_slope, stubler2015_sponges_temp_slope, reyesnivia2013_cylindrica_temp_slope, reyesnivia2013_cuneata_temp_slope, reyesnivia2014_dark_temp_slope,
               reyesnivia2014_light_temp_slope, fang2013_temp_slope )
Slope_pH = c(achlatis2017_pH_slope, stubler2015_sponges_pH_slope, reyesnivia2013_cylindrica_pH_slope, reyesnivia2013_cuneata_pH_slope, reyesnivia2014_dark_pH_slope,
               reyesnivia2014_light_pH_slope, fang2013_pH_slope )
Slope_temp_pH = c(achlatis2017_temp_pH_slope, stubler2015_sponges_temp_pH_slope, reyesnivia2013_cylindrica_temp_pH_slope, reyesnivia2013_cuneata_temp_pH_slope, reyesnivia2014_dark_temp_pH_slope,
             reyesnivia2014_light_temp_pH_slope, fang2013_temp_pH_slope )
Intercept_bio = c(achlatis2017_intercept, stubler2015_sponges_intercept, reyesnivia2013_cylindrica_intercept, reyesnivia2013_cuneata_intercept, reyesnivia2014_dark_intercept,
                  reyesnivia2014_light_intercept, fang2013_intercept )

res_model = cbind(Intercept_bio,Slope_temp,Slope_pH, Slope_temp_pH)

res_comb_bio=na.omit(res_model)

library(plotrix)
setwd("/Users/steevecomeau/Documents/Article/Published/Review_reef_budget/Data_for_Chris_P/")


z2=read.table("ensemble_model_combined_pH.csv", header=T, sep=";",dec="." ,na.string="NA", fill=TRUE)

z3=read.table("ensemble_model_combined_mean_sst.csv", header=T, sep=";",dec="." ,na.string="NA", fill=TRUE)





######################
##2021-2040
#######################

###SSP119_2021_2040


# Initialize the proportional change vector for each site
propor_SSP119_2021_2040_comb_bio <- numeric(length(z3$future_2021_2040_ssp119_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp119_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp119_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2021_2040_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp119_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_SSP119_2021_2040_comb_bio[j] <- propor_proj
  }
}


propor_SSP119_2021_2040_comb_bio


###ssp126_2021_2040


# Initialize the proportional change vector for each site
propor_ssp126_2021_2040_comb_bio <- numeric(length(z3$future_2021_2040_ssp126_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp126_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp126_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2021_2040_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp126_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp126_2021_2040_comb_bio[j] <- propor_proj
  }
}


propor_ssp126_2021_2040_comb_bio

###ssp245_2021_2040


# Initialize the proportional change vector for each site
propor_ssp245_2021_2040_comb_bio <- numeric(length(z3$future_2021_2040_ssp245_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp245_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp245_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2021_2040_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp245_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp245_2021_2040_comb_bio[j] <- propor_proj
  }
}


propor_ssp245_2021_2040_comb_bio

###ssp370_2021_2040


# Initialize the proportional change vector for each site
propor_ssp370_2021_2040_comb_bio <- numeric(length(z3$future_2021_2040_ssp370_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp370_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp370_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2021_2040_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp370_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp370_2021_2040_comb_bio[j] <- propor_proj
  }
}


propor_ssp370_2021_2040_comb_bio
###ssp585_2021_2040


# Initialize the proportional change vector for each site
propor_ssp585_2021_2040_comb_bio <- numeric(length(z3$future_2021_2040_ssp585_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2021_2040_ssp585_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp585_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2021_2040_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp585_2021_2040_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2021_2040_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp585_2021_2040_comb_bio[j] <- propor_proj
  }
}


propor_ssp585_2021_2040_comb_bio

######################################
### 2041-2060
####################################


###SSP119_2041_2060


# Initialize the proportional change vector for each site
propor_SSP119_2041_2060_comb_bio <- numeric(length(z3$future_2041_2060_ssp119_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp119_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp119_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2041_2060_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp119_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_SSP119_2041_2060_comb_bio[j] <- propor_proj
  }
}


propor_SSP119_2041_2060_comb_bio


###ssp126_2041_2060


# Initialize the proportional change vector for each site
propor_ssp126_2041_2060_comb_bio <- numeric(length(z3$future_2041_2060_ssp126_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp126_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp126_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2041_2060_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp126_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp126_2041_2060_comb_bio[j] <- propor_proj
  }
}


propor_ssp126_2041_2060_comb_bio

###ssp245_2041_2060


# Initialize the proportional change vector for each site
propor_ssp245_2041_2060_comb_bio <- numeric(length(z3$future_2041_2060_ssp245_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp245_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp245_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2041_2060_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp245_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp245_2041_2060_comb_bio[j] <- propor_proj
  }
}


propor_ssp245_2041_2060_comb_bio

###ssp370_2041_2060


# Initialize the proportional change vector for each site
propor_ssp370_2041_2060_comb_bio <- numeric(length(z3$future_2041_2060_ssp370_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp370_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp370_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2041_2060_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp370_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp370_2041_2060_comb_bio[j] <- propor_proj
  }
}


propor_ssp370_2041_2060_comb_bio
###ssp585_2041_2060


# Initialize the proportional change vector for each site
propor_ssp585_2041_2060_comb_bio <- numeric(length(z3$future_2041_2060_ssp585_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2041_2060_ssp585_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp585_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2041_2060_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp585_2041_2060_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2041_2060_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp585_2041_2060_comb_bio[j] <- propor_proj
  }
}


propor_ssp585_2041_2060_comb_bio




######################################
### 2081-2100
####################################


###SSP119_2081_2100


# Initialize the proportional change vector for each site
propor_SSP119_2081_2100_comb_bio <- numeric(length(z3$future_2081_2100_ssp119_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp119_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp119_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2081_2100_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp119_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp119_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_SSP119_2081_2100_comb_bio[j] <- propor_proj
  }
}


propor_SSP119_2081_2100_comb_bio


###ssp126_2081_2100


# Initialize the proportional change vector for each site
propor_ssp126_2081_2100_comb_bio <- numeric(length(z3$future_2081_2100_ssp126_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp126_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp126_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2081_2100_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp126_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp126_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp126_2081_2100_comb_bio[j] <- propor_proj
  }
}


propor_ssp126_2081_2100_comb_bio

###ssp245_2081_2100


# Initialize the proportional change vector for each site
propor_ssp245_2081_2100_comb_bio <- numeric(length(z3$future_2081_2100_ssp245_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp245_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp245_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2081_2100_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp245_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp245_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp245_2081_2100_comb_bio[j] <- propor_proj
  }
}


propor_ssp245_2081_2100_comb_bio

###ssp370_2081_2100


# Initialize the proportional change vector for each site
propor_ssp370_2081_2100_comb_bio <- numeric(length(z3$future_2081_2100_ssp370_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp370_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp370_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2081_2100_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp370_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp370_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp370_2081_2100_comb_bio[j] <- propor_proj
  }
}


propor_ssp370_2081_2100_comb_bio
###ssp585_2081_2100


# Initialize the proportional change vector for each site
propor_ssp585_2081_2100_comb_bio <- numeric(length(z3$future_2081_2100_ssp585_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp585_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp585_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2081_2100_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 4] * (z2$ssp585_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) *
      (z3$future_2081_2100_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp585_2081_2100_comb_bio[j] <- propor_proj
  }
}


propor_ssp585_2081_2100_comb_bio




Results_combination_Bioerosion_proj= cbind(z2[,(1:5)],propor_SSP119_2021_2040_comb_bio, propor_ssp126_2021_2040_comb_bio, propor_ssp245_2021_2040_comb_bio, propor_ssp370_2021_2040_comb_bio
                                           , propor_ssp585_2021_2040_comb_bio, propor_SSP119_2041_2060_comb_bio, propor_ssp126_2041_2060_comb_bio, propor_ssp245_2041_2060_comb_bio, propor_ssp370_2041_2060_comb_bio,
                                           propor_ssp585_2041_2060_comb_bio, propor_SSP119_2081_2100_comb_bio, propor_ssp126_2081_2100_comb_bio, propor_ssp245_2081_2100_comb_bio, propor_ssp370_2081_2100_comb_bio, propor_ssp585_2081_2100_comb_bio)


write.csv(Results_combination_Bioerosion_proj, "Bioerosion_effects_Combination.csv")



##### test




##Slopes calculations

# Compile lit data into slopes and intercepts



fang2013_bioerosion = c(2.23,	2.52,	3.14,	4.56)
fang2013_pH = c(0, -0.08, -0.21, -0.36)
fang2013_temp = c(0,	1.4, 2.9, 4.7)
model_fang2013 = lm(fang2013_bioerosion ~ fang2013_pH + fang2013_temp)
fang2013_pH_slope = coef(model_fang2013)[2]
fang2013_temp_slope = coef(model_fang2013)[3]
#fang2013_temp_pH_slope = coef(model_fang2013)[4]
fang2013_intercept = coef(model_fang2013)[1]



# Reyes-Nivia et al 2014 (including both light and dark)
reyesnivia2014_light_bioerosion = c(0.23,	0.21,	0.23,	0.28)
reyesnivia2014_pH = c(0, -0.1, -0.2, -0.4)
reyesnivia2014_temp = c(0,0.9, 2.7, 4.7)
model_reyesnivia2014_light = lm(reyesnivia2014_light_bioerosion ~ reyesnivia2014_pH + reyesnivia2014_temp)
reyesnivia2014_light_pH_slope = coef(model_reyesnivia2014_light)[2]
reyesnivia2014_light_temp_slope = coef(model_reyesnivia2014_light)[3]
reyesnivia2014_light_intercept = coef(model_reyesnivia2014_light)[1]
#reyesnivia2014_light_temp_pH_slope = coef(model_reyesnivia2014_light)[4]

reyesnivia2014_dark_bioerosion = c(0.5,	0.54,	0.77,	0.78)
model_reyesnivia2014_dark = lm(reyesnivia2014_dark_bioerosion ~ reyesnivia2014_pH + reyesnivia2014_temp)
reyesnivia2014_dark_pH_slope = coef(model_reyesnivia2014_dark)[2]
reyesnivia2014_dark_temp_slope = coef(model_reyesnivia2014_dark)[3]
reyesnivia2014_dark_intercept = coef(model_reyesnivia2014_dark)[1]
#reyesnivia2014_dark_temp_pH_slope = coef(model_reyesnivia2014_dark)[4]

reyesnivia2013_cuneata_bioerosion = c(8.04,	10.65,	11.76)
reyesnivia2013_pH = c(0, -0.19, -0.38)
reyesnivia2013_temp = c(0,2,4)
model_reyesnivia2013_cuneata = lm(reyesnivia2013_cuneata_bioerosion ~ reyesnivia2013_pH + reyesnivia2013_temp)
reyesnivia2013_cuneata_pH_slope = coef(model_reyesnivia2013_cuneata)[2]
reyesnivia2013_cuneata_temp_slope = coef(model_reyesnivia2013_cuneata)[3]
reyesnivia2013_cuneata_intercept = coef(model_reyesnivia2013_cuneata)[1]
#reyesnivia2013_cuneata_temp_pH_slope = coef(model_reyesnivia2013_cuneata)[4]

reyesnivia2013_cylindrica_bioerosion = c(12.67,	17.21,	23.96)
model_reyesnivia2013_cylindrica = lm(reyesnivia2013_cylindrica_bioerosion ~ reyesnivia2013_pH + reyesnivia2013_temp)
reyesnivia2013_cylindrica_pH_slope = coef(model_reyesnivia2013_cylindrica)[2]
reyesnivia2013_cylindrica_temp_slope = coef(model_reyesnivia2013_cylindrica)[3]
reyesnivia2013_cylindrica_intercept = coef(model_reyesnivia2013_cylindrica)[1]
#reyesnivia2013_cylindrica_temp_pH_slope = coef(model_reyesnivia2013_cuneata)[4]

# Stubler 2015 with and without sponges
stubler2015_sponges_bioerosion = c(0.01,	0.02,	0.05,	0.02,	0.11,	0.04)
stubler2015_pH = c(0, -0.36, -0.56, 0, -0.37, -0.53)
stubler2015_temp = c(0,0,0, 1,1,1)
model_stubler2015_sponges = lm(stubler2015_sponges_bioerosion ~ stubler2015_pH + stubler2015_temp)
stubler2015_sponges_pH_slope = coef(model_stubler2015_sponges)[2]
stubler2015_sponges_temp_slope = coef(model_stubler2015_sponges)[3]
stubler2015_sponges_intercept = coef(model_stubler2015_sponges)[1]
#stubler2015_sponges_temp_pH_slope = coef(model_stubler2015_sponges)[4]



achlatis2017_bioerosion = c(1, 1, 0.8, 0.8)
achlatis2017_pH = c(0, -0.23, 0, -0.26)
achlatis2017_temp = c(0,0,3,3)
model_achlatis2017 = lm(achlatis2017_bioerosion ~ achlatis2017_pH + achlatis2017_temp)
achlatis2017_pH_slope = coef(model_achlatis2017)[2]
achlatis2017_temp_slope = coef(model_achlatis2017)[3]
achlatis2017_intercept = coef(model_achlatis2017)[1]
#achlatis2017_temp_pH_slope = coef(model_achlatis2017)[4]


Slope_temp = c(achlatis2017_temp_slope, stubler2015_sponges_temp_slope, reyesnivia2013_cylindrica_temp_slope, reyesnivia2013_cuneata_temp_slope, reyesnivia2014_dark_temp_slope,
               reyesnivia2014_light_temp_slope, fang2013_temp_slope )
Slope_pH = c(achlatis2017_pH_slope, stubler2015_sponges_pH_slope, reyesnivia2013_cylindrica_pH_slope, reyesnivia2013_cuneata_pH_slope, reyesnivia2014_dark_pH_slope,
             reyesnivia2014_light_pH_slope, fang2013_pH_slope )
Intercept_bio = c(achlatis2017_intercept, stubler2015_sponges_intercept, reyesnivia2013_cylindrica_intercept, reyesnivia2013_cuneata_intercept, reyesnivia2014_dark_intercept,
                  reyesnivia2014_light_intercept, fang2013_intercept )

res_model = cbind(Intercept_bio,Slope_temp,Slope_pH)

res_comb_bio=na.omit(res_model)


###ssp585_2081_2100


# Initialize the proportional change vector for each site
propor_ssp585_2081_2100_comb_bio <- numeric(length(z3$future_2081_2100_ssp585_ensemble))

# Loop over each study instead of each site
for (i in 1:nrow(res_comb_bio)) {  # Loop through each study (res_comb_bio rows)
  
  # For each study, calculate the projected and present response for each site
  for (j in 1:length(z3$future_2081_2100_ssp585_ensemble)) {
    
    # Calculate present response (res_pres) based on intercept (only present conditions)
    res_pres <- res_comb_bio[i, 1]
    
    # Calculate projected response (res_proj) based on future pH and temperature changes
    res_proj <- res_comb_bio[i, 1] + 
      res_comb_bio[i, 3] * (z2$ssp585_2081_2100_ensemble[j] - z2$historical_1995_2014_ensemble[j]) +
      res_comb_bio[i, 2] * (z3$future_2081_2100_ssp585_ensemble[j] - z3$historical_1995_2014_ensemble[j])
    
    # Calculate proportional change for this study at this site
    propor_proj <- (res_proj - res_pres) / (res_pres)
    
    # Add weighted contribution for this study to the site-level proportional change
    propor_ssp585_2081_2100_comb_bio[j] <- propor_proj
  }
}


propor_ssp585_2081_2100_comb_bio
