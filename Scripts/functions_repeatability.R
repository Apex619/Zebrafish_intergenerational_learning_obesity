#### Custom functions

#Calculating repeatability for learning (Zantiks)
rpt_learning <- function(df) {
  x <- rpt(difference ~ sex +  (1 |  fishID), grname = "fishID", data = df, datatype = "Gaussian", nboot = 1000, npermut = 1000)
}



#Calculating repeatability for body weight with week (adjusted for week)
rpt_weight2 <- function(df) {
  x <- rpt(Weight_g ~ Age_Weeks + (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}




#Calculating repeatability for total distance travelled
rpt_tot_dist <- function(df) {
  x <- rpt(tot_dist ~ Sex + scale(Water_Time, scale=FALSE) + (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}



#Calculating repeatability for time spent in low zone
rpt_low_dur <- function(df) {
  x <- rpt(low_dur ~ Sex + scale(Water_Time, scale=FALSE)+ (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}


#Calculating repeatability for time spent in mid zone
rpt_mid_dur <- function(df) {
  x <- rpt(mid_dur ~ Sex + scale(Water_Time, scale=FALSE)+(1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}



#Calculating repeatability for time spent in high zone
rpt_high_dur <- function(df) {
  x <- rpt(high_dur ~ Sex + scale(Water_Time, scale=FALSE) + (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}




#Calculating repeatability for time spent freezing 
rpt_freezing_dur <- function(df) {
  x <- rpt(log(freezing_dur+1) ~ Sex + scale(Water_Time, scale=FALSE) + (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}



#Calculating repeatability for latency to the high zone
rpt_latency <- function(df) {
  x <- rpt(latency_high ~ Sex + scale(Water_Time, scale=FALSE)+(1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}



#Calculating repeatability for entries to the high zone
rpt_freq <- function(df) {
  x <- rpt(sqrt(freq_high) ~ Sex + scale(Water_Time, scale=FALSE) + (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}



# Writing functions for getting the difference between two tank's repeatablities
unlist_rptr <- function(rpt_model) { #unlisting the bootstrapped distribution 
  unlist(rpt_model$R_boot)
}

difference_boot <- function(boot1,boot2) { #calculating difference between tall and short (depending on which has the higher repeatability)
  boot1 - boot2
}

quantiles_diff_boot <- function(from_diff_boot) { #obtaining quantiles at 2.5% and 97.5% (becomes 95% CI)
  quantile(from_diff_boot, c(0.025, 0.975))
} 

