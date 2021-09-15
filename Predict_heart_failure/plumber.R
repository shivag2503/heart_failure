library(plumber)
library(dplyr)

model_predict <- readRDS('model.RDS')
scaler <- readRDS("scale_fun.RDS")

raw_data <- read.csv(file = "heart_failure_clinical_records_dataset.csv", header = T)

raw_data$age <- as.integer(raw_data$age)

descriptive <- c("age", "creatinine_phosphokinase", "ejection_fraction", "platelets", "serum_creatinine", "serum_sodium", "time" )

means <- summarise_all(raw_data[, descriptive], mean)
sds <- summarise_all(raw_data[, descriptive], sd)

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*") # Or whatever
  plumber::forward()
}

#* @get /predictions
predict_death_event <- function(age, cp, ef, p, sc, ss, t, s, g, d, hbp, a) {
  age <- (as.numeric(age) - means[1]) / sds[1]
  cp <- (as.numeric(cp) - means[2]) / sds[2]
  ef <- (as.numeric(ef) - means[3]) / sds[3]
  p <- (as.numeric(p) - means[4]) / sds[4]
  sc <- (as.numeric(sc) - means[5]) / sds[5]
  ss <- (as.numeric(ss) - means[6]) / sds[6]
  t <- (as.numeric(t) - means[7]) / sds[7]
  s <- as.factor(s)
  g <- as.factor(g)
  d <- as.factor(d)
  hbp <- as.factor(hbp)
  a <- as.factor(a)
  
  prediction_data <- data.frame(age, anaemia =  a, creatinine_phosphokinase = cp, diabetes = d, ejection_fraction = ef, high_blood_pressure = hbp,
                                platelets = p, serum_creatinine = sc, serum_sodium = ss, sex = g, smoking = s, time = t)
  
  prediction <- predict(model_predict, prediction_data, type = "response")
  if(prediction > 0.5) {
    return("Your chances are high for heart failure")
  } else {
    return("You are safe from heart failure")
  }
}