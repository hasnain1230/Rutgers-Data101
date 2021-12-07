class_dataset <- read.csv("~/Downloads/F21DataSurveyWithSectionsAnonymized (1).csv", header=FALSE)

chocolate_vs_vanilia_rotaryphone <- function() {
  dataset_table = table(class_dataset$V6)
  
  chocolate_lovers = sum(class_dataset$V6 == "Chocolate")
  vanilia_lovers = sum(class_dataset$V6 == "Vanilla")
  prior_cl = chocolate_lovers / (chocolate_lovers + vanilia_lovers)
  prior_vl = vanilia_lovers / (chocolate_lovers + vanilia_lovers)
  
  total = nrow(class_dataset)
  print(total)
  rot_phone = sum(grepl("Yes", class_dataset$V8, fixed = TRUE))
  rot_phone_cl = sum(grepl("Yes", class_dataset$V8, fixed = TRUE) & class_dataset$V6 == "Chocolate")
  rot_phone_vl = sum(grepl("Yes", class_dataset$V8, fixed = TRUE) & class_dataset$V6 == "Vanilla")
  print(rot_phone_cl)
  print(rot_phone_vl)
  
  cat("Probablity of a student liking chocolate:", prior_cl, "\n")
  cat("Probablity of a student liking vanilla:", prior_vl, "\n")
  cat("Believes they can use a rotary phone:", (rot_phone / total), "\n")
  cat("Believes they can use a rotary phone and likes chocolate", rot_phone_cl / total, "\n")
  cat("Believes they can use a rotary phone and likes vanilla", rot_phone_vl / total, "\n")
}

chocolate_vs_vanilia_Android <- function() {
  dataset_table = table(class_dataset$V6)
  
  chocolate_lovers = sum(class_dataset$V6 == "Chocolate")
  vanilia_lovers = sum(class_dataset$V6 == "Vanilla")
  prior_cl = chocolate_lovers / (chocolate_lovers + vanilia_lovers)
  prior_vl = vanilia_lovers / (chocolate_lovers + vanilia_lovers)
  
  total = nrow(class_dataset)
  # print(total)
  phone = sum(class_dataset$V14 == "Android")
  phone_android_cl = sum(class_dataset$V14 == "Android" & class_dataset$V6 == "Chocolate")
  phone_android_vl = sum(class_dataset$V14 == "Android" & class_dataset$V6 == "Vanilla")
  print(phone)
  print(phone_android_vl)
  print(phone_android_cl)
  
  cat("Probablity of a student liking chocolate:", prior_cl, "\n")
  cat("Probablity of a student liking vanilla:", prior_vl, "\n")
  cat("Probablity student uses Android", (phone / total), "\n")
  cat("Probablity Student Uses Android and likes Chocolate", phone_android_cl / total, "\n")
  cat("Probablity Student Uses Android and likes Vanilla", phone_android_vl / total, "\n")
}

chocolate_vs_vanilia_iPhone <- function() {
  dataset_table = table(class_dataset$V6)
  
  chocolate_lovers = sum(class_dataset$V6 == "Chocolate")
  vanilia_lovers = sum(class_dataset$V6 == "Vanilla")
  prior_cl = chocolate_lovers / (chocolate_lovers + vanilia_lovers)
  prior_vl = vanilia_lovers / (chocolate_lovers + vanilia_lovers)
  
  total = nrow(class_dataset)
  # print(total)
  phone = sum(class_dataset$V14 == "iPhone")
  phone_iPhone_cl = sum(class_dataset$V14 == "iPhone" & class_dataset$V6 == "Chocolate")
  phone_iPhone_vl = sum(class_dataset$V14 == "iPhone" & class_dataset$V6 == "Vanilla")
  
  print(phone)
  print(phone_iPhone_cl)
  print(phone_iPhone_vl)
  
  cat("Probablity of a student liking chocolate:", prior_cl, "\n")
  cat("Probablity of a student liking vanilla:", prior_vl, "\n")
  cat("Probablity student uses iPhone", (phone / total), "\n")
  cat("Probablity Student Uses iPhone and likes Chocolate", phone_iPhone_cl / total, "\n")
  cat("Probablity Student Uses iPhone and likes Vanilla", phone_iPhone_vl / total, "\n")
}

chocolate_vs_vanilia_rotaryphone()
chocolate_vs_vanilia_Android()
chocolate_vs_vanilia_iPhone()