######################################################################
##                                                                  ##                                  
##              Screw study to analyze dose following               ##
##                    screw incidence and duration                  ##
##                                                                  ##
##                                                                  ##                                 
######################################################################

# created by François Gardavaud
# date : 03/10/2021

###################### set-up environment section ################################

# Set the project path to the root level -
root.dir = rprojroot::find_rstudio_root_file()

# # load readxl package to read easyly Excel file with an install condition
# if(!require(readxl)){
#   install.packages("readxl")
#   library(readxl)
# }

# load lubridate package to determine patient age from birthdate with an install condition
if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}

# load doMC package for parallel computing
if(!require(doMC)){
  install.packages("doMC")
  library(doMC)
}
# load foreach package for parallel computing on for loop
if(!require(foreach)){
  install.packages("foreach")
  library(foreach)
}
# load doparallel package to implement parallel computing
# if(!require(doParallel)){
#   install.packages("doParallel")
#   library(doParallel)
# }
# load tictoc package to measure running time of R code
if(!require(tictoc)){
  install.packages("tictoc")
  library(tictoc)
}
# load excel package to write results in Excel file
if(!require(openxlsx)){
  install.packages("openxlsx")
  library(openxlsx)
}
# load dplyr for data handling
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
# load prettyR package for better statistical analysis
if(!require(prettyR)){
  install.packages("prettyR")
  library(prettyR)
}
# # load sulmarytoolspackage for better table output
# if(!require(summarytools)){
#   install.packages("summarytools")
#   library(summarytools)
# }

###############################################################################################################
###############################################################################################################
####################### First part : Match patient IPP between PACS and anapath file ##########################
###############################################################################################################
###############################################################################################################



############################### data import section ##################################
# read the database with data frame existing test
# my_all_data <- read_excel("data/CV-IR_Tenon_Radiologie_detailed_data_export_tronque.xlsx")

## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
# unfortunately Excel importation yield to parse errors in date data for instance.
# SO TO AVOID THIS PROBLEM THE USER HAVE TO IMPORT DATA IN CSV FORMAT 
# BY CONVERTING ORIGINAL DATA FROM .XLSX TO .CSV IN EXCEL SOFTWARE
# /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\

tic("to import interventionnal patient data in Rstudio")
if(exists("DoseWatch_export")){
  print("raw data importation have already done")
}else{
  # /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
  # The first following line is deprecated as DoseWatch export file format has changed with first lines added with no values and characters terms.
  # /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
  #DoseWatch_export <- read.csv2("data/CV-IR_Tenon_Radiologie_detailed_data_export.csv", sep = ";") 
  
  # operations to correctly read DoseWatch export file *NEW* format
  all_content = readLines("data/Interventional_Tenon_Radiologie_detailed_data_export.csv") # to read the whole file
  skip_content = all_content[-c(1,2,3)] # to suppress the first 3 rows with bad value yield to bad header with read.csv2 function
  DoseWatch_export <- read.csv2(textConnection(skip_content), sep = ";")
}
toc()

################################## data tailoring section ##################################
# data filter to keep only interested columns for this study
DoseWatch_Selected_data <- DoseWatch_export %>% select(Study.date..YYYY.MM.DD., Series.Time, Patient.ID, Accession.number,
                                                       Patient.birthdate..YYYY.MM.DD.,
                                                       Patient.weight..kg., Patient.size..cm.,
                                                       BMI, Standard.study.description,
                                                       Peak.Skin.Dose..mGy.,
                                                       Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
                                                       Total.Acquisition.DAP..mGy.cm..,Total.Fluoro.DAP..mGy.cm..,
                                                       Total.Air.Kerma..mGy.,
                                                       Total.Acquisition.Air.Kerma..mGy., Total.Fluoro.Air.Kerma..mGy.,
                                                       Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
                                                       Irradiation.Event.Type,Proprietary.Type, Dose.Preference,
                                                       Positioner.Primary.Angle..deg., Positioner.Secondary.Angle..deg.,
                                                       Field.of.View..cm.) # to select column of interest and keeping the column's name

# convert Series.time, Patient Birthdate and Study date  columns in right time format
DoseWatch_Selected_data <- DoseWatch_Selected_data %>%
  mutate(Series.Time = hm(Series.Time), Patient.birthdate..YYYY.MM.DD. = ymd_hms(Patient.birthdate..YYYY.MM.DD.),
         Study.date..YYYY.MM.DD. = as.POSIXct(Study.date..YYYY.MM.DD.))

# sort each line by Accession number and then by acquisition hour
DoseWatch_Selected_data <- arrange(DoseWatch_Selected_data, Accession.number, Series.Time)

######################## age patient computation #################################
#  instance null vector with appropriate dimension to bind with Study_data
Patient.Age <- rep(0, nrow(DoseWatch_Selected_data))

# Loop with parallelization to calculate patient age in years 
# and add this information to Study_data dataframe
# also have a condition to test global environment object for debugging
tic("for loop with parallelization")
if(exists("Study_data_age")){
  print("patient age computation have already done")
}else{
  cores <- detectCores()
  registerDoMC(cores - 1)
  Patient.Age <- foreach(i = 1:nrow(DoseWatch_Selected_data)) %dopar% {
    #naiss = ymd_hms(DoseWatch_Selected_data[i,4]) # deprecated line as mutate function can convert easily "time" column
    #evt = as.POSIXct(DoseWatch_Selected_data[i,1]) # deprecated line as mutate function can convert easily "time" column
    # by suppressing those 2 previous lines and use mutate function instead => Computing acceleration by a factor 6 !!!!!!
    age = as.period(interval(DoseWatch_Selected_data[i,5], DoseWatch_Selected_data[i,1]))@year
    Patient.Age <- age
  }
}
toc()

Patient.Age <- as.character(Patient.Age)
Study_data_age <-cbind(DoseWatch_Selected_data,Patient.Age)

Study_data_selected_age <- Study_data_age %>% select(Study.date..YYYY.MM.DD., Series.Time, Patient.ID, Accession.number,
                                                     Patient.Age,
                                                     Patient.birthdate..YYYY.MM.DD.,
                                                     Patient.weight..kg., Patient.size..cm.,
                                                     BMI, Standard.study.description,
                                                     Peak.Skin.Dose..mGy.,
                                                     Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
                                                     Total.Acquisition.DAP..mGy.cm..,Total.Fluoro.DAP..mGy.cm..,
                                                     Total.Air.Kerma..mGy.,
                                                     Total.Acquisition.Air.Kerma..mGy., Total.Fluoro.Air.Kerma..mGy.,
                                                     Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
                                                     Irradiation.Event.Type,Proprietary.Type, Dose.Preference,
                                                     Positioner.Primary.Angle..deg., Positioner.Secondary.Angle..deg.,
                                                     Field.of.View..cm.)

############### column format conversion #################
# convert patient years in numeric
Study_data_selected_age$Patient.Age <- as.numeric(Study_data_selected_age$Patient.Age)
Study_data_selected_age$Total.Acquisition.DAP..mGy.cm.. <- as.numeric(Study_data_selected_age$Total.Acquisition.DAP..mGy.cm..)
Study_data_selected_age$Total.Fluoro.DAP..mGy.cm.. <- as.numeric(Study_data_selected_age$Total.Fluoro.DAP..mGy.cm..)

############### retrieve study patient lines #################

# add a filter to choose dedicated exam for this study
Study_data_selected_exam <- Study_data_selected_age %>% filter(Accession.number == 30034736552 |
                                                                 Accession.number == 30034706684 | Accession.number == 30036882385 |
                                                                 Accession.number == 30037056758 | Accession.number == 30037489080 |
                                                                 Accession.number == 30039018501 | Accession.number == 30039041448 |
                                                                 Accession.number == 30039759054 | Accession.number == 30040086237 |
                                                                 Accession.number == 30040362160 | Accession.number == 30040182895 |
                                                                 Accession.number == 30041139556 | Accession.number == 30041839654 |
                                                                 Accession.number == 30042281874 | Accession.number == 30043223051 |
                                                                 Patient.ID == 8002206555) 

############### Compute Exam duration #################

# faire une boucle sur les patients puis sur l'heure d'acquisition 
Study_data_selected_exam[10,2] - Study_data_selected_exam[1,2]





############### generate output Excel file for other users in this study #################
write.xlsx(Study_data_selected_exam, 'output/Study_data.xlsx', sheetName = "Study_data",
           col.names = TRUE, row.names = FALSE, append = FALSE) # row.names = FALSE to avoid first column with index numbers

################## Global environment cleaning ###########################

# Remove dataframe which don't still have any interest
if(exists("Patient.Age")) {
  print("Global environment will be clean")
  rm (Patient.Age, DoseWatch_Selected_data, Study_data_selected_age, all_content, skip_content)
}else{
  print("Global environment already clean")
}


###############################################################################################################
###############################################################################################################
############# Second part : Bin data to express dose in function to angle #####################################
###############################################################################################################
###############################################################################################################

##########################################################################
##########################################################################
##########################################################################
######################## Some statistics #################################
##########################################################################
##########################################################################
##########################################################################

Global_stat <- summary(Study_data_selected_exam)
write.xlsx(Global_stat, 'output/Global_stat.xlsx', sheetName = "Global_stat",
           col.names = TRUE, row.names = TRUE, append = FALSE)

# Global_stat_test <- describe(Patient_merge_data_all_source_selected, num.desc=c("mean","median","sd","min","max","valid.n"))
# write.xlsx(Global_stat_test, 'output/Global_stat_test.xlsx', sheetName = "Global_stat_test",
#           col.names = TRUE, row.names = TRUE, append = FALSE)


Tomo.Number <- length(Patient_merge_data_all_source$Infos.suppl)
ExamAET.stat <- Patient_merge_data_all_source %>%
  group_by(AE.source) %>%
  summarise(
    count = n(),
  )
write.xlsx(ExamAET.stat, 'output/ExamAET.stat.xlsx', sheetName = "ExamAET.stat",
           col.names = TRUE, row.names = TRUE, append = FALSE)

# CBCT_position <- which(Study_data_prostate$Irradiation.Event.Type == "CBCT+")
# Exam_ID_list_CBCT <- table(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Accession.number, droplevels(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Irradiation.Event.Type))
# Study_data_prostate_CBCT <- subset(Study_data_prostate, Irradiation.Event.Type == "CBCT+")



