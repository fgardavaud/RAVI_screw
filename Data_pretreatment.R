######################################################################
##                                                                  ##                                  
##  Cone-beam computed tomography navigation software too perform   ##
##      percutaneous screw fixation of coxal bone metastases:       ##
##   an evaluation of feasibility, safety and outcomes in case of   ##
##                      steep angulation                            ##
##                                                                  ##
##                                                                  ##                                 
######################################################################

# created by François Gardavaud, MPE, M.Sc. Medical imaging department - Tenon University Hopistal
# date of creation : 01/26/2021
# last review : 03/31/2021
# project lead by Pr. François Cornelis, MD, PhD. Medical imaging department - Tenon University Hopistal

###################### set-up environment section ################################

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

# load tidyverse for data science such as data handling and visualization
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

# load rprojroot for path definition
if(!require(rprojroot)){
  install.packages("rprojroot")
  library(rprojroot)
}

# Set the project path to the root level.
root.dir = rprojroot::find_rstudio_root_file()

###############################################################################################################
###############################################################################################################
######################### First part : data pre-treatment #####################################################
###############################################################################################################
###############################################################################################################



############################### data import section ##################################

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
  # The first following line is deprecated as DoseWatch export file format has changed in v3.2.3 with first lines added with no values and characters terms.
  # /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
  #DoseWatch_export <- read.csv2("data/CV-IR_Tenon_Radiologie_detailed_data_export.csv", sep = ";")
  
  # operations to correctly read DoseWatch export file *NEW* format only for DoseWatch v3.2.3 and above.
  # if you have DoseWatch v3.1 or under comment the three following lines and uncomment the last previous command line.
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
                                                                 Accession.number == 30044931878 | Patient.ID == 8015168414 |
                                                                 Patient.ID == 8002206555) 

############### Compute Exam duration #################

#create a data frame with only the first and last row for each Accession number
Study_data_selected_exam_duration <- Study_data_selected_exam %>% 
  group_by(Accession.number) %>%
  slice(c(1, n())) %>% # select only the first and last row for each group value.
  mutate (Series.Time.Minute = hour(Series.Time)*60 + minute(Series.Time)) %>% # convert Series.Time in minutes
  ungroup()



#add two columns with time difference between first and second row for each exam in hour and in minute.
Study_data_selected_exam_duration <- Study_data_selected_exam_duration %>% 
  group_by(Accession.number) %>%
  dplyr::mutate(
    Exam.duration.Hour = (dplyr::last(Series.Time) - dplyr::first(Series.Time)),
    Exam.duration.Minute = (dplyr::last(Series.Time.Minute) - dplyr::first(Series.Time.Minute))
  )

# suppress unnecessary column in order to merge data in one dataframe
Study_data_selected_exam_duration_filtered <- Study_data_selected_exam_duration %>% select(Accession.number, Exam.duration.Hour, Exam.duration.Minute)
# Remove duplicates based on Accession number
Study_data_selected_exam_duration_filtered <- Study_data_selected_exam_duration_filtered[!duplicated(Study_data_selected_exam_duration_filtered$Accession.number), ] # to keep only one row for each exam time.

# merge data between big dataframe and dataframe with only exam duration
Study_data_selected_exam_with_duration <- merge(Study_data_selected_exam , Study_data_selected_exam_duration_filtered, by.x = "Accession.number", by.y = "Accession.number")
# sort each line by Accession number and then by acquisition hour
Study_data_selected_exam_with_duration <- arrange(Study_data_selected_exam_with_duration, Accession.number, Series.Time)

# Remove duplicates in order to have only one row by exam
Study_data_selected_exam_with_duration_without_duplicates <- Study_data_selected_exam_with_duration[!duplicated(Study_data_selected_exam_with_duration$Accession.number), ] # to keep only one row for each exam time.
# to select only row that not depends on sequence parameters
Study_data_selected_exam_with_duration_without_duplicates <- Study_data_selected_exam_with_duration_without_duplicates %>% select(Study.date..YYYY.MM.DD., Patient.ID, Accession.number,
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
                                                                                                                                  Exam.duration.Hour, Exam.duration.Minute)
                                                                                                                                  
############### generate output Excel file to perform statistical analysis in another environment #################
write.xlsx(Study_data_selected_exam_with_duration, 'output/Study_data_detailed.xlsx', sheetName = "Study_data_detailed",
           col.names = TRUE, row.names = FALSE, append = FALSE) # row.names = FALSE to avoid first column with index numbers
write.xlsx(Study_data_selected_exam_with_duration_without_duplicates, 'output/Study_data_general.xlsx', sheetName = "Study_data_general",
           col.names = TRUE, row.names = FALSE, append = FALSE) # row.names = FALSE to avoid first column with index numbers

################## Global environment cleaning ###########################

# Remove dataframe which don't still have any interest
if(exists("Patient.Age")) {
  print("Global environment will be clean")
  rm (Patient.Age, DoseWatch_Selected_data, Study_data_selected_age, all_content, skip_content)
}else{
  print("Global environment already clean")
}






