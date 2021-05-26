cat("\n\n################################################################################################")
cat("\n# START GENERATING PARTITIONS KOHONEN                                                            #")
cat("\n##################################################################################################\n\n") 

##################################################################################################
# Modeling Labels Correlations with Kohonen and Partitioning the Label Space With HClust         #
# Copyright (C) 2021                                                                             #
#                                                                                                #
# This code is free software: you can redistribute it and/or modify it under the terms of the    #
# GNU General Public License as published by the Free Software Foundation, either version 3 of   #
# the License, or (at your option) any later version. This code is distributed in the hope       #
# that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for    #
# more details.                                                                                  #
#                                                                                                #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
#                                                                                                #
##################################################################################################


##################################################################################################
# Script 
##################################################################################################


##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
sistema = c(Sys.info())
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/Generate-Partitions-Kohonen", sep="")
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/Generate-Partitions-Kohonen", sep="")
}
FolderScripts = paste(FolderRoot, "/scripts", sep="")



##################################################################################################
# Options Configuration                                                                          #
##################################################################################################
cat("\nR options config")
options(java.parameters = "-Xmx32g")
options(show.error.messages = TRUE)
options(scipen=30)



##################################################################################################
# Read the dataset file with the information for each dataset                                    #
##################################################################################################
cat("\nOpen datasets")
setwd(FolderRoot)
datasets <- data.frame(read.csv("datasets-hpml-k.csv"))



##################################################################################################
# ARGS COMMAND LINE                                                                              #
##################################################################################################
cat("\nGet Args")
args <- commandArgs(TRUE)



##################################################################################################
# Get dataset information                                                                        #
##################################################################################################
ds <- datasets[as.numeric(args[1]),]
cat("\nHPML-K DS \t ", as.numeric(args[1]))



##################################################################################################
# Get the number of cores                                                                        #
##################################################################################################
number_cores <- as.numeric(args[2])
cat("\nHPML-K: cores \t ", number_cores)



##################################################################################################
# Get the number of folds                                                                        #
##################################################################################################
number_folds <- as.numeric(args[3])
cat("\nHPML-K: folds \t ", number_folds)



##################################################################################################
# Get the number of folds                                                                        #
##################################################################################################
folderResults <- toString(args[4])
cat("\nHPML-K: folder \t ", folderResults)



##################################################################################################
# Get dataset name                                                                               #
##################################################################################################
dataset_name <- toString(ds$Name) 
cat("\nHPML-K: nome \t ", dataset_name)



##################################################################################################
# DON'T RUN -- it's only for test the code
# ds = datasets[2,]
# dataset_name = ds$Name
# number_dataset = ds$Id
# number_cores = 10
# number_folds = 10
# folderResults = "/dev/shm/birds"
##################################################################################################



##################################################################################################
# CONFIG THE FOLDER RESULTS                                                                      #
##################################################################################################
cat("\nCreate Folder")
if(dir.exists(folderResults)==FALSE){
  dir.create(folderResults)
}



##################################################################################################
# LOAD RUN.R                                                                                     #
##################################################################################################
cat("\nLoad sources")
setwd(FolderScripts)
source("run.R") 



##################################################################################################
#
##################################################################################################
cat("\nGet directories")
diretorios = directories(dataset_name, folderResults)



##################################################################################################
# execute the code and get the total execution time                                              #
# n_dataset, number_cores, number_folds, folderResults                                           #
##################################################################################################
cat("\nExecute GPKH")
timeFinal <- system.time(results <- gpkh(args[1], number_cores, number_folds, folderResults))
print(timeFinal)

# DO NOT RUN
# timeFinal <- system.time(results <- gpkh(2, number_cores, number_folds, folderResults))


##################################################################################################
# save the total time in rds format in the dataset folder                                        #
##################################################################################################
cat("\nSave Rds\n")
str0 <- paste(diretorios$folderResults, "/", dataset_name, "-results-gpkh.rds", sep="")
save(results, file = str0)



##################################################################################################
# save results in RDATA form in the dataset folder                                               #
##################################################################################################
cat("\nSave Rdata \n")
str1 <- paste(diretorios$folderResults, "/", dataset_name, "-results-gpkh.RData", sep="")
save(results, file = str1)



##################################################################################################
# compress the results for later transfer to the dataset folder                                  #
##################################################################################################
cat("\nCompress results \n")
setwd(diretorios$folderResults)
str = paste("tar -zcvf ", dataset_name, "-results-gpkh.tar.gz ", diretorios$folderResults, sep="")
print(system(str))



##################################################################################################
# copy file                                                                                      #
##################################################################################################
cat("\nCopy file tar \n")
str2 = paste("cp ", diretorios$folderResults, "/", dataset_name, "-results-gpkh.tar.gz ", diretorios$folderDatasetResults, sep="")
print(system(str2))



########################################################################################################################
cat("\n Copy to google drive")
origem = diretorios$folderDatasetResults
destino = paste("cloud:[2021]ResultadosExperimentos/Generate-Partitions-Kohonen-HClust/", dataset_name, sep="")
comando = paste("rclone -v copy ", origem, " ", destino, sep="")
system(comando)



##################################################################################################
# del                                                                                      #
##################################################################################################
cat("\nDelete folder \n")
str5 = paste("rm -r ", diretorios$folderResults, sep="")
print(system(str5))

cat("\nDel objects")
rm(list = ls())

cat("\nClear!")
gc()

cat("\n##################################################################################################")
cat("\n# END OF HPML-K. THANKS GOD !!                                                                   #")
cat("\n##################################################################################################")
cat("\n\n\n\n") 

if(interactive()==TRUE){ flush.console() }

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################