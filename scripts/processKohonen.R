##################################################################################################

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
shm = 0
FolderRoot = ""
if (sistema[1] == "Linux"){
  shm = 1
  FolderRoot = paste("/home/", sistema[7], "/Generate-Partitions-Kohonen-HClust", sep="")
} else {
  shm = 0
  FolderRoot = paste("C:/Users/", sistema[7], "/Generate-Partitions-Kohonen-HClust", sep="")
}
FolderScripts = paste(FolderRoot, "/scripts", sep="")



#############################################################################################################
# FUNCTION WSSPLOT                                                                                          # 
#   Objective:                                                                                              #
#      Generates the graph based on the sum of the group squares to find the optimal number o clusters.     #
# Parameters:                                                                                               #
#   data = codebooks vector                                                                                 #
#   nc = ranges from 2 to grid                                                                              #
#   seed =  seed for execution                                                                              #
#############################################################################################################
wssplot <- function(data, nc=nc, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="WSS_1")
} # END




##################################################################################################
# FUNCTION COMPUTE KOHONEN                                                                       #
#   Objective                                                                                    #
#      Modeling correlations using Kohonen                                                       #
#   Parameters                                                                                   #
#   Return                                                                                       #
##################################################################################################
computeKohonen <- function(ds, resLS, number_dataset, number_cores, number_folds, folderResults){
  
  if(interactive()==TRUE){ flush.console() }
  
  diretorios = directories(dataset_name, folderResults)
  
  cat("\nFrom 1 to 10 folds!")
  f = 1
  kohonenParalel <- foreach(f = 1:number_folds) %dopar%{
    
    cat("\nFold: ", f)   
    
    ############################################################################################################
    cat("\nLoad sources and packages")
    sistema = c(Sys.info())
    shm = 0
    FolderRoot = ""
    if (sistema[1] == "Linux"){
      shm = 1
      FolderRoot = paste("/home/", sistema[7], "/HPML-K", sep="")
    } else {
      shm = 0
      FolderRoot = paste("C:/Users/", sistema[7], "/HPML-K", sep="")
    }
    FolderScripts = paste(FolderRoot, "/scripts/", sep="")
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    setwd(FolderScripts)
    source("miscellaneous.R")
    
    
    #############################################################################################################
    # COLORS                                                                                                    #
    #############################################################################################################
    n <- 100
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    rainbowcols <- rainbow(20)
    coolBlueHotRed <- function(n, alpha = 1) {
      rainbow(n, end=4/6, alpha=alpha)[n:1]
    }
    colors <- function(n, alpha = 1) {
      rev(heat.colors(n, alpha))
    }
    
    ############################################################################################################
    #cat("\nCreate Folder Split")
    FolderSplit = paste(diretorios$folderResultsKohonen, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==FALSE){
      dir.create(FolderSplit)
    }
    
    ############################################################################################################
    #cat("\nCreate Folder Model")
    folderM = paste(FolderSplit, "/Model", sep="")
    if(dir.exists(folderM)==FALSE){
      dir.create(folderM)  
    }
    
    ############################################################################################################
    #cat("\nGet the Classes")
    classes = data.frame(resLS$Classes[f])
    classes = as.matrix(classes)
    
    #cat("\nNumber of iteractions kohonen")
    n_iterations = 1000
    
    #cat("\nRecalculate the map? T = True, F = False")
    recalculate_map = T
    
    #cat("\nDefine the dimmension of the som grid. x and y became of the file dataset")
    som_grid = kohonen::somgrid(xdim = ds$xn, ydim = ds$yn, topo="rectangular")
    setwd(folderM)
    save(som_grid, file = 'som-grid.Rdata')
    
    #cat("\nGenerating the model supersom")
    nome = paste("fold-", f, "-superSom.Rdata", sep="")
    setwd(folderM)
    if(recalculate_map == F & file.exists(nome) == T){
      setwd(folderM)
      load(nome)
    } else{
      #cat('\n| Calculating SuperSom |\n ')
      set.seed(123)
      model = kohonen::supersom(classes, grid = som_grid, rlen = n_iterations, alpha = c(1.0,0.01), 
                                radius = 5, maxNA.fraction = .5, keep.data = TRUE)
      setwd(folderM)
      save(model, file = nome)
    }
    
    #cat("\nSaving data about model")
    cat("\nDistances")
    distance = data.frame(model$distances)
    setwd(folderM)
    write.csv(distance, paste("fold-", f, "-distances.csv", sep=""))
    
    #cat("\nChanges")
    change = data.frame(model$changes)
    setwd(folderM)
    write.csv(change, paste("fold-", f, "-changes.csv", sep=""))
    
    #cat("\nCodes")
    codes = data.frame(model$codes)
    setwd(folderM)
    write.csv(codes, paste("fold-", f, "-code.csv", sep=""))
    
    #cat("\nWinners Units")
    uc = data.frame(model$unit.classif)
    setwd(folderM)
    write.csv(uc, paste("fold-", f, "-unit.csv", sep=""))
    
    #cat("\nGenerate distance matrix for codes")
    dist_m = dist(codes) %>% as.matrix()
    
    #cat("\nGenerate seperate distance matrix for map location")
    dist_on_map = kohonen::unit.distances(som_grid)
    
    #cat("\nExponentiate euclidean distance by distance on map")
    # ^ First operand raised to the power of second operand	a^b"
    dist_adj = dist_m ^ dist_on_map
    
    
    ############################################################################################################
    #cat("\nCreate folder Graphic")
    FolderGraphic = paste(FolderSplit, "/Graphics", sep="")
    if(dir.exists(FolderGraphic)==FALSE){
      dir.create(FolderGraphic)
    }
    
    #cat("\n obtain the data from model supersom")
    data = data.frame(model$data)
    
    #cat("\nObtain the codebooks vector from model supersom")
    codes = data.frame(model$codes)
    
    #cat("\nGenerates the graphics")
    #CHANGES = SHOWS THE MEAN DISTANCE TO THE CLOSEST CODEBOOK VECTOR DURING THE TRAINING"
    cat("\nChanges")
    setwd(FolderGraphic)
    pdf(paste("fold-", f, "-changes.pdf", sep=""), width = 10, height = 8)
    print(plot(model, type="changes"))
    dev.off()
    cat("\n")
    
    #cat("\nCounts")
    setwd(FolderGraphic)
    pdf(paste("fold-", f, "-counts.pdf", sep=""), width = 10, height = 8)
    print(plot(model, type="counts", palette.name = colors, heatkey = TRUE))
    dev.off()
    cat("\n")
    
    #cat("\nNeigbhour")
    setwd(FolderGraphic)
    pdf(paste("fold-", f, "-changes.pdf", sep=""), width = 10, height = 8)
    print(plot(model, type="dist.neighbours", palette.name=grey.colors))
    dev.off()
    cat("\n")
    
    #cat("\nCodes Segments")
    setwd(FolderGraphic)
    pdf(paste("fold-", f, "-segments.pdf", sep=""), width = 10, height = 8)
    print(plot(model, type="codes", codeRendering = "segments", palette.name = colors, heatkey = TRUE))
    dev.off()
    cat("\n")
    
    #cat("\nCodes Stars")
    setwd(FolderGraphic)
    pdf(paste("fold-", f, "-stars.pdf", sep=""), width = 10, height = 8)
    print(plot(model, type="codes", codeRendering = "stars", palette.name = colors, heatkey = TRUE))
    dev.off()
    cat("\n")
    
    #cat("\nCodes Lines")
    setwd(FolderGraphic)
    pdf(paste("fold-", f, "-lines.pdf", sep=""), width = 10, height = 8)
    print(plot(model, type="codes", codeRendering = "lines", palette.name = colors, heatkey = TRUE))
    dev.off()
    cat("\n")
    
    #cat("\nQuality")
    setwd(FolderGraphic)
    pdf(paste("fold-", f, "-quality.pdf", sep=""), width = 10, height = 8)
    print(plot(model, type = "quality", palette.name = colors, heatkey = TRUE))
    dev.off()
    cat("\n")
    
    #cat("\nMapping")
    setwd(FolderGraphic)
    pdf(paste("fold-", f, "-mapping.pdf", sep=""), width = 10, height = 8)
    print(plot(model, type = "mapping", pchs = 20, palette.name = colors, heatkey = TRUE))
    dev.off()
    cat("\n")
    
    
    ############################################################################################################
    #cat("\nCreate folder cluster")
    FolderCluster = paste(FolderSplit, "/Cluster", sep="")
    if(dir.exists(FolderCluster)==FALSE){
      dir.create(FolderCluster)
    }
    
    cat("\n From 1 to grid")
    
    if(interactive()==TRUE){ flush.console() }
    
    z = 1
    for(z in 1:ds$gridn){
      
      #cat("\nHierarchical Clustering to Group Codebooks")
      som_cluster <- cutree(hclust(dist(codes)), z)
      #print(som_cluster)
      #arquivo = paste("fold-", f, "-cluster-", z, ".csv", sep="")
      #d<-lapply(som_cluster, write, file= arquivo, append=T);
      
      a = c(som_cluster)
      
      setwd(FolderCluster)
      #write(paste("fold-", f, "-clusters", sep=""), arquivo)
      write.csv(a, paste("fold-", f, "-clusters-", z, ".csv", sep=""))
      
      #cat("\nClusters Mapping")
      setwd(FolderGraphic)
      pdf(paste("fold-", f, "-cluster-", z, "-mapping.pdf", sep = ""), width = 10, height = 8)
      plot(model, type="mapping", bgcol = col_vector[som_cluster]) 
      #add.cluster.boundaries(model, som_cluster)
      print(plot)
      dev.off()
      cat("\n")
      
      #cat("\nClusters Segments")
      setwd(FolderGraphic)
      pdf(paste("fold-", f, "-cluster-", z, "-segments.pdf", sep = ""), width = 10, height = 8)
      plot(model, type="codes", codeRendering = "segments", bgcol = col_vector[som_cluster]) 
      #add.cluster.boundaries(model, som_cluster)
      print(plot)
      dev.off()
      cat("\n")
      
      if(interactive()==TRUE){ flush.console() }
      gc()
    }
    
    if(interactive()==TRUE){ flush.console() }
    gc()
  }
  
  if(interactive()==TRUE){ flush.console() }
  gc()
  cat("\n##################################################################################################")
  cat("\n# END COMPUTE KOHONEN                                                                            #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


#############################################################################################################
# FUNCTION JOIN WINNERS INSTANCES                                                                           #
#   Objective:                                                                                              #
#        Aims to join the instances of class space with the neurons and their respective winning neurons    #
#   Parameters:                                                                                             #
#   Return:                                                                                                 #
#   Files Created:                                                                                          #
#############################################################################################################
joinWinnersInstances <- function(resLS, folderResults){
  
  if(interactive()==TRUE){ flush.console() }
  
  diretorios = directories(dataset_name, folderResults)
  
  cat("\nFrom 1 to 10 folds!")
  f = 1
  joinParalel <- foreach(f = 1:number_folds) %dopar%{
  
    ############################################################################################################
    FolderSplit = paste(diretorios$folderResultsKohonen, "/Split-", f, sep="")
    FolderM = paste(FolderSplit, "/Model", sep="")
    
    # Open the file with the winners neurons
    setwd(FolderM)
    winners = read.csv(paste("fold-",f,"-unit.csv", sep=""))
    
    # Transform csv in Data Frame
    winners = data.frame(winners)
    
    # changing the names of columns
    names(winners) = c("Instances", "WinnerNeuron")
    
    # Joint the two files: winners neurons and class space
    winners2 = cbind(winners, resLS$Classes[f])
    
    # Save the join information
    setwd(FolderSplit)
    write.csv(winners2, paste("fold-", f, "-winners.csv", sep=""), row.names = FALSE)
  
    if(interactive()==TRUE){ flush.console() }
    gc()
  
  } # fim do fold
  
  if(interactive()==TRUE){ flush.console() }
  gc()
  cat("\n##################################################################################################")
  cat("\n# END JOIN WINNER NEURONS INSTANCES                                                              #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



#############################################################################################################
# FUNCTION INSTANCES PER NEURONS                                                                            #
#    Objective:                                                                                             #
#        Saves in a ".csv" file the separated neurons with their respective instances.                      #
#        For example: neuron 1 with all of its 20 instances, neuron 2 with all of its 100 instances, and so #
#    Parameters:                                                                                            #
#   Return:                                                                                                 #
#   Folders Created:                                                                                        #
#   Files Created:                                                                                          #
#############################################################################################################
instancesPerNeurons <- function(ds){
  
  if(interactive()==TRUE){ flush.console() }
  
  diretorios = directories(dataset_name, folderResults)
  
  cat("\nFrom 1 to 10 folds!")
  f = 1
  ipnParalel <- foreach(f = 1:number_folds) %dopar%{
    
    ############################################################################################################
    FolderSplit = paste(diretorios$folderResultsKohonen, "/Split-", f, sep="")
    
    # importing winners
    setwd(FolderSplit)
    winners = data.frame(read.csv(paste("fold-", f, "-winners.csv", sep="")))
    
    neuron = c(0)
    numeroDeInstancias = c(0)
    porcentagem = c(0)
    totalInstanciasPorneuron = data.frame(neuron, numeroDeInstancias, porcentagem)
  
    # Starting the join 
    k = 1
    for(k in 1:ds$gridn){
      cat("\nNeuron: ", k)
      
      # Getting only the unit = k, I mean, only the winner neuron = k
      # K is the number of neurons ( k = 1, 2, 3, ..., grid)
      unit = data.frame(winners %>% filter(., winners$WinnerNeuron == k))
      
      # For neuron = 1, create a new file, for neuron = 2, new file, .....
      # k = 1, 2, 3, ..., grid
      # separando os neurons com suas respectivas inst?ncias
      #name = paste("fold-", f, "-Neuron-", k, ".csv", sep="")
      #write.csv(unit, name, row.names = FALSE)
      
      # salvando informações de estatísticas
      numeroDeInstancias = nrow(unit)
      neuron = k
      porcentagem = numeroDeInstancias/ds$gridn
      ipn = data.frame(neuron, numeroDeInstancias, porcentagem)
      totalInstanciasPorneuron = rbind(totalInstanciasPorneuron, ipn)
      
      k = k + 1
      
      if(interactive()==TRUE){ flush.console() }
      gc()
    } # fim do grupo/partição
    
    setwd(FolderSplit)
    nome = paste("fold-", f, "-ipn-percentagem.csv", sep="")
    write.csv(totalInstanciasPorneuron[-1,], nome, row.names = F)
    
    if(interactive()==TRUE){ flush.console() }
    gc()
  
  } # fim do fold
  
  if(interactive()==TRUE){ flush.console() }
  gc()
  cat("\n##################################################################################################")
  cat("\n# END INSTANCES PER NEURONS                                                                      #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



#############################################################################################################
# FUNCTION LABELS PER NEURONS                                                                               #
#    Objective:                                                                                             #
#        Saves in a ".csv" file the total of labels per neurons                                             #
#    Parameters:                                                                                            #
#   Return:                                                                                                 #
#   Files Created:                                                                                          #
#        LabelsPerNeuron.csv                                                                                #
#############################################################################################################
labelsPerNeurons <- function(ds){
  
  if(interactive()==TRUE){ flush.console() }
  
  diretorios = directories(dataset_name, folderResults)
  
  cat("\nFrom 1 to 10 folds!")
  f = 1
  lpnParalel <- foreach(f = 1:number_folds) %dopar%{
    
    ############################################################################################################
    FolderSplit = paste(diretorios$folderResultsKohonen, "/Split-", f, sep="")
    
    # importing winners
    setwd(FolderSplit)
    winners = data.frame(read.csv(paste("fold-", f, "-winners.csv", sep="")))
  
    # create a new data frame
    apagar = c(0)
    total2 = data.frame(apagar)
    
    # Starting the join 
    k = 1
    for(k in 1:ds$gridn){
      cat("\nNeuron: ", k)
    
      # Getting only the unit = k, I mean, only the winner neuron = k
      # K is the number of neurons ( k = 1, 2, 3, ..., grid)
      unit = data.frame(winners %>% filter(., winners$WinnerNeuron == k))
      
      # total
      total = data.frame(colSums(unit))
      str = paste("Neuron_", k, sep="")
      names(total) = str
      total2 = cbind(total2, total)
      
      # count
      k = k + 1
      
      if(interactive()==TRUE){ flush.console() }
      gc()
    }
    
    total3 = total2[c(-1,-2),-1]
    
    setwd(FolderSplit)
    write.csv(total3, paste("fold-", f, "-labelsPerNeuron.csv", sep=""))
    
    matrix_binary = total3
    
    # if label exist, then 1, otherwise, 0
    i = 0
    j = 0
    for(i in 1:nrow(matrix_binary)){
      for(j in 1:ncol(matrix_binary)){
        if(matrix_binary[i,j] != 0){
          matrix_binary[i,j] = 1
        } 
      } 
    } 
    
    setwd(FolderSplit)
    write.csv(matrix_binary, paste("fold-", f, "-labelsPerNeuron-binary.csv", sep=""))
    
    if(interactive()==TRUE){ flush.console() }
    gc()
  }
  
  if(interactive()==TRUE){ flush.console() }
  gc()
  cat("\n##################################################################################################")
  cat("\n# END LABELS PER NEURONS                                                                         #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



#############################################################################################################
# FUNCTION SELECT EXCLUSIVE LABLES                                                                          #
#   Objective:                                                                                              #
#   Parameters:                                                                                             #
#   Return:                                                                                                 #
#   Folders Created:                                                                                        #
#   Files Created:                                                                                          #
#############################################################################################################
selectExclusivesLabels <- function(ds, namesLabels){
  
  if(interactive()==TRUE){ flush.console() }
  
  #cat("\nGet directories")
  diretorios = directories(dataset_name, folderResults)
  
  #cat("\nCreate data frame to store clusters + neurons + labels")
  fold = c()
  partition = c()
  cluster = c()
  lpn2 = c()
  lpn3 = c()
  ncl_final = data.frame(fold, partition, cluster, lpn2)
  ncl_final_b = data.frame(fold, partition, cluster, lpn3)
  
  namesLabels = namesLabels
  
  cat("\nFrom 1 to 10 folds!")
  f = 1
  lpnParalel <- foreach(f = 1:number_folds) %dopar%{
    
    cat("\nFold: ", f)
    
    namesLabels = namesLabels
    
    ############################################################################################################
    cat("\nGet folders")
    FolderSplit = paste(diretorios$folderResultsKohonen, "/Split-", f, sep="")
    FolderCluster = paste(FolderSplit, "/Cluster", sep="")
    FolderIPN = paste(FolderSplit, "/InstancesPerNeurons", sep="")
    
    FolderAllPartitions = paste(FolderSplit, "/Partitions", sep="")
    if(dir.exists(FolderAllPartitions)==FALSE){
      dir.create(FolderAllPartitions)
    }
    
    #cat("\nOpen Label per Neurons")
    setwd(FolderSplit)
    lpn = data.frame(read.csv(paste("fold-",f,"-labelsPerNeuron.csv", sep="")))
    names(lpn)[1] = "label"
    lpn2 = data.frame(t(lpn))
    colnames(lpn2) = c(lpn2[1,])
    lpn2=lpn2[-1,]
    lpn2a = as.data.frame(apply(lpn2,2,as.numeric))
    
    #sapply(lpn2, class)
    
    #cat("\nOpen Label per Neurons Binary")
    setwd(FolderSplit)
    lpn_b = data.frame(read.csv(paste("fold-",f,"-labelsPerNeuron-binary.csv", sep="")))
    names(lpn_b)[1] = "label"
    lpn3 = data.frame(t(lpn_b))
    colnames(lpn3) = c(lpn3[1,])
    lpn3=lpn3[-1,]
    rownames(lpn3) = NULL
    lpn3a = as.data.frame(apply(lpn3,2,as.numeric))
    
    # número de partições
    num.part = ds$gridn - 1
    
    cat("\nFrom partition 2 to grid")
    k = 2
    while(k<=num.part){
      
      cat("\n\nPartition: ", k)
      
      #cat("\nCreate Data Frame")
      final = data.frame()
      
      #cat("\nGet Folders")
      FolderPartition = paste(FolderAllPartitions, "/Partition-", k, sep="")
      if(dir.exists(FolderPartition)==FALSE){
        dir.create(FolderPartition)
      }
      
      #cat("\nOpen info clusters")
      setwd(FolderCluster)
      name2 = paste("fold-",f,"-clusters-",k,".csv", sep="")
      cluster = data.frame(read.csv(name2))
      names(cluster) = c("neuron","cluster")
      frequencia = count(cluster, vars=cluster)
      nGroup = nrow(frequencia)
      neu = seq(from = 1, to = ds$gridn, by = 1)
      cluster$neuron = neu
      #print(cluster)
    
      #cat("\nUpdate data frame")
      fold = f
      partition = k
      ncl_t = cbind(fold, partition, cluster, lpn2a)
      rownames(ncl_t) = NULL
      #print(ncl_t)
      
      #cat("\nUpdate data frame")
      fold = f
      partition = k
      ncl_b = cbind(fold, partition, cluster, lpn3a)
      #print(ncl_b)
      
      #cat("\nSave all information: clusters, fold, partitions, groups, winners neuros, number instance, labels")
      setwd(FolderPartition)
      write.csv(ncl_t, paste("fold-", f, "-original-partition-", k, ".csv", sep=""), row.names = FALSE)
      write.csv(ncl_b, paste("fold-", f, "-original-partition-", k, "-binary.csv", sep=""), row.names = FALSE)
      
      #cat("\nFinal data frame")
      ncl_final = rbind(ncl_final, ncl_t)
      ncl_final_b = rbind(ncl_final_b, ncl_b)
      
      #cat("\nCreate columns names")
      nomesColunas = c()  
      j=1 
      while(j<=nGroup){
        nomesColunas[j] = paste("group-",j,sep="")
        j = j + 1
      }
      
      # Criando os data frames para armazenar o resultado dos rótulos exclusivos
      sumario_t = data.frame(matrix(0,nrow=ds$Labels,ncol=nGroup+1))
      colnames(sumario_t) = c("labels", nomesColunas)
      sumario_t$labels = c(namesLabels)
      
      sumario_b = data.frame(matrix(0,nrow=ds$Labels,ncol=nGroup+1))
      colnames(sumario_b) = c("labels", nomesColunas)
      sumario_b$labels = c(namesLabels)
      
      # separando os grupos
      g=1
      while(g<=nGroup){
        cat("\nGroup: ", g)
        group1 = data.frame(ncl_t %>% filter(., ncl_t$cluster == g))
        group3 = data.frame(ncl_b %>% filter(., ncl_b$cluster == g))
        
        group11 = group1[,c(-1,-2,-3,-4)]
        total = data.frame(apply(group11, 2, sum))
        names(total) = "total"
        sumario_t[g+1]=total
        
        group3a = group3[,c(-1,-2,-3,-4)]
        total_b = data.frame(apply(group3a, 2, sum))
        names(total_b) = "total"
        sumario_b[g+1]=total_b
        
        g = g + 1
        if(interactive()==TRUE){ flush.console() }
        gc()
      } # fim do grupo
      
      # salva as informações
      setwd(FolderPartition)
      write.csv(sumario_t, paste("original-total-labels-groups-", k, ".csv", sep=""), row.names = FALSE)
      write.csv(sumario_b, paste("original-total-neurons-labels-groups-", k, ".csv", sep=""), row.names = FALSE)
      sumario_b2 = ifelse((sumario_b!=0),1,0)
      
      #convert into matrix
      sum_mtb = as.matrix(sumario_b2[,-1])
      sum_mt = as.matrix(sumario_t[,-1])
      
      # verifica qtas vezes o rótulo aparece
      res = rowSums(sum_mt > 0)
      
      # aqui é onde de fato verifica os grupos e escolhe onde o rótulo ficará
      t = 1
      while(t<=ds$Labels){
        # retorna o maior grupo de cada rotulo
        res2 = apply(sum_mt, 1, which.max)
        if(res[t]!=1){
          # o grupo que o rótulo vai ficar é
          grupo_que_ficara = res2[t]
          # nos outros grupos o rótulo deve virar zero
          sum_mtb[t,] = 0
          sum_mtb[t,grupo_que_ficara] = 1
          #print(sum_mtb)
        }
        t = t + 1    
        if(interactive()==TRUE){ flush.console() }
        gc()
      }
      
      # salva informações
      final_partition = cbind(namesLabels, data.frame(sum_mtb))
      setwd(FolderPartition)
      write.csv(final_partition, paste("fold-", f, "-selected-partition-", k, ".csv", sep=""), row.names = FALSE)
      
      #barplot(sum_mtb, ylab= "Total", beside=TRUE, col=rainbow(5))
      #legend("topright", c(namesLabels), cex=0.6, bty="n", fill=rainbow(5));
      
      k = k + 1
      if(interactive()==TRUE){ flush.console() }
      gc()
    } # fim da partição
    
    #setwd(FolderSplit)
    #write.csv(ncl_final, paste("fold-", f, "-all-selected-partitions.csv", sep=""), row.names = FALSE)
    #write.csv(ncl_final_b, paste("fold-", f, "-all-selected-partitions-binary.csv", sep=""), row.names = FALSE)
    
    if(interactive()==TRUE){ flush.console() }
    gc()
    
  } # fim do fold
  
  if(interactive()==TRUE){ flush.console() }
  gc()
  cat("\n##################################################################################################")
  cat("\n# END SELECT LABELS                                                                              #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



#############################################################################################################
# FUNCTION 
#   Objective:                                                                                              #
#   Parameters:                                                                                             #
#   Return:                                                                                                 #
#   Folders Created:                                                                                        #
#   Files Created:                                                                                          #
#############################################################################################################
verifyGroupsEmpty <- function(ds, namesLabels){

  if(interactive()==TRUE){ flush.console() }

  #cat("\nGet directories")
  diretorios = directories(dataset_name, folderResults)
  
  FolderInfoPart = paste(diretorios$folderResultsDataset, "/InfoPartitions", sep="")
  if(dir.exists(FolderInfoPart)==FALSE){
    dir.create(FolderInfoPart)
  }

  cat("\nFrom 1 to 10 folds!")
  f = 1
  vgpParalel <- foreach(f = 1:number_folds) %dopar%{
    
    cat("\nFold: ", f)
    
    ############################################################################################################
    #cat("\nGet folders")
    FolderSplit = paste(diretorios$folderResultsKohonen, "/Split-", f, sep="")
    FolderAllPartitions = paste(FolderSplit, "/Partitions", sep="")
    
    FolderIPSplit = paste(FolderInfoPart, "/Split-", f, sep="")
    if(dir.exists(FolderIPSplit)==FALSE){
      dir.create(FolderIPSplit)
    }
    
    # cat("\nData frame")
    fold = c(0)
    partition = c(0)
    new.num.groups = c(0)
    resumePartitions = data.frame(fold, partition, new.num.groups)
    
    labelsPerGroups = data.frame()
    
    #cat("\nNumber of partitions")
    num.part = ds$gridn - 1 
    
    #cat("\nFrom partition 2 to grid")
    p = 2
    while(p<=num.part){
      cat("\n\tPartition: ", p)
      
      FolderIPSP = paste(FolderIPSplit, "/Partition-", p, sep="")
      if(dir.exists(FolderIPSP)==FALSE){
        dir.create(FolderIPSP)
      }
      
      FolderPartition = paste(FolderAllPartitions, "/Partition-", p, sep="")
      setwd(FolderPartition)
      partition = data.frame(read.csv(paste("fold-",f,"-selected-partition-",p,".csv", sep="")))
      
      #cat("\nDeleting all empty groups")
      partition3 = partition[!sapply(partition, function(x) all(x == 0))]
      new.num.groups = ncol(partition3)-1
      cat("\n\tNovo número de grupos: ", new.num.groups)
      
      if(new.num.groups == 1 ){
        cat("\n\tUm grupo")
        partition2 = data.frame(partition3[,-1])
        rotulosPorGrupo = data.frame(apply(partition2, 2, sum))
        names(rotulosPorGrupo) = "totalLabels"
        rownames(rotulosPorGrupo) = "group.1"
      } else {
        cat("\n\tMais de um grupo")
        partition2 = partition3[,-1]
        rotulosPorGrupo = data.frame(apply(partition2, 2, sum))
        names(rotulosPorGrupo) = "totalLabels"
      }
      
      setwd(FolderPartition)
      write.csv(rotulosPorGrupo, paste("fold-",f,"-new-labels-per-group-partition-", p,".csv", sep=""))
        
      # cat("\nData frame")
      fold = f
      partition = p
      resumePartitions = rbind(resumePartitions, data.frame(fold, partition, new.num.groups))
      
      setwd(FolderPartition)
      write.csv(partition3, paste("fold-",f,"-new-partition-",p,".csv", sep=""), row.names = FALSE)
      
      setwd(FolderIPSP)
      write.csv(partition3, paste("fold-",f,"-new-partition-",p,".csv", sep=""), row.names = FALSE)
      
      resumePartitions2 = resumePartitions[-1,]
      
      setwd(FolderIPSP)
      write.csv(resumePartitions2, paste("fold-",f,"-new-summary-partitions.csv", sep=""), row.names = FALSE)
      
      Folder = paste(diretorios$folderReportsDataset, "/FinalPartitions", sep="")
      if(dir.exists(Folder)==FALSE){
        dir.create(Folder)
      }
      setwd(Folder)
      write.csv(resumePartitions2, paste("fold-",f,"-new-summary-partitions.csv", sep=""), row.names = FALSE)
      
      nomesDosRotulos = partition3[,1]
      partition4 = partition3[,-1]
      partition4 = data.frame(t(partition4))
      colnames(partition4) = nomesDosRotulos
      
      group = c("")
      totalLabelsGroup = c(0)
      labelsPerGroups2 = data.frame(group, totalLabelsGroup)
      nomesGrupos = c(colnames(partition3))
      
      group2 = c(0)
      labels2 = ("")
      particao = data.frame(group2, labels2) 
      
      y = 1
      w = y + 1
      while(y<=new.num.groups){
        cat("\n\tGroup: ", y)
        
        res = partition3[-row(partition3)[partition3[,w] == 0],]
        
        # se não houver nenhum rótulo igual a zero
        if(nrow(res)==0){
          #cat("\n\tTodos os rótulos fazem parte deste grupo")
          
          totalLabelsGroup = ds$Labels
          group = nomesGrupos[w]
          labelsPerGroups2 = rbind(labelsPerGroups2, data.frame(group, totalLabelsGroup))
          
          group2 = y
          labels2 = namesLabels
          particao = rbind(particao, data.frame(group2, labels2))
          
          setwd(FolderIPSP)
          write.csv(namesLabels, paste("labels-in-this-group-", y, "-.csv", sep=""))
          
          setwd(FolderPartition)
          write.csv(namesLabels, paste("labels-in-this-group-", y, "-.csv", sep="")) 
          
        } else {
          #cat("\n\tAlguns rótulos fazem parte deste grupo")
          rotulosNesteGrupo = c(res[,1])
          
          group2 = y
          labels2 = rotulosNesteGrupo
          particao = rbind(particao, data.frame(group2, labels2))
          
          totalLabelsGroup = length(rotulosNesteGrupo)
          group = nomesGrupos[w]
          labelsPerGroups2 = rbind(labelsPerGroups2, data.frame(group, totalLabelsGroup))
          
          setwd(FolderIPSP)
          write.csv(rotulosNesteGrupo, paste("labels-in-this-group-", y, "-.csv", sep=""))
          
          setwd(FolderPartition)
          write.csv(rotulosNesteGrupo, paste("labels-in-this-group-", y, "-.csv", sep="")) 
        }
        
        w = w + 1
        y = y + 1
        gc()
      }
      
      labelsPerGroups2 = labelsPerGroups2[-1,]
      particao2 = particao[-1,]
      
      setwd(FolderPartition)
      write.csv(labelsPerGroups2, paste("total-labels-per-groups-partition-", p, "-.csv", sep=""))
      write.csv(particao2, paste("new-final-partition-", p, "-.csv", sep=""))
      
      setwd(FolderIPSP)
      write.csv(labelsPerGroups2, paste("total-labels-per-groups-partition-", p, "-.csv", sep=""))
      write.csv(particao2, paste("new-final-partition-", p, "-.csv", sep=""))
      
      Folder = paste(diretorios$folderReportsDataset, "/FinalPartitions", sep="")
      setwd(Folder)
      write.csv(particao2, paste("new-final-partition-", p, "-.csv", sep=""))
      
      p = p + 1 # increment partition
      if(interactive()==TRUE){ flush.console() }
      gc()
    } # fim partition
    
    if(interactive()==TRUE){ flush.console() }
    gc()
  } # fim fold
  
  if(interactive()==TRUE){ flush.console() }
  gc()
  cat("\n##################################################################################################")
  cat("\n# END VERIFY EMPTY GROUPS                                                                        #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



#############################################################################################################
# FUNCTION 
#   Objective:                                                                                              #
#   Parameters:                                                                                             #
#   Return:                                                                                                 #
#   Folders Created:                                                                                        #
#   Files Created:                                                                                          #
#############################################################################################################
generatedPartitionsKohonen <- function(ds, resLS, namesLabels, number_dataset, number_cores, 
                                       number_folds, dataset_name, folderResults){
  
  diretorios = directories(dataset_name, folderResults)
  
  cat("\n\n################################################################################################")
  cat("\n#Run: Compute kohonen")
  timeKohonen = system.time(computeKohonen(ds, resLS, number_dataset, number_cores, number_folds, folderResults))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\n#Run:Join instances with winners                                                                  #")
  timeJoin = system.time(joinWinnersInstances(resLS, folderResults))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\n#Run: Calculate labels per neurons                                                                #")
  timeLPN = system.time(labelsPerNeurons(ds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\n#Run: Calculate instances per neurons                                                             #")
  timeIPN = system.time(instancesPerNeurons(ds))
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\n#Run: Verify Exclusive Labels                                                                     #")
  timeSEL = system.time(selectExclusivesLabels(ds, namesLabels)) 
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\n#Run: Verify Empty Groups                                                                         #")
  timeVGE = system.time(verifyGroupsEmpty(ds, namesLabels)) 
  cat("\n\n################################################################################################")
  
  cat("\n\n################################################################################################")
  cat("\n#Runtime")
  timesKohonen = rbind(timeKohonen, timeJoin, timeLPN, timeSEL, timeVGE)
  setwd(diretorios$folderResultsKohonen)
  write.csv(timesKohonen, paste(dataset_name, "-kohonen-RunTime.csv"))
  cat("\n##################################################################################################")

  if(interactive()==TRUE){ flush.console() }
  gc()
  cat("\n##################################################################################################")
  cat("\n# END GENERATED PARTITIONS KOHONEN                                                               #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################