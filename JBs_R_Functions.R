#
##
### JB's R-Functions ###
##
#
#
# Julian Bischof
# 2021.08.18
# Encoding: ISO-8859-1
#
#Diese Samlung beinhaltet häufig genutzte Funktionen 
#aus eigener und fremder Entwicklung
#
# Fremdentwicklung ist entsprechend gekennzeichnet


#_______________________________________________________
# How to use ####
#_______________________________________________________
# Add the following script part in the beginning of a script that needs
# one or more ot the functions below.


# #calls my function R-Script Locally!
# source("D://OneDrive//OneDrive - Technological University Dublin//Bibliothek//Software//R//R_Functions//JBs_R_Functions.R")


# #calls my function R-Script via GitHub!
#
# #UNCOMMENT!!!!!
#
# library(downloader)
#
# SHA <- sha_url('https://raw.githubusercontent.com/julianbischof/R---JBs-Data-Cleaning-Function-Collection/main/JBs_R_Functions.R')
# SOURCE <- 'https://raw.githubusercontent.com/julianbischof/R---JBs-Data-Cleaning-Function-Collection/main/JBs_R_Functions.R'
# downloader::source_url(SOURCE, sha = SHA, prompt=FALSE)
# #downloader::source_url(SOURCE, prompt=FALSE)
# print('Downloaded and using JBs_R_Functions')
#
        # downloader package!!!!:
        # By default, source_url() checks the SHA-1 hash of the file.
        # If it differs from the expected value, it will throw an error.
        # The default expectation is that a hash is provided;
        # if not, source_url() will prompt the user, asking if
        # they are sure they want to continue, unless prompt=FALSE is used.
        # In other words, if you use prompt=FALSE, it will run the remote code
        # without checking the hash, and without asking the user.
        # 
        # The purpose of checking the hash is to ensure that the file has not changed.
        # a source_url command with a hash is posted in a public forum, then others who
        # source the URL (with the hash) are guaranteed to run the same code every time.
        # This means that the author doesn't need to worry about the security of the
        # server hosting the file. It also means that the users don't have to worry
        # about the file being replaced with a damaged or maliciously-modified version.
        # 
        # To find the hash of a local file, use digest(). For a simple way to find the
        # hash of a remote file, use sha_url().



#_______________________________________________________
# Table of Content ####
#_______________________________________________________
### Mode function providing the mode (German: Modus or Modalwert) of a vector
### DataNWG-BRE weighted dice for imputation of missing (-7) factor values
### Exploratory Data Analysis EDA
### Model the equation of a linear regression of lm()
### Show Variables containing cells with certain Value
### Writes the regression coefficients in a csv file
### Relative Importance of the Variables
### Comparison of Variation

###


#_______________________________________________________
# Functions ####
#_______________________________________________________



#_______________________________________________________
### Mode function providing the mode (German: Modus or Modalwert) of a vector ####
### https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
###


#_______________________________________________________
### DataNWG-BRE weighted dice for imputation of missing (-7) factor values ####
### Julian Bischof

### Leider sehr langsam beim überschreiben von Werten. In der Version ohne Funktion geht das über Boolean Filter und ist sehr schnell
### Der Boolean Filter hat hat in der Funktion irgendwie nicht funktioniert und die Zellen werden nun einzeln geprüft und überschrieben

### NEED of Improvement


bre.wighted.dice <- function(DB_BE_df, Cluster_Variable, Predictor_Variable, Predictor_Variable_Name, HRF_vector, Sum_HRF_vector_BE){
  
    # DB_BE_df = Dataframe of Database Breitenerhebung
    # Cluster_Variable = Variable (of DB_BE_df) used for cluster the predictor-dataset and that is used for determination of the dice options and frequency(dice-weights)
    # Cluster_Variable_Name = Name of Vector
    # Predictor_Variable = Variable (Vector) that is to be predicted (sampled) based on Cluster_Variable options and frequency
    # Predictor_Variable_Name = Name of Vector
    # HRF_vector = Hochrechnungsfaktor (Vektor) passend zu Cluster und Predictor Variablen
    # Sum_HRF_vector_BE = (Singe Integer Value) Summe aller HRF_vector in BE

  
    # TEST DATA
  
    # DB_BE_df = DB_BE
    # Cluster_Variable = hk_geb
    # #Cluster_Variable_Name = as.name("hk_geb")
    # Predictor_Variable = lampenart
    # Predictor_Variable_Name = "lampenart"
    # HRF_vector = HRF
    # Sum_HRF_vector_BE = Sum_HRF_BE
    
  
    # Gewichteter Würfel(sample) mit HRF_vector und normiert nach Cluster_Variable für alle Predictor_Variable mit -7 und Cluster_Variable >= 0
    Nrow_DB_BE_df <- as.vector(nrow(DB_BE_df))                                              # Count number of rows in DB_BE_df
    class(Cluster_Variable)
    #class(DB_BE_df$Cluster_Variable_Name)                                                  # Check the data class of a vector
    levels_Cluster_Variable <- levels(as.factor(Cluster_Variable))
    #levels_Cluster_Variable <- levels(as.factor(DB_BE_df$Cluster_Variable_Name))           # listet die Ausprägungen von factor vectoren auf
    
    # Gewichtung (A) und Auswürfeln (B) für jede Ausprägung von Cluster_Variable
    for (i in levels_Cluster_Variable){
      t <- as.data.frame(table(Predictor_Variable[Cluster_Variable==i]))                    # schreibt Ausprägungen und ihre Häufigkeiten in df
      Nrow_t <- nrow(t)                                                                     # ermittelt die Anzahl der zeilen in t
      
      
      # A: Gewichtung der Häufigkeit der Ausprägungen der Predictor_Variable in der Teilmenge Cluster_Variable==i, über normierte HRF_vector
      for(a in 1:Nrow_t){                                                                                                             # For-Schleife über alle Zeilen von t und damit über alle Ausprägungen(levels)
        Predictor_Variable_Cluster_Variable_i <- Predictor_Variable[Cluster_Variable==i]                                              # Predictor_Variableen in der Teilmenge Cluster_Variable == i
        HRF_vector_Cluster_Variable_i <- HRF_vector[Cluster_Variable==i]                                                              # HRF_vector (Hochrechnungsfaktor der Breitenerhebung) zur Teilmenge Cluster_Variable == i
        Predictor_Variable_Cluster_Variable_i_a <- Predictor_Variable_Cluster_Variable_i[Predictor_Variable_Cluster_Variable_i==a]    # Predictor_Variableen a in der Teilmenge Cluster_Variable == i
        HRF_vector_Cluster_Variable_i_a <- HRF_vector_Cluster_Variable_i[Predictor_Variable_Cluster_Variable_i==a]                    # HRF_vector der Predictor_Variableen a in der Teilmenge Cluster_Variable == i
        #Sum_norm_HRF_vector_Cluster_Variable_i_a <- sum(HRF_vector_Cluster_Variable_i_a)/Sum_HRF_vector_BE
        Sum_HRF_vector_Cluster_Variable_i_a <- sum(HRF_vector_Cluster_Variable_i_a)                                                   # Für Freq_t kann die Normierung eigentlich auch wegfallen, da es um das Ziehungsverhältniss geht
        t$Freq[t$Var1==a] <- t$Freq[t$Var1==a]*Sum_HRF_vector_Cluster_Variable_i_a/Sum_HRF_vector_BE                                  #Normierung auf NWG Anzahl
      }
      
      Freq_t <- t[ ,2]                                                                      # Häufigkeitenfektor für sample probability (prob)
      levels_Predictor_Variable__Cluster_Variable_i <- levels(as.factor(Predictor_Variable[Cluster_Variable==i]))
      #u <- sample(levels_Predictor_Variable__Cluster_Variable_i, size = 1, replace = TRUE, prob = Freq_t) # Test-Würfel
      #DB_BE_df$Predictor_Variable[DB_BE_df$Predictor_Variable==(-7) & DB_BE_df$Cluster_Variable==i] <- sample(levels_Predictor_Variable__Cluster_Variable_i, size = 1, replace = TRUE, prob = Freq_t)
      
      
      # B: Gewichtetes Sample zur Auswürfeln der Predictor_Variable im Fall, dass Predictor_Variable==-7 und Cluster_Variable=i
      # Schnellerer Part, funktioniert über Funktion nicht
      # for (j in 1:Nrow_DB_BE_df) {                                              # Prüft jede Zeile in DB_BE_df ob DB_BE_df$Predictor_Variable[j]==(-7) & DB_BE_df$Cluster_Variable[j]==i
      #   if(DB_BE_df$Predictor_Variable_Name[j]==(-7) & DB_BE_df$Cluster_Variable[j]==i){
      #     DB_BE_df$Predictor_Variable_Name[j] <- sample(levels_Predictor_Variable__Cluster_Variable_i, size = 1, replace = TRUE, prob = Freq_t) # Wenn bedingung (if) TRUE wird für die Zeile(j) und die Cluster_Variable(i) ein Sample gezogen
      #   }
      
      
        for (j in 1:Nrow_DB_BE_df) {                                                                                                              # Prüft jede Zeile in DB_BE_df ob DB_BE_df$Predictor_Variable[j]==(-7) & DB_BE_df$Cluster_Variable[j]==i
          if(DB_BE_df[j,Predictor_Variable_Name]==(-7) & DB_BE_df[j,Cluster_Variable]==i){
            DB_BE_df[j,Predictor_Variable_Name] <- sample(levels_Predictor_Variable__Cluster_Variable_i, size = 1, replace = TRUE, prob = Freq_t) # Wenn bedingung (if) TRUE wird für die Zeile(j) und die Cluster_Variable(i) ein Sample gezogen
          }
      }
    }
}
###


#_______________________________________________________
### Exploratory Data Analysis EDA ####
### Fred Joop
eda.shape <- function(x, ...){            # Zuweisung Funktion
  par(mfrow=c(2, 2))                      # Aufteilen Graphik-Fenster
  plot (x, pch=19, col=2, main="raw data")# Streudiagramm Daten
  hist (x, main="histogram of x")         # Histogramm Daten
  box()                                   # Box um das Histogramm
  boxplot (x, main="boxplot of x")        # Boxplot Daten
  qqnorm (x, main="QQ-Plot of x")         # QQ-Plot Daten
  qqline (x, lwd=3, col=2)                # Einzeichnen QQ-Linie
  par(mfrow=c(1, 1))                      # Rueckstellen Graphikfenster
}
###



#_______________________________________________________
### Model the equation of a linear regression of lm() ####
### requires library(dplyr)
### Unknown Source
model_equation <- function(model, ...) {
  format_args <- list(...)
  
  model_coeff <- model$coefficients
  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                  model_coeff_sign == 1 ~ " + ",
                                  model_coeff_sign == 0 ~ " + ")
  model_eqn <- paste(strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
                     "=",
                     paste(if_else(model_coeff[1]<0, "- ", ""),
                           do.call(format, format_args)[1],
                           paste(model_coeff_prefix[-1],
                                 do.call(format, format_args)[-1],
                                 " * ",
                                 names(model_coeff[-1]),
                                 sep = "", collapse = ""),
                           sep = ""))
  return(model_eqn)
}
###


#_______________________________________________________
### Show Variables containing cells with certain Value ####
# Julian Bischof
# 2020.12.18
# Doku to the right! --> 
variable.with.certain.values <- function(df, cells.input, ...){                      # df = to be searched dataframe; cells.input = Such-Value
  df.with.certain.value <- df==cells.input                                           # Prüft ob value in Variable vorliegt
  df.with.certain.value <- as.data.frame(df.with.certain.value)                      # Wandelt die Boolean Matrix in Dataframe um
  
  df.with.certain.value.Summary <- summarise_each(df.with.certain.value, funs(mean)) # Bildet Mittelwerte über Boolean-Variablen
  
  n_cols_df <- ncol(df)                                                              # Zählt Spaltenanzahl in Dataframe
  
  for (i in 1:n_cols_df) {
    
    Variable_Name_of_col_i <- names(df.with.certain.value.Summary)[i]                # Bestimmen von Variablennamen von Spalte i
    
    Spalte_i <- df.with.certain.value.Summary[Variable_Name_of_col_i]                # Auswahl von Variablenspalte auf Basis von i
    
    if (Spalte_i > 0)                                                                # gibt alle zutreffenden Spalten in Console aus
    {
      print(Variable_Name_of_col_i)
    }
  }
}
###


#_______________________________________________________
### Writes the regression coefficients in a csv file ####
#http://geokitchen.blogspot.de/2012/10/r-writing-regression-summary-to-table.html
# Regression Coefficient as .csv , can be extended to add other objects too

regr_tab <- function(reg_model, PFAD){ # reg_model is the regression model, PFAD is the name and path of the csv file you want 
  # coefficients in dataframe
  regr_tab <- data.frame(summary(reg_model)$coefficients)
  # grab the coefficients
  colnames(regr_tab) <- colnames(summary(reg_model)$coefficients)
  # get the p-vals 
  regr_tab[ ,4] <- ifelse(regr_tab[ ,4] < .001, "< 0.001", 
                          ifelse(regr_tab[ ,4] < .01, "< 0.01", 
                                 round(regr_tab[ ,4], 3)))
  
  # format the table
  summary = format(regr_tab, autoformat = 1)
  # write it as a csv file 
  write.csv(summary, PFAD)
}
###




#_______________________________________________________
### Relative Importance of the Variables ####

# install.packages("relaimpo")
library(relaimpo)

# function definition [R in Action, Kabacoff] Page 216
# relweights(regression_data, col="lightgray") 
#Ausführen der definierten Funktion der Bewertung der relativen Wichtigkeit der Variablen
relweights <-
  function(fit,...){                         
    R <- cor(fit$model)   
    nvar <- ncol(R)          
    rxx <- R[2:nvar, 2:nvar] 
    rxy <- R[2:nvar, 1]      
    svd <- eigen(rxx)        
    evec <- svd$vectors                           
    ev <- svd$values         
    delta <- diag(sqrt(ev))  
    lambda <- evec %*% delta %*% t(evec)        
    lambdasq <- lambda ^ 2   
    beta <- solve(lambda) %*% rxy           
    rsquare <- colSums(beta ^ 2)                   
    rawwgt <- lambdasq %*% beta ^ 2    
    import <- (rawwgt / rsquare) * 100 
    lbls <- names(fit$model[2:nvar])   
    rownames(import) <- lbls
    colnames(import) <- "Weights"
    barplot(t(import),names.arg=lbls,
            ylab="% of R-Square",
            xlab="Predictor Variables",
            main="Relative Importance of Predictor Variables", 
            sub=paste("R-Square=", round(rsquare, digits=3)),
            ...)  
    return(import)
  }





#_______________________________________________________
### Comparison of Variation ####
Compare.Variation <- function(x, y, namex, namey, histfrom, histto, histby){
  #x = Variablenteilmenge X
  #y = Variablenteilmente Y
  #namex = Name der Teilmente X
  #namey = Name der Teilmente Y
  #histfrom = Histogram Startwert
  #histto = Histogram Endwert
  #histby = Histogram, Anzahl an Unterteilungen
  
  hist_sequence <- seq(from = histfrom, to = histto, by = histby)
  y_sum <- hist(y, hist_sequence, main=paste("Histogram of ",namey), xlab=namey)
  x_sum <- hist(x, hist_sequence, main=paste("Histogram of ",namex), xlab=namex)
  x_verteilung <- (x_sum$counts/length(x))*100
  y_verteilung <- (y_sum$counts/length(y))*100
  
  path <- paste(wd,"/",namex,".png", sep="")  #definiert ablagepfad und name der zu speichernden Grafik unter Working Directory
  png(file=path,    
      width=1200, height=600)                 #Speichert Grafik in der angegebenen Auflösung
  par(mfrow=c(3, 1))                          #Aufteilen Graphik-Fenster
  hist(y, hist_sequence, main=paste("Histogram of ",namey), xlab=namey, labels = as.character(round(y_verteilung,2)))
  hist(x, hist_sequence, main=paste("Histogram of ",namex), xlab=namex, labels = as.character(round(x_verteilung,2)))
  
  #Abweichung in Prozent
  abweichung <- x_verteilung-y_verteilung     #Berechnung der Abweichung der Verteilung der Variablenteilmenten X und Y
  plot(abweichung, main="Abweichung der Verteilung in %-Punkte", ylab = "Abweichung in %-Punkte")
  abline(h=0, col="blue")                     #Einbau 0-Linie in Plot
  par(mfrow=c(1, 1))                          #Rueckstellen Graphikfenster
  dev.off()
  
  #Wiederholung der Grafiken, damit diese auch im R-Fenster angezeigt werden
  par(mfrow=c(3, 1))                          #Aufteilen Graphik-Fenster
  hist(y, hist_sequence, main=paste("Histogram of ",namey), xlab=namey, labels = as.character(round(y_verteilung,2)))
  hist(x, hist_sequence, main=paste("Histogram of ",namex), xlab=namex, labels = as.character(round(x_verteilung,2)))
  
  #Abweichung in Prozent
  abweichung <- x_verteilung-y_verteilung     #Berechnung der Abweichung der Verteilung der Variablenteilmenten X und Y
  plot(abweichung, main="Abweichung der Verteilung in %-Punkte", ylab = "Abweichung in %-Punkte")
  abline(h=0, col="blue")                     #Einbau 0-Linie in Plot
  par(mfrow=c(1, 1))                          #Rueckstellen Graphikfenster
  
  #Angabe der Abweichung in der Console
  abweichung
}
###
