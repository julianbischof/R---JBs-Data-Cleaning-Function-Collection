#
##
### JB's R-Functions ###
##
#
#
# Julian Bischof
# 2022.10.31
# Encoding: UTF-8
#
#Diese Samlung beinhaltet haeufig genutzte Funktionen 
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
### Prepares Parallel Computing Environment for CARET model training
### Removes outliers of measured and calculated energy based on ratio, distribution and set percentile
### Function for save hk_geb based calibration factors (ratio measured and calculated energy) with according hk_geb in dataframe
### Function for save hk_geb_agg based calibration factors (ratio measured and calculated energy) with according hk_geb_agg in dataframe
### Function for the extrapolation of full set (no missing) numerical values of the imuptated ENOB:DataNWG interview data set to the entire German non-domestic building stock (Mean, without error calculation)
### Determination of correlation between categorical variables and numerical variables via ANOVA analysis
### Repetitive linear regression application for determination the significant variables
### Defininig stuetzstellen - correction factor values - based on distribtion
### Defininig stuetzstellen klassen - classes of correction factor values - based on distribtion
### Row dublication in data frame based on row weights to be applied for later weighting e.g. of machine learning model training

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

### Leider sehr langsam beim ueberschreiben von Werten. In der Version ohne Funktion geht das ueber Boolean Filter und ist sehr schnell
### Der Boolean Filter hat hat in der Funktion irgendwie nicht funktioniert und die Zellen werden nun einzeln geprueft und ueberschrieben

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
    
  
    # Gewichteter Wuerfel(sample) mit HRF_vector und normiert nach Cluster_Variable fuer alle Predictor_Variable mit -7 und Cluster_Variable >= 0
    Nrow_DB_BE_df <- as.vector(nrow(DB_BE_df))                                              # Count number of rows in DB_BE_df
    class(Cluster_Variable)
    #class(DB_BE_df$Cluster_Variable_Name)                                                  # Check the data class of a vector
    levels_Cluster_Variable <- levels(as.factor(Cluster_Variable))
    #levels_Cluster_Variable <- levels(as.factor(DB_BE_df$Cluster_Variable_Name))           # listet die Auspraegungen von factor vectoren auf
    
    # Gewichtung (A) und Auswuerfeln (B) fuer jede Auspraegung von Cluster_Variable
    for (i in levels_Cluster_Variable){
      t <- as.data.frame(table(Predictor_Variable[Cluster_Variable==i]))                    # schreibt Auspraegungen und ihre Haeufigkeiten in df
      Nrow_t <- nrow(t)                                                                     # ermittelt die Anzahl der zeilen in t
      
      
      # A: Gewichtung der Haeufigkeit der Auspraegungen der Predictor_Variable in der Teilmenge Cluster_Variable==i, ueber normierte HRF_vector
      for(a in 1:Nrow_t){                                                                                                             # For-Schleife ueber alle Zeilen von t und damit ueber alle Auspraegungen(levels)
        Predictor_Variable_Cluster_Variable_i <- Predictor_Variable[Cluster_Variable==i]                                              # Predictor_Variableen in der Teilmenge Cluster_Variable == i
        HRF_vector_Cluster_Variable_i <- HRF_vector[Cluster_Variable==i]                                                              # HRF_vector (Hochrechnungsfaktor der Breitenerhebung) zur Teilmenge Cluster_Variable == i
        Predictor_Variable_Cluster_Variable_i_a <- Predictor_Variable_Cluster_Variable_i[Predictor_Variable_Cluster_Variable_i==a]    # Predictor_Variableen a in der Teilmenge Cluster_Variable == i
        HRF_vector_Cluster_Variable_i_a <- HRF_vector_Cluster_Variable_i[Predictor_Variable_Cluster_Variable_i==a]                    # HRF_vector der Predictor_Variableen a in der Teilmenge Cluster_Variable == i
        #Sum_norm_HRF_vector_Cluster_Variable_i_a <- sum(HRF_vector_Cluster_Variable_i_a)/Sum_HRF_vector_BE
        Sum_HRF_vector_Cluster_Variable_i_a <- sum(HRF_vector_Cluster_Variable_i_a)                                                   # Fuer Freq_t kann die Normierung eigentlich auch wegfallen, da es um das Ziehungsverhaeltniss geht
        t$Freq[t$Var1==a] <- t$Freq[t$Var1==a]*Sum_HRF_vector_Cluster_Variable_i_a/Sum_HRF_vector_BE                                  #Normierung auf NWG Anzahl
      }
      
      Freq_t <- t[ ,2]                                                                      # Haeufigkeitenfektor fuer sample probability (prob)
      levels_Predictor_Variable__Cluster_Variable_i <- levels(as.factor(Predictor_Variable[Cluster_Variable==i]))
      #u <- sample(levels_Predictor_Variable__Cluster_Variable_i, size = 1, replace = TRUE, prob = Freq_t) # Test-Wuerfel
      #DB_BE_df$Predictor_Variable[DB_BE_df$Predictor_Variable==(-7) & DB_BE_df$Cluster_Variable==i] <- sample(levels_Predictor_Variable__Cluster_Variable_i, size = 1, replace = TRUE, prob = Freq_t)
      
      
      # B: Gewichtetes Sample zur Auswuerfeln der Predictor_Variable im Fall, dass Predictor_Variable==-7 und Cluster_Variable=i
      # Schnellerer Part, funktioniert ueber Funktion nicht
      # for (j in 1:Nrow_DB_BE_df) {                                              # Prueft jede Zeile in DB_BE_df ob DB_BE_df$Predictor_Variable[j]==(-7) & DB_BE_df$Cluster_Variable[j]==i
      #   if(DB_BE_df$Predictor_Variable_Name[j]==(-7) & DB_BE_df$Cluster_Variable[j]==i){
      #     DB_BE_df$Predictor_Variable_Name[j] <- sample(levels_Predictor_Variable__Cluster_Variable_i, size = 1, replace = TRUE, prob = Freq_t) # Wenn bedingung (if) TRUE wird fuer die Zeile(j) und die Cluster_Variable(i) ein Sample gezogen
      #   }
      
      
        for (j in 1:Nrow_DB_BE_df) {                                                                                                              # Prueft jede Zeile in DB_BE_df ob DB_BE_df$Predictor_Variable[j]==(-7) & DB_BE_df$Cluster_Variable[j]==i
          if(DB_BE_df[j,Predictor_Variable_Name]==(-7) & DB_BE_df[j,Cluster_Variable]==i){
            DB_BE_df[j,Predictor_Variable_Name] <- sample(levels_Predictor_Variable__Cluster_Variable_i, size = 1, replace = TRUE, prob = Freq_t) # Wenn bedingung (if) TRUE wird fuer die Zeile(j) und die Cluster_Variable(i) ein Sample gezogen
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
  df.with.certain.value <- df==cells.input                                           # Prueft ob value in Variable vorliegt
  df.with.certain.value <- as.data.frame(df.with.certain.value)                      # Wandelt die Boolean Matrix in Dataframe um
  
  df.with.certain.value.Summary <- summarise_each(df.with.certain.value, funs(mean)) # Bildet Mittelwerte ueber Boolean-Variablen
  
  n_cols_df <- ncol(df)                                                              # Zaehlt Spaltenanzahl in Dataframe
  
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
#Ausfuehren der definierten Funktion der Bewertung der relativen Wichtigkeit der Variablen
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
      width=1200, height=600)                 #Speichert Grafik in der angegebenen Aufloesung
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


#_______________________________________________________
### Prepares Parallel Computing Environment for CARET model training ####
# Sets the number of processors (All - 1) for training. 
# Needs to be run befor caret training. # Initializing of Function for parallel training: amp_up_models()
# Further the caret packege needs to have parallelization enabled in the control sequence: "allowParallel = TRUE,"
amp_up_models <- function(){
  library(parallel)
  library(doParallel)
  no_cores <- parallel::detectCores() - 1
  #Leave one core available for Operating system
  cluster <- makePSOCKcluster(no_cores)
  registerDoParallel(cluster)
  cat("Model amped and ready to go with:", no_cores, "cores. \n")
}
###




#_______________________________________________________
### Removes outliers of measured and calculated energy based on ratio, distribution and set percentile ####
ratio.based.outlier.removal <- 
function(dataset, 
measurement, 
calculation, 
upper_outlier_percentile){
# dataset: Dataframe of the dataset that is to be cleaned of outliers (removal of outlier cases)
# measurement: Vector of measured value of that determines the reality
# calculation: Vector of calculated value that generally tries to predict the measured value 
# upper_outlier_percentile: Percentile that defines the value above which the matching m_to_c cases are removed e.g. 0.98 for the 98 percentile

m_to_c <- as.double(measurement) / as.double(calculation) # define ratio

# Number of objects with measured consumption greater than the calculated demand
number_m_to_c_above <- length(m_to_c[m_to_c > 1])
# Number of objects with measured consumption smaller than the calculated demand
number_m_to_c_below <- length(m_to_c[m_to_c < 1])

# Determination of the upper Faktor based on the upper_outlier_percentile
Faktor <- sort(m_to_c)[upper_outlier_percentile * length(m_to_c)] # by representing value

# Determination of the lower Faktor based on the upper "Faktor" based on the ratios of the sample above and below 1
lowerFaktor <- (Faktor / number_m_to_c_above) * number_m_to_c_below

EinsdurchFaktor <- 1 / lowerFaktor

dataset$m_to_c <- m_to_c

    # Generate subset of outliers for further analysis
    dataset_outliers <- subset(dataset, m_to_c > Faktor | m_to_c < EinsdurchFaktor) # Generate subset of removed outliers
    dataset_outliers$uk_geb_BE
    dataset_outliers$m_to_c

    Comparison <- as.data.frame(dataset_outliers$GebäudeID)
    Comparison$uk_geb_BE <- dataset_outliers$uk_geb_BE
    # Comparison$hk_geb_BE <- dataset_outliers$hk_geb_BE
    Comparison$m_to_c <- dataset_outliers$m_to_c
    Comparison$w_erz_art_et <- dataset_outliers$w_erz_art_et
    Comparison # Summary of Outlier Values

print("The plot shows the densitiy of the ratio between the measuerd values (M) and the calculated values (C) -> m_to_c.")
ggplot(dataset, aes(x = m_to_c)) +
    geom_density()

# mean(m_to_c)

dataset <- subset(dataset, m_to_c < Faktor) # Option for upper and lower unrealistic value removal above
m_to_c <- m_to_c[m_to_c < Faktor]
dataset <- subset(dataset, m_to_c > EinsdurchFaktor)
m_to_c <- m_to_c[m_to_c > EinsdurchFaktor]

print("The following cases have been removed from the dataset as outliers:")
print(Comparison)

return(dataset)
}
###




#_______________________________________________________
### Function for save hk_geb based calibration factors (ratio measured and calculated energy) with according hk_geb in dataframe ####
simple.hk_geb.calibration <-
function(hk_geb_BEs,
      en_cons,
      en_dem,
      hk_geb_BE,
      HRF) {
  #..............................................................................................................
  # Function for save factors with according hk_geb in dataframe.... 
  # replace [1,] by i running the length of hk_geb_BEs.... 
  #..............................................................................................................
  
  #..............................................................................................................
  #......................
  # Variable Definition:
  #......................
  # hk_geb: Vector listing the existing hk_geb of the buildings... This is the categorising variables seperating the 
  #         sample in subsets according to hk_geb and determinging the correction factors.
  # The following must have the same length, e.g. belonging from one dataframe
  # en_cons: vector providing the measured energy consumption
  # en_dem: vector providing the calculated energy demand
  # hk_geb_BE: vector providing the hk_geb_BE class (clustering variable)
  # HRF: vector providing the weighting factor (HRF) representing the representation of the case in the stock
  #..............................................................................................................

  length_hk_geb <- length(hk_geb_BEs)
  simple_calibration_factors_hk_geb  <- data.frame(simple_cal_factor = (1:length_hk_geb))
  simple_calibration_factors_hk_geb$hk_geb <- hk_geb_BEs
  i = 0
  for (hk_geb_BE_ in hk_geb_BEs){
    i = i + 1
                    simple_calibration_factors_hk_geb$hk_geb[i] <- as.vector(hk_geb_BE_)
                    simple_calibration_factors_hk_geb$simple_cal_factor[i] <- 
                      as.double(sum(en_cons[hk_geb_BE==hk_geb_BE_]*HRF[dt$hk_geb_BE==hk_geb_BE_]) / 
                      sum(en_dem[hk_geb_BE==hk_geb_BE_]*HRF[hk_geb_BE==hk_geb_BE_]))
  }
  print(simple_calibration_factors_hk_geb)
  return(simple_calibration_factors_hk_geb)
}
###



#_______________________________________________________
### Function for save hk_geb_agg based calibration factors (ratio measured and calculated energy) with according hk_geb_agg in dataframe ####
simple.hk_geb_agg.calibration <-
function(hk_geb_aggs,
      en_cons,
      en_dem,
      hk_geb_agg,
      HRF) {
  #..............................................................................................................
  # Function for save factors with according hk_geb_agg in dataframe.... 
  # replace [1,] by i running the length of hk_geb_aggs.... 
  #..............................................................................................................
  
  #..............................................................................................................
  #......................
  # Variable Definition:
  #......................
  # hk_geb_agg: Vector listing the existing hk_geb of the buildings... This is the categorising variables seperating the 
  #         sample in subsets according to hk_geb and determinging the correction factors.
  # The following must have the same length, e.g. belonging from one dataframe
  # en_cons: vector providing the measured energy consumption
  # en_dem: vector providing the calculated energy demand
  # hk_geb_agg: vector providing the hk_geb_agg class (clustering variable)
  # HRF: vector providing the weighting factor (HRF) representing the representation of the case in the stock
  #..............................................................................................................

  length_hk_geb_agg <- length(hk_geb_aggs)
  simple_calibration_factors_hk_geb_agg  <- data.frame(simple_cal_factor = (1:length_hk_geb_agg))
  simple_calibration_factors_hk_geb_agg$hk_geb_agg <- hk_geb_aggs
  i = 0
  for (hk_geb_agg_ in hk_geb_aggs){
    i = i + 1
                    simple_calibration_factors_hk_geb_agg$hk_geb_agg[i] <- as.vector(hk_geb_agg_)
                    simple_calibration_factors_hk_geb_agg$simple_cal_factor[i] <- 
                      as.double(sum(en_cons[hk_geb_agg==hk_geb_agg_]*HRF[dt$hk_geb_agg==hk_geb_agg_]) / 
                      sum(en_dem[hk_geb_agg==hk_geb_agg_]*HRF[hk_geb_agg==hk_geb_agg_]))
  }
  print(simple_calibration_factors_hk_geb_agg)
  return(simple_calibration_factors_hk_geb_agg)
}
###



#_______________________________________________________
### Function for the extrapolation of full set (no missing) numerical values of the 
#   imuptated ENOB:DataNWG interview data set to the entire German non-domestic building stock
#   (Mean, without error calculation) ####
extrapolation.DE.quantitiy.energy <-
      function(variable,
               variable.name,
               extrapol.factor,
               subset.upscaling.factor) {
        #..............................................................................................................
        # Function for the extrapolation of full set (no missing) numerical values of the
        # imuptated ENOB:DataNWG interview data set to the entire German non-domestic building stock
        # (Mean, without deviation calculation)
        #..............................................................................................................
        
        #..............................................................................................................
        #......................
        # Variable Definition:
        #......................
        # variable: is the value that is to extrapolated onto the entire German Non-Domestic Building Stock
        # variable.name. Name of the variable
        # extrapol.factor: is the weighting/extrapolation factor. In case of ENBO:DataNWG the HRF
        # subset.upscaling.factor: Upscaling due to not considered (before removed) cases, such as the -555
        #..............................................................................................................
        
        # weighting variable vector
        variable.quantity.weighted = variable * extrapol.factor
        
        # sumation on stock and upscaling
        DE.variable.kWh <-
          sum(variable.quantity.weighted) * subset.upscaling.factor
        
        # unit transformation
        DE.variable.MWh <- DE.variable.kWh / 1000
        DE.variable.GWh <- DE.variable.MWh / 1000
        DE.variable.TWh <- DE.variable.GWh / 1000
        
        # Return the result
        print(paste("extrapolated ", variable.name, " in TWh/a:", DE.variable.TWh))
        return(DE.variable.TWh)
      }
###




#_______________________________________________________
### Determination of correlation between categorical variables and numerical variables via ANOVA analysis ####
      anova_auswertung <- function(dt, x, y, target_name) {
        args <- c(x, y)
        if (length(args) > 1) {
          f_str <- paste(target_name, " ~ ", paste(purrr::map(args, as_string), collapse = " * "), collapse = " ~ ") # generates anova formula arguments en_cons ~ combination of two variables (conected with *)
        } else if (length(args) == 1) {
          f_str <- paste(target_name, " ~ ", as.character(args), collapse = " ~ ") # in case of only one variable generate anova formula arguments en_cons ~ variable
        } else {
          stop("Fehler: Keine Variable genannt!")
        }
        f <- as.formula(f_str) # define f_str as formula
        anova <- aov(f, data = dt) # run anova for defined fomula with data dt
        s <- anova_summary(anova)
        df <- as.data.table(s)
        df <- df[p < 0.05, .(p)] # determination of significance via p-value (significance given if p < 0.05)
        if (x == y) { # in case of variables are the same ... do not consider as significant
          return(NULL)
        }
        if (nrow(df) > 0) { # for significant veriables and combinations of variables determine eta_squared
          df <- effectsize::eta_squared(anova) %>% as.data.table() # eta_squared -> correlation coefficient between metric and categorical variable
          df[, formel := f_str]

          return(df[, 1:6]) # return data frame including significant variable combinations
        }
        # skript for identifying error causing variables (not right data type)
                # for(i in 1:length(c1)){
                # print(c1[i])
                # print(c2[i])
                # anova_auswertung(dt, c1[i], c2[i], target)
                # }
      }
###



#_______________________________________________________
###  Repetitive linear regression application for determination the significant variables ####
  significant.variables <- function(dt, target, target_vector, weights, name) {
    # dt - dataframe with target variable and predictor variables to be tested on their significance
    # target - target variable
    # target_vector
    # weights HRF must be set before
    # name under which results are saved in tex table and RData


    ### Klassisch linear, um relevante Spalten herauszufinden
    i <- 0
    form <- as.formula(paste(as.character(target), " ~ .", sep = "", collapse = ""))
    # sig <- c(0.999, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, rep(0.05,1000)) # Keeping Variables with significance P Value of 0.05
    sig <- c(0.999, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05, rep(0.01, 1000)) # Keeping Variables with significance P Value of 0.01
    # sig <- c(0.999, rep(0.05,1000))

    # 0.999 f(aeoeuesss)r NA-Koeffizienten elimination am Anfang (siehe in der Schleife);
    # Danach wird in 0.1 er schritten die "signifikanz"erh(aeoeuesss)ht. Das hat zur Folge,
    # dass erst einmal nur die wirklich nicht significanten varibalen raus fliegen
    # Es erfolgt ein "aussieben" von nicht signifikaten Variblen mit stetiger Erh(aeoeuesss)hung des
    # p-Value niveaus um am Ende nur noch die wirklich signifikanten Variablen zu bekommen.
    # Dieses vorgehen verhindert, dass einige Variablen nicht direkt am Anfang rausfliegen
    # obwohl sie bei Betrachtung im kleineren/signifikanteren Pool eine h(aeoeuesss)here Signifikanz aufweisen.
    # Ziel ist es die Anzahl der ber(aeoeuesss)cksichtigten Variablen auf die Notwendigen zu reduzieren...
    # was zum einen eine einfache Anwendung des sp(aeoeuesss)teren Calibrierungsmodells zul(aeoeuesss)sst und
    # auch den Rechenaufwand reduziert
    while (1 == 1) {
      lm_2 <- lm(
        data = dt,
        form,
        weights = weights
      )
      a <- data.frame(names(lm_2$coefficients))
      row.names(a) <- a[, 1]
      b <- data.frame(summary(lm_2)$coefficients)
      b <- data.frame(b[, 4])
      row.names(b) <- row.names(summary(lm_2)$coefficients)
      d <- merge(a, b, by = "row.names", all = TRUE)
      d <- d[, c(2, 3)]
      colnames(d) <- c("Spalte", "PWert")
      # Backticks entfernen
      d$Spalte <- gsub("\`", "", d$Spalte, perl = TRUE)

      d[which(d$Spalte == "(Intercept)"), ]$Spalte <- target # heist nur in d so, f(aeoeuesss)r Reihenfolgenbeibehaltung!, da beides an erster Stelle steht (en_cons wird nicht mit intercept gleich gesetzt. d.h. die n(aeoeuesss)chsten lm() l(aeoeuesss)ufe sind unbeeintr(aeoeuesss)chtigt)
      # Pivotisieren
      d <- d %>% pivot_wider(names_from = Spalte, values_from = PWert)
      # Spaltenreihenfolge harmonisieren
      d <- d[names(dt)] # namen von dt m(aeoeuesss)ssen zu d passen
      # Entpivotisieren
      d <- d %>% pivot_longer(cols = everything(), names_to = "Spalte", values_to = "PWert")

      d$PWert <- as.double(d$PWert)
      d$PWert <- ifelse(is.na(d$PWert), 1, d$PWert) # replace NA with 1

      # Gr(aeoeuesss)(aeoeuesss)ten Wert bei Signifikanz bestimmen
      i <- i + 1
      m_index <- which(d$PWert > sig[i]) # im ersten Durchgang sig > 0.999 -> removes NAs; im zweiten Durchgang wird bei 0.9 gesiebt... siehe sig

      v <- rep(TRUE, length(d$PWert))
      v[m_index] <- FALSE
      v[1] <- TRUE # intercept immer beibehalten. K(aeoeuesss)nnte FALSE sein, wenn intercept rausfliegt.

      anz_col_vorher <- dim(dt)[2]
      dt <- dt[, v, with = FALSE]
      # add target varible in case it was eliminated in this process
      if (target == "delta") {
        # dt$delta_b_v <- target_vector
        dt$delta <- target_vector
      }
      if (target == "ratio") {
        # dt$delta_b_v <- target_vector
        dt$ratio <- target_vector
      }
      if (target == "delta_b_v_percentage") {
        dt$delta_b_v_percentage <- target_vector
      }
      if (target == "en_cons") {
        dt$en_cons <- target_vector
      }
      anz_col_nachher <- dim(dt)[2]

      print(paste("Entferne ", length(m_index), " Variablen. Runde:", i))

      if (anz_col_vorher == anz_col_nachher) {
        print(paste("Variablen auf Signifikanz geprueft. Runden:", i))
        break # for Schleife verlassen
      }
    }



    Relevant_Variables <- summary(lm_2)
    print(summary(lm_2))

    path <- getwd()
    saveRDS(Relevant_Variables, file = paste(path, "//", name, "_Relevant_Variables", ".RData", sep = ""))

    library(xtable)
    print(xtable(Relevant_Variables, type = "latex"), file = paste(path, "//", name, "_Relevant_Variables", ".tex", sep = ""))


    # RMSE
    sqrt(mean(lm_2$residuals^2))
    # Plot
    dt$results <- lm_2$fitted.values
    ggplot(data = dt, aes(x = results, y = en_cons)) +
      geom_point() +
      geom_function(fun = function(x) x)
    dt$results <- NULL

    return(dt)
  }
###

#_______________________________________________________
###  Defininig stuetzstellen - correction factor values - based on distribtion ####
  def.stuetzstellen <- function(df, target, GitHub_Data_Access) {
    # df - Dataframe with variables for later training in caret, that is to be extended by "klassen" (classes) with the stuetzstellen (gemoetric average correction factors for each class)
    # target - target value that is the basis for the distribution and the generation of the stuetzstellen

    df$ziel <- target # target is added to df
    # df$en_cons <- NULL

    # ggplot(df,aes(y = ziel)) + geom_boxplot()
    ggplot(df, aes(x = ziel)) +
      geom_density()

    q <- quantile(df$ziel, seq(from = 0.1, to = 0.9, by = 0.1))
    # q <- quantile(df$ziel, seq(from = 0.05, to = 0.95, by = 0.05))
    q
    mi <- min(df$ziel)
    ma <- max(df$ziel)

    df$klasse <- 0
    df$klasse <- ifelse(df$ziel <= q[1], 1, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[2] & df$klasse == 0, 2, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[3] & df$klasse == 0, 3, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[4] & df$klasse == 0, 4, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[5] & df$klasse == 0, 5, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[6] & df$klasse == 0, 6, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[7] & df$klasse == 0, 7, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[8] & df$klasse == 0, 8, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[9] & df$klasse == 0, 9, df$klasse)
    df$klasse <- ifelse(df$ziel > q[9] & df$klasse == 0, 10, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[10] & df$klasse == 0, 10, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[11] & df$klasse == 0, 11, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[12] & df$klasse == 0, 12, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[13] & df$klasse == 0, 13, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[14] & df$klasse == 0, 14, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[15] & df$klasse == 0, 15, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[16] & df$klasse == 0, 16, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[17] & df$klasse == 0, 17, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[18] & df$klasse == 0, 18, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[19] & df$klasse == 0, 19, df$klasse)
    # df$klasse <- ifelse(df$ziel > q[19] & df$klasse == 0, 20, df$klasse)

    df$klasse <- as.factor(df$klasse)
    df$ziel <- NULL

    q <- c(mi, q, ma)
    stuetzstellen <- q[1:10] + diff(q) / 2
    # stuetzstellen <- q[1:20] + diff(q)/2

    # saveRDS(stuetzstellen, file="E:/GitHub_Data_Access/stuetzstellen_of_calibration_models.RData")
    saveRDS(stuetzstellen, file = paste(GitHub_Data_Access, "stuetzstellen_of_calibration_models", ".RData", sep = ""))

    # to read again: readRDS("E:/GitHub_Data_Access/stuetzstellen_of_calibration_models.RData")

    # #print plot DEVIATION V/B
    # print_path <- paste("Deviation_Measurments_to_Calculations", ".pdf" ,sep = "") #PDF
    # dev.print(pdf, print_path)
    # print_path <- paste("Deviation_Measurments_to_Calculations", ".png" ,sep = "") #png
    # dev.print(png, file=print_path, width=1600, height=900)

    print("Stuetzstellen")
    print(stuetzstellen)
    return(stuetzstellen)
  }
  ###


#_______________________________________________________
###  Defininig stuetzstellen klassen - classes of correction factor values - based on distribtion ####
  def.stuetzstellen.klassen <- function(df, target) {
    # df - Dataframe with variables for later training in caret, that is to be extended by "klassen" (classes) with the stuetzstellen (gemoetric average correction factors for each class)
    # target - target value that is the basis for the distribution and the generation of the stuetzstellen

    df$ziel <- target # target is added to df
    # df$en_cons <- NULL

    # ggplot(df,aes(y = ziel)) + geom_boxplot()
    ggplot(df, aes(x = ziel)) +
      geom_density()

    q <- quantile(df$ziel, seq(from = 0.1, to = 0.9, by = 0.1))
    # q <- quantile(df$ziel, seq(from = 0.05, to = 0.95, by = 0.05))
    q
    mi <- min(df$ziel)
    ma <- max(df$ziel)

    df$klasse <- 0
    df$klasse <- ifelse(df$ziel <= q[1], 1, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[2] & df$klasse == 0, 2, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[3] & df$klasse == 0, 3, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[4] & df$klasse == 0, 4, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[5] & df$klasse == 0, 5, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[6] & df$klasse == 0, 6, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[7] & df$klasse == 0, 7, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[8] & df$klasse == 0, 8, df$klasse)
    df$klasse <- ifelse(df$ziel <= q[9] & df$klasse == 0, 9, df$klasse)
    df$klasse <- ifelse(df$ziel > q[9] & df$klasse == 0, 10, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[10] & df$klasse == 0, 10, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[11] & df$klasse == 0, 11, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[12] & df$klasse == 0, 12, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[13] & df$klasse == 0, 13, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[14] & df$klasse == 0, 14, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[15] & df$klasse == 0, 15, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[16] & df$klasse == 0, 16, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[17] & df$klasse == 0, 17, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[18] & df$klasse == 0, 18, df$klasse)
    # df$klasse <- ifelse(df$ziel <= q[19] & df$klasse == 0, 19, df$klasse)
    # df$klasse <- ifelse(df$ziel > q[19] & df$klasse == 0, 20, df$klasse)

    df$klasse <- as.factor(df$klasse)
    df$ziel <- NULL

    q <- c(mi, q, ma)
    stuetzstellen <- q[1:10] + diff(q) / 2
    # stuetzstellen <- q[1:20] + diff(q)/2

    # saveRDS(stuetzstellen, file="E:/GitHub_Data_Access/stuetzstellen_of_calibration_models.RData")
    # to read again: readRDS("E:/GitHub_Data_Access/stuetzstellen_of_calibration_models.RData")

    print("Classes added to df")

    return(df)
  }
###


#_______________________________________________________
### Row dublication in data frame based on row weights to be applied for later weighting e.g. of machine learning model training####
  row.weight.dublication <- function(weights, df) {
    # weights - Weights of the cases (rows), HRF in the example of DataNWG
    # df - dataframe with the rows that are to be dublicated according to the weights

    # library(splitstackshape)
    weights_standardized <- weights / min(weights) # to reduce the number of rows and calculation time
    df <- expandRows(df, count = weights_standardized, count.is.col = FALSE)
    # weights are equal to HRF (not in df, so that not considered in models)
    # print("cases dublicated accoring to weights")
    return(df)
  }
###




#_______________________________________________________
### Caret batch k-fold cross-validation training and comparative plotting ####
  caret.batch.training <- function(Problem_Type, caret_models, df, output, ctrl, met, test_name, specific_demand, specific_consumption, EBF, df_prediction, stuetzstellen) {
    # VARIBALES
    # Problem_Type -  Defines the typo of machine learning problem: Either Regression OR Classification
    # caret_models -  List of models that are to be trained and compared - Vector with model names; https://rdrr.io/cran/caret/man/models.html
    # df -            Dataframe with data for training. Must include all input and output data. For applying weights, the cases are to be dublicated accordingly
    # output -        Output variable of models - Variable in df
    # ctrl -          control sequence defining how caret is to train and test the models
    #                 e.g. ctrl <- trainControl(method = "repeatedcv", allowParallel = TRUE,  repeats = 5, number = 10, savePredictions = "final")
    # met -           Defined metric for comparing the models by. Suggestion: Regression Models: "Rsquared", Classification Models: "Accuracy"
    # test_name -     Name of the training and comparison test. Under this name all the outputs are saved into the working directory
    # specific_demand - Specific demand of initial data for plot of uncalibrated data: df$en_dem/df$EnergyRefArea_DIBS_BE
    # specific_consumption - Specific measured consumption of initial data for plot of uncalibrated data: dt$en_cons/dt$EnergyRefArea_DIBS_BE
    # EBF -           Energy relevant area: dt$EnergyRefArea_DIBS_BE
    # df_prediction - Dataframe for prediction the measured consumption (is df, without dublications according to weights)
    # stuetzstellen - Classification class correction factors for prediction and validataion plots



    # PACKAGES REQUIERED
    # library(caret)


    # Implement empty lists and the running variables
    i <- length(caret_models)
    j <- 0
    model_overview <- list()

    for (model in caret_models) {
      set.seed(5) # allow reproducability of results
      j <- j + 1

      # measure time of model training (START)
      start_time <- Sys.time()


      form <- as.formula(paste(as.character(output), " ~ .", sep = "", collapse = ""))
      # train model
      results <-
        caret::train(
          form,
          data = df,
          trControl = ctrl,
          method =  paste(as.character(model), collapse = ""),
          # metric = "Accuracy"
          metric = met
        )

      # measure time of model training (END)
      end_time <- Sys.time()
      modelling_time <- end_time - start_time
      time_unit <- units(modelling_time)

      saveRDS(results, paste(model, "__", test_name, ".rds", sep = "", collapse = )) # added: 2023.03.23: "__", test_name,
      # load the trained model elswhere use:
      # my_model <- readRDS("model.rds")

      print(paste("trained model", as.character(model), "in [", time_unit, "]:", modelling_time, collapse = ))

      # transfer the results of this loops go through to list named after the model
      assign(paste(as.character(model)), results)

      # summarize the different models in one list to handle results later
      model_overview[[length(model_overview) + 1]] <- get(model)
    }

    # name models in model_overview list, so that they show correctly in plots etc.
    i <- 0
    for (model in caret_models) {
      i <- i + 1
      names(model_overview)[i] <- model
    }

    # generate summary overview of results and sort according to Accuracy
    results <-
      resamples(model_overview)


    path <- getwd()

    # summarize the distributions
    print("Results:")
    print(summary(results))

    # Save summary results in text document
    sink(paste(path, "//", test_name, "Summary_Results", ".txt", sep = "")) # TESTING
    # sink("E:/BE_Reg_Verbrauch_Bedarf/H0_Heat/RF_Classification_TEST_Summary_Results.txt")
    print(summary(results))
    sink() # returns output to the console


    # give timings of models
    print("Timings:")
    print(results$timings)

    # Save modelling times in text document
    sink(paste(path, "//", test_name, "Modelling_Time_Summary", ".txt", sep = "")) # TESTING
    # sink("E:/BE_Reg_Verbrauch_Bedarf/H0_Heat/RF_Classification_TEST_Summary_Results.txt")
    print(results$timings)
    sink() # returns output to the console

    # ..........................................
    # comparison of model prediction via plots####
    # ..........................................
    par(mfrow = c(round(((length(caret_models) + 1) / 6) + 0.49), 6)) # Grafik ausma(aeoeuesss)e an Modelanzahl Anpassen # Potrait 5 cols

    # 1: Verbrauch zu Bedarf ohne Predict
    plot(specific_demand, specific_consumption, main = "DIBS demand", ylab = "Measurment", xlab = "DIBS", xlim = c(0, 600), ylim = c(0, 600)) # Verbrauch zu Bedarf
    abline(0, 1, col = "blue")
    abline(0, 2, col = "green")
    abline(0, 0.5, col = "green")

    # 2: All tested models mit predict
    for (model in caret_models) {
      predict_output <- predict.train(get(model), df_prediction, se.fit = FALSE)
      if (Problem_Type == "Regression") {
        rounded_predict <- round(predict_output / EBF, digits = 4)
        if (target_name == "delta_b_v") {
          rounded_predict <- round((df$en_dem - predict_output) / EBF, digits = 4)
        }
        if (target_name == "delta_b_v_percentage") {
          rounded_predict <- round((df$en_dem - ((predict_output / 100) * df$en_dem)) / EBF, digits = 4)
        }
      } else {
        rounded_predict <- round(stuetzstellen[predict_output] * specific_demand, digits = 4)
      }
      plot(rounded_predict, specific_consumption, main = paste(as.character(model), collapse = ""), ylab = "Measurment", xlab = paste("DIBS cor. by ", as.character(model), collapse = " "), xlim = c(0, 600), ylim = c(0, 600)) # Verbrauch zu Kalibriertem-Bedarf
      abline(0, 1, col = "blue")
      abline(0, 2, col = "green")
      abline(0, 0.5, col = "green")
    }

    par(mfrow = c(1, 1)) # Grafikfenster zu einem zur(aeoeuesss)cksetzen

    # print plot
    print_path <- paste(test_name, "Plots_Summary", ".pdf", sep = "") # PDF
    dev.print(pdf, print_path)

    print_path <- paste(test_name, "Plots_Summary", ".png", sep = "") # png
    # dev.print(png, file=print_path, width=1600, height=900) # Horizontal
    dev.print(png, file = print_path, width = 900, height = 1600) # Potrait

    saveRDS(results, file = paste(path, "//", test_name, "Model_Training_RESULTS", ".RData", sep = ""))
    return(results)

      # #..........................................
      # # comparison of model prediction via individual plots####
      # #..........................................

      # # here the individual plots of the coparing plot above are indivdually ploted and saved!

      # # 1: Verbrauch zu Bedarf ohne Predict
      # plot(specific_demand, specific_consumption, main = "DIBS demand", ylab = "Measurment", xlab = "DIBS", xlim=c(0,600), ylim=c(0,600)) # Verbrauch zu Bedarf
      # abline(0,1,col="blue")
      # abline(0,2,col="green")
      # abline(0,0.5,col="green")
      # #print plot
      # print_path <- paste(test_name, "DIBS_", "Plot", ".pdf" ,sep = "") #PDF
      # dev.print(pdf, print_path)
      #   print_path <- paste(test_name, "DIBS_", ".png" ,sep = "") #png
      # dev.print(png, file=print_path, width=900, height=900)


      # # 2: All tested models mit predict
      # for(model in caret_models){
      #   predict_output <- predict.train(get(model), df_prediction, se.fit = FALSE)
      #   if (Problem_Type == "Regression") {
      #     rounded_predict <- round(predict_output/EBF, digits = 4)
      #     if(target_name == "delta_b_v"){
      #       rounded_predict <- round((df$en_dem - predict_output)/EBF, digits = 4)
      #     }
      #     if(target_name == "delta_b_v_percentage"){
      #       rounded_predict <- round((df$en_dem - ((predict_output/100)*df$en_dem))/EBF, digits = 4)
      #     }
      #   } else {
      #     rounded_predict <- round(stuetzstellen[predict_output] * specific_demand, digits = 4)
      #   }
      #   plot(rounded_predict, specific_consumption, main = paste(as.character(model), collapse = ""), ylab = "Measurment", xlab = paste("DIBS cor. by ", as.character(model), collapse = " "), xlim=c(0,600), ylim=c(0,600)) # Verbrauch zu Kalibriertem-Bedarf
      #   abline(0,1,col="blue")
      #   abline(0,2,col="green")
      #   abline(0,0.5,col="green")
      #   #print plot
      #   print_path <- paste(test_name, model, "_", "Plot", ".pdf" ,sep = "") #PDF
      #   dev.print(pdf, print_path)
      #   print_path <- paste(test_name, model, "_", "Plot", ".png" ,sep = "") #png
      #   dev.print(png, file=print_path, width=900, height=900) # Potrait
      # }
  }
###