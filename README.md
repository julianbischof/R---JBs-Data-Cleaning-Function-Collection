# R---JBs-Data-Cleaning-Function-Collection
 Collection of R functions for data cleaning and analysis
 
 # Either download and source locally:
 source("D://PATH//JBs_R_Functions.R")
 
 # Or call form GitHub Repo:
 For example via the downloader R package
    library(downloader)
    
    SHA <- sha_url('https://raw.githubusercontent.com/julianbischof/R---JBs-Data-Cleaning-Function-Collection/main/JBs_R_Functions.R')
    SOURCE <- 'https://raw.githubusercontent.com/julianbischof/R---JBs-Data-Cleaning-Function-Collection/main/JBs_R_Functions.R'
    downloader::source_url(SOURCE, sha = SHA, prompt=FALSE)
    print('Downloaded and using JBs_R_Functions')

 # In case of url Errors it might be fixed using
 See https://stackoverflow.com/questions/7715723/sourcing-r-script-over-https

 # # Windows:
If Internet Explorer is configured to access the web using your organization's proxy, you can direct R to use these IE settings instead of the default R settings. This change can be made once by the following steps:

    Save your work and close all R sessions you may have open.

    Edit the following file. (Note: Your exact path will differ based on your R installation)

    C:\Program Files\R\R-2.15.2\etc\Rprofile.site

Open this "Rprofile.site" file in Notepad and add the following line on a new line at the end of the file:

    utils::setInternet2(TRUE)

You may now open a new R session and retry your "source" command.


# # Linux alikes:
Use G. Grothendieck's suggestion. At the command prompt within R type:
    source(pipe(paste("wget -O -", "https://github.com/enter/your/url/here.r")))

You may get an error saying:

    cannot verify certificate - - - - Self-signed certificate encountered.

At this point it is up to you to decide whether you trust the person issuing the self-signed certificate and proceed or to stop.

If you decide to proceed, you can connect insecurely as follows:

    source(pipe(paste("wget -O -", "https://github.com/enter/your/url.r", "--no-check-certificate")))

