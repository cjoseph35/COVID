library(rmarkdown)
library(taskscheduleR)
library(git2r)

Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
setwd("C:\\Users\\CONNO\\OneDrive\\Documents\\GitHub\\COVID")
rmarkdown::render_site(encoding = 'UTF-8')

## GIT ##

# Add the file

add(repo = getwd(), path = "Map.html")
add(repo = getwd(), path = "index.html")
add(repo = getwd(), path = "index.Rmd")
add(repo = getwd(), path = "Map.Rmd")
add(repo = getwd(), path = "taskschedule.log")
add(repo = getwd(), path = "USCOVID_TimeSeries.xlsx")
add(repo = getwd(), path = "Downstate.gif")
add(repo = getwd(), path = "DownstateLog.gif")
add(repo = getwd(), path = "DSonly.gif")
add(repo = getwd(), path = "DSonlylog.gif")
add(repo = getwd(), path = "about.html")
add(repo = getwd(), path = "fptable.html")
add(repo = getwd(), path = "taskschedule.R")
add(repo = getwd(), path = "COVID.Rproj")
add(repo = getwd(), path = "OpenAndClose.vbs")
add(repo = getwd(), path = "_site.yml")
add(repo = getwd(), path = "_site.yml")
add(repo = getwd(), path = "_config.yml")
# Commit the file
commit(repo = getwd(), message = paste0("Update as at: ", Sys.time(), "%Y-%m-%d %H:%M:%S"))

# Push to the Repo
push(object = getwd(), 
     credentials = cred_user_pass(username = "cjoseph35",
                                  password = "Sammydoo02"))
