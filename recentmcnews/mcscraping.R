#install.packages('rvest')
#install.packages('stringr')
#install.packages('dplyr')
#install.packages('lubridate')
#install.packages('readr')
#install.packages("xlsx")

# Load packages
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)
library(xlsx)

#Place the folder path where the excel file with urls is
excelname <- "D:/RProgramming/recentmcnews/mclinks.xlsx"

#Place the folder path where you want to save the results
linksfolder <- "D:/RProgramming/recentmcnews/"

#Getting today's date and converting it to character to verify the articles date
todaydate <- Sys.Date()
format(todaydate, format="%d %b %Y")
todaysdate <- as.character(todaydate)

#Get current time and convert to string to append to the html file
timenow <- Sys.time()
currenttime <- as.character(timenow)
currenttime <- str_replace_all(currenttime, "-", "")
currenttime <- str_replace_all(currenttime, " ", "")
currentdt <- str_replace_all(currenttime, ":", "")

filename = paste("mcnews",currentdt,".html",sep="")
file.create(paste(linksfolder,filename,sep=""))
#file.create(filename)
res <- read.xlsx(excelname, 1)
i <- 0
for(val in res[[1]]){
  i=i+1
  #print(res[[2]][[i]])
  print(val)
  # Read web page
  webpage <- read_html(val)
  
  # Extract records info
  results <- webpage %>% html_nodes(".PT3.a_10dgry")
  if(length(results)==0){
    print("Sorry! No Results for for the above page. Proceeding next!")
    next
  }
  #Get the first result
  textval <- html_text(results[1], trim = TRUE)
  
  #Get the date from the above and convert it to yyyy-mm-dd format to compare with today's date in character
  dateval <- substring(textval,11,21)
  datep = substring(dateval,1,2)
  monthp = substring(dateval,4,6)
  yearp = substring(dateval,8,11)
  
  if(monthp == "Jan"){
    monthp = "01"
  } else if(monthp == "Feb"){
    monthp = "02"
  } else if(monthp == "Mar"){
    monthp = "03"
  } else if(monthp == "Apr"){
    monthp = "04"
  } else if(monthp == "May"){
    monthp = "05"
  } else if(monthp == "Jun"){
    monthp = "06"
  } else if(monthp == "Jul"){
    monthp = "07"
  } else if(monthp == "Aug"){
    monthp = "08"
  } else if(monthp == "Sep"){
    monthp = "09"
  } else if(monthp == "Oct"){
    monthp = "10"
  } else if(monthp == "Nov"){
    monthp = "11"
  } else if(monthp == "Dec"){
    monthp = "12"
  }
  
  mcdate <- paste(yearp,monthp, datep, sep="-")
  
  #todaysdate <- "2018-08-03"
  #print(mcdate)
  #print(todaysdate)
  if(todaysdate == mcdate){
    
    linkstitles <- webpage %>% html_nodes(".FL.PR20")
    #Get the first result
    link <- xml_attrs(xml_child(linkstitles[[1]], 1))[["href"]]
    link <- paste("http://www.moneycontrol.com",link,sep="")
    #link <- val
    title <- xml_attrs(xml_child(linkstitles[[1]], 1))[["title"]]
    linktitle <- paste("<a href='",link,"'>",title,"</a><br>",sep="")
    #write(linktitle,file=filename,append=TRUE)
    linkdes <- webpage %>% html_nodes(".PT3") %>% html_text()
    linkdes <- paste(linkdes,"</br></br>",sep="")
    #print(linkdes[3])
    file=paste(linksfolder,filename,sep="")
    write(paste("<h3>",res[[2]][[i]],"</h3>",sep=""),file,append=TRUE)
    write(linktitle,file,append=TRUE)
    write(linkdes[3],file,append=TRUE)
  }
}