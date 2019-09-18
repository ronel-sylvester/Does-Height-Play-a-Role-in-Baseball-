
install.packages('rsconnect')
library(rsconnect)
rsconnect::setAccountInfo(name='ronel-sylvester',
                          token='B199A5BB021B09D3C1E7299393AE054C',
                          secret='NiVbZAxxxzRE6jOGk739TD4GJ2tIoT3KiJfi7Z+M')
rsconnect::deployApp('C:/Users/ronel/OneDrive/Documents/College/Level/Stattleship')
getwd()
setwd(dir)
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
options(PACKAGE_MAINFOLDER="C:/Users/ronel/OneDrive/Documents/College/Level/Stattleship")
