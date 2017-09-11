data(package="datasets") 
?AirPassengers
data(package=.packages(all.available=TRUE))

install.packages(arules)                   
library(arules)
data(Groceries)

install.packages(RODBC)                   
library(RODBC)
odbcDataSources()
conn=odbcConnect("SQLServer ",uid="forR",pwd="123456") 

setwd  ( "D://R//DATA" )
write.csv ( Insurance, " Insurance.csv" )
Insur_csv = read.csv ( " Insurance.csv" ) 
head ( Insur_csv ) 
Insur_csv 1 = read.table ( " Insurance.csv" ) 
head ( Insur_csv 1 ) 
Insur_csv 2 = read.table ( "Insurance.csv", header=TRUE, sep="," )
head ( Insur_csv 2 )  
write.table ( Insurance, "Insurance.txt" )
Insur_txt = read.table ( "Insurance.txt" )
head(Insur_txt)
Insur_txt1 = read.csv ( "Insurance.txt", header=TRUE, sep="" ) 
head ( Insur_txt1 )  
library ( RODBC )
channel = odbcConnectExcel ( file.choose() )     
channel    
sqlTables ( channel )
Insur = sqlFetch ( channel, "Sheet1" )
odbcClose ( channel )  
head( Insur )  
xlsfile = file.path ( path.package('gdata'),'xls','iris.xls' )
iris = read.xls ( xlsfile )
library ( foreign )  
DRINK_spss = read.spss ( file="DRINK.sav" )   
DRINK_spss 
DRINK_spss1 = read.spss ( file="DRINK.sav", to.data.frame=TRUE ) 
head ( DRINK_spss1 )  
library ( Hmisc ) 
DRINK_spss2 = spss.get ( "DRINK.sav" ) 
head ( DRINK_spss2 )

u1="http://stockdata.stock.hexun.com/2008en/zxcwzb.aspx?stockid=000002&type=1&date=2013.06.30"
tables1 = readHTMLTable(u1)
names(tables1)
tables1[[1]]
tables1[[2]]
tables1[[3]]
tables1[[4]]







