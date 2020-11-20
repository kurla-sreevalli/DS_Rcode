library(rvest)
library(magrittr)
library(XML)
a<-10
rev<-NULL
url1<-"https://www.tripadvisor.in/Hotel_Review-g147399-d2354539-Reviews-or"
url2<-"-The_Venetian_on_Grace_Bay-Providenciales_Turks_and_Caicos.html#REVIEWS"
for(i in 0:8){
  url<-read_html(as.character(paste(url1,i*a,url2,sep="")))
  ping<-url %>%
    html_nodes(".partial_entry") %>%
    html_text() 
  rev<-c(rev,ping)
}
rev
write.table(rev,"travel.txt")
