#Use spell-checker algortihm to match the company in 2000 to Fortune
library("stringr")
library(XML)  #for parsing

setwd("C:\\Users\\HUANGJ\\Desktop\\items\\Women as ceos")

# #Reading lemmetization spell checker
# source("C://Users//HUANGJ//Desktop//items//Rewards research//Twitter analysis//spell_checker_universe function.R")
# 
# #read csv
# fortune = read.csv("fortune.csv",stringsAsFactors = FALSE)
# top2000 = read.csv("top2000.csv",stringsAsFactors = FALSE); top2000 = subset(top2000,top2000$Name!= "")
# 
# top2000_dat = top2000$Name
# 
# #For every fortune list, fuzzy match with a top2000 company
# for(i in 1:nrow(fortune)){
#   print(i)
#   # print(a$word[i])
#   fortune$fuzzy_matched_wrd[i] = mapply(correct, taxanomies_dat = "top2000_dat", word = fortune$Company[i])
# }
# 
# #For every fortune list, find if it's part of top2000 company
# for(i in 1:nrow(fortune)){
#   print(i)
#   # print(a$word[i])
#   if(length(grep(fortune$Company[i],top2000_dat)) >0){
#     fortune$grep_word[i] = top2000$Name[grep(fortune$Company[i],top2000_dat)]    
#   }else{
#     fortune$grep_word[i] = ""
#   }
# }
# 
# #Matched words
# fortune$matched_wrd = ""
# fortune$matched_wrd = ifelse(fortune$fuzzy_matched_wrd != "" | fortune$grep_word == "",fortune$fuzzy_matched_wrd,fortune$grep_word)
                             
                      
###################################Scraping direclty from website########################
#functions in the main function
locate_start = function(x,pat){
  end = str_locate_all(pattern = pat,x)[[1]][1,1]
  return (end)
}

locate_end = function(x,pat){
  end = str_locate_all(pattern = pat,x)[[1]][1,2]
  return (end)
}

# html = readLines("http://fortune.com/fortune500/exxon-mobil/")
# a = as.data.frame(html)
# a$html = as.character(a$html)
# 
# grep('company-info-card-Industry.1.0">',a$html)
# 
# # company-info-card-Sector.1.0">
# locate_end(a$html[12],'company-info-card-Industry.1.0">')
# locate_end(a$html[12],'company-info-card-HQ Location">')
# 
# #Extract skills
# freelance$skillstart = mapply(locate_end,x = freelance$html, pat = "\"skills\":")
# freelance$skillend = mapply(locate_start,x = freelance$html, pat = "\"duration")
# 
# freelance$skill = substring(freelance$html, first = freelance$skillstart, last = freelance$skillend)  



# html = htmlParse(html,useInternalNodes = T)
# data_display = getNodeSet(html,"//script[@type = 'text/javascript']")  #<script type="text/javascript">
# data_display = getNodeSet(html,"//p[@class= 'remove-bottom-margin' data-reactid='.mf7plivjzu.1.0.5.1:1.4.0.0.0.0.1.0.$company-info-card-Industry.1.0']")  #<script type="text/javascript">
# 
fortune1000 = read.csv("fortune1000.csv",stringsAsFactors = FALSE)
# fortune1000$name = fortune1000$COMPANY
# fortune1000$name = as.character(fortune1000$name)
# fortune1000$name = tolower(fortune1000$name)
# # fortune1000$name = gsub(".", "dotttttt", fortune1000$name, perl=T)
# fortune1000$name = gsub("([^-][[:punct:]])", "", fortune1000$name, perl=T)

# fortune1000$link = paste
fortune1000$lastchar = substring(fortune1000$COMPANY, first = nchar(fortune1000$COMPANY), last = nchar(fortune1000$COMPANY))  
fortune1000$COMPANY = ifelse(fortune1000$lastchar == "-",substring(fortune1000$COMPANY, first = 1, last = nchar(fortune1000$COMPANY)-1)  ,
                             fortune1000$COMPANY)

# http://fortune.com/fortune500/exxon-mobil/
fortune1000$link = paste("http://fortune.com/fortune500/",fortune1000$COMPANY,"/",sep = "")
fortune1000$industry = ""
fortune1000$sector = ""

for(i in 494:nrow(fortune1000)){
  print(i)
  html = readLines(fortune1000$link[i]) 
  a = as.data.frame(html); a$html = as.character(a$html)
  ind = locate_end(a$html[grep('company-info-card-Industry.1.0">',a$html)],'company-info-card-Industry.1.0">')
  sec = locate_end(a$html[grep('company-info-card-Sector.1.0">',a$html)],'company-info-card-Sector.1.0">')  
  
  
  # grep('company-info-card-Industry.1.0">',a$html)  
  fortune1000$industry[i] = substring(a$html[grep('company-info-card-Industry.1.0">',a$html)], first = ind + 1, last = ind+30)  
  fortune1000$industry[i] = substring(fortune1000$industry[i], first = 1, last = -1 + str_locate(fortune1000$industry[i],"<"))  
  
  fortune1000$sector[i] = substring(a$html[grep('company-info-card-Sector.1.0">',a$html)], first = sec + 1, last = sec+30)  
  fortune1000$sector[i] = substring(fortune1000$sector[i], first = 1, last = -1 + str_locate(fortune1000$sector[i],"<"))  
  print(fortune1000$sector[i])
}

write.csv(fortune1000,"fortune_scrape_dat.csv",row.names = FALSE)




