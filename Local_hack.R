rm(list = ls(all = TRUE))
library(jsonlite)
setwd("/Users/jacobzhu/Desktop/github_data/data")
recipes <- stream_in(file("recipes.json"))
recipes$num_ingredients <- sapply(recipes$Ingredients, function(x) unlist(length(x)))
recipes$num_instructions <- sapply(recipes$Method, function(x) unlist(length(x)))


wp =function(a){
  b = gsub('\"',"",a,fixed=TRUE)
  b = gsub(',','',b)
  b = gsub('(','',b,fixed=TRUE)
  b = gsub(')','',b,fixed=TRUE)
  b = strsplit(b,' ')
  return(unlist(b))
}

word=NULL
count=data.frame(word)
for (i in 1:nrow(recipes)){
  b = as.data.frame(wp(recipes$Ingredients[i]))
  count=rbind(count,b)
  print(i)
}

table=as.data.frame(ftable(count))


dic = data.frame(matrix(1,nrow = length(unique(wp(recipes$Ingredients[1]))), ncol=2))
colnames(dic) = c("name","count")
dic$name = unique(wp(recipes$Ingredients[1]))
for (i in 2:nrow(recipes)){
  rn = wp(recipes$Ingredients[i])
  for(j in 1:length(rn)){
    if(rn[j]%in%dic$name){
      dic[dic$name%in%rn[j],"count"] = dic[dic$name%in%rn[j],"count"]+1
    }else{
      nm = data.frame(matrix(1,nrow = 1,ncol = 2))
      colnames(nm) = c("name","count")
      nm$name = rn[j]
      nm$count = 1
      dic = rbind(dic,nm)
    }
  }
  print(i)
}
1
