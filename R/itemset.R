library(arules)
library(tagcloud)
users = read.csv("../dataset/flickr-original.csv", sep="\t",header=TRUE)

# Iteration over distinct users.
it = 1
print("Lenght  of users ")
print(length(unique(users$user)))
urls = c()
us = c()
for (distinct in unique(users$user) ){
    fileName = paste(c("clouds/tagCloud",distinct,".png"),collapse="_")
    
    print("user")
    print(distinct)
    # Take all transactions of a single user.
    tmp = subset(users, user == distinct)
    print("dimension tmp")
    print(dim(tmp))
    # Til now hashtags and legend contains strings with separators, we have to 
    # transform this strings in word arrays.
    hashtags = tmp$hashtags
    legend = tmp$legend
    bdtr = list()
    i = 1
    for(str in hashtags){
      
      #In tokens we now have the list of words for a given row.
      tokens = strsplit(str,",")
      vectorize = tokens[[1]]
      bdtr[[i]] = unique(vectorize)
      i=i+1
    }
    
    # We call eclat algorithm with support = 0.3 ( 30% of number of transactions)
    # and maximum length = 10.
    print("before eclat")
    t = eclat(bdtr,parameter=list(support=0.4,maxlen=3))
    print("after eclat")
    # Get items and quality of a specific user.
    ret = as(items(t),"matrix")
    print("after as matr")
    qual = quality(t)$support
    print("after qual")
    it = it + 1
    nam = colnames(ret)
    weights = c() 
    keywds = c()
    tot = dim(ret)[1]
    print("before if")
    if(tot!=0){
      for(i in 1:tot){
        
        trues = which(ret[i,]==TRUE)
        if(length(trues)!=0){
          keywds = append(keywds,paste(nam[trues],collapse=","))
          weights = append(weights,qual[i]*100)
          
        }
      }
      print("afterfor")
      if(length(keywds)!=1  && length(keywds)<= 3000){
        urls = append(urls,fileName)
        us = append(us,distinct)
        print("beforetagcloud")
        print(length(keywds))
        print(length(weights))
        png(fileName,1600,1200)
        tagcloud(keywds,weights)
        dev.off()
        print("aftertagclourd")
      }
    }else{
      toprint = paste(c("The user",distinct,"has zero itemsets"),collapse=" ")
      print(toprint)
    }
}

install.packages("R2HTML")
library(R2HTML)
library(googleVis)
table = data.frame(names=us,links=urls)
table = transform(table,links=paste('<a href = ', shQuote(urls), '>See TagCloud</a>')) 
x   <- gvisTable(table, options = list(allowHTML = TRUE))
plot(x)
