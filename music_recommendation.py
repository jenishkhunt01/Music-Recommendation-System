artists = read.table("artists.dat",sep="\t",fileEncoding = "UTF-8",stringsAsFactors = F,comment.char = "",quote="",header = T)
str(artists)

user_artists = read.table("user_artists.dat",sep="\t",header = T)
str(user_artists)


library(tidyr)
user_artists_wide = user_artists %>% spread(key=artistID,value=weight)
dim(user_artists_wide)

artists$charid=paste0("I",artists$id)

userids=user_artists_wide$userID
user_artists_wide$IuserID = NULL
rownames(user_artists_wide) = paste0("U",userids)
colnames(user_artists_wide) = paste0("I",colnames(user_artists_wide))
user_artists_wide[1:6,1:10]

visits_byitem=colSums(user_artists_wide[,-1],na.rm = T)

visits_1k = user_artists_wide[,order(visits_byitem,decreasing = T)[1:1000]]
num_visits=apply(visits_1k,1,function(x) return(sum(!is.na(x))))
visits_1k = visits_1k[num_visits>10,]
dim(visits_1k)

visits_1k=t(scale(t(visits_1k))[,])

#Function that computes similarity
similarity_user=function(all_data,user_data){
  #Transpose so that users are on columns
  all_data=t(all_data)
  #Use pearson correlation
  score = cor(all_data,user_data,use = "pairwise")
  return(score)
}



#Function that predicts rating and list recommendation
ubcf_recommend=function(data,user_data,num_rec=20,num_sim=50){
  user_sim = similarity_user(data,user_data)
  #Replace NA with zero
  data_na0=data
  data_na0[is.na(data_na0)]=0
  top_sim_users=order(user_sim,decreasing = T)[1:num_sim]
  ratings=(user_sim[top_sim_users,1]%*%data_na0[top_sim_users,])
  prediction=NULL
  prediction$ratings=ratings
  #Set rating of already rated item to NA
  ratings[!is.na(user_data)]=NA
  prediction$recommendation=order(ratings,decreasing = T)[1:20]
  #To do remove already visited artists
  return(prediction)
}


install.packages("recommenderlab")

library(recommenderlab)
visits_1k_rrm=as(as.matrix(visits_1k),"realRatingMatrix")
set.seed(100)
eval_sets <- evaluationScheme(data = visits_1k_rrm, method = "split", train = .8, given = 10, goodRating=3, k = 1)


getArtistName=function(artistid){
  return(artists$name[artists$charid==artistid])
}
#Vectorize the function to enable get values for a vector of ids
getArtistName=Vectorize(getArtistName)


#Convert realRatingMatrix to regular Matrix
train=as.matrix(getRatingMatrix(getData(eval_sets,"train")))
test=as.matrix(getRatingMatrix(getData(eval_sets,"known")))
#realRating Matrix stores missing value as 0, convert them back to NA
train[train==0]=NA
test[test==0]=NA
#Check prediction for one user
pred1=ubcf_recommend(train,test[1,])
cat("\n**** Recommendation for user",rownames(test)[1],"\n")

recommend=getArtistName(colnames(train)[pred1$recommendation])
cat(recommend,sep="\n")