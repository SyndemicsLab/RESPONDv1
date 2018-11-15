

# OUD transition #

# given the shell table generated for the user
# mimic the user's input 
# Read shell table
oud_trans_tb1<-read.csv("inputs/oud_trans.csv") 

# to be changed to the corresponding location in Golnaz's computer, including the name of the table
OUD_col_name<-names(oud_trans_tb1)
oud_trans_tb1[] <- lapply(oud_trans_tb1, as.character)
set.seed(1234)
# distribution pool
dist_pool<-c("uniform","exponential","poisson","beta","gamma") # later, set up 0 for impossible transition

# Attampt to mimic filled OUD transition table as end user

half_ntype_oud<-lmax/2
stopifnot(half_ntype_oud%%1==0)

# Make mask table to be test and prepare for making cohort
mask_fun<-function(total_num_compartments,lmax){
  res<-NULL
  half_ntype_oud<-lmax/2
  stopifnot(half_ntype_oud%%1==0)
  mask1<-matrix(1,total_num_compartments,half_ntype_oud)
  nrow_bfoud<-total_num_compartments/lmax
  col_id<-rep(1:half_ntype_oud,2*nrow_bfoud)
  mask2<-matrix(0,total_num_compartments,half_ntype_oud)
  index<-2**(col_id-1)
  mask2<-do.call(rbind,lapply(index,function(i){as.integer(intToBits(i)[1:half_ntype_oud])}))
  mask_oudtrans<-as.data.frame(cbind(mask1,mask2))
  colnames(mask_oudtrans)<-colnames(oud_trans_tb1[seq(5,ncol(oud_trans_tb1),by=3)])
  mask_oudtrans[] <- lapply(mask_oudtrans, as.integer)
  res <- mask_oudtrans
  return(res)
}
mask_oudtrans<-mask_fun(total_num_compartments,lmax)

for (i in 1:nrow(oud_trans_tb1)){
  for (j in seq(5,ncol(oud_trans_tb1),by=3)){ # 5 = the column after initial_status, 3 = dist, p1, p2
    # Random choose distributions
    oud_trans_tb1[i,j]<-sample(dist_pool,1)
    if (oud_trans_tb1[i,j]=="uniform"){
      oud_trans_tb1[i,(j+1)]<-sample(seq(0,0.7,by=0.01),1)
      oud_trans_tb1[i,(j+2)]<-sample(seq(0.71,1,by=0.01),1)
    }
    if (oud_trans_tb1[i,j]=="exponential") {
      oud_trans_tb1[i,(j+1)]<-sample(seq(6,20,by=0.01),1)
      oud_trans_tb1[i,(j+2)]<-NA
    }
    if (oud_trans_tb1[i,j]=="poisson") {
      oud_trans_tb1[i,(j+1)]<-sample(seq(1,2,by=0.2),1)
      oud_trans_tb1[i,(j+2)]<-NA
    }
    if (oud_trans_tb1[i,j]=="beta"){
      oud_trans_tb1[i,(j+1)]<-sample(seq(1,5,by=0.01),1)
      oud_trans_tb1[i,(j+2)]<-sample(seq(1,10,by=0.01),1)
    }
    if (oud_trans_tb1[i,j]=="gamma"){
      oud_trans_tb1[i,(j+1)]<-sample(seq(2,6,by=0.01),1)
      oud_trans_tb1[i,(j+2)]<-sample(seq(3,10,by=0.01),1)
    }
    
  }
}

tease_0prob<-function(oud_trans_tb1,mask_oudtrans,myop_dist,myop_param){
  
  oud_trans_tb1 = as.matrix(oud_trans_tb1)
  mask_oudtrans = as.matrix(mask_oudtrans)
  res<-matrix(NA,nrow = nrow(oud_trans_tb1),ncol = ncol(oud_trans_tb1))
  
  col_id_dist<-seq(5,ncol(oud_trans_tb1),by=3) # because dist,p1,p2 in total 3
  col_id_p1<-col_id_dist+1
  col_id_p2<-col_id_dist+2
  
  df_dist<-oud_trans_tb1[,col_id_dist]
  df_p1<-oud_trans_tb1[,col_id_p1]
  df_p2<-oud_trans_tb1[,col_id_p2]
  
  stopifnot(dim(df_dist)==dim(mask_oudtrans))
  stopifnot(dim(df_p1)==dim(mask_oudtrans))
  stopifnot(dim(df_p2)==dim(mask_oudtrans))
  
  dt_dist<-matrix(mapply(myop_dist,df_dist,mask_oudtrans),nrow(mask_oudtrans),ncol(mask_oudtrans))
  dt_p1<-matrix(mapply(myop_param,df_p1,mask_oudtrans),nrow(mask_oudtrans),ncol(mask_oudtrans))
  dt_p2<-matrix(mapply(myop_param,df_p2,mask_oudtrans),nrow(mask_oudtrans),ncol(mask_oudtrans))
  
  res[,col_id_dist]<-dt_dist
  res[,col_id_p1]<-dt_p1
  res[,col_id_p2]<-dt_p2
  
  res[,1:4] = oud_trans_tb1[,1:4]
  res = as.data.frame(res)
  colnames(res) = colnames(oud_trans_tb1)
  
  return(res)
}

# myop_dist (distribution = 0 means it is impossible for this transition)
myop_dist<-function(dist_char,maskij){
  if(maskij==0){
    return("0")
  }
  else{
    return(dist_char)
  }
}

# myop_param (p1 and p2 would be NA when distribution = 0)
myop_param<-function(param_char,maskij){
  if(maskij==0){
    return(NA)
  }
  else{
    return(param_char)
  }
}

rule2prob <- function(oneRule){
  # Input: list of expr (Need userinput to be a expr)
  # Output: list of probs
  randDraw <- lapply(oneRule, function(expr){eval(parse(text = expr))})
  # Normalization
  summ <- sum(unlist(randDraw))
  probList <- lapply(randDraw, function(i){i/summ})
  
  round_preserve_sum <- function(x, digits = 0) {
    up <- 10 ^ digits
    x <- x * up
    y <- floor(x)
    indices <- tail(order(x-y), round(sum(x)) - sum(y))
    y[indices] <- y[indices] + 1
    y / up
  }
  probList_rd<-round_preserve_sum(unlist(probList),6)
  return(probList_rd)
}

# refill and correct the table 
oud_trans_tb1<-tease_0prob(oud_trans_tb1,mask_oudtrans,myop_dist,myop_param)

write.csv(oud_trans_tb1,file="inputs/oud_trans.csv",row.names = FALSE,quote = FALSE)




