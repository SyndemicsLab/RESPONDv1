

# Block transition

# Given the shell table generated for the user
# mimic the user's input 
# Read shell table
block_trans_tb<-read.csv("inputs/block_trans.csv") 

block_col_name<-names(block_trans_tb)
block_trans_tb[] <- lapply(block_trans_tb, as.character)
set.seed(2345)
# Distribution pool
dist_pool<-c("uniform","exponential","poisson","beta","gamma") # later, set up 0 for impossible transition

# Attampt to mimic filled OUD transition table as end user
ntype_to_trt<-1+((imax+1)/2)

# Make mask table to be test and prepare for making cohort
mask_fun<-function(total_num_compartments,imax){
  res<-NULL
  ntype_to_trt<-(imax+1)/2
  mask_blktrans<-matrix(1,total_num_compartments,ntype_to_trt)
  correspond_post_mask<-rep(c(0,rep(1,(imax-1))),total_num_compartments/imax)
  mask_blktrans<-data.frame(mask_blktrans,correspond_post_mask)
  colnames(mask_blktrans)<-colnames(block_trans_tb[seq(5,ncol(block_trans_tb),by=3)])
  mask_blktrans[] <- lapply(mask_blktrans, as.integer)
  res <- mask_blktrans
  return(res)
}
mask_blktrans<-mask_fun(total_num_compartments,imax)


# Create distribution for all cells to block_trans_tb
for (i in 1:nrow(block_trans_tb)){
  for (j in seq(5,ncol(block_trans_tb),by=3)){ # 5 = the column after initial_status, 3 = dist, p1, p2
    # Random choose distributions
    block_trans_tb[i,j]<-sample(dist_pool,1)
    if (block_trans_tb[i,j]=="uniform"){
      block_trans_tb[i,(j+1)]<-sample(seq(0,0.7,by=0.01),1)
      block_trans_tb[i,(j+2)]<-sample(seq(0.71,1,by=0.01),1)
    }
    if (block_trans_tb[i,j]=="exponential") {
      block_trans_tb[i,(j+1)]<-sample(seq(6,20,by=0.01),1)
      block_trans_tb[i,(j+2)]<-NA
    }
    if (block_trans_tb[i,j]=="poisson") {
      block_trans_tb[i,(j+1)]<-sample(seq(1,2,by=0.2),1)
      block_trans_tb[i,(j+2)]<-NA
    }
    if (block_trans_tb[i,j]=="beta"){
      block_trans_tb[i,(j+1)]<-sample(seq(1,5,by=0.01),1)
      block_trans_tb[i,(j+2)]<-sample(seq(1,10,by=0.01),1)
    }
    if (block_trans_tb[i,j]=="gamma"){
      block_trans_tb[i,(j+1)]<-sample(seq(2,6,by=0.01),1)
      block_trans_tb[i,(j+2)]<-sample(seq(3,10,by=0.01),1)
    }
    
  }
}

# Teasing out the probilities that should be 0
# assumption: the no treatment should be at the first row in the initial_trt column so this index of the row 
# would jump from 1st row
block_trans_tb[seq(1,nrow(block_trans_tb),by=imax),(ncol(block_trans_tb)-2)]<-"0"
block_trans_tb[seq(1,nrow(block_trans_tb),by=imax),((ncol(block_trans_tb)-1):ncol(block_trans_tb))]<-
  lapply(block_trans_tb[seq(1,nrow(block_trans_tb),by=imax),((ncol(block_trans_tb)-1):ncol(block_trans_tb))],
         function(irow){irow<-NA})

# for user
write.csv(block_trans_tb,file="inputs/block_trans.csv",row.names = FALSE,quote = FALSE)

