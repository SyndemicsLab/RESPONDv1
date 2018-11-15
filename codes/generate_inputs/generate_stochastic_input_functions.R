#Read stochastic input csv files and draw a value based on the distriburions and create deterministic values for simulation model.

# Entering cohort
generate_entering_cohort_size <- function(){
  entering_cohort_size <- round(runif(length(time_varying_entering_cohort_cycles),500,1000),digits = 0)
  return(entering_cohort_size)
}
# SMR
generate_stochastic_SMR <- function (){
SMR <- round(runif(total_num_compartments, min = 1, max= 10), digits = 3) 
return (SMR)
}
#--------------------------------------------------------------------------------------------------------------------
# All_types overdose
generate_stochastic_all_types_overodse <- function (){
all_types_overdose_matrix <- matrix(round(runif(total_num_compartments*length(time_varying_overdose_cycles)/2,0.1,0.4), digits = 3), nrow = total_num_compartments/2, ncol = length(time_varying_overdose_cycles))
return (all_types_overdose_matrix)
}
#--------------------------------------------------------------------------------------------------------------------
# Fatal overedose
generate_fatal_overdose <- function(){
fatal_overdose_ratio <- round(runif(length(time_varying_overdose_cycles),0.02,0.2), digits = 2)
return(fatal_overdose_ratio)
}
#--------------------------------------------------------------------------------------------------------------------
# OUD_transition
generate_oud_trans_matrix <- function ()
{
  # Apply the transition Rule ("Markov Matrix"): 
  # 1) Draw from distribution 
  # 2) Assign impossible transition (P=0)
  # 3) Renormalize
  
  # This part would be the user's input
  source("codes/generate_inputs/tmp/sim_oud_trans_stochastic_user_input.R")
  oud_trans_tb1<-read.csv("inputs/oud_trans.csv")
  oud_col_name<-names(oud_trans_tb1)
  oud_trans_tb1[] <- lapply(oud_trans_tb1, as.character)
  
  Rule_irow<-as.data.frame(matrix("",ncol=lmax,nrow =nrow(oud_trans_tb1)))
  Rule_irow[] <- lapply(Rule_irow, as.character)
  # assign column name to rule_irow
  colname_Rule_irow<-NULL
  for (i in 1:lmax){
    colname_Rule_irow<-c(colname_Rule_irow,paste("to_",oud[i],"_function",sep=""))
  }
  colnames(Rule_irow)<-colname_Rule_irow
  
  
  counter_j<-0
  for (i in 1:nrow(oud_trans_tb1)){
    for (j in seq(5,ncol(oud_trans_tb1),by=3)){ # 3 = columns of dist p1 p2, 5 = the starting column of distributions
      if(j >= 5 & j <= max(seq(5,ncol(oud_trans_tb1),by=3)))
        counter_j=counter_j+1
      if (oud_trans_tb1[i,j]=="uniform"){  # uniform
        Rule_irow[i,counter_j]<-as.character(c(paste("runif","(","1",",",oud_trans_tb1[i,(j+1)],",",oud_trans_tb1[i,(j+2)],")",sep="")))
      }
      if (oud_trans_tb1[i,j]=="exponential"){ # uniform
        Rule_irow[i,counter_j]<-as.character(c(paste("rexp","(","1",",",oud_trans_tb1[i,(j+1)],")",sep="")))
      }
      if (oud_trans_tb1[i,j]=="poisson"){  # uniform
        Rule_irow[i,counter_j]<-as.character(c(paste("rpois","(","1",",",oud_trans_tb1[i,(j+1)],")",sep="")))
      }
      if (oud_trans_tb1[i,j]=="beta"){  # uniform
        Rule_irow[i,counter_j]<-as.character(c(paste("rbeta","(","1",",",oud_trans_tb1[i,(j+1)],",",oud_trans_tb1[i,(j+2)],")",sep="")))
      }
      if (oud_trans_tb1[i,j]=="gamma"){  # uniform
        Rule_irow[i,counter_j]<-as.character(c(paste("rgamma","(","1",",",oud_trans_tb1[i,(j+1)],",",oud_trans_tb1[i,(j+2)],")",sep="")))
      }
      if (oud_trans_tb1[i,j]=="0"){
        # meaning prob of transition = 0, making sure the cell says "0"
        Rule_irow[i,counter_j]<-as.character(c(paste("rbinom","(","1",",",1,",",0,")",sep="")))
      }
    } #reset
    counter_j<-0
  }
  
  
  #set.seed(1234)
  oud_trans_matrix <- t(apply(Rule_irow,1,function(iList){rule2prob(iList)}))
  
  #source("UserInput_OUD_transition_JW.R")
  mask_oudtrans<-mask_fun(total_num_compartments,lmax)
  # Output matrix
  oud_trans_matrix_final<-as.matrix(oud_trans_matrix*mask_oudtrans)
  return(oud_trans_matrix_final)
  #for (i in seq(1:ncol(oud_trans_matrix))){
  #  colnames(oud_trans_matrix_final)[i]<-c(paste("to_",oud[i],"_value",sep=""))
  #}
}

#--------------------------------------------------------------------------------------------------------------------
# Block transition
generate_block_trans_matrix <- function ()
{
  source("codes/generate_inputs/tmp/sim_block_trans_stochastic_user_input.R")
  
  
  
  
  # Block transition (stochastic input generation)
  
  # Apply the transition Rule ("Markov transition Matrix"): 
  rule2prob <- function(oneRule){
    # Input: list of expr (Need userinput to be a expr)
    # Output: list of probs
    randDraw <- lapply(oneRule, function(expr){eval(parse(text = expr))})
    # Normalization
    summ <- sum(unlist(randDraw))
    probList <- lapply(randDraw, function(i){i/summ})
    # Round values while preserve their rounded sum to be 1
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
  
  
  # This part would be the user's input
  block_trans_tb<-read.csv("inputs/block_trans.csv")
  block_col_name<-names(block_trans_tb)
  block_trans_tb[] <- lapply(block_trans_tb, as.character)
  
  Rule_irow<-as.data.frame(matrix("",ncol=2+((imax-1)/2),nrow =nrow(block_trans_tb)))
  Rule_irow[] <- lapply(Rule_irow, as.character)
  # assign column name to rule_irow
  colname_Rule_irow<-NULL
  for (i in 1:(1+((imax-1)/2))){
    colname_Rule_irow<-c(colname_Rule_irow,paste("to_",block[i],"_function",sep=""))
  }
  colname_Rule_irow<-c(colname_Rule_irow,"to_corresponding_post_trt_function")
  colnames(Rule_irow)<-colname_Rule_irow
  
  
  
  counter_j<-0
  for (i in 1:nrow(block_trans_tb)){
    for (j in seq(5,ncol(block_trans_tb),by=3)){ # 3 = columns of dist p1 p2, 5 = the starting column of distributions
      if(j >= 5 & j <= max(seq(5,ncol(block_trans_tb),by=3)))
        counter_j=counter_j+1
      if (block_trans_tb[i,j]=="uniform"){  
        Rule_irow[i,counter_j]<-as.character(c(paste("runif","(","1",",",block_trans_tb[i,(j+1)],",",block_trans_tb[i,(j+2)],")",sep="")))
      }
      if (block_trans_tb[i,j]=="exponential"){ 
        Rule_irow[i,counter_j]<-as.character(c(paste("rexp","(","1",",",block_trans_tb[i,(j+1)],")",sep="")))
      }
      if (block_trans_tb[i,j]=="poisson"){  
        Rule_irow[i,counter_j]<-as.character(c(paste("rpois","(","1",",",block_trans_tb[i,(j+1)],")",sep="")))
      }
      if (block_trans_tb[i,j]=="beta"){  
        Rule_irow[i,counter_j]<-as.character(c(paste("rbeta","(","1",",",block_trans_tb[i,(j+1)],",",block_trans_tb[i,(j+2)],")",sep="")))
      }
      if (block_trans_tb[i,j]=="gamma"){  
        Rule_irow[i,counter_j]<-as.character(c(paste("rgamma","(","1",",",block_trans_tb[i,(j+1)],",",block_trans_tb[i,(j+2)],")",sep="")))
      }
      if (block_trans_tb[i,j]=="0"){
        # meaning prob of transition = 0
        Rule_irow[i,counter_j]<-as.character(c(paste("rbinom","(","1",",",1,",",0,")",sep="")))
      }
    }
    #reset the counter_j for new i
    counter_j<-0
  }
  
  
  blk_trans_matrix <- t(apply(Rule_irow,1,function(iList){rule2prob(iList)}))
  
  
  mask_blktrans<-mask_fun(total_num_compartments,imax) 
  # output matrix
  blk_trans_matrix_final<-as.matrix(blk_trans_matrix*mask_blktrans)
  return(blk_trans_matrix_final)
 
}

# Block initiation effect
generate_block_init_matrix <- function()
{
  blk_init_mat <- matrix(round(runif(lmax*imax,0.1,0.8),digits=1),nrow=lmax, ncol = imax)
  return(blk_init_mat)
}
  #----------------------------------------------------------------------------------------------------------------


