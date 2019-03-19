# utility functions

# set up the number of draws for each time
n_draw <- 1


# 1. function to get the expression strings of the prior distributions
# !! Applicable to: SMR, FatalOD, All types OD, Entering Corhort, OUD trans, Block_trans, Block_init_effect
# input: distribution, parameter 1, and parameter 2
# output: expression of the distribution with distribution name and two parameters
### Note and Constrains: 
### 1) Name of distribution from the end user must be exactly same as following
### 2) This function is supposed to meet the prior distributions' draw from proportion/probability, and rates
get_dist_str <- function(dist, par1, par2){
  if (dist=="beta"){
    expr = as.character(c(paste("r",dist,"(",n_draw,",","shape1 =",par1,",","shape2 =",par2,")",sep="")))
  } # proportion/probability
  if (dist=="unif"){
    expr = as.character(c(paste("r",dist,"(",n_draw,",","min =",par1,",","max =",par2,")",sep="")))
  } # proporition/probability
  if (dist=="truncnorm"){
    expr = as.character(c(paste("r",dist,"(",n_draw,",","mean =",par1,",","sd =",par2,",","a = 0","b = 1",")",sep=""))) 
  } # proportion/probability
  if (dist=="gamma"){
    expr = as.character(c(paste("r",dist,"(",n_draw,",","shape =",par1,",","scale =",par2,")",sep=""))) 
  } # rate
  if (dist=="lnorm"){
    expr = as.character(c(paste("r",dist,"(",n_draw,",","meanlog =",par1,",","sdlog =",par2,")",sep=""))) 
  } # rate
  if (dist=="nrom"){
    expr = as.character(c(paste("r",dist,"(",n_draw,",","mean =",par1,",","sd =",par2,")",sep=""))) 
  } # rate
  if (dist=="binom"){
    expr = as.character(c(paste("r",dist,"(",n_draw,",","size =",par1,",","prob =",par2,")",sep=""))) 
  }
  return(expr)
} # output is expression of the distribution with distribution name and two parameters


# 2.[sub-step] function to get an array of all the imported sets of distribution+parameter, by specifing index_matrix such as, matrix(c(5,6,7,8,9,10,11,12,13),3,3)
# !! Applicable to: SMR, FatalOD, All types OD, Entering Corhort, OUD trans , Block_trans, Block_init_effect
# input: dataframe of input table and index_matrix
# output: an array of the expression
### Note and Constrains: 
### 1) This function support from 1 year only to n years
### 2) Each year must has 3 columns, 1st distribution, 2nd param1, 3rd param2
df_to_array <- function(dat,index_matrix){
  if(!is.matrix(index_matrix)) index_matrix = matrix(index_matrix,nrow = 3) # nrow = 3 means each year must have 3 columns, but no constrains to number of years
  
  arr = c()
  for(i in 1:ncol(index_matrix)){
    idf = dat[,index_matrix[,i]]
    imat = as.matrix(idf)
    arr = cbind(arr,imat)
  }
  
  arr = array(arr, dim = c(nrow(dat),dim(index_matrix)))
  return(arr) 
}

# 3. Function to create a matrix with 1,2,...n columns, each cell is an expression to be drawn from
# !! Applicable to: SMR, FatalOD, All types OD, Entering Corhort, OUD trans, Block_trans, Block_init_effect
# input: dataframe of input table and index_matrix
# output: a matrix with n columns (= n  years) and each cell is an expression to be drawn from
### Note and Constrains: 
### 1) This function support from 1 year only to n years
### 2) The input dataframe is full dataset from the end user with the stratification ahead, the index_matrix will specify the columns of distribution/params
### 3) Each year must have 3 columns, 1st distribution, 2nd param1, 3rd param2
create_expr_matrix = function(df,index_matrix){
  arr = df_to_array(df,index_matrix)
  res = apply(arr, c(1,3), FUN = function(inputs){get_dist_str(inputs[1],inputs[2],inputs[3])})
  return(res)
}


# 4. Function that given the expression matrix, for each cell, draw a value from the expression
# !! Applicable to: SMR, FatalOD, All types OD , Block_trans, Block_init_effect
# input: the output from Function 3 (create_expr_matrix) 
# ouput: a matrix with each cell is a drawn value for the corresponding expression
expr2rv = function(expr_matrix,precision){
  res = vapply(c(expr_matrix),
               FUN = function(expr){ return(eval(parse(text = expr)))},
               FUN.VALUE = numeric(1)
  ) 
  # c(expr_matrix) make all the matrix to be a vector (vertically and then horizontally list the cells out)
  # FUN apply the same function to each element in this vector
  # FUN.VALUE defines the type of data you are expecting
  
  # rounding to precision digits
  res = round(res,digits=precision)
  # organize the vector to matrix by defining the nrow and ncol as the original matrix
  res = structure(res, dim = dim(expr_matrix))
  
  # remove the attritbutes
  attr(res, "names") <- NULL
  
  return(res)
}


# 5. Expanding function for all types of overdose
# !! Applicable to: All types OD only
# expand from the condense version to the dimension of the shell table for all types of overdose
expend_shelldim_alltyp_od<-function(shorttb,oud,sex,agegrp,block,jmax,kmax,lmax){
  # create first 4 columns (stratifications) as the shell table
  factor_perm<-expand.grid(oud[1:(lmax/2)],sex,agegrp,block) 
  colnames(factor_perm)<-c("oud","sex","agegrp","block")
  factor_perm<-factor_perm[,c("block","agegrp","sex","oud")]
  # create an empty table and copy the first 4 columns from the shell table
  data1<-shorttb[FALSE,]
  data1[nrow(data1)+(jmax*kmax*(lmax/2)),] <- NA
  data1[,c(1:4)]<-factor_perm[,c("block","agegrp","sex","oud")]
  data1$agegrp<-rep(agegrp,each=4)
  # assign values from the condensed table to the expanded table (shell table)
  for(l in 1:(lmax/2)){
    l_sub<-subset(shorttb,shorttb$oud==oud[l])
    for(k in 1:kmax){
      k_sub<-subset(l_sub,l_sub$sex==sex[k])
      for(j in 1:jmax){
        j_sub<-k_sub[which(as.numeric(substring(k_sub$agegrp,1,2))<=as.numeric(substring(agegrp[j],1,2)) & as.numeric(substring(k_sub$agegrp,4,5))>=as.numeric(substring(agegrp[j],4,5))),]
        data1[(data1$agegrp==agegrp[j] & data1$sex==sex[k] & data1$oud==oud[l]),-c(1:4)]<-j_sub[,-c(1:4)]
      }
    }
  }
  return(data1) # this should the table filled out with the dist, param1, and param2
}



# 6. Function to draw from unique expression rows for Entering cohort
# !! Applicable to: Entering Corhort only
# apply this function to the unique rows of the expression matrix
# conditions: 
# 1) The odd row is always for male "m", and even row is alwasy for female "f"
# 2) The odd row is always larger than the next even row

# Note: This function can use for other distribution with the same condition (odd > even)
expr2rv_entercohort <- function(expr_matrix,n_draw){
  # create an empty vector to save all the cells in sequence (if matrix (i.e. 3 years), vertically go through then horizontally) in a vector
  draw_vector<-NULL
  # make the expression matrix to be a vector, this is required by using vapply function
  expr_matrix_to_vec<-c(expr_matrix)
  # calculate the length of the vector (n of cells of the matrix)
  v_length<-length(expr_matrix_to_vec)
  # the index of the odd row (for male) These rows will be drawn first and independently
  odd_row_id<-seq(1,v_length,by=2)
  # loop for all the odd rows and indirectly apply the draw for the corresponding even row
  for (i in odd_row_id ){
    # !! independently draw the odd row for male
    drawn_m<-vapply(expr_matrix_to_vec[i],
                    FUN = function(expr){ return(eval(parse(text = expr)))},
                    FUN.VALUE = numeric(1)
    ) # for male, independent draw
    # expr_matrix[c(TRUE,FALSE),] chooses the odd rows (for male) to be independently drawn
    # c(expr_matrix) make all the matrix to be a vector (vertically and then horizontally list the cells out)
    # FUN apply the same function to each element in this vector
    # FUN.VALUE defines the type of data you are expecting
    # random uniform generator to scale down the CDF
    u<-runif(n_draw,0,1)
    # re-write rdist to make pdist to draw the CDF probability from the CDF function
    # replace r by p for the even row's expression
    p_dist<-vapply(expr_matrix_to_vec[i+1],
                   FUN = function(rstring){ logi<-mapply("==",substr(rstring,1,1),"r") 
                   substr(rstring[logi],1,1) <- "p" 
                   return(rstring)},
                   FUN.VALUE = character(1)
    )
    # scaled down the probability (cdf) by applying a random drawn value from unif(0,1) (u)
    scaled<-u*vapply(c(p_dist),
                     FUN = function(expr){ return(eval(parse(text = expr)))},
                     FUN.VALUE = numeric(1)
    )
    # re-write rdist to make qdist to draw a random variable by CDF
    # replace r by q for the even row's expression
    general_q_dist<-vapply(expr_matrix_to_vec[i+1],
                           FUN = function(rstring){ logi<-mapply("==",substr(rstring,1,1),"r") 
                           substr(rstring[logi],1,1) <- "q" 
                           return(rstring)},
                           FUN.VALUE = character(1)
    )
    # for the qdist function, we need to plug in the scaled value, so replace "(1," by "(scaled,"
    # here "(scaled," is saved above
    # because gsub doesn't recognize the parenthese, so use \\ in front of it
    q_dist<-vapply(c(general_q_dist),
                   FUN = function(rstring){return(gsub("\\(1,","(scaled,",rstring))},
                   FUN.VALUE = character(1)
    )
    # draw a random variable value from q_dist
    drawn_f<-vapply(c(q_dist),
                    FUN = function(expr){ return(eval(parse(text = expr)))},
                    FUN.VALUE = numeric(1)
    )
    # paste drawn female and male together
    # !! the first draw will be the last one ---> need to inverse
    draw_vector<-c(draw_vector,drawn_f,drawn_m) 
  }
  # !! inverse the vector so that the first is male for first age group
  draw_vector<-rev(draw_vector) 
  # do not need to round to certain decimal places, will do at last step
  
  # convert the draw_vector back to the same dimension as the expression matrix
  res <- structure(draw_vector, dim = dim(expr_matrix))
  # remove the attritbutes
  attr(res, "names") <- NULL
  
  return(res)
}



# 7. Function to expand the unique rows to the full length by applying the rule of repeating each combo of sex for entering cohort
# !! Applicable to: Entering Corhort 
# Apply this function to the vector 
# Note: need the rule to repeat
# Input: Drawn matrix in a condensed version, Rule to repeat the drawn values, n_time_varying, jmax=agegroup, kmax=sex
# Output: a matrix in a full length of age group * sex
expand_unique_to_full_entercohort <- function (enter_cohort_draw_value,rule_to_repeat,n_time_varying,jmax,kmax) {
  # expand the condensed to the full length of entering cohort table
  # split the row index of the condense dataframe (unique rows) to each age group (male + female) 
  split2two<-split(enter_cohort_draw_value,ceiling(seq_along(enter_cohort_draw_value)/2)) # divided by 2 because we combind male and female
  # !! apply the rule of how each age group will repeat
  rule_rep<-rep(rule_to_repeat,n_time_varying) 
  # apply the rule_rep to repeat the row index
  rep_drawn_byRule<-rep(split2two,rule_rep)
  # unlist to get a vector 
  expand_drawn_vect<-unlist(rep_drawn_byRule)
  # remove the attributes
  attr(expand_drawn_vect, "names") <- NULL
  # wrap as a matrix by defining the matrix dimensions
  # dimension of row
  dim_nrow<-jmax*kmax
  # dimension of col
  dim_ncol<-n_time_varying
  # dimension of the drawn matrix to be assigned
  drawn_matrix_dim<-c(dim_nrow,dim_ncol)
  # final enter_cohort_drawn
  enter_cohort_drawn<-structure(expand_drawn_vect, dim = drawn_matrix_dim)
  
  return (enter_cohort_drawn)
}



# 8. Function to renormalize the values in each of columns while preserving them to have number of digits = precision at the end 
# !! Applicable to: Entering cohort, 
# Conditions:
# 1) The input should have multiple columns (!! NOT work for vector)
# Input: a matrix with multiple columns, each column is to be renormalized while preserving them to be with 6 decimal places
# Output: a matrix with multiple columns, summation of each column should be 1, each cell has 6 decimal places
normalize2one_rd <- function(matrix_to_renorm,precision){
  # renormalize each cell within each column, making the summation of each column is 1
  enter_cohort_draw_renorm<-apply(matrix_to_renorm,2,FUN = function(i) { i/sum(i)})
  # https://biostatmatt.com/archives/2902
  # Round values while preserve their rounded sum to be 1
  round_preserve_sum <- function(x, digits = 0) {
    up <- 10 ^ digits
    x <- x * up
    y <- floor(x)
    indices <- tail(order(x-y), round(sum(x)) - sum(y))
    y[indices] <- y[indices] + 1
    y / up
  }
  # apply the round_preserve_sum function to have it summed up 1 and with 6 digits
  normalized_rd_matrix<-round_preserve_sum(enter_cohort_draw_renorm,precision)
  return(normalized_rd_matrix)
}






# 9. Get the parameters vector for the OUD transition from the input matrix
# Applicable to: OUD transition only
# For each parameter(at most we have two), call this function once to get a vector of the param value in the order of the OUD_Trans_Rule based on the coordinate defined in the rank_coord()
# Input: the effective subset of the inputtable --unique_set, the row IDs of one param, and column IDs of one param
# Output: A vector of the values of one param for a sepecific group 
get_param_vec = function(inputtable, row_IDs, col_IDs, rank_coord){
  # read the inputtable cell values by the assigned columns and rows
  core_param_matrix = inputtable[row_IDs,col_IDs]
  # as.matrix
  core_param_matrix<-as.matrix(core_param_matrix)
  # based on the coordinate in the rank_coord and its order, re-organize the cell values
  ordered_param_vec<-apply(rank_coord,1,
                           FUN = function(irow){
                             return(core_param_matrix[irow[1],irow[2]])
                           })
  
  return(ordered_param_vec)
}


# 10. Function to draw values in order defined by rank_coord for each of n_trans params
# Applicable to: OUD transition only
# The function is used to get a drawn value for each transition probability which has real distribution and params.
# Constrains:
# 1) Two params should list the values in the same order as rank_coord
# 2) Given the order of the values in the params, draw the values for each of them and the returned drawn value vector should be in the same order as rank_coord() (i.e. first = biggest)
# 3) Distribution options: beta or unif (exactly type in "unif")
# 4) In this draw, we don't need n_drawn, because for each oud_trans, the ability is to draw one for each of them
# Input: n_trans(number of transition to be drawn), a vector listing the distribution of each of them, a vector listing param1 of each of them, a vector listing param2 of each of them
# Output: a vector (length = n_trans), the first value should be the biggest one, the last two should be smallest one
get_priordist_odtrans<-function(n_trans,dist_vec,param1_vec,param2_vec){
  
  # generate a random number uu from the standard uniform distribution in [0,1]
  # Note: This is also applied to the first biggest drawn (i.e. the first one is scaled down as well by uu)
  uu <- runif(n_trans,0,1) 
  # set up the initial value of the denominator of the conditional probability
  # i.e. The biggest one doesn't have the conditional probability applied to it
  condition_denominator <- 1 
  # set up the initial emply value of one drawn value
  draw_inver_cdf_i <- NULL 
  # set up the initial value of the drawn value list
  vec_drawn <- NULL 
  # a for loop for each of elements until n = n_trans
  for (i in 1:n_trans){
    # first option = beta
    if (dist_vec[i]=="beta"){ 
      alpha <- param1_vec
      beta <- param2_vec
      # do for each parameter in the param list, including the first independent one(i.e. biggest one)
      # the last two in the param list are independent but both are smaller than the third from the end (6th)
      if (i < n_trans){
        # scale down given the probability function of the last one (a scale down on conditional probability)
        uu_i <- uu[i]*condition_denominator 
        # draw for the next one from the quantile function (ive-CDF)
        draw_inver_cdf_i <- qbeta(uu_i,alpha[i],beta[i])
        # update the denominator for the next draw (a smaller draw)
        # the condition_denominator is the probability of getting the drawn value from the last one given its current one's param1 and param2
        condition_denominator <- pbeta(draw_inver_cdf_i,alpha[i+1],beta[i+1])
      }
      # for the smallest (last) one, it should be smaller than the 6th one, instead of one before(7th)
      if (i == n_trans){
        # scale down given the probability function of the 6th one (a scale down on conditional probability)
        uu_i <- uu[i]*pbeta(vec_drawn[2],alpha[i],beta[i])
        # draw for the last one from the quantile function (ive-CDF)
        draw_inver_cdf_i <- qbeta(uu_i,alpha[i],beta[i])
      }
    } # end with if dist = beta
    # second option = unif
    if (dist_vec[i]=="unif"){
      lower <- param1_vec
      upper <- param2_vec
      # do for each probability, including the first independent one
      if (i < n_trans){
        # scale down given the probability function of the last one (a scale down on conditional probability)
        uu_i <- uu[i]*condition_denominator 
        # draw for the next one from the quantile function (ive-CDF)
        draw_inver_cdf_i <- qunif(uu_i,lower[i],upper[i])
        # update the denominator for the next draw (a smaller draw)
        # the condition_denominator is the probability of getting the drawn value from the current one given its next one's param1 and param2
        # This is to Update for NEXT! So use [i+1]
        condition_denominator <- punif(draw_inver_cdf_i,lower[i+1],upper[i+1])
      }
      if (i == n_trans){
        # scale down given the probability function of the 6th one (a scale down on conditional probability)
        # Why select vec_draw[2]?
        # Because: by i = n_trans-1, there are n_trans-1 elements in the vec_draw, they are listed from the smallest to the largest,
        # for the last draw = n_trans, we want it to be smaller than the 6th one (n_trans-2), but in vec_draw, it is at 2nd place
        uu_i <- uu[i]*punif(vec_drawn[2],lower[i],upper[i])
        # draw for the last one from the quantile function (ive-CDF)
        draw_inver_cdf_i <- qunif(uu_i,lower[i],upper[i])
      }
    } # end with if dist = unif
    
    # append to vector
    vec_drawn<-c(draw_inver_cdf_i,vec_drawn) # the biggest one (which is first drawn) is at the last place (opposite to the order of rank_corrd())
  }
  
  # reverse the vector to make the order of this drawn value vector = the order of rank_coord() (i.e. the first one is the biggest one)
  vec_drawn<-rev(vec_drawn)
  return(vec_drawn) # the returned the list, first is the biggest one, the last one is the smallest one.
}


# 11. Function after drawn, to recover the oud_trans matrix
# Applicable to: OUD transition only
# fill in the correct places in the oud_trans matrix from the drawn_vector
# Constrains:
# 1) The recover matrix must be the same matrix as inputtable
# 2）For the unique set matrix, assign value = 1 - summation of the rest in a row 
# 3) For the last cell in the odd row in this matrix, and the second last cell in the even in this matrix, we assign 0, meaning the transition prob = 0
# Input: the drawn vector in length of n_trans, define the number of rows of the matrix(=number of destination(lmax)), number of columns of the matrix(=number of starting(lmax))
# and precision (how many digits rounding to)
# Output: a filled in matrix of one gender within in one age group
recover_drawn_matrix<-function(drawn_vec,rank_coord,nrow,ncol,precision=6){
  new_matrix<-matrix(0,nrow=nrow,ncol=ncol) 
  for(i in 1:nrow(rank_coord)){
    new_matrix[rank_coord[i,1],rank_coord[i,2]]<-drawn_vec[i]
  }
  # round the original values once before 1-summation
  new_matrix<-round(new_matrix,digits = precision)
  # enforcen the self-circle is 1 - summation of others
  diag(new_matrix) <- 1-rowSums(new_matrix)
  # rounding the final new_matrix after 1-summation
  new_matrix<-round(new_matrix,digits = precision)
  return(new_matrix)
}



# 12. Function to wrap up the steps from drawing and filling and duplicate to both gender for one unique agegroup
# Applicable to: OUD transition only
# Input: all the distinct input for function(get_priordist_odtrans) and function(recover_drawn_matrix)
# Output: a rbinded matrix of 1 agegrp* 2 sex* 4 oud_trans matrix
duplicate_for_all_age<-function(n_trans,dist_vec,param1_vec,param2_vec,rank_coord,lmax,precision){
  #draw values
  drawn_vec<-get_priordist_odtrans(n_trans,dist_vec,param1_vec,param2_vec)
  # fill the drawn back to the matrix
  update_unique_draw<-recover_drawn_matrix(drawn_vec,rank_coord,lmax,lmax,precision) # to do number of oud =lmax??
  # duplicate for another gender within one agegroup
  dup_one_agegrp<-rbind(update_unique_draw,update_unique_draw)
  # create empty matrix
  rbind_agegrps<-dup_one_agegrp[FALSE,]
  # rbind the duplicated rows(snowing ball for all age groups)
  rbind_agegrps<-rbind(rbind_agegrps,dup_one_agegrp)
  # enforce to as.matrix
  rbind_agegrps<-as.matrix(rbind_agegrps)
  
  return(rbind_agegrps)
}

