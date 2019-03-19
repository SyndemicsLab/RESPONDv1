# for testing the code
# make sure all the input tables are there
# run respond_main.R up to "# choose input type and then create them"
# make sure 
# source("codes/generate_inputs/utility_functions.R")


# <1> SMR 
# Constrains and varying:
# 1) Stratified by sex, and OUD status; apply the same drawn values to all the age groups (Decided 03/18/2019)
# 2) Not varying between different years
# Function to generate the drawn values matrix (or a vector if n_time_varying = 1)
# input: the input data table from the end user which has stratification, dist/param1/param2 sets, number of time_varying years
# output: a matrix (1 column to n columns) of drawn values
generate_SMR <- function () {
  
# import the input data table for SMR from the end users
inputtable<-read.csv("inputs/SMR.csv")
select_columns<- as.numeric(c(seq(1,ncol(inputtable))[5:ncol(inputtable)])) # from the 5th column (first 4 are stratifications)
n_time_varying<-1

# cut to a effective subset, as it is not stratified by age group, but we set all age groups there.
# draw first sex*oud rows, and duplicate them to all the rest rows
row_index_smr<-seq(1,kmax*lmax,by=1) # row index
subset<-inputtable[row_index_smr,] # subset

# specify the index_matrix (which columns are expression columns and how many are they)
## Note: 
## 1) All types of overdose has three years values (time-varying)
index_matrix<-matrix(select_columns,nrow=3,ncol=n_time_varying)

# get the matrix of expression
dist_matrix<-create_expr_matrix(subset,index_matrix)

# draw a value from each distribution expression in each cell
draw_value<-expr2rv(dist_matrix,precision)

# expand to all age groups
smr_draw_value<- rep(draw_value,jmax)

# if vector, make it as vector
smr_drawn<-as.vector(smr_draw_value)

return(smr_drawn)
}


# <2> Fatal Overdose
# Constrains and varying:
# 1) Doesn't stratify age/sex/OUD
# 2) Varying between different yeras 
# Function to generate the drawn values matrix (or a vector if n_time_varying = 1)
# input: the input data table from the end user which has stratification, dist/param1/param2 sets, number of time_varying years
# output: a matrix (1 column to n columns) of drawn values
generate_fatal_overdose <- function () {

# import the input data table for Fatal Overdose from the end users
inputtable<-read.csv("inputs/fatal_overdose.csv")
select_columns<- as.numeric(c(seq(1,ncol(inputtable))[1:ncol(inputtable)]))# from the 1st column (no stratification)
n_time_varying<-length(time_varying_overdose_cycles)

# specify the index_matrix (which columns are expression columns and how many are they)
## Note: 
## 1) All types of overdose has three years values (time-varying)
## 2) HOWEVER, the structure of this is each year is one row, currently we have 3 rows representing 3 years. So even though we have time varying, we only have one column
index_matrix<-matrix(select_columns,nrow=3,ncol=n_time_varying)

# get the matrix of expression
dist_matrix<-create_expr_matrix(inputtable,index_matrix)

# draw a value from each distribution expression in each cell
fatal_od_draw_value<-expr2rv(dist_matrix,precision) 
# return 3 col * 1 row, 6 digits

# make it as vector (even thought it is multiple columns)
fatal_od_drawn<-as.vector(fatal_od_draw_value)

return(fatal_od_drawn)
}



# <3> Getting all types of overdose
# constrains and varying:
# 1) Stratified by Age(18), Gender(2), and OUD status(2)
# 2) Varying between different years
# Function to generate the drawn values matrix (or a vector if n_time_varying = 1)
# input: the input data table from the end user which has stratification, dist/param1/param2 sets, number of time_varying years
# output: a matrix (1 column to n columns) of drawn values
generate_all_types_overodse <- function () {
  
# import the input data table for overall overdose probability from the end users
inputtable<-read.csv("inputs/all_types_overdose.csv")
select_columns<- as.numeric(c(seq(1,ncol(inputtable))[5:ncol(inputtable)])) # from the 5th column (first 4 are stratifications)
n_time_varying<-length(time_varying_overdose_cycles)
  
# specify the index_matrix (which columns are expression columns and how many are they)
## Note: 
## 1) All types of overdose has three years values (time-varying)
index_matrix<-matrix(select_columns,nrow=3,ncol=n_time_varying)

# get the matrix of expression
dist_matrix<-create_expr_matrix(inputtable,index_matrix)

# draw a value from each distribution expression in each cell
all_type_od_draw_value<-expr2rv(dist_matrix,precision)
# return the rounded the number of digits

# append the first 4 columns (stratifications) to expand from condensed age group to 18 age groups
shorttb_drawn<-data.frame(inputtable[,1:4],all_type_od_draw_value)

# expand the drawn values with 5 age groups to the full length (18 age groups)
# within one condensed age group in one gender,each sub agegroup has the same drawn values as others
full_all_types_od_drawn<-expend_shelldim_alltyp_od(shorttb=shorttb_drawn,oud=oud,sex=sex,agegrp=agegrp,block=block,jmax=jmax,kmax=kmax,lmax=lmax)
# return 18*2*2 rows, 3 cols

# remove the first 4 columns
full_all_types_od_drawn<-full_all_types_od_drawn[,-(1:4)]

# if multiple columns, make it as matrix 
full_all_types_od_drawn<-as.matrix(full_all_types_od_drawn)

return(full_all_types_od_drawn)
}





# <4> Entering cohort 
# constrains and varying:
# 1) Stratified by Age (5 age groups)(input is 18 but params and dist are duplicated) and Gender (2 groups)
# 2) Varying between different years
# input: the input data table from the end user has two stratifications (age and sex) and is in full length (18 * 2), number of time_varying years
# output: a matrix with number of time_varying columns in full length (18*2)

generate_entering_cohort <- function() {
  
  # import the user input table 
  inputtable<-read.csv("inputs/entering_cohort.csv")
  
  # test if the odd row is male and even row is female, because later in the expr2rv_entercohort function, will use odd and even row as index
  # try(  if(unique(inputtable[c(TRUE,FALSE),"sex"]) %in% "f") 
  #   stop("The odd rows in sex should be 'm'!")  )
  
  # select the columns to draw from
  select_columns<-as.numeric(c(seq(1,ncol(inputtable))[3:ncol(inputtable)]))
  # define the time-varying
  n_time_varying<-length(time_varying_entering_cohort_cycles)
  
  # create the index_matrix for column index to be pasted and drawn from
  index_matrix<-matrix(select_columns,nrow=3,ncol=n_time_varying)
  
  # create expression matrix, each cell is an expression to run
  dist_matrix<-create_expr_matrix(inputtable,index_matrix)
  
  # find the unique row
  unique_row_index<-which(duplicated(dist_matrix)==FALSE)
  unique_row_tb<-dist_matrix[unique_row_index,] # assuming always first is male second is female
  
  # call the function to draw from unique rows (condensed)
  enter_cohort_draw_value<-expr2rv_entercohort(unique_row_tb,n_draw)
  # dim = # of unique rows * 3 columns
  
  # rule of how each age group will repeat (each age group combined male and female)
  # e.g., the first unique combo (m+f) is for 10-19 which contains 10-14 and 15-19 so it repeats two times
  rule_to_repeat<-c(2,1,2,3,10) # first unique row repeats 2 times, second unique row repeast 1 time, ... the last unique row repeats 10 times
  
  # call the function to expand from the unique rows to the full length (18 agegrp) by applying the rule_to_repeat
  enter_cohort_draw_value<-expand_unique_to_full_entercohort(enter_cohort_draw_value,rule_to_repeat,n_time_varying,jmax,kmax)
  
  # if multiple cols, make it as matrix
  enter_cohort_drawn<-as.matrix(enter_cohort_draw_value)
  
  # renormalize it to force them added up one within one column (because this is the proportion)
  enter_cohort_drawn<-normalize2one_rd(enter_cohort_drawn,precision)
  
  return(enter_cohort_drawn)
}


# <5> OUD transition
# constrains and varying:
# 1) Stratified by Age (18 age groups)(input is 18 but params and dist are duplicated, the drawn should be the different) 
#                  and Gender (2 groups)(input is 2 within each group but drawn should be same)
# 2) With four sets of "dist","param1","param2" as four destinations, the order (left->right) of the destination  == the order(up->dowm) of the starting point
# 3) There are order of 8 transitions, we call it OUD_Trans_Rule: 
# Say 1=act_NonInj, 2=Act_Inj, 3=Nonact_NonInj,4=Nonact_Inj. The rule: 1_2>2_1>4_2>3_1>1_3>2_4>3_2>4_1
# 4) Within each sex of one agegroup, the diagnoal is 1 - summation of all other columns in this row
# 5) Always the odd row has last cell = 0 (probability of transition = 0), the even row has last cell = 0 (probability of transition = 0). The is same for ALL the row in the input table
# 6) For the OUD transition probability, we only allow the end user to input "beta" or "unif" distritbion. For beta, the param1=alpha, param2=beta. For unif, param1=lower bound, param2=upper bound.
# (cont') DO NOT type in "uniform" in the distribution column

# input: the input data table from the end user has two stratifications (age and sex) and is in full length (18*2*4), number of destintions
# output: a matrix with destination in full length (18*2*4)

generate_oud_trans_matrix <- function() {
  # import the user input table
  inputtable<- read.csv("inputs/oud_trans.csv")  # !!TO DO change path
  
  # Because each age group should have different set of drawn values from the same set of params and dist, but within them, male and female have the same values
  # We decided to draw the whole unique set of params as many as times as the number of age groups, then assgin them to each of age groups
  # Within one age group, for male and female, we duplicated that drawn
  
  # Decide the number of times to draw the whole set
  n_times_to_draw<-length(unique(inputtable$agegrp))
  # Decide the number of times to duplicate within each age group
  n_times_to_dup<-length(unique(inputtable$sex))
  
  # Only keep the first agegroup & male, draw them n_times_to_draw times, duplicate each of them once for female in the same age group
  unique_set<-inputtable[1:lmax,]
  
  # Define the OUD_Trans_Rule by typing in the coordinate in input matrix of each rank
  # because this step is static (fixed), so just output the matrix of (n_trans * 2) highest one is at the first row, the smallest one is at the last row
  # create a rank matrix (ranking coordination 1 to 8 (hightest))
  # output: a n_trans * 2 matrix as a rank coordination, each row has two columns, each row is the coordinate of the OUD_trans in the inputtable,
  # (cont') 1st is coordination(row), 2nd is coordination(col), The smallest oud_trans coordinate in the inputtable is at the last row
  rank_coord<-function () {
    res <- rbind(c(1,2), #8th (highest rank = first row)
                 c(2,1), #7
                 c(4,2), #6
                 c(3,1), #5
                 c(1,3), #4
                 c(2,4), #3
                 c(3,2), #1st (the last two ranks are both smaller than 3rd one but independent between them)
                 c(4,1), #1st (lowest rank = last row)
                 NULL)
    rank_coord <- res
    return(rank_coord)
  }
  
  # Number of OUD transition need to be drawn from the real distribution
  n_trans<-nrow(rank_coord())
  
  # Define row_IDs for function "get_param_vec"
  effective_row_IDs<-1:nrow(unique_set)
  # dist
  dist_col_IDs<-seq(5,ncol(unique_set),by=3)
  # param1
  param1_col_IDs<-seq(6,ncol(unique_set),by=3)
  # param2
  param2_col_IDs<-seq(7,ncol(unique_set),by=3)
  # get distribution vector (we do not enforce all of them should be same)
  dist_vec<-get_param_vec(unique_set,effective_row_IDs,dist_col_IDs,rank_coord())
  # get param 1 vector (the first one should be the biggest)
  param1_vec<-get_param_vec(unique_set,effective_row_IDs,param1_col_IDs,rank_coord())
  # get param 2 vector (the first one should be the biggest)
  param2_vec<-get_param_vec(unique_set,effective_row_IDs,param2_col_IDs,rank_coord())
  
  # reapeat the drawing and recovering as many times as the total number of age groups(n times= jmax)
  # replicate the same process jmax times to get all age groups values(from darwing a vector of values to filling to the matrix and duplicate for both genders)  
  # output is a 3d array
  full_drawn_oud_trans_array<-replicate(jmax, duplicate_for_all_age(n_trans,dist_vec,param1_vec,param2_vec,rank_coord(),lmax,precision))
  # convert the 3d array to a full length of age group*sex*oud_trans matrix (nrow=jmax*kmax*lmax,ncol=lmax)
  full_oud_trans_drawn<-matrix(NA,nrow = 0,ncol = lmax)
  for(j in 1:dim(full_drawn_oud_trans_array)[3]){
    full_oud_trans_drawn<-rbind(full_oud_trans_drawn,full_drawn_oud_trans_array[,,j])
  }
  
  return(full_oud_trans_drawn)
}



# <6> Block transition (Temporary !)

# We just create drawn values matrix for no treatment only
generate_block_trans_matrix<-function(){
  inputtable<-read.csv("inputs/block_trans.csv")
  n_row<-nrow(inputtable)
  to_no_trt_drawn<-rep(1,n_row)
  to_post_trt_drawn<-rep(0,n_row)
  full_block_trans_drawn<-data.frame(to_no_trt_drawn,to_post_trt_drawn)
  # if multiple columns, make as a matrix
  full_block_trans_drawn<-as.matrix(full_block_trans_drawn)
  
  return(full_block_trans_drawn)
  
}



# <7> Block initiation effect (Temporary !)

# We just create drawn values matrix for no treatment only (This is not able to run with more treatments)
generate_block_init_matrix<-function(){
  
  inputtable<-read.csv("inputs/block_init_effect.csv")
  
  # import the input data table for block_init_effect from the end users
  select_columns<- as.numeric(c(seq(1,ncol(inputtable))[2:ncol(inputtable)])) # from the 2nd column (first column is stratification)
  n_time_varying<-1
  
  # Specify the dimensions for the array
  dimensions<-length(select_columns)/3
  # Specify the index_matrix
  # Note
  ## 1) block_init_effect doesn't have time varying
  index_matrix<-matrix(select_columns,nrow=3,ncol=dimensions)
  
  # get the matrix of expression
  dist_matrix<-create_expr_matrix(inputtable,index_matrix)
  
  # draw a value from each distribution expression in each cell
  draw_value<-expr2rv(dist_matrix,precision)
  
  # if multiple columns, make as a vector
  full_block_init_effect_drawn<-as.matrix(draw_value)
  
  return(full_block_init_effect_drawn)
}