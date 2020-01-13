# This toy model runs a combo test to check the final outputs of simulation with R results. 
# It is completely automated unless there are other values for number of OUD states than 4. In this case, user needs
# to manually change 2 lines in checking overdose part.
options(digits=12) 

#install.packages("data.table") 
library(data.table)

# ------------------------------------------------------------------------------------------------------------------
# Check initial cohort
# compare the following stats with general_stats file for cycle 0
init_cohort_input <- read.csv("input1/init_cohort.csv")
stat_input <- matrix(nrow = ceiling(imax/2), ncol = (2+lmax))
for (i in 1:(ceiling(imax/2)))
{
  block_name <- block[i]
  stat_input[i,1] <- sum(init_cohort_input$counts[which(init_cohort_input$block==block_name)])
  stat_input[i,2] <- sum(init_cohort_input$counts[which(init_cohort_input$block==block_name & init_cohort_input$sex==sex[1])])
  for (l in 1:lmax) 
  {
    stat_input[i,l+2] <- sum(init_cohort_input$counts[which(init_cohort_input$block==block_name & init_cohort_input$oud==oud[l])])
  }
}
stat_output <- read.csv("output1/general_stats1.csv")
stat_output <- data.matrix(stat_output)
stat_output <- stat_output[-(ceiling(imax/2)+1):-imax,2:(3+lmax)]

isTRUE(all.equal(stat_input,stat_output, check.attributes=FALSE))
# ------------------------------------------------------------------------------------------------------------------
# Check aging module
aging_cycles <- c(3,8,13,18,23,28,33,38,44)
aging_cycles <- aging_cycles  + 1  # 1st row is cycle 0
size_after_aging <- matrix(nrow=simulation_duration+1, ncol = dim(out$general_outputs)[2])
size_after_aging[1,] <- out$general_outputs[1,]

for (c in 2:(simulation_duration+1))
{
  if (c %in% aging_cycles)
  {
    size_after_aging[c,] <- shift(out$general_outputs[c-1,],kmax*lmax, fill = 0)
    for (i in seq(1,imax*jmax*kmax*lmax,jmax*kmax*lmax))
    {
      size_after_aging[c,i:(i+jmax*kmax*lmax-1)] <- shift(out$general_outputs[c-1,i:(i+jmax*kmax*lmax-1)],kmax*lmax, fill = 0)
    }
  }
  else {
    size_after_aging[c,] <- out$general_outputs[c-1,]
  }
}
# -------------------------------------------------------------------------------------------------------------------
# Check entering cohort 
size_after_entering_cohort <- size_after_aging
interval_id <- 1
interval_floor <- 1
interval_ceiling <- 6

for (c in interval_floor:interval_ceiling)
{
  size_after_entering_cohort[c+1,seq(1,jmax*kmax*lmax,lmax)] <- size_after_aging[c+1,seq(1,jmax*kmax*lmax,lmax)] + entering_cohort_matrix[,interval_id]
}

isTRUE(all.equal(round(size_after_entering_cohort, digits=12), round(out$general_outputs,digits=12), check.attributes=FALSE))
# -------------------------------------------------------------------------------------------------------------------
# check oud_trans module
num_cmp <- imax*jmax*kmax*lmax
tmp <- matrix(nrow = num_cmp, ncol = lmax)
size_after_oud_trans <- matrix(nrow = simulation_duration+1, ncol = num_cmp)
size_after_oud_trans[1,] <- out$general_outputs[1,]
for (c in 2:(simulation_duration+1))
{
  for (it in 1:lmax)
  {
    tmp[,it] <- matrix(size_after_entering_cohort[c,], ncol = 1)*oud_trans_matrix[,it]
  }
  for (it in 1:lmax)
  {
    for (i in seq(1,num_cmp,lmax))
    {
      size_after_oud_trans[c,i:(i+lmax-1)] <- apply(tmp[i:(i+lmax-1),], 2, sum)
    }
  }
}
isTRUE(all.equal(round(size_after_oud_trans, digits=12), round(out$general_outputs,digits=12), check.attributes=FALSE))
# -------------------------------------------------------------------------------------------------------------------
# Checking block transition 
active_size <- lmax/2
num_trts <- (imax-1)/2
b_size <- imax*lmax      #here block is considered as 1 age/sex combo with all OUD states.
bmax <- jmax*kmax   # indicates for how many blocks we want to check the results
trt <- matrix(data = NA, nrow=imax, ncol=jmax*kmax*lmax)   # +1 for cycle column
B <- matrix(data = NA, nrow=imax*jmax*kmax*lmax, ncol=num_trts+2)  # here matrix B includes all trts + no_trt + corresponding_post_trt
final_trt <- matrix(data = NA, nrow=imax, ncol=bmax*lmax)   
num_admission_trts <- matrix(0, nrow = simulation_duration+1, ncol = floor(imax/2))
size_after_blk_trans <- matrix(nrow = simulation_duration+1,ncol=num_cmp)
size_after_blk_trans[1,] <- out$general_outputs[1,]
# adjust for time-varying block trans
cols <- num_trts+2
interval_id <- 1
interval_floor <- 1           
interval_ceiling <- 3

#for (cycle in 2:(simulation_duration+1)) # starting from 2, to eliminate cycle 0
for (cycle in (interval_floor+1):(interval_ceiling+1))
{
  block_trans_matrix_tmp <- block_trans_matrix[,((interval_id-1)*cols+1):(interval_id*cols)]
  for (i in 1:imax)   # read simulation outputs
  {
    trt[i,] <- size_after_oud_trans[cycle,((i-1)*kmax*jmax*lmax+1):(i*num_cmp/imax)]
  }
  it <- 1
  for (j in 1:(jmax*kmax*lmax)) # create matrix B
  {
    for (i in 1:imax)
    {
      B[it,] <- trt[i,j]*block_trans_matrix_tmp[it,]
      it <- it+1
    }
  }
  
  for (b in 0:(bmax-1)) {
    for (l in 1:active_size)
    {
      for (i in 1:(num_trts+1))
      {
        final_trt[i,(b*lmax+l)] <- (B[(b*b_size+(l-1)*imax+i),i]  +  (sum(B[(b*b_size+(l-1)*imax+1):(b*b_size+(l-1)*imax+imax),i])-B[(b*b_size+(l-1)*imax+i),i])*block_init_effect_matrix[l,i]
                                    +  (sum(B[(b*b_size+(l-1)*imax+imax*active_size+1):(b*b_size+(l-1)*imax+imax*active_size+imax),i])- B[(b*b_size+(l-1)*imax+imax*active_size+i),i])*(1-block_init_effect_matrix[(l+active_size),i]))
        if (i >1)
        {
          final_trt[i+num_trts,(b*lmax+l)] <- (B[(b*b_size+(l-1)*imax+i+num_trts),ncol(B)]
                                               + B[(b*b_size+(l-1)*imax+i),ncol(B)]*block_init_effect_matrix[l,i+num_trts]
                                               + B[(b*b_size+(l-1)*imax+i+imax*active_size),ncol(B)]*(1-block_init_effect_matrix[l+active_size,i+num_trts]))
          
        }
        if ( i > 1)
        {
          num_admission_trts[cycle,i-1] <- num_admission_trts[cycle,i-1] + (sum(B[(b*b_size+(l-1)*imax+1):(b*b_size+(l-1)*imax+imax),i])-B[(b*b_size+(l-1)*imax+i),i])
        }
      }
    }
    
    for (l in (1+active_size):lmax)
    {
      for (i in 1:(num_trts+1))
      {
        final_trt[i,(b*lmax+l)] <- (B[(b*b_size+(l-1)*imax+i),i]  +  (sum(B[(b*b_size+(l-1)*imax+1):(b*b_size+(l-1)*imax+imax),i])-B[(b*b_size+(l-1)*imax+i),i])*block_init_effect_matrix[l,i]
                                    +  (sum(B[(b*b_size+(l-1)*imax-imax*active_size+1):(b*b_size+(l-1)*imax-imax*active_size+imax),i])- B[(b*b_size+(l-1)*imax-imax*active_size+i),i])*(1-block_init_effect_matrix[(l-active_size),i]))
        
        if (i >1)
        {
          final_trt[i+num_trts,(b*lmax+l)] <- (B[(b*b_size+(l-1)*imax+i+num_trts),ncol(B)]
                                               + B[(b*b_size+(l-1)*imax+i),ncol(B)]*block_init_effect_matrix[l,i+num_trts]
                                               + B[(b*b_size+(l-1)*imax+i-imax*active_size),ncol(B)]*(1-block_init_effect_matrix[l-active_size,i+num_trts]))
          
        }
        if ( i > 1)
        {
          num_admission_trts[cycle,i-1] <- num_admission_trts[cycle,i-1] + (sum(B[(b*b_size+(l-1)*imax+1):(b*b_size+(l-1)*imax+imax),i])-B[(b*b_size+(l-1)*imax+i),i])
        }
      }
    }
  }
  tmp <- 0
  for (i in 1:imax)
  {
    tmp <- c(tmp,final_trt[i,])
  }
  size_after_blk_trans[cycle,] <- tmp[-1]
}
isTRUE(all.equal(round(size_after_blk_trans, digits=12), round(out$general_outputs,digits=12), check.attributes=FALSE))

# -------------------------------------------------------------------------------------------------------------------
# check overdose module
tmp1 <- which(substring(general_IDs, 8, 9) == oud_idx[1])
tmp2 <- which(substring(general_IDs, 8, 9) == oud_idx[2])
#tmp3 <- which(substring(general_IDs, 8, 9) == oud_idx[3])
tmp <- sort(c(tmp1,tmp2))
#tmp <- sort(c(tmp1))
all_overdose <- matrix(nrow = simulation_duration+1, ncol = dim(out$overdose_outputs)[2])
size_after_overdose <- size_after_blk_trans
all_overdose[1,] <- 0
size_after_overdose[1,] <-out$general_outputs[1,]
active_ouds <- size_after_blk_trans[,tmp]
interval_id <- 1
interval_floor <- 1
interval_ceiling <- 6
#checking overdose output
for (c in interval_floor:interval_ceiling)
{
  all_overdose[c+1,] <- active_ouds[c+1,]*all_types_overdose_matrix[,interval_id]
  print(isTRUE(all.equal(round(all_overdose[c+1,] ,digits = 12),round(out$overdose_outputs[c+1,],digits = 12))))
}
# checking compartment size after overdose
for (c in interval_floor:interval_ceiling)
{
  size_after_overdose[c+1,tmp] <- active_ouds[c+1,]-(all_overdose[c+1,]*fatal_overdose_vec[interval_id])
}
# -------------------------------------------------------------------------------------------------------------------
# check mortality module
# check mortality probabilities
mort_input <- mort_vec
# check mortality output (other modules are turned off)
num_of_death <- matrix(nrow = simulation_duration+1, ncol = num_cmp)
num_of_death[1,] <- 0
size_after_mortality <- matrix(nrow = simulation_duration+1, ncol = num_cmp)
size_after_mortality[1,] <- out$general_outputs[1,]
for (c in 2:(simulation_duration+1))
{
  num_of_death[c,] <- size_after_overdose[c,]*mort_input
}
# check mortality output
for (c in 1:(simulation_duration+1))
{
  print(isTRUE(all.equal(round(num_of_death[c,],digits = 12),round(out$mortality_outputs[c,],digits = 12))))
}
for (c in 2:(simulation_duration+1))
{
  size_after_mortality[c,] <- size_after_overdose[c,] - num_of_death[c,]
}
#--------------------------------------------------------------------------------------------------------------------
# copmaring final size
for (c in 1:(simulation_duration+1))
{
  print(isTRUE(all.equal(round(size_after_mortality[c,],digits = 12),round(out$general_outputs[c,],digits = 12))))
}
# comparing admission to detox
isTRUE(all.equal(round(num_admission_trts,digits = 12),round(out$admission_to_trts, digits = 12)))

# -------------------------------------------------------------------------------------------------------------------
# check cost/life module
# treatment utilization and pharmaceutical cost
trts <- size_after_blk_trans[,(1+num_cmp/imax):((1+num_trts)*num_cmp/imax)]
trt_blk_start_id <- seq(1,dim(trts)[2],num_cmp/imax)
trt_size <- matrix(nrow = simulation_duration, ncol= num_trts)
for (c in 1:simulation_duration)
{
  for (i in 1:length(trt_blk_start_id))
  {
    trt_size[c,i] <- sum(trts[c+1,trt_blk_start_id[i]:(trt_blk_start_id[i]-1+num_cmp/imax)])
  }
}

trt_util_cost1_r <- matrix(nrow = simulation_duration, ncol= num_trts)
pharma_cost1_r <- matrix(nrow = simulation_duration, ncol= num_trts)
trt_util_cost2_r <- matrix(nrow = simulation_duration, ncol= num_trts)
pharma_cost2_r <- matrix(nrow = simulation_duration, ncol= num_trts)
trt_util_cost3_r <- matrix(nrow = simulation_duration, ncol= num_trts)
pharma_cost3_r <- matrix(nrow = simulation_duration, ncol= num_trts)

for (c in 1:simulation_duration)
{
  trt_util_cost1_r[c,] <- trt_size[c,] * treatment_utilization_cost[,1]
  pharma_cost1_r[c,] <- trt_size[c,] * pharmaceutical_cost[,1]
  trt_util_cost2_r[c,] <- trt_size[c,] * treatment_utilization_cost[,2]
  pharma_cost2_r[c,] <- trt_size[c,] * pharmaceutical_cost[,2]
  trt_util_cost3_r[c,] <- trt_size[c,] * treatment_utilization_cost[,3]
  pharma_cost3_r[c,] <- trt_size[c,] * pharmaceutical_cost[,3]
}

trt_util_cost_r <- cbind(trt_util_cost1_r,trt_util_cost2_r,trt_util_cost3_r)
pharma_cost_r <- cbind(pharma_cost1_r,pharma_cost2_r,pharma_cost3_r)
# -------------------------------------------------------------------------------------------------------------------
# healthcare utilization cost
blk_start_id <- seq(1,num_cmp,num_cmp/imax)
healthcare_util_cost1_r <- matrix(nrow = simulation_duration, ncol= imax)
healthcare_util_cost2_r <- matrix(nrow = simulation_duration, ncol= imax)
healthcare_util_cost3_r <- matrix(nrow = simulation_duration, ncol= imax)
for (c in 1:simulation_duration)
{
  for (i in 1:length(blk_start_id))
  {
    healthcare_util_cost1_r[c,i] <- sum(size_after_blk_trans[c+1,blk_start_id[i]:(blk_start_id[i]-1+num_cmp/imax)] * healthcare_utilization_cost[blk_start_id[i]:(blk_start_id[i]-1+num_cmp/imax),1])
    healthcare_util_cost2_r[c,i] <- sum(size_after_blk_trans[c+1,blk_start_id[i]:(blk_start_id[i]-1+num_cmp/imax)] * healthcare_utilization_cost[blk_start_id[i]:(blk_start_id[i]-1+num_cmp/imax),2])
    healthcare_util_cost3_r[c,i] <- sum(size_after_blk_trans[c+1,blk_start_id[i]:(blk_start_id[i]-1+num_cmp/imax)] * healthcare_utilization_cost[blk_start_id[i]:(blk_start_id[i]-1+num_cmp/imax),3])
  }
}
healthcare_util_cost_r <- cbind(healthcare_util_cost1_r,healthcare_util_cost2_r,healthcare_util_cost3_r)
# -------------------------------------------------------------------------------------------------------------------
# overdose cost
blk_start_id <- seq(1,num_cmp/2,jmax*kmax*lmax/2)
od_cost1_r <- matrix(nrow = simulation_duration, ncol= imax)
od_cost2_r <- matrix(nrow = simulation_duration, ncol= imax)
od_cost3_r <- matrix(nrow = simulation_duration, ncol= imax)

interval_id <- 1
interval_floor <- 1
interval_ceiling <- 6

#for (c in 1:simulation_duration)
for (c in interval_floor:interval_ceiling)  
{
  for (i in 1:length(blk_start_id))
  {
    od_cost1_r[c,i] <- sum(out$overdose_outputs[c+1,blk_start_id[i]:(blk_start_id[i]-1+jmax*kmax*lmax/2)]) * 
      (fatal_overdose_vec[interval_id] * overdose_cost[2,1] + (1-fatal_overdose_vec[interval_id]) * overdose_cost[1,1])
    od_cost2_r[c,i] <- sum(out$overdose_outputs[c+1,blk_start_id[i]:(blk_start_id[i]-1+jmax*kmax*lmax/2)]) * 
      (fatal_overdose_vec[interval_id] * overdose_cost[2,2] + (1-fatal_overdose_vec[interval_id]) * overdose_cost[1,2])
    od_cost3_r[c,i] <- sum(out$overdose_outputs[c+1,blk_start_id[i]:(blk_start_id[i]-1+jmax*kmax*lmax/2)]) * 
      (fatal_overdose_vec[interval_id] * overdose_cost[2,3] + (1-fatal_overdose_vec[interval_id]) * overdose_cost[1,3])
  }
}
od_cost_r <- cbind(od_cost1_r,od_cost2_r,od_cost3_r)

#--------------------------------------------------------------------------------------------------------------------
# get and compare accumulated costs to simulation outputs

acc_cost_index <- seq(1,simulation_duration,periods)

# healthcare utilization cost
acc_healthcare_util_cost_r <- matrix(nrow = length(acc_cost_index), ncol = imax*length(cost_perspectives))
for (i in 1:length(acc_cost_index))
{
  acc_healthcare_util_cost_r[i,] <- colSums(healthcare_util_cost_r[acc_cost_index[i]:(acc_cost_index[i]-1+periods),])
}
isTRUE(all.equal(acc_healthcare_util_cost_r,out$healthcare_utilization_cost))

# pharmaceutical and treatment utilization cost
acc_trt_util_cost_r <- matrix(nrow = length(acc_cost_index), ncol = num_trts*length(cost_perspectives))
acc_pharma_cost_r <- matrix(nrow = length(acc_cost_index), ncol = num_trts*length(cost_perspectives))
for (i in 1:length(acc_cost_index))
{
  acc_trt_util_cost_r[i,] <- colSums(trt_util_cost_r[acc_cost_index[i]:(acc_cost_index[i]-1+periods),])
  acc_pharma_cost_r[i,] <- colSums(pharma_cost_r[acc_cost_index[i]:(acc_cost_index[i]-1+periods),])
}
isTRUE(all.equal(acc_trt_util_cost_r,out$treatment_utilization_cost))
isTRUE(all.equal(acc_pharma_cost_r,out$pharmaceutical_cost))

# overdose cost
acc_od_cost_r <- matrix(nrow = length(acc_cost_index), ncol = length(cost_perspectives)*imax)
for (i in 1:length(acc_cost_index))
{
  acc_od_cost_r[i,] <- colSums(od_cost_r[acc_cost_index[i]:(acc_cost_index[i]-1+periods),])
}
isTRUE(all.equal(acc_od_cost_r,out$overdose_cost))

# -------------------------------------------------------------------------------------------------------------------
# total cost and discounted cost 
total_cost1_r <- rep(0, simulation_duration/periods)
total_cost2_r <- rep(0, simulation_duration/periods)
total_cost3_r <- rep(0, simulation_duration/periods)

for (c in 1:(simulation_duration/periods))
{
  total_cost1_r[c] <- (sum(acc_healthcare_util_cost_r[c,1:imax]) + sum(acc_trt_util_cost_r[c,1:num_trts])
                       + sum(acc_pharma_cost_r[c,1:num_trts]) + sum(acc_od_cost_r[c,1:imax]))
  
  total_cost2_r[c] <- (sum(acc_healthcare_util_cost_r[c,(imax+1):(2*imax)]) + sum(acc_trt_util_cost_r[c,(num_trts+1):(2*num_trts)])
                       + sum(acc_pharma_cost_r[c,(num_trts+1):(2*num_trts)]) + sum(acc_od_cost_r[c,(imax+1):(2*imax)]))
  
  total_cost3_r[c] <- (sum(acc_healthcare_util_cost_r[c,(2*imax+1):(3*imax)]) + sum(acc_trt_util_cost_r[c,(2*num_trts+1):(3*num_trts)])
                       + sum(acc_pharma_cost_r[c,(2*num_trts+1):(3*num_trts)]) + sum(acc_od_cost_r[c,(2*imax+1):(3*imax)]))
}

total_cost_r <- cbind(total_cost1_r,total_cost2_r,total_cost3_r)

output_file <- read.csv("output1/cost_life/total_costs1.csv")
output_file <- as.matrix(output_file[,2:ncol(output_file)])
total_cost_output <- matrix(nrow = simulation_duration/periods, ncol=length(cost_perspectives))
for (c in 1:nrow(total_cost_output))
{
  for (i in 1:length(cost_perspectives))
  {
    total_cost_output[c,i] <- sum(output_file[c,((i-1)*imax+1):(i*imax)])
  }
}
isTRUE(all.equal(total_cost_output,total_cost_r,check.attributes =FALSE))

total_cost <- colSums(total_cost_r)
output_file <- read.csv("output1/cost_life/CE_costs1.csv")
output_file <- as.vector(output_file[,2])
isTRUE(all.equal(output_file,total_cost,check.attributes =FALSE))

total_sum_of_all <- sum(healthcare_util_cost_r) + sum(od_cost_r) + sum(trt_util_cost_r) + sum(pharma_cost_r)
total_sum_of_all_output <- sum(output_file)
isTRUE(all.equal(total_sum_of_all_output,total_sum_of_all))

# Discounting
# For this part assume acc_costs are the the same as non_acc costs, in the other words, periods=1
acc_healthcare_util_cost_r <- healthcare_util_cost_r
acc_trt_util_cost_r <- trt_util_cost_r
acc_pharma_cost_r <- pharma_cost_r
acc_od_cost_r <- od_cost_r
periods <- 1

# recalculate total_cost

disc_cost1 <- 0
disc_cost2 <- 0
disc_cost3 <- 0
for (c in 1:simulation_duration)
{
  disc_cost1 <- disc_cost1 + total_cost_r[c,1]/(1+discounting_rate)^c
  disc_cost2 <- disc_cost2 + total_cost_r[c,2]/(1+discounting_rate)^c
  disc_cost3 <- disc_cost3 + total_cost_r[c,3]/(1+discounting_rate)^c
}
disc_cost <- c(disc_cost1,disc_cost2,disc_cost3)
isTRUE(all.equal(disc_cost,out$total_cost_per_perspective[,2], check.attributes = FALSE))

#check printed outputs
input_cost <- read.csv("output1/cost_life/healthcare_utilization_cost1.csv")
input_cost <- data.matrix(input_cost)
input_cost <- input_cost[,2:ncol(input_cost)]
isTRUE(all.equal(input_cost,acc_healthcare_util_cost_r,check.attributes=FALSE))

input_cost <- read.csv("output1/cost_life/pharmaceutical_cost1.csv")
input_cost <- data.matrix(input_cost)
input_cost <- input_cost[,2:ncol(input_cost)]
isTRUE(all.equal(input_cost,acc_pharma_cost_r,check.attributes=FALSE))

input_cost <- read.csv("output1/cost_life/overdose_cost1.csv")
input_cost <- data.matrix(input_cost)
input_cost <- input_cost[,2:ncol(input_cost)]
isTRUE(all.equal(input_cost,acc_od_cost_r,check.attributes=FALSE))

input_cost <- read.csv("output1/cost_life/treatment_utilization_cost1.csv")
input_cost <- data.matrix(input_cost)
input_cost <- input_cost[,2:ncol(input_cost)]
isTRUE(all.equal(input_cost,acc_trt_util_cost_r,check.attributes=FALSE))



