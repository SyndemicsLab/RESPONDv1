generate_output_IDs <- function(){
  # Initialize ID vectors
  block_idx <<- c(0:(imax-1))
  agegrp_idx <<- c(0:(jmax-1))
  sex_idx <<- c(0:(kmax-1))
  oud_idx <<- c(0:(lmax-1))
  
  #updating block IDs
  for (i in 1:imax)
  {
    if (i < 11)
    {
      block_idx[i] <<- paste("0",block_idx[i],sep='')
    }
  }
  
  #updateing age groups IDs
  for (j in 1:jmax)
  {
    if (j < 11)
    {
      agegrp_idx[j] <<- paste("0",agegrp_idx[j],sep='')
    }
  } 
  
  #updateing sex IDs
  for (k in 1:kmax)
  {
    if (k < 11)
    {
      sex_idx[k] <<- paste("0",sex_idx[k],sep='')
    }
  }   
  
  #updateing oud IDs
  for (l in 1:lmax)
  {
    if (l < 11)
    {
      oud_idx[l] <<- paste("0",oud_idx[l],sep='')
    }
  }   
  idx_perm <- expand.grid(oud_idx,sex_idx,agegrp_idx,block_idx)
  colnames(idx_perm) <- c("oud_idx","sex_idx","agegrp_idx","block_idx")
  idx_perm <- idx_perm[,c("block_idx","agegrp_idx","sex_idx","oud_idx")]
  general_IDs <<- paste("r",idx_perm$block_idx, idx_perm$agegrp_idx,idx_perm$sex_idx,idx_perm$oud_idx, sep="") 

  active_oud_idx <- oud_idx[-((lmax/2+1):lmax)]
  idx_perm <- expand.grid(active_oud_idx,sex_idx,agegrp_idx,block_idx)
  colnames(idx_perm) <- c("active_oud_idx","sex_idx","agegrp_idx","block_idx")
  idx_perm <- idx_perm[,c("block_idx","agegrp_idx","sex_idx","active_oud_idx")]
  active_oud_IDs <<- paste("r", idx_perm$block_idx, idx_perm$agegrp_idx, idx_perm$sex_idx, idx_perm$active_oud_idx, sep="") 
}


