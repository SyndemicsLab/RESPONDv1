//[[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <string>
#include <iostream>
#include <fstream>
#include <math.h>
#include <algorithm>
#include <vector>
#include <cmath>
using namespace Rcpp;
using namespace std;

// Here, i, j, k & l represent block, age bracket, gender and OUD status respectively.

// [[Rcpp::export]]
List sim (
 const NumericVector init_demographics_vec,
 const NumericMatrix entering_cohort_matrix, const NumericVector entering_cohort_cycles,
 const NumericMatrix OUD_trans_matrix,
 const NumericVector block_trans_cycles, NumericMatrix block_trans_matrix_all, const NumericMatrix block_init_eff_matrix,
 const NumericVector overdose_cycles, const NumericMatrix overdose_matrix,const NumericVector fatal_overdose_vec,
 const NumericVector mort_vec,
 const int imax,const int jmax, const int kmax, const int lmax,
 const int duration, const int cycles_in_age_brackets, const int periods,
 const NumericMatrix health_util_cost_matrix, const NumericMatrix trt_util_cost_matrix, const NumericMatrix med_cost_matrix, 
 const NumericMatrix od_cost_matrix, const NumericMatrix util_matrix,
 const double discouting_rate)
  {
    // ------------------------------------------------------------------------------------------------------------
    // initialization
    int num_trts = (imax-1)/2;                // Number of actual trt episodes, excluding no_trt and post_trts
    int num_active_oud=lmax/2;                // Number of active OUD states
    int cost_perspectives = health_util_cost_matrix.ncol();
    double cohort[duration+1][imax][jmax][kmax][lmax];
    double overdose[duration+1][imax][jmax][kmax][num_active_oud];
    double mortality[duration+1][imax][jmax][kmax][lmax];
    NumericMatrix num_admission_trts(duration+1,num_trts);  // number of admissions to actual treatment episodes, excluding no_trt and post_trts
    //block transition matrix for each time-varying interval. It is initialized by 1st interval values.
    NumericMatrix block_trans_matrix = block_trans_matrix_all(Range(0,block_trans_matrix_all.nrow()-1),Range(0,num_trts+2-1));
    int blk_trans_cycle_idx = 0;
    // initializing cost vectors/matrices
    double health_util_cost[duration/periods][imax][cost_perspectives];
    double od_cost[duration/periods][imax][cost_perspectives];
    double trt_util_cost[duration/periods][num_trts][cost_perspectives];
    double med_cost[duration/periods][num_trts][cost_perspectives];
    NumericMatrix total_cost(cost_perspectives,2);
    NumericMatrix util(3,2);

    int it=0;                     // the counter that moves from the first row of input vector/matrix to its end
    int it2=0;                    // second counter to be used by some modules
    double tmp=0;                 // temporary variable to be used by various modules
    double tmp2=0;                // temporary variable to be used by various modules
    int cost_period_idx = 0;      // index representing cost/life period
    double fatal_od = 0;
    double non_fatal_od = 0;
    
    for (int i=0; i < imax; ++i)            //for each block
      for (int j=0; j < jmax; ++j)          // for each age bracket
        for (int k=0; k < kmax; ++k)        // for each gender
          for (int l=0; l < lmax; ++l)      // for each OUD status
          {
            if (i < num_trts+1)             // for actual treatment episodes + no_treatment episode
            {
              cohort[0][i][j][k][l] = init_demographics_vec[it];
              ++it;
            }
            else
              cohort[0][i][j][k][l] = 0;      // initialize post_treatment episodes with 0s

            mortality[0][i][j][k][l] = 0;     // no mortality at cycle 0

            if (l < num_active_oud)
              overdose[0][i][j][k][l] = 0;    // no overdose at cycle 0
          }
          
    // initializing cost arrays to 0      
    for (int i=0; i < duration/periods; ++i)
      for (int ii=0; ii < imax; ++ii)
        for (int iii=0; iii < cost_perspectives; ++iii)
        {
          health_util_cost[i][ii][iii] = 0;
          od_cost[i][ii][iii] = 0;
        }
 
    for (int i=0; i < duration/periods; ++i)
      for (int ii=0; ii < num_trts; ++ii)
        for (int iii=0; iii < cost_perspectives; ++iii)
        {
          trt_util_cost[i][ii][iii] = 0;
          med_cost[i][ii][iii] = 0;
        }         
    
    // calculating aging cycles
    NumericVector aging_cycles(1+floor((duration-floor(cycles_in_age_brackets/2+1))/cycles_in_age_brackets));
    for (int c=0; c < aging_cycles.size(); ++c)
    {
      aging_cycles[c] = floor((2*c+1)*cycles_in_age_brackets/2+1);
    }
    // -------------------------------------------------------------------------------------------
    for (int cycle=1; cycle < duration+1; ++cycle)
    {
      // filling cohort array at new cycle with the last state of previous cycle. It will then be updated by different modules.
      for (int i=0; i < imax; ++i)            // for each block
        for (int j=0; j < jmax; ++j)          // for each age bracket
          for (int k=0; k < kmax; ++k)        // for each gender
            for (int l=0; l < lmax; ++l)      // for each OUD status
              cohort[cycle][i][j][k][l]=cohort[cycle-1][i][j][k][l];
      //
      if (cycle > 1)
      {
        if (periods == 1)
          ++cost_period_idx;
        else if ((cycle % periods) == 1)
          ++cost_period_idx;
      }
      // ---------------------------------------------------------------------------------------
      // Aging module
      if (std::find(aging_cycles.begin(), aging_cycles.end(), cycle) != aging_cycles.end())
      {
        for (int i=0; i < imax; ++i)          // for each block
          for (int k=0; k < kmax; ++k)        // for each gender
            for (int l=0; l < lmax; ++l)      // for each OUD status
            {
              for (int j=1; j < jmax; ++j)    // for each age bracket, excluding the first one
                cohort[cycle][i][j][k][l] =  cohort[cycle-1][i][j-1][k][l] ;

              cohort[cycle][i][0][k][l] = 0;  // empty first age bracket
            }
      }
      // ----------------------------------------------------------------------------------------
      // Entering cohort
      int cycle_idx = -1;

      for (int i=0; i < entering_cohort_cycles.size(); ++i)     // getting entering cohort size based on the cycle
      {
        if (cycle_idx == -1 and cycle <= entering_cohort_cycles[i])
          cycle_idx = i;
      }
  
      it = 0;
      for (int j=0; j < jmax; ++j)            // add the entering cohort to each compartment
        for (int k=0; k < kmax; ++k)
        {
            cohort[cycle][0][j][k][0] += entering_cohort_matrix(it,cycle_idx);
            ++it;
        }
      // ----------------------------------------------------------------------------------------
      // OUD transition
      double A[lmax][lmax];         //number of persons moving between different OUD states.
      // each element is multiplication of a cell in M and its corresponding cohort size.
      it=0;                         //start from the first row of OUD_trans_matrix or "M"
      for (int i=0; i < imax; ++i)
        for (int j=0; j < jmax; ++j)
          for (int k=0; k < kmax; ++k)
          {
            for (int l=0; l < lmax; ++l)
            { //updating matrix A
              for (int x=0; x < lmax; ++x)
                A[l][x] = cohort[cycle][i][j][k][l]*OUD_trans_matrix(it,x);
              ++it;
            }
            for (int l=0; l < lmax; ++l)
            {
              tmp=0;
              for (int x=0; x < lmax; ++x)  // sum of columns in matrix A which represents the final population of OUD states
                tmp += A[x][l];             // transpose of matrix A
              cohort[cycle][i][j][k][l] = tmp;
            }
          }
      // -----------------------------------------------------------------------------------------------------------
      // Block transition
      // In this part, in order to make it easier to understand, <=  criteria is used for block loops.

      cycle_idx = -1;
      for (int i=0; i < block_trans_cycles.size(); ++i)  // find the time-varying interval
      {
        if (cycle_idx == -1 and cycle <= block_trans_cycles[i])
          cycle_idx = i;
      }
      if (blk_trans_cycle_idx != cycle_idx)   // update only if the interval has changed
      {
        blk_trans_cycle_idx = cycle_idx;
        block_trans_matrix = block_trans_matrix_all(Range(0,block_trans_matrix_all.nrow()-1),Range(blk_trans_cycle_idx*(num_trts+2),(blk_trans_cycle_idx+1)*(num_trts+2)-1));
      }
      
      double B[imax][num_trts+2];         // 1 for no_trt and 1 for corresponding_post_trt
      double trt_init_effected_grp[num_active_oud][imax];  //groups that block initiation would change their OUD status
      it=0;                                 // reset the iterator
      for (int j=0; j < jmax; ++j)
        for (int k=0; k < kmax; ++k)
          for (int l=0; l < lmax; ++l)
          {
            for (int i=0; i <= imax-1; ++i)  // updating matrix B
            {
              for (int x=0; x <= num_trts + 2 - 1; ++x)  // adding 1 for no_trt and 1 for corresponding_post_trt, -1 for index adjustment
                 B[i][x] = cohort[cycle][i][j][k][l] * block_trans_matrix(it,x);
              ++it;
            }

            for (int i=0; i <= num_trts + 1 - 1; ++i)  // adding 1 for no_trt, excluding corresponding post_trt, -1 for index adjustment
            {
              tmp=0;
              tmp2=0;
              for (int x=0; x <= imax-1; ++x)              // sum of columns in matrix B which represents the final population of blocks
              {
                if (x != i)
                  tmp += B[x][i];
                else
                  tmp2 = B[x][i];
              }

              if (i > 0)                                    // excluding no_trt
              {
                num_admission_trts(cycle,i-1) += tmp;        // adding number of admissions to each actual treatment
              }

              cohort[cycle][i][j][k][l] = tmp2 + tmp * block_init_eff_matrix(l,i);
              if (l < num_active_oud)
                trt_init_effected_grp[l][i] = tmp * (1-block_init_eff_matrix(l,i));
              else
              {
                cohort[cycle][i][j][k][l-num_active_oud] += tmp * (1-block_init_eff_matrix(l,i));
                cohort[cycle][i][j][k][l] += trt_init_effected_grp[l-num_active_oud][i];
              }
            }
            // for post_trts
            for (int x=1; x <= num_trts; ++x)          // excluding no_trt
            {
              cohort[cycle][x+num_trts][j][k][l] = B[x][num_trts+1] * block_init_eff_matrix(l,x+num_trts) + B[num_trts+x][num_trts+1];
              if (l < num_active_oud)
                trt_init_effected_grp[l][x+num_trts]  = B[x][num_trts+1] * (1-block_init_eff_matrix(l,x+num_trts));
              else
              {
                cohort[cycle][x+num_trts][j][k][l-num_active_oud] += B[x][num_trts+1] * (1-block_init_eff_matrix(l,x+num_trts));
                cohort[cycle][x+num_trts][j][k][l] += trt_init_effected_grp[l-num_active_oud][x+num_trts];
              }
            }
          }
      // -----------------------------------------------------------------------------------------------------------
      // Cost/life module
      if (health_util_cost_matrix(0,0) != -1)
      {
        double size = 0;
        double util_min=0, util_mult=0;
        it = 0;
        for (int i=0; i < imax; ++i)
        {
            for (int j=0; j < jmax; ++j)
              for (int k=0; k < kmax; ++k)
                for (int l=0; l < lmax; ++l)
                {
                  size = cohort[cycle][i][j][k][l];
                  for (int ii=0; ii < cost_perspectives; ++ii)
                  { // healthcare utilization cost
                    tmp = size *  health_util_cost_matrix(it,ii);
                    health_util_cost[cost_period_idx][i][ii] += tmp;
                    total_cost(ii,0) += tmp;
                    tmp2 = tmp/pow(1+discouting_rate, cycle);
                    total_cost(ii,1) += tmp2;

                    if (i > 0 and i < num_trts+1)
                    { // treatment utilization cost
                      tmp = size * trt_util_cost_matrix(i-1,ii);
                      trt_util_cost[cost_period_idx][i-1][ii] += tmp;
                      total_cost(ii,0) += tmp;
                      tmp2 = tmp/pow(1+discouting_rate, cycle);
                      total_cost(ii,1) += tmp2;
                      // pharmaceutical cost
                      tmp = size * med_cost_matrix(i-1,ii);
                      med_cost[cost_period_idx][i-1][ii] += tmp;
                      total_cost(ii,0) += tmp;
                      tmp2 = tmp/pow(1+discouting_rate, cycle);
                      total_cost(ii,1) += tmp2;
                    }
                  }
                  // life and utility
                  util(0,0) += size;
                  util(0,1) += size/pow(1+discouting_rate, cycle);
                  util_min = size * util_matrix(it,0);
                  util(1,0) += util_min;
                  util(1,1) += util_min/pow(1+discouting_rate, cycle); 
                  util_mult = size * util_matrix(it,1);
                  util(2,0) += util_mult;
                  util(2,1) += util_mult/pow(1+discouting_rate, cycle);
                  
                  ++it;
                }
       }
      } 
      // ------------------------------------------------------------------------------------------------------------------     
      // overdose module
      it=0;                         //start from the first element of overdose vector
      cycle_idx=-1;
      
      for (int i=0; i < overdose_cycles.size(); ++i)     // getting overdose values based on the cycle
      {
        if (cycle_idx == -1 and cycle <= overdose_cycles[i])
          cycle_idx = i;
      }
      
      for (int i=0; i < imax; ++i)
        for (int j=0; j < jmax; ++j)
          for (int k=0; k < kmax; ++k)
            for (int l=0; l < num_active_oud; ++l)
            {
              tmp = cohort[cycle][i][j][k][l] * overdose_matrix(it,cycle_idx);  //total number of all overdoses
              overdose[cycle][i][j][k][l] = tmp;
              //util[0][0] -= tmp * overdose_util;
              //util[0][1] -= (tmp * overdose_util)/pow(1+discouting_rate,cycle);
              fatal_od = tmp*fatal_overdose_vec[cycle_idx];
              non_fatal_od = tmp - fatal_od;
              cohort[cycle][i][j][k][l] -= fatal_od;
              if (health_util_cost_matrix(0,0) != -1)
              {
                for (int ii=0; ii < cost_perspectives; ++ii)
                { // overdose
                  tmp = non_fatal_od * od_cost_matrix(0,ii) + fatal_od * od_cost_matrix(1,ii);
                  od_cost[cost_period_idx][i][ii] += tmp;
                  total_cost(ii,0) += tmp;
                  tmp2 = tmp/pow(1+discouting_rate, cycle);
                  total_cost(ii,1) += tmp2;
                }
              }
              ++it;
            }
      // ----------------------------------------------------------------------------------------------------------
      // mortality module
      it=0;
      for (int i=0; i < imax; ++i)
        for (int j=0; j < jmax; ++j)
          for (int k=0; k < kmax; ++k)
            for (int l=0; l < lmax; ++l)
            {
              tmp = cohort[cycle][i][j][k][l] * mort_vec[it];
              cohort[cycle][i][j][k][l] -= tmp;
              mortality[cycle][i][j][k][l] = tmp;
              ++it;
            }
     // ---------------------------------------------------------------------------------------------------------
//cout<<"end of cycle "<<cycle <<"---------------------------------------------"<<endl;
    } //end of cycles

    // Return outputs
    NumericMatrix general_outputs(duration+1,imax*jmax*kmax*lmax);
    NumericMatrix overdose_outputs(duration+1,imax*jmax*kmax*num_active_oud);
    NumericMatrix mortality_outputs(duration+1,imax*jmax*kmax*lmax);
    
    NumericMatrix healthcare_util_cost(duration/periods,imax*cost_perspectives);
    NumericMatrix overdose_cost(duration/periods,imax*cost_perspectives);
    NumericMatrix treatment_util_cost(duration/periods,num_trts*cost_perspectives);
    NumericMatrix pharma_cost(duration/periods,num_trts*cost_perspectives);

    for (int t=0; t < duration+1; ++t)     // for each time step
    { it=0;
      it2=0;
      for (int i=0; i < imax; ++i)
        for (int j=0; j < jmax; ++j)
          for (int k=0; k < kmax; ++k)
            for (int l=0; l < lmax; ++l)
            {
              general_outputs(t,it)=cohort[t][i][j][k][l];
              mortality_outputs(t,it)=mortality[t][i][j][k][l];
              if (l < num_active_oud){
                overdose_outputs(t,it2)=overdose[t][i][j][k][l];
                ++it2;}
              ++it;
            }
    }
    
    // print healthcare utilization, fatal and non-fatal overdose costs
    if (health_util_cost_matrix(0,0) != -1)
    {
      for (int row=0; row < healthcare_util_cost.nrow(); ++row)
      {
        it=0;
        for (int i=0; i < cost_perspectives; ++i)
          for (int ii=0; ii < imax; ++ii)
          {
            healthcare_util_cost(row,it) = health_util_cost[row][ii][i];
            overdose_cost(row,it) = od_cost[row][ii][i];
            ++it;
          }
      }
      
      // print treatment utilization and pharmaceutical costs
      for (int row=0; row < healthcare_util_cost.nrow(); ++row)
      {
        it=0;
        for (int i=0; i < cost_perspectives; ++i)
          for (int ii=0; ii < num_trts; ++ii)
          {
            treatment_util_cost(row,it) = trt_util_cost[row][ii][i];
            pharma_cost(row,it) = med_cost[row][ii][i];
            ++it;
          }
      }
    } 

    if (health_util_cost_matrix(0,0) != -1)
    {
      return Rcpp::List::create(
        Rcpp::Named("general_outputs") = general_outputs,
        Rcpp::Named("overdose_outputs") = overdose_outputs,
        Rcpp::Named("mortality_outputs") = mortality_outputs,
        Rcpp::Named("admission_to_trts") = num_admission_trts,
        Rcpp::Named("healthcare_utilization_cost") = healthcare_util_cost,
        Rcpp::Named("treatment_utilization_cost") = treatment_util_cost,
        Rcpp::Named("pharmaceutical_cost") = pharma_cost,
        Rcpp::Named("overdose_cost") = overdose_cost,
        Rcpp::Named("total_cost_per_perspective") = total_cost,
        Rcpp::Named("utility") = util
      );
    }
    
    return Rcpp::List::create(
      Rcpp::Named("general_outputs") = general_outputs,
      Rcpp::Named("overdose_outputs") = overdose_outputs,
      Rcpp::Named("mortality_outputs") = mortality_outputs,
      Rcpp::Named("admission_to_trts") = num_admission_trts
    );

  }
