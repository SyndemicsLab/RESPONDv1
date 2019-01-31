//[[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <string>
#include <iostream>
#include <fstream>
#include <math.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;
using namespace std;

// Here, i, j, k & l represent block, age bracket, gender and OUD status respectively.

// [[Rcpp::export]]
List sim (
 const NumericVector init_demographics_vec,
 const NumericMatrix entering_cohort_matrix, const NumericVector entering_cohort_cycles,
 const NumericMatrix OUD_trans_matrix,
 const NumericMatrix block_trans_matrix, const NumericMatrix block_init_eff_matrix, const int detox_id,
 const NumericMatrix overdose_matrix, const NumericMatrix fatal_overdose_matrix,
 const NumericVector mort_vec,
 const int imax,const int jmax, const int kmax, const int lmax,
 const int duration, const int cycles_in_age_brackets)
  {
    // ------------------------------------------------------------------------------------------------------------
    // initialization
    int num_trts = (imax-1)/2;                // Number of actual trt episodes, excluding no_trt and post_trts
    int num_active_oud=lmax/2;                // Number of active OUD states

    double cohort[duration+1][imax][jmax][kmax][lmax];
    double overdose[duration+1][imax][jmax][kmax][num_active_oud];
    double mortality[duration+1][imax][jmax][kmax][lmax];
    NumericMatrix num_admission_trts(duration+1,num_trts);  // number of admissions to actual treatment episodes, excluding no_trt and post_trts
      
    int it=0;                     // the counter that moves from the first row of input vector/matrix to its end
    int it2=0;                    // second counter to be used by some modules
    double tmp=0;                 // temporary variable to be used by various modules
    double tmp2=0;                // temporary variable to be used by various modules

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
          // calculating aging cycles
          NumericVector aging_cycles(1+floor(duration-floor(cycles_in_age_brackets/2+1))/cycles_in_age_brackets);
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
      double B[imax][num_trts+2];         // 1 for no_trt and 1 for corresponding_post_trt
      double trt_init_effected_grp[num_active_oud][imax];  //groups that blcok initiation would change their OUD status
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
      // overdose module
      it=0;                         //start from the first element of overdose vector
      cycle_idx=-1;
      bool cycle_status=false;

      for (int i=0; i < fatal_overdose_matrix.nrow(); ++i)     // finding the time_interval based on the cycle
      {
        if (cycle_status == false and cycle <= fatal_overdose_matrix(i,0))
        {
          cycle_idx = i;
          cycle_status = true;
        }
      }
      for (int i=0; i < imax; ++i)
        for (int j=0; j < jmax; ++j)
          for (int k=0; k < kmax; ++k)
            for (int l=0; l < num_active_oud; ++l)
            {
              tmp = cohort[cycle][i][j][k][l] * overdose_matrix(it,cycle_idx);
              overdose[cycle][i][j][k][l] = tmp;
              cohort[cycle][i][j][k][l] -= (tmp * fatal_overdose_matrix(cycle_idx,1));
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

    } //end of cycles

    // Return outputs
    NumericMatrix general_outputs;
    NumericMatrix overdose_outputs;
    NumericMatrix mortality_outputs;

    general_outputs=NumericMatrix(duration+1,imax*jmax*kmax*lmax);
    overdose_outputs=NumericMatrix(duration+1,imax*jmax*kmax*num_active_oud);
    mortality_outputs=NumericMatrix(duration+1,imax*jmax*kmax*lmax);

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
    return Rcpp::List::create(
      Rcpp::Named("general outputs") = general_outputs,
      Rcpp::Named("overdose outputs") = overdose_outputs,
      Rcpp::Named("mortality outputs") = mortality_outputs,
      Rcpp::Named("admission to trts") = num_admission_trts
      );

  }
