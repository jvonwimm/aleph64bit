C! 1st deck of MUONID
       Code for performing the official muon identification.
       Most of this is for internal use only.
       The function HDODGY may be used externally to deterimine whether
       or not a given run has good HCAL digital tube information

 ! ALEPHLIB 21.6
    MUREDO - Moved to Julia library (version 285) because it calls Julia
             routines (M.Cattaneo 19/02/97)

 ! ALEPHLIB 21.2
         ALPROB Fix a small bug: ALPROB gets the required numbers of
                random numbers according to the run, event number and
                seed. If the routine was called twice or more following
                with the same seed the result was not the same because a
                vector was not defined in the right way. (A.Gregorio)
         MRMHIT Fix a small bug: MRMHIT masks Monte Carlo bank MHIT to
                take into account muon chambers inefficiencies. During
                the 1993 the chambers 4C and 4D were off for a certain
                period and in order to consider this inefficiency inside
                MRMHIT a new bank (D4CD) was filled for 93 Monte Carlo.
                This bank was filled in the wrong position and was not
                present if a muon hit has not been fired. (A.Gregorio)
 ! ALEPHLIB 20.0
         MRMHIT treat chambers 4c,4d in 93 monte carlo , add bank D4CD to
                show this has been done
         MUIDO  declare format of D4CD
         MUREDO drop old D4CD and add new one to output list
