C! 1st entry in A-set
 ! JULIA 308
    AAMAIN : Change TIMEST value to get better timing resolution (O.Callot)

 ! JULIA 305
    AJMMCL : fix variable type inconsistencies in function calls,
             for Linux                            (A.Waananen)

 ! JULIA 303 correction file 7
    ALGTWA : Run 29977 also had bunch trains                 (M.Cattaneo)

 ! JULIA 302 (Tracking upgrade)
    AAMAIN : Increase length of IW array to 3M words (from 2M) (D.Casper)
    
 ! JULIA 281
   AAMAIN - Increase max number of BOS banks to 3000
   ABOLDR - remove include version.h, since we don't have correction files
            with CVS. JULRUN makes sure JULIA version of routine is picked up.

 ! JULIA 279
   make several cosmetic changes in several sets to move easily to CVS
   (F. Ranjard, Nov 95)

 ! JULIA 272
   AAMAIN - remove CRAY flag
            open JULIACARDS file if it exists

