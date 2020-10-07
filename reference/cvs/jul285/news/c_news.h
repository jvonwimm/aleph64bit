C! 1st entry in C_SET

 ! JULIA 280
   CFPNMP, CRCJOB, CTHCLU : replace WRITE(6, by WRITE(IW(6),
                           (H. Drevermann, Feb 96)
   CTKCHG : opening "'" should have a closing "'" within the same
           line for cvs (F. Ranjard, Feb 96)

 * corr file 279.1
   CASHET : remove comments after *CA for LIGHT (P. Comas, Nov 95)
 ! JULIA 279

 ! JULIA 275
   CDANG : restrict COMN1 to [-1.,1.] to avoid precision problems
          (P. Comas, 18-OCT-1994)
   CTRPAR : avoid division by zero: no need to step in the helix if
           STEP = 0  (A. Bonissant,P. Comas, 29-SEP-1994)
 ! JULIA 272
   CINIJO : define POT name-indices even when no output file is required

