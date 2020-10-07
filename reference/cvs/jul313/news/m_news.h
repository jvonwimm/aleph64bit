C! 1st entry in M_set
 ! JULIA 313
    MPREDG : Revert chains x and y in astros 6A : 0C<->01: 
             Fixes miscabling in Y2K data since run 53000         (A.Venturi)

 * corr file 307.1
    MPREDG : If max no. of hits reached, fill MHIT up to max no. of hits
             rather than return empty MHIT                         (G.Bagliesi)

 ! JULIA 285
    MUREDO - Moved to Julia library from Alephlib (version 216) because it 
             calls Julia routines (A.Waananen 5/12/96, M.Cattaneo 19/02/97)

 ! JULIA 284
   MPREDG : Ignore bits 8-11 in data from new Astros (G.Bagliesi 9-10-96)

 ! JULIA 280
   MUASS,MUTEST : opening "'" should have a closing "'" within the same
           the same line for cvs (F. Ranjard, Feb 96)
   MUNEWR : change LOUTRL and LDEBRL to IW(6) instead of 6 (P.Comas,
           Feb 96)

 * corr file 279.2
   MUSLCM : bug which rejected 2% of the hits in the middle angle muon
           chambers fixed (A.Venturi, Dec 95)
 ! JULIA 279

 * corr file 275.5
   MPREDG: correction for bad cabling in first runs of 1994 data,
          from 25000 to 25895 (P.Campana, Feb 95)
 ! JULIA 275
 * corr file 274.1
   MUASS: allow to have the MUEX banks in the data even
          if there is no hit in the muon chambers (A.Venturi).

