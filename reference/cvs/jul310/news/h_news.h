C! 1st entry in H_set
 * corr file 307.1
    HCORFC : NEW. HCAL tower energy correction for the bug that was 
             present on the online software in 1996, 1997 and early 1998 data
                                                                   (A.Sciaba)
    HPRANA : Add call to HCORFC                                    (A.Sciaba)

 ! JULIA 305
    HCCONS.H : COMMON block is also defined in Alephlib with longer
               length. COPYed Alephlib definition to JULIA        (M.Cattaneo)
    HCLCRA : fix variable type inconsistencies in function calls, 
             for Linux                                            (A.Waananen)

 ! JULIA 304
    HVMASK : Get database from LRGEOM rather than LRCONS (A.Sciaba')

 * corr file 303.4
    HVMASK : Get run number with ABRUEV rather than from rcurnt.h  (A.Sciaba)

 ! JULIA 303
   HCMASK.H New - COMMON containing MASKHV array            (A.Sciaba)
   HINIRU : Add call to HVMASK                              (A.Sciaba)
   HVMASK : New - Fills MASKHV with HV masking information from HSSR 
                  Slow Control bank                         (A.Sciaba)
   HMROAD : Take into account info in MASKHV when calculating expected
            number of hits in HCAL                          (A.Sciaba)
 
 ! JULIA 280
   HPRANA,HPRANP,HPRDIG,HRCPAT,HTUBFI : opening "'" should have a
       closing "'" within the same line for cvs (F. Ranjard, Feb 96)

 ! JULIA 279

 ! JULIA 278
   HT0CJJ : new common deck added (A.Messineo, Aug 95)
   HTOWCA, HCATOW, HRDCAL: Barrel and endcap normalization
                          per wagon (A.Messineo, Aug 95)
   FTRACK, MUCUTS : protect to avoid overflow in PIBE2 (P.Comas, Aug 95)

 ! JULIA 272
 * corr file 271.6
   HMROAD : add protection.
 * corr file 271.1
   HCBHIS : HBPRO was called with a wrong number of arguments
