 ! ALEPHLIB 31.6
    UBANGL : Bound angle by taking MOD, instead of DO loop!      (M.Cattaneo)
    UFSTAT : Protect against crazy values of ALP                 (M.Cattaneo)

 ! ALWPHLIB 31.3 correction file no.1
    UFTKAL: Protect against overflow in REAL*8 to REAL*4 conversion, 
            fix line longer than 72 characters                  (M.Cattaneo)

 ! ALEPHLIB 31.3
    UF2CRD,UF2TRK,UFDISC,UFJACO,UFLOSS,UFQPRC,UFTKAL,UFVDMS,UTPROC
     Double precision fixes for Linux                           (D.Smith)

 ! ALEPHLIB 30.6
    UFG2GT,UFTKAL : fix variable type inconsistencies in function calls, 
                    for Linux                            (A.Waananen)
    UFTKAL : Disable removal of outliers if track has 4 or less remaining
             3-D coordinates. Fail if numerical problem would reduce track
             to exactly 3 hits.                               (D.Casper)
      
 ! ALEPHLIB 30.5 correction file 2
    UFECAL : Remove CALL from function references, for Linux. (A.Waananen)

  ! ALEPHLIB 30.5 correction file 1
    UFGAIN - Protect against zero determinant
    UFTKAL - Fit the track (using a zero gain matrix for this point) 
             even if something went wrong in the determinant       (D.Casper)

  ! ALEPHLIB 30.3 correction file 1
    UFTKAL - protect against incorrect angular subtraction during smoothing;
             add support for tracing the fit of a track by calling UFTRAC(.TRUE.)
             immediately before fitting.                           (D.Casper)

  ! ALEPHLIB 30.3
    UFDISC - Set the energy loss to zero if it is more than 5% of
             the track's starting energy                           (D.Casper)
    UFLOSS - Set the energy loss to zero if it is more than 5% of
             the track's starting energy                           (D.Casper)
    UFVDMS - Protect against very high incidence angle tracks      (D.Casper)

  ! ALEPHLIB 30.2 correction file 9
    UFVDMS - fix overwrite of flag enabling energy loss in Kalman filter
                                                                   (D.Casper)
  ! ALEPHLIB 30.2
    introduction of the new Kalman filter (D.Casper)
    new routines:
    UBANGL  UF2SCA  UFBEBL  UFCOVA  UFGAIN  UFLOSS  UFSCOV  UFSWMS  
    UFTRAN  ULOAD   UF2ARC  UF2TRK  UFBERR  UFDISC  UFGETT  UFQPRC  
    UFSTAT  UFSWMX  UFTTRA  UPCONS  UF2CRD  UF2USE  UFCHI2  UFECAL  
    UFJACO  UFREIN  UFSWMC  UFTKAL  UFVDMS  UTPROC
