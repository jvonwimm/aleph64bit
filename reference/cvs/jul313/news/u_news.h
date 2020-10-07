C! 1st entry in U_set
 ! JULIA 310
    UFITMS,UMXINV,UVPFIT,UVPMAT,UXSFIT,UXSMAT :
             Double precision fixes for Linux       (D.Smith)

 * corr file 305.4
    UFITMS : Tighten numerical checks                              (D.Casper)
             Bring phi back into 0-2*pi range after Newtonian step (M.Cattaneo)

 * corr file 305.3
    UCTGFT : Test error return of UFITMS                          (D.Casper)
    UFITMS : Make sure all tracks have sensible covariance matrix (D.Casper)
             Fix precision problem for GG0P variable            (M.Cattaneo)

 ! JULIA 305
    UFITMS : fix variable type inconsistencies in function calls, 
             for Linux                                            (A.Waananen)

 * corr file 304.2
    UFITMS : Protect against divide by zero (M.Cattaneo)

 * corr file 303.5
    UCTGFT : Protect against crazy fits by calling sanity check (D.Casper)

 ! JULIA 303
    UFITMS : Disable Newtonian for 3-hit tracks (D.Casper)

 ! JULIA 280
    UFITMS : replace WRITE(6, by WRITE(IW(6), (H.Drevermann, Feb 96)

 ! JULIA 278
    UFITMS : rearrange DASIN argument to avoid crash (P.Comas, Sep 95)

