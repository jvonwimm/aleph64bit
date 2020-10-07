C! 1st entry in Y_set
 ! JULIA 313
    YFVERT : Return if UXSFIT falis (previously, YFVERT carried on
	     using results of previous call to UXSFIT!)    (M.Cattaneo)

 ! JULIA 310
    YKVDET : Double precision fixes for Linux       (D.Smith)

 ! JULIA 305
    YKSFIT : fix variable type inconsistencies in function calls, 
             for Linux                                            (A.Waananen)

 * corr file 304.1
    YKINIT : Remove CALL from function references, for Linux. (A.Waananen)

 ! JULIA 304
    YKFFIT : Suppress Kalman filter error message               (D.Casper)
    YKSRCH : Allow more than one bit to be set in the hypothesis mask, 
             if candidate passes more than one cut; if is INCOMING
             to two kink candidates, save only the best candidate 
             (smallest chi^2) to avoid confusing DALI, ENFLW, etc.  
             Also drop output bank at entry, if present.         (D.Casper)

 * corr file 303.2
    YKSFIT : Check basic quality cuts (minimum distance and #bad hits) 
             here rather than in calling routine, to avoid asking YTOP 
             for impossible fits                                 (D.Casper)
    YKSRCH : Remove basic quality cuts                           (D.Casper)
 
 ! JULIA 303
    YKSFIT : Bug fix - do not call YFTVTC with identical input
             and output vertex arguments                       (M.Cattaneo)

 * corr file 302.6
    YKFFIT : Add protections against negative SQRT               (D.Casper)
    YKSRCH : Store a record of what mass cuts are passed in YKNK (D.Casper)

 * corr file 302.5
    In this correction file we implement the kinks package by P.Rensing
    with minor corrections by D.Casper

    YKNKCM.H New - Common blocks for kinks 
    YKNKPT.H New - Common block for kinks
    YKFFIT : New - Full fit of two tracks to seach for a kink
    YKINIT : New - Initialize the cuts used in the Kink analysis 
    YKLDC  : New - Load kink coordinate arrays for an FRFT track
    YKNEM2 : New - Find the mass^2 of a kink's neutral daughter
    YKSFIT : New - Simple fit of two tracks to seach for a kink
    YKSRCH : Main Kink search routine, replaces dummy routine from JULIA 302
    YKVDET : New - Look for free VDET hits which match the inner track 
             of a potential kink. This routine is inserted in the library
             for completenes but is never called because it relies on VGLOB.
             The code calling it (in YKSRCH) is commented out. 

 ! JULIA 302 (Tracking upgrade)
    YKSRCH : Main Kink search routine. Dummy for now   (M.Cattaneo)
