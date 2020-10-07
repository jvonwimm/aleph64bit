#if defined(DOC)
C! 1st entry in I_set
 ! JULIA 305
    ITEXTN : fix variable type inconsistencies in function calls, 
             for Linux                                            (A.Waananen)

 ! JULIA 302 (Tracking upgrade)
    IREFIN : Don't drop old ICCO, to keep valid coordinates (D.Casper)
    ITCINF.H Increase JRESMX from 80 to 200                 (D.Casper)

 ! JULIA 280
   IFIND1,IFIND2,IGETTI,IMATCH,IREOPD,ITBOOK,ITCTRK,ITLNK1,ITLNK2,
   ITRECF,ITRECI,ITREE,ITRKCI : opening "'" should have a closing "'"
        within the same line for cvs (F. Ranjard, Feb 96)

 * corr file 279.2
   IENANG, IPHCOR : avoid use of NLINK (O.Callot, Dec 95)
   IFITP  : optimize code as CHIELI always .ge.0 (O.Callot, Dec 95)
   IFIND1,IFIND2,IGICHA,IPREDA,ITCREC,ITRAK : call ALTIME only if ITTI
           card used (O.Callot, Dec 95)
   ITTIME : common modified to include logical IFITTI (O.Callot, Dec 95)
 ! JULIA 279

 ! JULIA 278
   INSECT : Add test for intersection. Also tighten sin(A) test to
           not allow angles too near 90 deg (J.Sedgbeer, Aug 95)

 ! JULIA 275

 * corr file 274.3
   INSECT : protect against square root of negative value (P.Comas).
 ! JULIA 274
#endif
