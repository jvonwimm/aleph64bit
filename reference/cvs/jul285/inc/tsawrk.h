      PARAMETER (JTNWIC=1,JTNWNW=2,JTNWRM=3,JTNWTG=4,JTNWTI=5,
     &           JTNWZZ=6,JTNWD2=7,JTNWG2=8,JTNWNN=9,JTNWRR=10,
     &           JTNWPH=11,JTNWS2=12,JTNWQW=13,JTNWZO=14,
     &           JTNWIR=15,JTNWPS=16,JTNWZS=17,LNWCOA=17)
      COMMON/TSAWRK/JTHPHI,JTPHIS,JTPHCL,JTNWCO,
     &              JTTCHA,JTTCTC,JTWRK1,JTWRK2,JTWRK3
#if defined(DOC)
C
C----------------------------------------------------------------
C! Workbank pointers for small angle tracking code of the TPC
C
C -------------------------------------------------------------
C | +--------+                                                |
C | | JTNWCO |          Number of columns = 17                |
C | +--------+          Number of new coordinates             |
C |-----------------------------------------------------------|
C | 1  JTNWIC     Coordinate number in   TPCO if positive     |
C |                                      TBCO if negative     |
C | 2  JTNWNW     Number of wires in the fit                  |
C | 3  JTNWRM     Mean radius over range of wires             |
C | 4  JTNWTG     Slope of R0 vs bucket no. in fit            |
C | 5  JTNWTI     Intercept of R0 vs bucket no. in fit        |
C | 6  JTNWZZ     Z position from fit                         |
C | 7  JTNWD2     sigma**2 of fitted slope                    |
C | 8  JTNWG2     sigma**2 of fitted bucket number            |
C | 9  JTNWNN     Chi**2/dof of fit to wires (999.= no fit)   |
C |10  JTNWRR     Radial position at middle of pad of at      |
C |               the endplate crossing                       |
C |11  JTNWPH     Phi position (from TPCO or recomputed in    |
C |               the case of a coordinate from TBCO)         |
C |12  JTNWS2     Sigma**2 in r*phi                           |
C |13  JTNWQW     1000*cluster no. + fit no.                  |
C |14  JTNWZO     Chain number (reference to JTTCHA)          |
C |15  JTNWIR     Padrow number in the sector                 |
C |16  JTNWPS     R*PHI in the sector system (raw)            |
C |17  JTNWZS     Z in the sector system (raw)                |
C +-----------------------------------------------------------+
#endif
