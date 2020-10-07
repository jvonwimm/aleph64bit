      SUBROUTINE YMFV0S
C--------------------------------------------------------------
C! Interface between julia and v0 package
C   Author M.A.Ciocci 6/10/90
C   MODIFIED:M.A.Ciocci 28/1/93
C       DESCRIPTION
C       ============
C?  Calls the steering routine for v0 reconstruction,
C?  ymfv0v, take decision about histograms and fills the y0josu
C?  common (summary for V0)
C? This package drop the yv0v bank (if it is present)
C? and rewrite the same bank WITH THE NR OF THE YV1C BANK.
C? (look at yv0v bank -> sbank yv0v)
C? (it can  use the mean value for run of beam beam crossing point).
C? It give (if you want) the histograms for v0 candidate after
C? any sequential cut (yv1c).
C? And give as argument output
C?
C?     IERCO/INTEGER         code for error:
C?                           if 0 all is ok
C?                           if 1, problems with coovariance matrix
C?                           of charged tracks in ychiv2
C?                           if 2, problems with coovariance matrix
C?                           of charged tracks in ynv0ve
C?                           if 3, problems with coovariance matrix
C?                           of charged tracks in ynvonf
C?                           if 4, problems in yvcame with frft
C?                           if 5, problems inverting coovariance
C?                           matrix of reconstructed V0 vertex
C?                           candidate
C?                           if 6, the number of V0 candidates is
C?                           greater than 50
C?                           if 7, no booked enlarged space for yv0v
C?                           if 8, problems with working banks
C?                           if 9 no space for yv0v bank
C?                           if 10 no yv1c on dbase or cards
C?
C?     NUMV0/INTEGER         Number of V0 candidates (at
C?                           maximum 50)
C?
C?     CHIOR(2,50)/REAL      chi2 increasing constraining
C?                           the track coming from V0 candidate
C?                           to the origin
C?    QGA(50)/REAL          Q value in the gamma hypothesis
C?    CHQGA(50)/REAL        chi2 of Q value in the gamma hypothesis
C?    QLA(50)/REAL          Q value in the lambda hypothesis
C?    CHQLA(50)/REAL        chi2 of Q value in the lambda hypothesis
C?    QAL(50)/REAL          Q value in the antilambda hypothesis
C?    CHQAL(50)/REAL        chi2 of Q value in the antilambda
C?                          hypothesis
C?    QK0(50)/REAL          Q value in the k0 hypothesis
C?    CHQK0(50)/REAL        chi2 of Q value in the k0 hypothesis
C?    XMGA(50)/REAL         invariant mass in the gamma hypothesis
C?    CXMGA(50)/REAL        chi2 of invariant mass in the gamma
C?                          hypothesis
C?    XMLA(50)/REAL         invariant mass in the lambda hypothesis
C?    CXMLA(50)/REAL        chi2 of the invariant mass in the
C?                          lambda hypothesis
C?    XMAL(50)/REAL         invariant mass in the antilambda
C?                          hypothesis
C?    CXMAL(50)/REAL        chi2 of invariant mass  in the
C?                          antilambda hypothesis
C?    XMK0(50)/REAL         invariant mass in the k0 hypothesis
C?    CXMK0(50)/REAL        chi2 of the invariant mass in the
C?                          k0 hypothesis
C?   NCOR(2,50)/INTE        number of coordinates before the
C?                          reconstructed vertex for tracks
C?                          coming from v0
C?   DISCO(2,50)/REAL       radial distance for the furthest
C? To use this package:
C?   a) data base must be opened
C?   b) frft,frtl,pyer,eveh must be present ( make unpacking
C?      tp,it,fi )
C?   c) yv1c bank (that contains cuts for v0 selection)
C?      must be present on data base or given by data cards
C?      if you want change cut for v0 analysis.
C?   d) fvcl,ficl,ftcl,vdco,itco,tpco must be present
C?      only if you check the number of coordinates
C?      before the reconstructed vertex
C?      ( make unpacking tp,it,fi)
C?      To understand cuts look the ddl of yv1c bank
C?      (-> sbank yv1c)
C?    Beware that the different cuts used in different period
C?    of Aleph data taking (89-90 91-92 93...) are sumarized
C?    by the different NR of the yv1c bank. So on the data base
C?    the yv1c is present with 3 different value for NR
C?YV1C 1/ 25  1  1  1  99999999
C?*1989-1990
C?*      IV     O1     O2     CT     PS         RV    ZV    DI
C?        1      0.0    0.    2.     0.         180. 220. 0.06
C?*      CF     CS     CO     MA     PM    NT   CB    NC    MD
C?       13.    0.92    0.    50.    0.     4    0    0      0.
C?*      D0     Z0     WK        WL         WG
C?       999.   999.   0.1E+05  0.1E+05     0.1E+05
C?YV1C 2/ 25  1  1  1  99999999
C?*1991-1992
C?*      IV     O1     O2     CT     PS         RV    ZV    DI
C?       1      0.0    0.     3.    0.0        180.  220. 0.06
C?*      CF     CS     CO     MA     PM    NT   CB    NC    MD
C?       25.    0.5    0.0    9999.  0.1   4     2    10    1.5
C?*      D0     Z0     WK     WL     WG
C?       999.   999.   9999. 9999.   9999.
C?YV1C 3/ 25  1  1  1  99999999
C?*1993 and later i hope...
C?*      IV     O1     O2     CT     PS         RV    ZV    DI
C?       1      0.0    0.     2.    0.0        180.  180. 0.06
C?*      CF     CS     CO     MA     PM    NT   CB    NC    MD
C?       13.    0.92   0.0    9999.  0.    5     0    33    0.
C?*      D0     Z0     WK     WL     WG
C?       999.   999.   0.5    0.50  0.50
C?*****************************************************************
C?*     To  change cut change values of cut on the following cards *
C?*      and add these cards
C?******************************************************************
C?YV1C 4/ 25  1  1  1  99999999
C?*user private cards
C?*      IV     O1     O2     CT     PS         RV    ZV    DI
C?       1      0.0    0.     2.    0.0        180.  180. 0.06
C?*      CF     CS     CO     MA     PM    NT   CB    NC    MD
C?       13.    0.92   0.0    9999.  0.    5     0    33    0.
C?*      D0     Z0     WK     WL     WG
C?       999.   999.   0.5    0.50  0.50
C?***********************************************
C?*                   iflag=0, primary vertex is assumed to
C?*                    be (0,0,0)
C?*   IV/I            iflag=1  primary vertex defined by pyer
C?*                    bank
C?*                    iflag=2 primary defined using the jsum bank
C?*                    (mean value of primary run per run)
C?************************************************************
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
C
C+ common for v0 summary
C
#include "y0josu.h"
C+   output of package:dimension
C
      REAL CHIOR(2,50),QGA(50),CHQGA(50),QK0(50),CHQK0(50),QLA(50),
     $CHQLA(50),QAL(50),CHQAL(50),XMGA(50),CXMGA(50),XMK0(50),CXMK0(50),
     $     XMLA(50),CXMLA(50),XMAL(50),CXMAL(50),NCOR(2,50),DISCO(2,50)
C
C+  END OUTPUT DIMENSION
C
         INTEGER IV0BK
C
C+       logical for v0 histograms, if true you will have all histogram
C+       for v0
C
         LOGICAL IBOOK
C+  TAKE DECISION ABOUT HISTOGRAMS IN ACCORD WITH LEVEL
C
      IHISLV = 0
C
      IF(FDETRF(JULYR)) THEN
        JOFF = JULYR * 1000
        IHISLV = JHISRF(JULYR)
      ENDIF
C
      IF(FDETRF(JULYT)) THEN
        JOFF = JULYT * 1000
        IHISLV = JHISRF(JULYT)
      ENDIF
            IF(IHISLV.GT.2)THEN
               IBOOK=.TRUE.
               ELSE
               IBOOK=.FALSE.
            ENDIF
C
C+       If ibook=.true. Histograms will stat from iv0bk+200
C
C
C+          HISTOGRAM WILL START FROM JOFF+200
C
            IV0BK=JOFF
C
C+     CALL PACKAGE
C
      CALL  YMFV0V(IBOOK,IV0BK,IERCO,NUMV0,CHIOR,QGA,CHQGA,QK0,
     $                  CHQK0,QLA,CHQLA,QAL,CHQAL,XMGA,CXMGA,XMK0,
     $                  CXMK0,XMLA,CXMLA,XMAL,CXMAL,NCOR,DISCO)
C
C+   TEST IERCO
C
         IF(IERCO.NE.0)THEN
           IF(IERCO.EQ.1)CALL REPORT ('YCHIV2','PROBLEM COOV. MATR.',0)
           IF(IERCO.EQ.2)CALL REPORT ('YNV0VE','PROBLEM COOV. MATR.',0)
           IF(IERCO.EQ.3)CALL REPORT ('YNV0NF','PROBLEM COOV. MATR.',0)
           IF(IERCO.EQ.4)CALL REPORT ('YVCAME','PROBLEM WITH FRFT ',0)
           IF(IERCO.EQ.5)CALL REPORT ('YMFV0V','INVERTING VVMAT',0)
           IF(IERCO.EQ.6)CALL REPORT ('YMFV0V','NUMBER OF V0 GE 50',0)
           IF(IERCO.EQ.7)CALL REPORT
     $       ('YMFV0V','NOT ENLARGED SPACE FORYV0V',0)
           IF(IERCO.EQ.8)CALL REPORT
     $       ('YMFV0V','PROBLEMS WITH WORKING BANKS',0)
           IF(IERCO.EQ.9)CALL REPORT
     $       ('YMFV0V','NO SPACE FOR YV0V BANK',0)
           IF(IERCO.EQ.10)CALL REPORT
     $       ('YMFV0V','NO YV1C BANK ON DBASE OR ON DATA CARDS',0)
           IF(IERCO.EQ.11)CALL REPORT
     $       ('YMFV0V','NO EVEH BANK',0)
            GO TO 999
         ENDIF
C
C+      INCREASE THE NUMBER OF V0
C
                NV0FOU=NV0FOU+NUMV0
C
C+      INCREASE THE NUMBER OF EVENT WITH V0
C
        IF (NUMV0.NE.0)THEN
             NEVV0F=NEVV0F+1
             IF (FDEBRF.AND.(JDBDRF(JULYR).GT.1.OR.JDBDRF(JULYT).GT.1))
     $                                            CALL PRTABL('YV0V',0)
        ENDIF
 999         CONTINUE
             RETURN
             END
#endif
