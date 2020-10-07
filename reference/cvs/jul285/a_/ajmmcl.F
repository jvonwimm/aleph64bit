      SUBROUTINE AJMMCL(YCUT,EVIS,NJETS)
C--------------------------------------------------------------
C! - Perform Jet Finding
C!    Author  : M.N Minard       10/02/91
C!     Input   : YCUT     Ycut valuee (M/EVIS)**2
C!               EVIS     Visible energy
C!                        Evis=0 ,computed from the input
C!                        particle energy
C!     Output  : NJETS    Number of reconstructed jets
C!     ACTBUF common used
C?
C!======================================================================
#ifndef DOC
#include "actbuf.h"
#include "jetset.h"
      INTEGER KW, LEN , LENTM , LENTP
      DIMENSION PP(5,KTBIMX)
#include "bcs.h"
      KW= 0
C
C-    Fill ACTBUF buffer
C
      DO 10 IL = 1, N
        QTBIX(IL)= P(IL,1)
        QTBIY(IL)= P(IL,2)
        QTBIZ(IL)= P(IL,3)
        QTBIE(IL)= P(IL,4)
   10 CONTINUE
      KTBI = N
      KTBO = -1
      IF ( KTBI.GT.0) THEN
        KTBO = 0
C
C-    LENTM is the length in words of MASMAT array
C-    LENTP is the length in words of PP array
        LENTM = KTBI*KTBI
        LENTP = KTBI*5
        LEN = LENTM+LENTP
        LENM = KTBI
        CALL WBANK(IW,KW,LEN,*100)
C
C-        JW(K) is to be used as a REAL*4 array
C
        CALL QFMMCL( YCUT,EVIS,IW(KW),IW(KW+LENTM)
     &    , LENM,KTBI,QTBIX,QTBIY,QTBIZ,QTBIE,KTBIT
     &    , KTBOF,KTBO,QTBOX,QTBOY,QTBOZ,QTBOE,QTBOR,KTBIMX,KTBOMX)
        CALL WDROP( IW, KW )
        GO TO 200
C
C-    In case working bank fails take slow option
C
  100   CONTINUE
        CALL QGMMCL( YCUT,EVIS,KTBI,QTBIX,QTBIY,QTBIZ,QTBIE,KTBIT
     &    , KTBOF,KTBO,QTBOX,QTBOY,QTBOZ,QTBOE,QTBOR,KTBIMX,KTBOMX,PP)
  200   CONTINUE
C
C-    Fill output array
C
        DO 210 IL = 1, N
          K(IL,1) = KTBOF(IL)
  210   CONTINUE
        IF ( KTBO.GE.0) THEN
          DO 220 IJ =1,KTBO
            P(N+IJ,1) = QTBOX(IJ)
            P(N+IJ,2) = QTBOY(IJ)
            P(N+IJ,3) = QTBOZ(IJ)
            P(N+IJ,4) = QTBOE(IJ)
  220     CONTINUE
        ENDIF
      ENDIF
      NJETS = KTBO
      RETURN
      END

#endif
