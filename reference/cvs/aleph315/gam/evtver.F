      SUBROUTINE EVTVER (X,Y,Z)
C----------------------------------------------------------------------
C!  - Get Event Vertex (X,Y,Z) from YTOPOL (x,y) and mean z0
C!
C!   Author   :- E. Lancon              8-FEB-1993
C!
C!   Inputs:
C!        -   None
C!
C!   Outputs:
C!        -   X,Y       /R    x,y position of event vertex from
C!                            YTOPOL PYER bank
C!            Z         /R    from average z0 of tracks with UFITQL
C!                            quality flag = 1 or 2
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "pyerjj.h"
#include "frftjj.h"
#include "pfrfjj.h"
#include "fridjj.h"
C
      DATA KRUNO, KEVTO / 0,0 /
      DATA XO,YO,ZO / 3*0. /
      DATA NAPYER,NAFRFT,NAPFRF,NAFRID /4*0/
#include "bmacro.h"
C----------------------------------------------------------------------
      IF (NAPYER.EQ.0) THEN
        NAPYER = NAMIND('PYER')
        NAFRFT = NAMIND('FRFT')
        NAPFRF = NAMIND('PFRF')
        NAFRID = NAMIND('FRID')
      ENDIF
C
C?   New Run, Evt ???
C
      CALL ABRUEV (KRUN, KEVT)
C
      IF ( KRUN.EQ.KRUNO .AND. KEVT.EQ.KEVTO ) THEN
C
C?   Same (run,evt) get previous values
C
        X = XO
        Y = YO
        Z = ZO
      ELSE
C
C?   Get (x,y) from PYER , First Vertex should be of type 1 or 3
C?            Type of vertex:
C?            =1 if it is the main vertex
C?            =3 if main vtx from 2 trks + beam spot (Bhabha)
C
        X = 0.
        Y = 0.
        KPYER = IW(NAPYER)
        IF ( KPYER.GT.0 ) THEN
          NPYER = LROWS(KPYER)
          IF ( NPYER.GE.1 ) THEN
            IVTP = ITABL(KPYER,1,JPYETY)
            IF ( IVTP.EQ.1 .OR. IVTP.EQ.3 ) THEN
              X = RTABL(KPYER,1,JPYEVX)
              Y = RTABL(KPYER,1,JPYEVY)
            ENDIF
          ENDIF
        ENDIF
C
C?   Get Z from average Z0 of tracks with quality flag = 1 or 2
C?   see UFITQL, only for evts with at least 2 good tracks
C?          Track quality flag
C?            1 = Good track from the origen
C?            2 = Good track but momentum > Ebeam
C
        KFRID = IW(NAFRID)
        KFRFT = IW(NAFRFT)
        JATZ0 = JFRFZ0
        IF (KFRFT.LE.0) THEN
          KFRFT = IW(NAPFRF)
          JATZ0 = JPFRZ0
        ENDIF
        NMEAN = 0
        ZMEAN = 0.
        IF (KFRFT.GT.0) THEN
          NFRFT = LROWS(KFRFT)
          DO IFRFT =  1, NFRFT
            IQUAL = ITABL(KFRID,IFRFT,JFRIQF)
            IF ( IQUAL.EQ.1 .OR. IQUAL.EQ.2 ) THEN
              NMEAN = NMEAN + 1
              ZMEAN = ZMEAN + RTABL(KFRFT,IFRFT,JATZ0)
            ENDIF
          ENDDO
        ENDIF
        IF ( NMEAN.GE.2 ) THEN
          Z = ZMEAN / FLOAT(NMEAN)
        ELSE
          Z = ZO
        ENDIF
C
        XO = X
        YO = Y
        ZO = Z
      ENDIF
C
  999 RETURN
      END
#endif
