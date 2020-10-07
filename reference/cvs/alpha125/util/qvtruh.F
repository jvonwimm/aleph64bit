      SUBROUTINE QVTRUH(ITRK, IVHIT)
CKEY UTIL VDET /USER
C-----------------------------------------------------------------------
C! Provide array of number of correctly associated Vdet hits
C!  per layer for given track.
C
C A. Bonissent 30-05-96
C Input:      ITRK      /I     Alpha track number
C Output:     IVHIT(1)  /I     Num Vdet hits OK in r-phi on inner layer
C             IVHIT(2)  /I     Num Vdet hits OK in r-phi on outer layer
C             IVHIT(3)  /I     Num Vdet hits OK in z     on inner layer
C             IVHIT(4)  /I     Num Vdet hits OK in z     on outer layer
C Note: r-phi = U and z = W in Vdet scheme.
C-----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      PARAMETER (MXPAT=200)
      DIMENSION IPAT(MXPAT),IFKIN(MXPAT),IVHIT(4)
      SAVE LRUN,LEVT,JVI,IPAT
      DATA LRUN,LEVT / -999,-999 /
#include "qmacro.h"
C----------------------------------------------------------------------
      CALL VZERO(IVHIT,4)
      IF (ITRK.LT.KFCHT .OR. ITRK.GT.KLCHT) RETURN
C
C++   If we have a new event:
C++   for Mini, check existance of Vdet information and
C++   for DST, call VDHITS.
C
      IF (KEVT.NE.LEVT .OR. KRUN.NE.LRUN) THEN
         LEVT = KEVT
         LRUN = KRUN
         JVI = 0
         IF (XMINI) THEN
            MVER = MINGTV(DUM)
            IF (MVER.GE.70) JVI = 34
            IF (MVER.GE.90) JVI = 27
         ELSE
            IF (KNCHT.GT.MXPAT)
     &         CALL QWMESE('0QVDHIT: too many tracks - increase MXPAT')
            DO ITK=1,KNCHT
              IALP = ITK + KFCHT-1
              IFKIN(ITK) = KBESTM(IALP)-KFMCT+1
            ENDDO
            CALL VZERO(IPAT,KNCHT)
            CALL VTRUH(IPAT,IFKIN,MXPAT)
         ENDIF
      ENDIF
C
C++   Return if Mini but no Vdet information.
C
      IF (XMINI .AND. JVI.EQ.0) RETURN
C
C++   Obtain hit pattern.
C
      IFRFT = ITRK - KFCHT + 1
      IF (XMINI) THEN
         KDTRA = IW(NADTRA)
         IPAT1 = ITABL(KDTRA,IFRFT,JVI)
      ELSE
         IPAT1 = IPAT(IFRFT)
      ENDIF
      IPAT1 = ISHFT(IPAT1,-10)
      IVHIT(1) = IAND(IPAT1,  3)
      IVHIT(2) = IAND(IPAT1, 12)/ 4
      IVHIT(3) = IAND(IPAT1, 48)/16
      IVHIT(4) = IAND(IPAT1,192)/64
C
      RETURN
      END
#endif
