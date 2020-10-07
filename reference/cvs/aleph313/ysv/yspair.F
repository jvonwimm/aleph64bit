      SUBROUTINE YSPAIR(NGOOD,IER)
C------------------------------------------------------------------------------
C! Fit pairs of helices and V0s to a common vertex
C
C       Author:  D.Casper 04/07/96
C     Modified:  D.Casper 30/09/97
C                Do not put in YSVX/-2 track pairs failing cuts
C
C       Returns:
C
C       NGOOD = Number of vertices passing the cuts
C
C       IER = -1 if input banks not found
C       IER = 1  if output banks could not be created
C       IER = 0  if normal return
C
C       Input banks:    YSTL    Helix list
C                       YS0L    V0 list
C
C       Output bank:    YSVX/-2 List of all pair vertices
C
C------------------------------------------------------------------------------
#ifndef DOC
#include "ystljj.h"
#include "ys0ljj.h"
#include "ysvxjj.h"
#include "ysvwrk.h"
#include "bcs.h"
C
      DATA NYSTL/0/
      SAVE NYSTL, NYS0L, NYSVX
#include "bmacro.h"
C
      IF (NYSTL.EQ.0) THEN
        NYSTL = NAMIND('YSTL')
        NYS0L = NAMIND('YS0L')
        NYSVX = NAMIND('YSVX')
      ENDIF
C
      NGOOD = 0
C
C+ Find input banks
C
      KYSTL = IW(NYSTL)
      KYS0L = IW(NYS0L)
      IF (KYSTL.LE.0 .OR. KYS0L.LE.0) THEN
        IER = -1
        GOTO 999
      ENDIF
C
C+  Create YSVX/-2 bank
C
      CALL AUBOS('YSVX',-2,LMHLEN+(LROWS(KYSTL)+LROWS(KYS0L))*LYSVXA,
     &                  KYSVX2,IGARB)
      IF (IGARB.EQ.1) THEN
        KYSTL = IW(NYSTL)
        KYS0L = IW(NYS0L)
      ELSE IF (IGARB.EQ.2) THEN
        IER = 1
        GOTO 999
      ENDIF
      IW(KYSVX2+LMHCOL) = LYSVXA
      IW(KYSVX2+LMHROW) = 0
C
C+  Set-up type of fit (0 vertex, 2 track, 0 V0, 0 circle)
C
      CALL YSFSET(0,2,0,0,IER)
      IF (IER.NE.0) GOTO 999
C
C+  Loop over pairs of good charged tracks and fit
C
      DO IT1 = 1, LROWS(KYSTL)-1
        IF (ITABL(KYSTL,IT1,JYSTQF).EQ.0) THEN
            CALL YSTADD(1,IT1,IER)
            IF (IER .NE. 0) GOTO 999
            DO IT2 = IT1+1, LROWS(KYSTL)
                IF (ITABL(KYSTL,IT2,JYSTQF).EQ.0) THEN
                    CALL YSTADD(2,IT2,IER)
                    IF (IER.NE.0) GOTO 999
                    CALL YSVFIT(.TRUE.,-2,IER,IGARB)
                    IF (IER.LT.0) GOTO 999
                    IF (IGARB.GT.0) THEN
                        KYSTL = IW(NYSTL)
                        KYS0L = IW(NYS0L)
                        KYSVX2 = NLINK('YSVX',-2)
                    ENDIF
                    IF (IER.EQ.0) THEN
                      CALL YSPCUT(KYSVX2,IT1,IT2)
                      IF (ITABL(KYSVX2,LROWS(KYSVX2),JYSVQU).EQ.0) THEN
                          NGOOD = NGOOD + 1
                      ELSE
                          IW(KYSVX2+LMHROW) = IW(KYSVX2+LMHROW)-1
                      ENDIF
                    ENDIF
                ENDIF
            ENDDO
        ENDIF
      ENDDO
C
C+  Set-up type of fit (0 vertex, 1 track, 1 V0, 0 circle)
C
      CALL YSFSET(0,1,1,0,IER)
      IF (IER.NE.0) GOTO 999
C
C+  Loop over charged track/V0 pairs and fit
C
      DO IT1 = 1, LROWS(KYSTL)
        IF (ITABL(KYSTL,IT1,JYSTQF).EQ.0) THEN
            CALL YSTADD(1,IT1,IER)
            IF (IER .NE. 0) GOTO 999
            CHIP1 = RTABL(KYSTL,IT1,JYSTCP)
            DO IT2 = 1, LROWS(KYS0L)
                IF (ITABL(KYS0L,IT2,JYS0QF).EQ.0) THEN
                    CALL YS0ADD(1,IT2,IER)
                    IF (IER.NE.0) GOTO 999
                    CALL YSVFIT(.TRUE.,-2,IER,IGARB)
                    IF (IER.LT.0) GOTO 999
                    IF (IGARB.GT.0) THEN
                        KYSTL = IW(NYSTL)
                        KYS0L = IW(NYS0L)
                        KYSVX2 = NLINK('YSVX',-2)
                    ENDIF
                    IF (IER.EQ.0) THEN
                      CALL YSPCUT(KYSVX2,IT1,-IT2)
                      IF (ITABL(KYSVX2,LROWS(KYSVX2),JYSVQU).EQ.0) THEN
                        NGOOD = NGOOD + 1
                      ELSE
                        IW(KYSVX2+LMHROW) = IW(KYSVX2+LMHROW)-1
                      ENDIF
                    ENDIF
                ENDIF
            ENDDO
        ENDIF
      ENDDO
C
C+  Set-up type of fit (0 vertex, 0 track, 2 V0, 0 circle)
C
      CALL YSFSET(0,0,2,0,IER)
      IF (IER.NE.0) GOTO 999
C
C+  Loop over pairs of good V0s and fit
C
      DO IT1 = 1, LROWS(KYS0L)-1
        IF (ITABL(KYS0L,IT1,JYS0QF).EQ.0) THEN
            CALL YS0ADD(1,IT1,IER)
            IF (IER .NE. 0) GOTO 999
            DO IT2 = IT1+1, LROWS(KYS0L)
                IF (ITABL(KYS0L,IT2,JYS0QF).EQ.0) THEN
                    CALL YS0ADD(2,IT2,IER)
                    IF (IER.NE.0) GOTO 999
                    CALL YSVFIT(.TRUE.,-2,IER,IGARB)
                    IF (IER.LT.0) GOTO 999
                    IF (IGARB.GT.0) THEN
                        KYSTL = IW(NYSTL)
                        KYS0L = IW(NYS0L)
                        KYSVX2 = NLINK('YSVX',-2)
                    ENDIF
                    IF (IER.EQ.0) THEN
                        CALL YSPCUT(KYSVX2,-IT1,-IT2)
                        IF (ITABL(KYSVX2,LROWS(KYSVX2),JYSVQU).EQ.0)
     &                      NGOOD = NGOOD + 1
                    ENDIF
                ENDIF
            ENDDO
        ENDIF
      ENDDO
C      CALL YSVSRT
      IER = 0
 999  CONTINUE
      CALL YSFCLR

      RETURN
      END
#endif
