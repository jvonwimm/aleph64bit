      SUBROUTINE YSPVFT(IER)
C------------------------------------------------------------------------------
C! Fit tracks and V0s to primary vertex
C
C       Author:  D.Casper 04/07/96
C
C       IER = -1 if input banks not found
C       IER = 1  if output banks could not be created
C       IER = 0  if normal return
C
C       Input banks:    YSTL        Internal list of helices
C                       YSCL        Internal list of circles
C                       YSVX/0      Primary vertex
C
C       Output bank:    YSVX/1      Fit of each track, circle and V0
C                                   with primary vertex
C
C------------------------------------------------------------------------------
#ifndef DOC
#include "ystljj.h"
#include "yscljj.h"
#include "ys0ljj.h"
#include "ysvxjj.h"
#include "ysvwrk.h"
#include "bcs.h"
C
      DATA NYSTL/0/
      SAVE NYSTL, NYS0L, NYSVX, NYSCL
#include "bmacro.h"
C
      IF (NYSTL.EQ.0) THEN
        NYSTL = NAMIND('YSTL')
        NYS0L = NAMIND('YS0L')
        NYSVX = NAMIND('YSVX')
        NYSCL = NAMIND('YSCL')
      ENDIF
C
C+ Find input banks
C
      KYSTL = IW(NYSTL)
      KYS0L = IW(NYS0L)
      KYSCL = IW(NYSCL)
      KYSVX0 = IW(NYSVX)
      IF (KYSTL.LE.0 .OR. KYS0L.LE.0
     &      .OR. KYSVX0.LE.0 .OR. KYSCL.LE.0) THEN
        IER = -1
        GOTO 999
      ENDIF
C
C+  Create YSVX/1 bank
C
      CALL AUBOS('YSVX',1,LMHLEN+(LROWS(KYSTL)+LROWS(KYSCL)
     &          +LROWS(KYS0L))*LYSVXA,KYSVX1,IGARB)
      IF (IGARB.EQ.1) THEN
        KYSTL = IW(NYSTL)
        KYS0L = IW(NYS0L)
        KYSCL = IW(NYSCL)
        KYSVX0 = IW(NYSVX)
      ELSE IF (IGARB.EQ.2) THEN
        IER = 1
        GOTO 999
      ENDIF
      IW(KYSVX1+LMHCOL) = LYSVXA
      IW(KYSVX1+LMHROW) = 0
C
C+  Set-up type of fit (1 vertex, 1 track, 0 V0, 0 circles)
C
      CALL YSFSET(1,1,0,0,IER)
      IF (IER.NE.0) GOTO 999
C
C+  Load primary vertex
C
      CALL YSVADD(1,0,1,IER)
      IF (IER.NE.0) GOTO 999
C
C+  Loop over good charged tracks and fit to the primary
C
      DO IT = 1, LROWS(KYSTL)
        IYSTL = KROW(KYSTL,IT)
        IF (IW(IYSTL+JYSTQF).EQ.0) THEN
            CALL YSTADD(1,IT,IER)
            IF (IER.NE.0) GOTO 999
            CALL YSVFIT(.FALSE.,1,IER,IGARB)
            IF (IER.LT.0) GOTO 999
            IF (IGARB.GT.0) THEN
                KYSTL = IW(NYSTL)
                KYS0L = IW(NYS0L)
                KYSCL = IW(NYSCL)
                KYSVX0 = IW(NYSVX)
                KYSVX1 = NLINK('YSVX',1)
                IYSTL = KROW(KYSTL,IT)
            ENDIF
            IF (IER.EQ.0) RW(IYSTL+JYSTCP)=
     &          RW(KROW(KYSVX1,LROWS(KYSVX1))+JYSVC2)
        ENDIF
      ENDDO
C
C+  Set-up type of fit (1 vertex, 0 track, 0 V0, 1 circles)
C
      CALL YSFSET(1,0,0,1,IER)
      IF (IER.NE.0) GOTO 999
C
C+  Load primary vertex
C
      CALL YSVADD(1,0,1,IER)
      IF (IER.NE.0) GOTO 999
C
C+  Loop over good ITC tracks and fit to the primary
C
      DO IT = 1, LROWS(KYSCL)
        IYSCL = KROW(KYSCL,IT)
        IF (IW(IYSCL+JYSCQF).EQ.0) THEN
            CALL YSCADD(1,IT,IER)
            IF (IER.NE.0) GOTO 999
            CALL YSVFIT(.FALSE.,1,IER,IGARB)
            IF (IER.LT.0) GOTO 999
            IF (IGARB.GT.0) THEN
                KYSTL = IW(NYSTL)
                KYS0L = IW(NYS0L)
                KYSCL = IW(NYSCL)
                KYSVX0 = IW(NYSVX)
                KYSVX1 = NLINK('YSVX',1)
                IYSCL = KROW(KYSCL,IT)
            ENDIF
            IF (IER.EQ.0) RW(IYSCL+JYSCCP)=
     &          RW(KROW(KYSVX1,LROWS(KYSVX1))+JYSVC2)
        ENDIF
      ENDDO
C
C+  Set-up type of fit (1 vertex, 0 track, 1 V0, 0 circles)
C
      CALL YSFSET(1,0,1,0,IER)
      IF (IER.NE.0)GOTO 999
C
C+  Load primary vertex
C
      CALL YSVADD(1,0,1,IER)
      IF (IER.NE.0) GOTO 999
C
C+  Loop over good V0s and fit to the primary
C
      DO IV = 1, LROWS(KYS0L)
        IYS0L = KROW(KYS0L,IV)
        IF (IW(IYS0L+JYS0QF).EQ.0) THEN
            CALL YS0ADD(1,IV,IER)
            IF (IER.NE.0) GOTO 999
            CALL YSVFIT(.FALSE.,1,IER,IGARB)
            IF (IER.LT.0) GOTO 999
            IF (IGARB.GT.0) THEN
                KYSTL = IW(NYSTL)
                KYS0L = IW(NYS0L)
                KYSCL = IW(NYSCL)
                KYSVX0 = IW(NYSVX)
                KYSVX1 = NLINK('YSVX',1)
                IYS0L = KROW(KYS0L,IT)
            ENDIF
            IF (IER.EQ.0) RW(IYS0L+JYS0CP)=
     &          RW(KROW(KYSVX1,LROWS(KYSVX1))+JYSVC2)
        ENDIF
      ENDDO
      IER = 0
 999  CONTINUE
      CALL YSFCLR

      RETURN
      END
#endif
