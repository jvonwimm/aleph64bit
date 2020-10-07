*DK hcgest
      SUBROUTINE HCGEST
C --------------------------------------------
C
C! Implements the module GENERATE HSTREAMER
C! Saturation effects are handled in this routine
C!
C!       Authors :G.Catanesi,G.Zito 86/05/21
C!       Modified:G.Catanesi        87/01/17
C!
C!      Input bank : JDHCSE McHcTubesSegment
C!      Output bank: JDHCHI  McHcHits
C!
C!       -Called by :HCASIG
C!      -Calls      :HCNSTR from this .HLB
C!                   SORTRQ from CERNLIB
C-------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "jobcom.h"
#include "iocom.h"
#include "hccoun.h"
#include "hcnamc.h"
#include "hcsejj.h"
#include "hchijj.h"
      INTEGER HCNSTR
#include "bmacro.h"
C
C        Loop on Tube segments (track elements intersecting
C            single tube)
C
      NHCSE = LROWS (JDHCSE)
C
C     Order track elements intersecting the same tube in the
C       y coordinate
C
      KHCSE = JDHCSE + LMHLEN
      IPREV = 0
      NELEM = 0
      DO 10 I = 1, NHCSE
         IF( IW(KHCSE+5).EQ.IPREV) THEN
            NELEM = NELEM + 1
         ELSE
            IF(NELEM.GT.1) CALL SORTRQ(IW(IEL1),LHCSEA,NELEM,7)
            IEL1 = KHCSE + 1
            IPREV = IW(KHCSE+5)
            NELEM = 1
         ENDIF
         KHCSE = KHCSE + LHCSEA
   10 CONTINUE
      IF(NELEM.GT.1) CALL SORTRQ(IW(IEL1),LHCSEA,NELEM,7)
      KHCSE = JDHCSE + LMHLEN
      IPREV = 0
      NHCHI = 0
      DO 30 I=1, NHCSE
         IF(IW(KHCSE+5).EQ.IPREV) THEN
C
C takes in account saturation effect
C
            Y1=RW(KHCHI+2)
            DY1=RW(KHCHI+3)
            Y2=RW(KHCSE+7)
            DY2=RW(KHCSE+8)
            DYT = (DY1 + DY2)/2.
            IF(DYT .GT. ABS(Y2-Y1))THEN
               DYMAX = MAX(DY1,DY2)
               IF(DYT*2.GT.DYMAX)THEN
                  RW(KHCHI+3) = DYT + ABS(Y2-Y1)
                  IF(Y1.GT.Y2)THEN
                     RW(KHCHI+2) = Y2+ABS(Y1-Y2)/2.
                  ELSE
                     RW(KHCHI+2) = Y2-ABS(Y1-Y2)/2.
                  ENDIF
               ELSE
                  RW(KHCHI+3) = DYMAX
                  IF(DY1.GE.DY2)THEN
                     RW(KHCHI+2) = Y1
                  ELSE
               RW(KHCHI+2)=Y2
                  ENDIF
               ENDIF
               GOTO 20
            ENDIF
         ENDIF
C
         NHCHI = NHCHI + 1
         KHCHI = KROW (JDHCHI,NHCHI)
         IW(KHCHI+1) = I
         RW(KHCHI+2) = RW(KHCSE+7)
         RW(KHCHI+3) = RW(KHCSE+8)
         IPREV = IW(KHCSE+5)
   20    IW(KHCSE+13) = NHCHI
   30 KHCSE = KHCSE + LHCSEA
      IW(JDHCHI+2) = NHCHI
C
      KHCHI = JDHCHI + LMHLEN
      DO 40 I=1,NHCHI
         IW(KHCHI+4) = HCNSTR (RW(KHCHI+3))
   40 KHCHI = KHCHI + LCOLS(JDHCHI)
C
C
      IF(FHCDB1)THEN
         WRITE(LOUTIO,500) NHCHI
         KHCHI = JDHCHI + LMHLEN
         WRITE (LOUTIO,510) (IW(K+1),RW(K+2),RW(K+3),IW(K+4) ,K=KHCHI,
     +   KHCHI+NHCHI*LHCHIA-1,LHCHIA)
      ENDIF
      RETURN
  500 FORMAT (/1X,'+++HCGEST+++ HCHI  McHcHits ',I5/
     +' Seg#   YStreamer   Proj.on.wi  #ofStreamer')
  510 FORMAT (I5,F11.2,F12.2,I10)
      END
#endif
