*DK asreve
      SUBROUTINE ASREVE
C ----------------------------------------------------------------
C - F.Ranjard - 861015
C! Decode EVEH KEVH and ASEV banks
C - Called by : ASIEVE                                 from this .HLB
C ---------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
#include "jqcom.h"
#include "kincom.h"
#include "evehjj.h"
#include "kgjjpar.h"
#include "ajjpar.h"
      DIMENSION JRND(LRND,LPRO)
#include "bmacro.h"
C ----------------------------------------------------------------
C - save random #s set by data cards
      IF (NEVTJO .EQ. 1) CALL UCOPY (IRNDJO,JRND,LPRO*LRND)
C
C - Decode EVEH bank
      KEVEH = IW(NAEVEH)
      IF (KEVEH .NE. 0) THEN
        ECMSKI = IW(KEVEH+JEVETE) * 1.E-6
        IF (IW(KEVEH+JEVEES).NE.1) THEN
          KERRJO = IW(KEVEH+JEVEES) - LOFFMC
          CALL ALTELL ('ASREVE: incomplete input ',KERRJO,'NEXT')
        ELSE
C         for complete events
C           Decode kine event header KEVH bank
          JKEVH = IW(NAKEVH)
          IF (JKEVH .NE. 0) THEN
            KKEVH = JKEVH + LMHLEN
            NITRKI    = IW(KKEVH+JKEVNT)
            NIVXKI    = IW(KKEVH+JKEVNV)
            IDEVKI    = IW(KKEVH+JKEVPI)
            WEITKI    = RW(KKEVH+JKEVWT)
            IF (IPROJO(1) .NE. 0) THEN
C                 KINE process is required, take data card RNDM/RINI
              IRNDJO(1,1) = JRND(1,1)
              IRNDJO(2,1) = JRND(2,1)
              IRNDJO(3,1) = JRND(3,1)
            ELSE
              IRNDJO(1,1) = IW(KKEVH+JKEVRN)
              IRNDJO(2,1) = IW(KKEVH+JKEVSR)
              IRNDJO(3,1) = IW(KKEVH+JKEVTR)
            ENDIF
          ENDIF
C        Decode ASEV bank
          JASEV = IW(NAASEV)
          IF (JASEV .NE. 0) THEN
            KASEV = JASEV + LMHLEN
            DO 6 I=2,LROWS(JASEV)
              IF (IPROJO(I) .NE. 0) THEN
C                    the process is requested, take data card rndm
                IRNDJO(1,I) = JRND(1,I)
                IRNDJO(2,I) = JRND(2,I)
                IRNDJO(3,I) = JRND(3,I)
              ELSE
                IRNDJO(1,I) = ITABL(JASEV,I,JASERG)
                IRNDJO(2,I) = ITABL(JASEV,I,JASERG+1)
                IRNDJO(3,I) = ITABL(JASEV,I,JASERG+2)
              ENDIF
    6       CONTINUE
          ENDIF
        ENDIF
      ENDIF
      CALL VZERO (JRND,LPRO*LRND)
C
C - debug
      IF (FDEBJO) THEN
        WRITE (LOUTIO,820) NEVTJO,IW(KEVEH+6),(IW(KEVEH+I),I=1,5)
     &                     ,(IW(KEVEH+I),I=11,12)
     &                     ,FLOAT(IW(KEVEH+13))*1.E-6
  820   FORMAT (/1X,'+++ASREVE+++ event# ',I5,', trigger# ',I5,
     &           ', EVEH ',7I10,F12.4)
        WRITE (LOUTIO,822) IPROKI,NIVXKI,NITRKI,WEITKI
  822   FORMAT (14X,'Process id. = ',I8,'  # of input vertices = ',I3,
     &               '  # of input tracks = ',I4,'  weight = ',E12.4)
        WRITE (LOUTIO,821) IRNDJO
  821   FORMAT (14X,'random generator roots used during this job '
     &              /(14X,3I15,10X,3I15))
        IF (JASEV .GT. 0) WRITE (LOUTIO,823) (IW(KASEV+M),M=1,IW(JASEV)
     &                                                         -LMHLEN)
  823   FORMAT (14X,'random generator roots used during previous job '
     &              /(14X,3I15,10X,3I15))
      ENDIF
      RETURN
      END
#endif
