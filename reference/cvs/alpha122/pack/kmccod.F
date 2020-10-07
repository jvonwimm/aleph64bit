      FUNCTION KMCCOD (IEXT)
CKEY PART /INTERNAL
C----------------------------------------------------------------------
C! get internal particle code for external code IEXT.
C! Called from QFIMCD
C!                                                  H.Albrecht 03.11.88
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
#include "partjj.h"
      CHARACTER CNAM * 12, CHAINT * 4
      DATA IERR /0/
C
C       Particle code must be positive
C
      IF (IEXT .LE. 0)  GO TO 20
C
C         Look into translation table
C
      IF (IEXT .LE. IW(KOQTRA))  THEN
        KMCCOD = IW(KOQTRA+IEXT)
        IF (KMCCOD .GT. 0)  GO TO 90
      ENDIF
C
C         Does external particle table exist ?
C
      LAPART = IW(NAPART)
      NPART = 0
      IF (LAPART .NE. 0)  NPART = IW(LAPART+2)
C
C         Particle not on MC table
C
      IF (IEXT .GT. NPART)  THEN
C          PTRA card given <--> KMCCOD < 0
        IF (KMCCOD .LT. 0)  THEN
          KMCCOD = - KMCCOD
          GO TO 30
        ENDIF
C         standard particle ?
        IF (IEXT .GT. KNQPST)  GO TO 20
        KMCCOD = IEXT
        GO TO 30
      ENDIF
C
C       row address in PART bank
C
      LREXT = LAPART + IEXT * IW(LAPART+1) - IW(LAPART+1) + LMHLEN
C
C       IANT > 0 : set particle - anti-particle relation
C
      IANT = 0
C
C          PTRA card given <--> KMCCOD < 0
C
      IF (KMCCOD .LT. 0)  THEN
        KMCCOD = - KMCCOD
        GO TO 10
      ENDIF
C
C       get particle name from MC table
C
      CNAM(1:4)  = CHAINT (IW(LREXT+JPARNA))
      CNAM(5:8)  = CHAINT (IW(LREXT+JPARNA+1))
      CNAM(9:12) = CHAINT (IW(LREXT+JPARNA+2))
C
C       lnew = 0 : new particle
C
      KMCCOD = KPARTX (CNAM,LNEW)
      IF (LNEW .NE. 0)  GO TO 10
C
C       Get particle - anti-particle relation from MC table
C
      IANT = IW(LREXT+JQPAAN)
      IF (IANT .LE. 0 .OR. IANT .GT. NPART)  IANT = IEXT
      IF (IANT .EQ. IEXT)  THEN
C         default is part=anti-part <--> IANT = 0
        IANT = 0
      ELSE
        LRANT = LAPART + IANT * IW(LAPART+1) - IW(LAPART+1) + LMHLEN
        CNAM(1:4)  = CHAINT (IW(LRANT+JPARNA))
        CNAM(5:8)  = CHAINT (IW(LRANT+JPARNA+1))
        CNAM(9:12) = CHAINT (IW(LRANT+JPARNA+2))
        IANT = KPARTX (CNAM,LNEW)
        IF (LNEW .NE. 0)  IANT = 0
      ENDIF
C
C       Set particle - anti-particle relation for new particles
C
   10 LRINT = KOQPAR + KMCCOD * KCQPAR
      IF (IANT .NE. 0)  THEN
        IW(LRINT+JQPAAN) = IANT
        IW(KOQPAR+IANT*KCQPAR+JQPAAN) = KMCCOD
      ENDIF
C
C       standard particle ?
C
      IF (KMCCOD .LE. KNQPST)  GO TO 30
C
C       particle already defined by data cards ?
C
      IF (IAND (IW(KOQPBT+KMCCOD), KBIT(2)) .NE. 0)  GO TO 30
C
C       Store particle attributes for non-standard particles
C
      IW(LRINT+JQPAGN) = IW(LREXT+JQPAGN)
      IW(LRINT+JQPACO) = IW(LREXT+JQPACO)
      RW(LRINT+JQPAMA) = RW(LREXT+JQPAMA)
      RW(LRINT+JQPACH) = RW(LREXT+JQPACH)
      RW(LRINT+JQPALT) = RW(LREXT+JQPALT)
      RW(LRINT+JQPAWI) = RW(LREXT+JQPAWI)
      IW(KOQPBT+KMCCOD) = IOR (IW(KOQPBT+KMCCOD), KBIT(3))
      GO TO 30
C
C       Unknown particle name
C
   20 WRITE (CNAM,1001) IEXT
      KMCCOD = KPCOMP (CNAM)
C
C       Unallowed particle code
C
      IF (IEXT .GT. 0 .AND. IEXT .LE. MAX (5000, NPART))  GO TO 30
      IERR = IERR + 1
      IF (IERR .GT. 10)  GO TO 90
      CALL QWMESS
     &  ('_KMCCOD_ MC particle code out of range.')
      CALL QWMESE
     &  ('         Time consumption will increase considerably.')
      GO TO 90
C
C       Fill translation table
C
   30 IF (IEXT .GE. IW(KOQTRA))
     +  CALL QSBANK ('QTRA', MAX (NPART, IEXT+200))
      IW (KOQTRA+IEXT) = KMCCOD
C
 1001 FORMAT ('?',I11)
   90 END
#endif
