      SUBROUTINE TRHFLG(HPT,IFLG)
C
C---------------------------------------------------------------------
C! Which TPC padrows is a given helix expected to hit?
CKEY TPCDES TRACK TPC / USER
C  Author:  R. Johnson    13-07-89
C
C  Input:     HPT(5)    /I      Track helix parameters
C  Output:    IFLG(21)  /I      0 if no hit expected on this row
C                               1 if a hit is expected on this row
C
C  NOTE:  the TPC geometry commons must be initialized by a call to
C         TRDDAF before this routine is called
C
C----------------------------------------------------------------------
#ifndef DOC
C
#include "alcons.h"
#include "bcs.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tclbjj.h"
C
      SAVE
      DIMENSION IFLG(*),S(2),PHI(2),Z(2),HPT(*)
      DIMENSION ZCUT(LTPDRO),NSECR(2),ISLST(24,2),ISECR(LTPDRO)
      DIMENSION NBADS(LTSECT),IOBAD(LTSECT)
      LOGICAL FIRST
      DATA FIRST/.TRUE./,NPRT/0/
      DATA ISECR/9*1,12*2/
      DATA NSECR/12,24/
      DATA ISLST/ 1, 2, 3, 4, 5, 6,19,20,21,22,23,24,12*0,
     &            7, 8, 9,10,11,12,13,14,15,16,17,18,
     &           25,26,27,28,29,30,31,32,33,34,35,36/
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTCLB=NAMIND('TCLB')
        NTKAP=NAMIND('TKAP')
C
C++     For each padrow, calculate a limit on how close the coordinate
C++     can be to the endplate before becoming distorted due to the
C++     track passing through the endplate and not traversing the full
C++     length of the pad (assuming the track came from the I.P.)
C
        DO 9 I=1,LTPDRO
          ZCUT(I)=0.5*TPDHGT(1)*ZTPCMX/(TPPROW(I)+0.5*TPDHGT(1)) + 0.1
    9   CONTINUE
      ENDIF
C
      DO 10 IR=1,LTPDRO
        IFLG(IR)=0
   10 CONTINUE
C
C++   Find pointers into TKAP for each sector
C
      CALL VZERO(NBADS,LTSECT)
      NBTOT=0
      KTCLB=IW(NTCLB)
      DO 83 ISLOT=1,LROWS(KTCLB)
        NBADS(ISLOT)=ITABL(KTCLB,ISLOT,JTCLNT)
        IOBAD(ISLOT)=NBTOT
        NBTOT=NBTOT+NBADS(ISLOT)
   83 CONTINUE
      KTKAP=IW(NTKAP)
C
C++   Loop over all TPC padrows
C
      DO 500 IR=1,NTPROW
C
C++     Find intersection point with this padrow.  Skip if none.
C
        CALL THLCIR(HPT,TPPROW(IR),S,PHI,Z,IERR)
        IF (IERR.NE.0) GO TO 500
C
C++     Which TPC end are we in?
C++     Skip points too close to the TPC endplate (same as in TCOOR).
C
        IF (ABS(Z(1)).GT.(ZTPCMX-ZCUT(IR))) THEN
          GO TO 500
        ELSEIF (Z(1).LT.0.) THEN
          IEND=2
        ELSE
          IEND=1
        ENDIF
C
C++     What is the sector row number?
C
        IF (IR.LE.NTPDRW(1)) THEN
          IRS=IR
        ELSE
          IRS=IR-NTPDRW(1)
        ENDIF
C
C++     Loop over all sectors containing this padrow
C
        INOUT=ISECR(IR)
        DO 450 JJ=1,NSECR(INOUT)
          ISLOT=ISLST(JJ,INOUT)
          IF (IENDTP(ISLOT).NE.IEND) GO TO 450
          ISTYP=ITPTYP(ISLOT)
C
C++       Convert the point to the sector reference frame, ignoring
C++       sector-to-sector alignment corrections.
C
          IF (IEND.EQ.2) THEN
            PHIS= PHI(1) - TPPHI0(ISLOT)
          ELSE
            PHIS= TPPHI0(ISLOT) - PHI(1)
          ENDIF
          IF (PHIS.LT.-PI) THEN
            PHIS=PHIS+TWOPI
          ELSEIF (PHIS.GT.PI) THEN
            PHIS=PHIS-TWOPI
          ENDIF
C
C++       Require the point to be at least one half pad from the
C++       sector edge
C
          PHMX= TPDPHW(IRS,ISTYP)-0.5*TPDPHS(IRS,ISTYP)
          IF (ABS(PHIS).GT.PHMX) GO TO 450
C
C++       Check that it isn't within 1-pad spacing of a dead pad
C
          IF (KTKAP.NE.0) THEN
            NTPAD=NTPDPR(IRS,ISTYP)+2
            DO 660 JB=1,NBADS(ISLOT)
              JWD=IW((KTKAP+LMHLEN+IOBAD(ISLOT))+JB)
              JSLOT=IBITS(JWD,24,8)
              IF (JSLOT.NE.ISLOT) THEN
                IF (NPRT.LT.5) THEN
                  NPRT=NPRT+1
                  KEVEH=IW(NAMIND('EVEH'))
                  IF (KEVEH.NE.0) THEN
                    IEVT=IW(KEVEH+6)
                    IRUN=IW(KEVEH+2)
                    WRITE(6,643) IRUN,IEVT
                  ELSE
                    WRITE(6,644)
                  ENDIF
  643             FORMAT(' THTFLG: run ',I6,' event ',I6,',',
     &                   ' TCLB and TKAP are out of sequence.')
  644             FORMAT(' THTFLG: TCLB and TKAP are out of sequence.')
                ENDIF
                GO TO 660
              ENDIF
              JROW=IBITS(JWD,8,8)
              IF (JROW.NE.IRS) GO TO 660
              JTHR=IBITS(JWD,16,8)
              IF (JTHR.NE.255) GO TO 660
              IPAD=IBITS(JWD,0,8)
              RPHS=TPDSEP(ISTYP)*((0.5*FLOAT(NTPAD+1))-FLOAT(IPAD))
              DIF=ABS(RPHS-PHIS*TPPROW(IR))
              IF (DIF.LT.TPDSEP(ISTYP)) GO TO 451
  660       CONTINUE
          ENDIF
C
          IFLG(IR)=1
          GO TO 451
  450   CONTINUE
  451   CONTINUE
  500 CONTINUE
C
  999 CONTINUE
      RETURN
      END
#endif
