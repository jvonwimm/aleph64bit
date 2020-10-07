      SUBROUTINE SIBKPR
C-----------------------------------------------------------------------
C!  - Print Scal output banks
C!
C!   Author   : B.Bloch-Devaux 910129
C!
C!     Input  :
C!              'SPDA',SCLS'
C!
C!     Description :
C!     ===========
C!     Print Scal banks in nice "graphics" format
C?
C?     IF level 4 THEN
C?        Dump all output banks
C?     IF level 3 THEN
C?        Print cluster storeys
C?     IF level 2 THEN
C?        Print a line per cluster over threshold
C?     IF level 1 THEN
C?        Print number of clusters and total energy
C?        Print SILH and SILU banks
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rlunit.h"
#include "rflags.h"
#include "sinamc.h"
#include "spdajj.h"
#include "sclsjj.h"
#include "srecjj.h"
      PARAMETER( NMODU=2, NSTOR=12, NROWS=32, NCOLS=16)
      DIMENSION IFLAG(NMODU,NSTOR)
#include "bmacro.h"
C -------------------------------------------------------------
      IF(JDBDRF(JULSI).GE.3) WRITE(LDEBRL,2000)
C
C Constants
      ECUT=0.010
      KSREC = IW(NASREC)
      IF(KSREC.GT.0) ECUT = RTABL(KSREC,1,JSREEC)
C
C Level 4?
      IF(JDBDRF(JULSI).LE.3) GOTO 250
      IF(IW(NASMPD).GT.0) CALL PRTABL('SMPD',0)
      IF(IW(NASPDA).GT.0) CALL PRTABL('SPDA',0)
      IF(IW(NASCLS).GT.0) CALL PRTABL('SCLS',0)
C
C Level 3?
  250 IF(JDBDRF(JULSI).LE.2) GOTO 450
C
C  Locate Storey Data
      KINDX = IW(NASPDA)
      IF(KINDX .LE. 0)                                 GOTO 999
      IF (LROWS(KINDX) .EQ. 0)                         GOTO 999
C
C Initialise print table
      KLISTW = 0
      LEN = NMODU*NROWS*NCOLS*NSTOR
      CALL WBANK(IW,KLISTW,LEN,*995)
      CALL VZERO(IFLAG,NMODU*NSTOR)
      IFL = 0
C
C For all rows in SPDA
      DO 330 J = 1,LROWS(KINDX)
        KR = KROW(KINDX,J)
        IAD = IW(KR+JSPDAD)
        CALL SIDCOZ(IAD,MODU,IPL,IROW,ICOL)
        IF (MODU.EQ.-1) GO TO 330
        E=0.
C
C Store the pad energies at their address in 100 MeV units
        LOFF = (((MODU-1)*NSTOR+IPL-1)*NCOLS+ICOL-1)*NROWS+IROW
        IW(KLISTW+LOFF) = INT(RW(KR+JSPDEN)/ECUT)
        IF(IW(KLISTW+LOFF).GE.999) IW(KLISTW+LOFF)=999
        IF(IW(KLISTW+LOFF).LE.-99) IW(KLISTW+LOFF)=-99
        E = E + RW(KR+JSPDEN)
C
C and flag the stack if energy is above threshold
        IF(E.GT.ECUT) THEN
          IFLAG(MODU,IPL)=1
          IFL = 1
        ENDIF
  330 CONTINUE
      IF(IFL.NE.0) WRITE (LDEBRL,4000)
C
C Print storey energies
      DO 360 MODU = 1,NMODU
        IFMOD = 0
        DO 350 IY = 1,NSTOR
          ISTO = IY
C
C Print only if the plan contains energy
          IF(IFLAG(MODU,ISTO).EQ.0)              GOTO 350
          IF(IFMOD.EQ.0) THEN
            WRITE(LDEBRL,4001) MODU
            WRITE(LDEBRL,4003)
          ENDIF
          IFMOD = 1
C
C Find the row in the print table
          KADR = KLISTW + ((MODU-1)*NSTOR+ISTO-1)*NROWS*NCOLS
C
C Now print the rows
          WRITE(LDEBRL,4004) ISTO
          DO 349 IRAD=1,NCOLS
            WRITE(LDEBRL,4005) IRAD,
     &                (IW(I),I=KADR+1,KADR+NROWS)
            KADR = KADR + NROWS
  349     CONTINUE
  350   CONTINUE
  360 CONTINUE
C
C Drop print table
      CALL WDROP(IW,KLISTW)
C
  450 CONTINUE
C
C  Print SCLS bank
      KINDX = IW(NASCLS)
      IF(KINDX .LE. 0)                                 GOTO 999
      IF (LROWS(KINDX) .EQ. 0)                         GOTO 999
      IF(JDBDRF(JULSI).GE.2) WRITE (LDEBRL,5000)
      NCLUS = LROWS(KINDX)
      ETOT = 0.
      DO 500 ICL = 1,NCLUS
        ETOT = ETOT + RTABL(KINDX,ICL,JSCLEC)
C
C Level 2?
        IF(JDBDRF(JULSI).LE.1)                        GOTO 500
        IMODU = 1
        IF ( RTABL(KINDX,ICL,JSCLZC).LT.0.) IMODU = 2
        WRITE(LDEBRL,5001) ICL,IMODU,RTABL(KINDX,ICL,JSCLEC)
        WRITE(LDEBRL,5002)
     &     RTABL(KINDX,ICL,JSCLRC),RTABL(KINDX,ICL,JSCLPC),
     &     RTABL(KINDX,ICL,JSCLWR),RTABL(KINDX,ICL,JSCLWP)
  500 CONTINUE
C
C Level 1 print
      WRITE(LDEBRL,6000) NCLUS,ETOT
      CALL PRTABL('SILH',0)
      CALL PRTABL('SILU',0)
C
      GOTO 999
C
  995 WRITE(LDEBRL,7000)
      GOTO 999
C
 2000 FORMAT(/,' +++ SIBKPR +++ output banks from SCal',
     &       /,' =====================================')
 4000 FORMAT(//,'  Pad  Data  per plane ',
     &       /,'  energies in 10  MeV units',
     &       /,'  -1 means dead or non-existing pad',
     &       /,'  ===========================================')
 4001 FORMAT(/,1X,'Module',I3)
 4003 FORMAT(1X,'COL',1X,'   1   2   3   4   5   6   7   8   9',
     *       2X,'10  11  12  13  14  15  16  17  18  19  20  21',
     *       2X,'22  23  24  25  26  27  28  29  30  31  32',
     *       /,1X,'ROW')
 4004 FORMAT(1X,'PLANE ' ,I3)
 4005 FORMAT(1X,I3,1X,32I4)
 5000 FORMAT(/,2X,'Clusters with energy > 0.0 GeV:')
 5001 FORMAT(/,2X,'Cluster',I2,2X,' Module',I2,'   E (GeV) =',F8.2)
 5002 FORMAT(2X,'Radius(cm ) =',F8.4,'   Phi (rad)=',F8.4,
     &       '   Rsize(cm) =',F6.2,' Phisize=',F6.3)
 6000 FORMAT(/,2X,'SIBKPR: ',I3,' SCal clusters with E>0.0 GeV',
     &         4X,'Etot = ',F8.2,/)
 7000 FORMAT(' +++ SIBKPR +++ no room for work bank')
C
  999 CONTINUE
      END
#endif
