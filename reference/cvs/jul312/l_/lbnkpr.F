      SUBROUTINE LBNKPR
C-----------------------------------------------------------------------
C!   Print Lcal output banks
C!
C!   Author   : P. H. Hansen  870401
C!   Modified : P. H. Hansen  890102
C!   Modified : P. H. Hansen  960501 Only print when there are clusters
C!
C!     Input  :
C!              'LSDA','LPDA',LCLU'
C!
C!     Description :
C!     ===========
C!     Print Lcal banks in nice "grafics" format
C?
C?     IF level 4 THEN
C?        Dump all output banks
C?     IF level 3 THEN
C?        Print cluster storeys as viewed from the origin
C?        Print wire energies of each storey
C?     IF level 2 THEN
C?        Print a line per cluster over threshold
C?     IF level 1 THEN
C?        Print number of clusters and total energy
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rlunit.h"
#include "rflags.h"
#include "lbasic.h"
#include "lcnamc.h"
#include "lsdajj.h"
#include "lpdajj.h"
#include "lclujj.h"
#include "lrwgjj.h"
#include "lcrejj.h"
#include "lwbank.h"
      DIMENSION IFLAG(NMODU,NROWS)
#include "bmacro.h"
#include "lcmacr.h"
C -------------------------------------------------------------
      KINDX = IW(NALCLU)
      IF(KINDX .LE. 0)                                 GOTO 999
      IF (LROWS(KINDX) .EQ. 0)                         GOTO 999
      IF(JDBDRF(JULLC).GE.3) WRITE(LDEBRL,2000)
C
C Constants
      ECUT=0.100
      KLCRE = IW(NALCRE)
C     IF(KLCRE.GT.0) ECUT = RTABL(KLCRE,1,JLCREC)
C
C Level 4?
      IF(JDBDRF(JULLC).LE.3) GOTO 250
      IF(IW(NALSDA).GT.0) CALL PRTABL('LSDA',0)
      IF(IW(NALPDA).GT.0) CALL PRTABL('LPDA',0)
      IF(IW(NALCLU).GT.0) CALL PRTABL('LCLU',0)
C
C Level 3?
  250 IF(JDBDRF(JULLC).LE.2) GOTO 450
C
C  Locate Storey Data
        KINDX = IW(NALSDA)
        IF(KINDX .LE. 0)                                 GOTO 999
        IF (LROWS(KINDX) .EQ. 0)                         GOTO 999
C
C Initialise print table
        KLISTW = 0
        LEN = NMODU*NROWS*NCOLS*NSTOR
        CALL WBANK(IW,KLISTW,LEN,*995)
        CALL VZERO(IFLAG,NMODU*NROWS)
        IFL = 0
C
C For all rows in LSDA
        DO 330 J = 1,LROWS(KINDX)
          KR = KROW(KINDX,J)
          IPAD = IW(KR+JLSDAD)
          MODU=LCMOD(IPAD)
          IROW=LCROW(IPAD)
          ICOL=LCCOL(IPAD)
          E=0.
C
C Store the storey energies at their address in 100 MeV units
          DO 320 IST=1,3
            LOFF = (((MODU-1)*NROWS+IROW-1)*NSTOR+IST-1)*NCOLS+ICOL
            IW(KLISTW+LOFF) = INT(RW(KR+JLSDEN+IST-1)/ECUT)
            IF(IW(KLISTW+LOFF).GE.999) IW(KLISTW+LOFF)=999
            IF(IW(KLISTW+LOFF).LE.-99) IW(KLISTW+LOFF)=-99
            E = E + RW(KR+JLSDEN+IST-1)
  320     CONTINUE
C
C and flag the row if energy is above threshold
          IF(E.GT.ECUT) THEN
             IFLAG(MODU,IROW)=1
             IFL = 1
          ENDIF
  330   CONTINUE
        IF(IFL.NE.0) WRITE (LDEBRL,4000)
C
C Print storey energies as seen for the interaction point
        DO 360 MODU = 1,NMODU
           IFMOD = 0
           DO 350 IY = 1,NROWS
              IROW = NROWS - IY + 1
C
C Print only if the row contains energy
              IF(IFLAG(MODU,IROW).EQ.0)              GOTO 350
              IF(IFMOD.EQ.0) THEN
                 WRITE(LDEBRL,4001) MODU
                 IF(MOD(MODU,2).EQ.1) THEN
                    WRITE(LDEBRL,4002)
                 ELSE
                    WRITE(LDEBRL,4003)
                 ENDIF
              ENDIF
              IFMOD = 1
C
C Find the row in the print table
              KADR = KLISTW + ((MODU-1)*NROWS+IROW-1)*NSTOR*NCOLS
C
C Find the first and the last column in the acceptance
              KLRWG = IW(NALRWG)
              IC1 = ITABL(KLRWG,IROW,JLRWLC)
              IC2 = IC1 + ITABL(KLRWG,IROW,JLRWNC) -1
C
C Put content = -1, if outside acceptance
              DO 345 IC=1,NCOLS
                 IF(IC.LT.IC1.OR.IC.GT.IC2) THEN
                    IW(KADR+IC) = -1
                    IW(KADR+IC+NCOLS) = -1
                    IW(KADR+IC+2*NCOLS) = -1
                 ENDIF
  345         CONTINUE
C
C Now print the rows
              IF(MOD(MODU,2).EQ.1) THEN
                 WRITE(LDEBRL,4004) IROW,
     &                (IW(I),I=KADR+NCOLS,KADR+1,-1)
                 WRITE(LDEBRL,4005)
     &                (IW(I),I=KADR+2*NCOLS,KADR+NCOLS+1,-1)
                 WRITE(LDEBRL,4006)
     &                (IW(I),I=KADR+3*NCOLS,KADR+2*NCOLS+1,-1)
              ELSE
                 WRITE(LDEBRL,4004) IROW,(IW(I),I=KADR+1,KADR+NCOLS)
                 WRITE(LDEBRL,4005) (IW(I),I=KADR+NCOLS+1,KADR+2*NCOLS)
                 WRITE(LDEBRL,4006)
     &                (IW(I),I=KADR+2*NCOLS+1,KADR+3*NCOLS)
              ENDIF
  350      CONTINUE
  360   CONTINUE
C
C Drop print table
      CALL WDROP(IW,KLISTW)
C
C Print LPDA bank
      KINDX = IW(NALPDA)
      IF(KINDX.LE.0)                                   GOTO 450
      NM = LROWS(KINDX)
      IF(NM.EQ.0)                                      GOTO 450
      DO 430 M=1,NM
         KADR=KROW(KINDX,M)
         NP = LCOLS(KINDX)
         IF(IW(KADR+1).GE.1) THEN
         WRITE(LDEBRL,4500) IW(KADR+1)
         WRITE(LDEBRL,4501) (RW(I),I=KADR+2,KADR+NP)
         ENDIF
  430 CONTINUE
  450 CONTINUE
C
C  Print LCLU bank
      KINDX = IW(NALCLU)
      IF(KINDX .LE. 0)                                 GOTO 999
      IF (LROWS(KINDX) .EQ. 0)                         GOTO 999
      IF(JDBDRF(JULLC).GE.2) WRITE (LDEBRL,5000)
      NCLUS = LROWS(KINDX)
      ETOT = 0.
      DO 500 ICL = 1,NCLUS
         ETOT = ETOT + RTABL(KINDX,ICL,JLCLE4)
C
C Level 2?
         IF(JDBDRF(JULLC).LE.1)                        GOTO 500
         IF(RTABL(KINDX,ICL,JLCLE4).LT.5.*ECUT)        GOTO 500
         WRITE(LDEBRL,5001) ICL,ITABL(KINDX,ICL,JLCLMO),
     &     RTABL(KINDX,ICL,JLCLE4)
         SIZE = SQRT(RTABL(KINDX,ICL,JLCLXX)**2+
     &               RTABL(KINDX,ICL,JLCLYY)**2)
         FLEAK= RTABL(KINDX,ICL,JLCLE3)/RTABL(KINDX,ICL,JLCLE4)
         WRITE(LDEBRL,5002)
     &     RTABL(KINDX,ICL,JLCLT4),RTABL(KINDX,ICL,JLCLF4),
     &     SIZE,FLEAK
  500   CONTINUE
C
C Level 1 print
        WRITE(LDEBRL,6000) NCLUS,ETOT
C
      GOTO 999
C
  995 WRITE(LDEBRL,7000)
      GOTO 999
C
 2000 FORMAT(/,' +++ LBNKPR +++ output banks from LCal',
     &       /,' =====================================')
 4000 FORMAT(//,'  Storey Data as seen from the interaction :',
     &       /,'  Storey1',/,'  Storey2',/,'  Storey3',
     &       /,'  energies in 100 MeV units',
     &       /,'  -1 means dead or non-existing storey',
     &       /,'  ===========================================')
 4001 FORMAT(/,1X,'Module',I3)
 4002 FORMAT(1X,'COL',2X,'  16  15  14  13  12  11  10   9   8',
     *       3X,'7   6   5   4   3   2   1',/,1X,'ROW')
 4003 FORMAT(1X,'COL',2X,'   1   2   3   4   5   6   7   8   9',
     *       3X,'10  11  12  13  14  15  16',/,1X,'ROW')
 4004 FORMAT(1X,I3,2X,16I4)
 4005 FORMAT(6X,16I4)
 4006 FORMAT(6X,16I4,/)
 4500 FORMAT(//,'  Wire plane map of module:',I3,/)
 4501 FORMAT(2X,10F5.1,/,2X,18F5.1,/,2X,10F5.1,/,2X,'Sum:',F10.1)
 5000 FORMAT(/,2X,'Clusters with energy > 0.5 GeV:')
 5001 FORMAT(/,2X,'Cluster',I2,2X,' Module',I2,'   E (GeV) =',F8.2)
 5002 FORMAT(2X,'Theta (rad) =',F8.4,'   Phi (rad)=',F8.4,
     &       '   Size (cm) =',F6.2,'   E3/E =',F6.3)
 6000 FORMAT(/,2X,'LBNKPR: ',I3,' LCal clusters with E>0.5 GeV',
     &         4X,'Etot = ',F8.2,/)
 7000 FORMAT(' +++ LBNKPR +++ no room for work bank')
C
  999 CONTINUE
      END
#endif
