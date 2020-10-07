      SUBROUTINE ENFJET(IREJET)
C-----------------------------------------------------------------------
C! Create jet bank corresponding to EFOL/3 (ENFLW).
C
C     Author: Patrick Janot      18-Jul-92
C
C     Input  : EFOL bank number 3
C  Outputs: -- irejet = 0 if no room for bank EJET
C              EJET bank number 3
C
C     Called by CREFLW
C     The jet link in EFOL/3 is supplied.
C-----------------------------------------------------------------------
C
#ifndef DOC
C
      PARAMETER (NPMX=200)
      DIMENSION PX(NPMX),PY(NPMX),PZ(NPMX),PE(NPMX),MW(NPMX)
      DIMENSION DUM1(NPMX,NPMX),DUM2(5,NPMX),JNOFP(NPMX)
      PARAMETER (NJMX=50)
      DIMENSION PJX(NJMX),PJY(NJMX),PJZ(NJMX),PJE(NJMX)
      DIMENSION X(3),DX(3)
      SAVE LRUN,ELEP,INIT,IERR,IEFLW,NEFLW,CUTSI,EPSIL,NRUSI
      DATA YCUT / 0.003 /
      DATA LRUN / -999 /
C
      CHARACTER*8 PROGNAM
      CHARACTER*4 CHAINT
C
      INTEGER IRFSI(5),IRLSI(5)
      DATA IRFSI / 14647 , 16600 , 3*0 /
      DATA IRLSI / 14831 , 20000 , 3*0 /
      DATA NRUSI / 2 /
      DATA INIT / 0 / , IERR / 0 / , CUTSI / 0.058 / , EPSIL / 1.E-08 /
C
#include "qcde.h"
#include "qhac.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
      IREJET = 0
      KEFOL = NLINK('EFOL',3)
      IF (KEFOL.LE.0) RETURN
      NEFOL = LROWS(KEFOL)
      IF (NEFOL.LE.0) RETURN
C
C Check on real data runs of 1992 with SICAL in the setup :
C Check also the presence of NOSC cards to kill Sical clusters anyway
C
      IRSIB = 0
      DO 5 IRU = 1,NRUSI
         IF (KRUN.GE.IRFSI(IRU).AND.KRUN.LE.IRLSI(IRU)) IRSIB = IRU
 5    CONTINUE
      IF ( NLINK('NOSC',0) .GT. 0 ) irsib = 999
C
      IF ( irsib .GE. 1 ) THEN
        mindat = 0
        narhah = NAMIND('RHAH')
        IF ( narhah .GT. 0 ) irhah = IW(narhah)
        IF ( irhah .GT. 0 ) THEN
          DO istep = 1, LROWS(irhah)
            prognam(1:4) = CHAINT(ITABL(irhah,istep,1))
            prognam(5:8) = CHAINT(ITABL(irhah,istep,2))
            IF ( prognam .EQ. 'MINI    ' ) mindat = ITABL(irhah,istep,3)
          ENDDO
        ENDIF
      ENDIF
C
C++   Obtain LEP energy for new run.
C
      CALL ABRUEV(IRUN,IEVT)
      IF (IRUN.NE.LRUN) THEN
         CALL GETLEP(IRUN, IFOUND,IFILL,NUSED,ELEP,X,DX)
         LRUN = IRUN
      ENDIF
      ENORM = ELEP
C
C++   Load ENFLW objects into vectors.
C
      J = 0
      DO 100 I=1,NEFOL
         IF (J.GE.NPMX) GOTO 100
C --------------------------------------------------------------------
C Special patch to kill SICAL clusters from 1992 real data only :
C and for all data if NOSC card is present
         IF ( irsib .GE. 1 ) THEN
           IF ( ITABL(kefol,i,jefoty) .EQ. 8 ) GOTO 100
           IF ( ITABL(kefol,i,jefoty) .EQ. 7 .AND.
     .          XMINI .AND. mindat .LT. 931128 ) THEN
             PXI = RTABL(KEFOL,I,JEFOPX)
             PYI = RTABL(KEFOL,I,JEFOPY)
             PZI = RTABL(KEFOL,I,JEFOPZ)
             PTI = SQRT ( pxi**2 + pyi**2 )
             angn = PIBY2
             IF ( ABS(pzi) .GT. epsil ) angn = ABS(pti/pzi)
             IF ( angn .GT. cutsi ) GOTO 101
             IF ( irsib.GT.1 .OR. (irsib.EQ.1.AND.pzi.LT.0.)) GOTO 100
           ENDIF
         ENDIF
  101    CONTINUE
C --------------------------------------------------------------------
         J = J + 1
         PX(J) = RTABL(KEFOL,I,JEFOPX)
         PY(J) = RTABL(KEFOL,I,JEFOPY)
         PZ(J) = RTABL(KEFOL,I,JEFOPZ)
         PE(J) = RTABL(KEFOL,I,JEFOEW)
         MW(J) = KROW (KEFOL,I)
  100 CONTINUE
C
C++   Perform jet finding.
C
      NP = J
      IF (NP.LE.0) RETURN
      CALL FJMMCL(YCUT,ENORM,'E ','NORMAL',
     &  DUM1,DUM2,NP,PX,PY,PZ,PE,NJMX, NJ,PJX,PJY,PJZ,PJE,JNOFP)
      IF (NJ.LE.0) RETURN
C
C++   Create EJET bank.
C
      LEN = LMHLEN + LEJETA * NJ
      KEJET = NBANK('EJET',3,LEN)
      IF ( KEJET .EQ. 0 ) THEN
         WRITE (IW(6),*)
     .    ' **** ENFJET **** Unable to create EJET bank.'
         GOTO 997
      ENDIF
      IW(KEJET+LMHCOL) = LEJETA
      IW(KEJET+LMHROW) = NJ
C
      DO 300 I=1,NJ
         RW(KROW(KEJET,I)+JEJEPX) = PJX(I)
         RW(KROW(KEJET,I)+JEJEPY) = PJY(I)
         RW(KROW(KEJET,I)+JEJEPZ) = PJZ(I)
         RW(KROW(KEJET,I)+JEJEPE) = PJE(I)
  300 CONTINUE
C
C++   Supply the jet link in EFOL.
C
      DO 400 I=1,NP
  400 IW(MW(I)+JEFOLJ) = JNOFP(I)
C
      CALL BLIST(IW,'S+','EJET')
      IREJET = 1
      GO TO 999
C No room for banks :
 997  CALL QWMESE(
     +'0ENFJET_ : Unable to create ENFLW banks :--> Enlarge BOS IW !!')
  999 RETURN
C
      END
#endif
