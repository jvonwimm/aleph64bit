*DK asgpar
      SUBROUTINE ASGPAR
C ----------------------------------------------------------------------
C. - F.Ranjard - 850828
C! Fill 'PTYP' Geant3 bank with 'PART' BOS bank
C. - Define branching ratios and decay modes for standard GEANT particle
C.   (copy of GPART from GEANT pam)
C. - called by    ASIGEA                                from this .HLB
C. - calls        GSDK                                  from GEANT pam
C ----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "iocom.h"
#include "kincom.h"
#include "jobcom.h"
      CHARACTER*13 NAME
      CHARACTER*4 CHAINT
#include "gcbank.h"
#include "gcphys.h"
#include "gcnum.h"
      DATA IFI /0/
C.
C ----------------------------------------------------------------------
      IF (NAPART.EQ.0) THEN
         CALL ALTELL ('ASGPAR : NO PARTicle data bank ',0,'STOP')
      ENDIF
C
C - 'PART' : # of words/particle, # of particles and then
C            GEANT# (I), name(3words), tracking type (I), mass, charge,
C            time of life
      KPART = IW(NAPART)
      LP1   = IW(KPART+1)
      LP2   = IW(KPART+2)
      KP    = KPART + LMHLEN
      LROW = MIN (LP2,NGPART)
      DO 1 I=1,LROW
         IF (IFI.GT.0 .AND. IW(KP+5).EQ.NOTRKI) GOTO 1
         IF (IW(KP+5).EQ.NOTRKI) IFI=1
         NAME(1:4) = CHAINT(IW(KP+2))
         NAME(5:8) = CHAINT(IW(KP+3))
         NAME(9:13)= CHAINT(IW(KP+4))//'$'
         CALL GSPART (IW(KP+1),NAME,IW(KP+5),RW(KP+6),RW(KP+7)
     &               ,RW(KP+8),0,0)
 1       KP = KP + LP1
C
C - Define decay modes
C
      IF (IGDCAY.GT.0 .AND. IPROJO(2).GT.0) THEN
         KGDEC=NLINK('GDEC',0)
         IF (KGDEC.LE.0) CALL ALTELL ('AGSPAR: NO GDEC bank ',0,'STOP')
         LP1=IW(KGDEC+1)
         LP2=IW(KGDEC+2)
         KP=KGDEC+LMHLEN
         DO 10 I=1,LP2
            CALL GSDK(IW(KP+1),RW(KP+2),IW(KP+8))
            KP=KP+LP1
  10     CONTINUE
      ENDIF
C - Debug
      IF(IPRIJO(20).EQ.1) THEN
         CALL GPPART(0)
         IF (IPRIJO(19).EQ.1)  CALL PRGDEC
      ENDIF
C
      RETURN
      END
#endif
