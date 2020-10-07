      SUBROUTINE MUCATR(ITRA,PR1OUT,PR2OUT,IFLAG)
C---------------------------------------------------------------
C! Probability for a track to be a muon from MUCALO
C  Author:    R.Tenchini     900128
CKEY MUCAL TRACK CALO / USER
C  Input :
C     ITRA = track index in PFRF
C  Output:
C     PR1OUT  = Probability (from 0 to 100%) to have muon in
C               the calobject linked to that track
C     PR2OUT  = Same meaning as PR1, but for prompt muons only
C               (i.e. exiting HCAL)
C     IFLAG = return flag.
C                          1 = OK
C                          0 = Calob without Digital Patterns
C                         -1 = Track not linked to calobs
C                         -2 = Maklis or Mucalo vectors underdimensioned
C
C - Input banks : PCRL
C
C--------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "mucapa.h"
#include "bcs.h"
#include "pcrljj.h"
      DIMENSION IPLIS(LENVEC),ICLOS(LENVEC)
      PARAMETER(MAXL=100)
      DIMENSION NCAVEC(MAXL)
      DATA IFI /0/
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C - 1st entry : set name-indices
C
      IF (IFI .EQ. 0) THEN
         IFI = 1
         NAPCRL = NAMIND('PCRL')
      ENDIF
C
C - next entry
C
      IPCRL = IW(NAPCRL)
      IF(IPCRL.LE.0) GOTO 909
C
      IFLAG=-2
      CALL MAKLIS(IPCRL,JPCRPF,JPCRPC,ITRA,NCLOB,ICLOS,IER)
      IF(IER.NE.0) RETURN
      IFLAG=-1
      IF(NCLOB.EQ.0) RETURN
      PR1OUT=0.
      PR2OUT=0.
      IFLAG=0
      DO 100 J=1,NCLOB
         ICAL=ICLOS(J)
         CALL MAKLIS(IPCRL,JPCRPC,JPCRPP,ICAL,NPATT,IPLIS,IER)
         IF(IER.NE.0) THEN
            IFLAG=-2
            RETURN
         ENDIF
         IF(NPATT.EQ.0) GO TO 100
         CALL MUCALO(ICAL,PR1,ERPR1,PR2,ERPR2,NCAL,NCAVEC,MAXL,IER)
         IF(IER.NE.0) THEN
            IFLAG=-2
            RETURN
         ENDIF
         IFLAG=1
         IF(PR1OUT.LT.PR1) PR1OUT=PR1
         IF(PR2OUT.LT.PR2) PR2OUT=PR2
100   CONTINUE
909   RETURN
      END
#endif
