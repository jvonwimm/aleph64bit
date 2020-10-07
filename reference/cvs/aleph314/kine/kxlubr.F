      SUBROUTINE KXLUBR
C------------------------------------------------------------------
C  P. Henrard - 881117           B.Bloch-Devaux -881205
C! Modify decay scheme inside LUND for requested particles
CKEY KINE KINGAL LUND DECAY  /  INTERNAL
C  Bos data cards are used with the following convention:
C  GADB : define new decay channel for given particle in LUND
C  GREP : replace a final state by another one
C  GMOB : defines the branching ratios of a LUND particle
C
C  GADB KF KDP1 KDP2 KDP3 KDP4
C       KF Refers to the Lund particle code
C       KDP1-KDP4 Refer to the lund code of the 4 particles final state
C        to be added at the end of the list of decay modes.
C       (if less than 4 particles needed, fill others with 0
C        include matrix element code in KDP1 if necessary)
C  GREP KF  II  KDP1 KDP2 KDP3 KDP4
C       KF Refers to the Lund particle code
C       II Refers to the decay mode number in the list for particle KF
C       KDP1-KDP4 Refer to the lund code of the 4 particles final state
C       (if less than 4 particles needed, fill others with 0
C        include matrix element code in KDP1 if necessary)
C  GMOB KF  BR(1)..................BR(n)
C       KF Refers to the Lund particle code
C       BR(1)...BR(n) are the non-cumulated branching fractions
C       of the n decay modes defined for particle KF
C
C  you may have as many GADB as you want as long as you do not
C  overfill the IDBLU3 array (dimension 400) otherwise an error message
C  is issued and the process is stopped.
C  GREP cards are treated in sequence as they appear.
C  GMOB cards should define as many Branching fractions as decay modes
C  including those added through GADB cards.
C
C - structure : SUBROUTINE
C               User entry name :KXLUBR
C               External references:NAMIND/BKFMT/BLIST(BOS77)
C                                   LULIST(JETSET)
C               Comdecks refenced :BCS,LUNDCOM
C
C - usage : CALL KXLUBR
C------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "lundcom.h"
      NAMI =NAMIND('GADB')
      IF (IW(NAMI ).EQ.0) GO TO 96
      JGADB=NAMI +1
 100  JGADB=IW(JGADB-1)
      IF(JGADB.EQ.0) GOTO 97
C
C Create a new line in the original LUND particle data table
C for the particule NKF1
C
      NKF1=IW(JGADB+1)
      DO 200 K=L3IDB,1,-1
        IF(IDBLU3(K).LE.(IDBLU3(NKF1+1)-1))GOTO 200
        IF(IDBLU3(K).GE.L3CBR)GO TO 1000
C Shift pointer to create space for new entry
        IDBLU3(K)=IDBLU3(K)+1
        IF(IDBLU3(K).EQ.IDBLU3(K+1))GOTO 200
        LL=IDBLU3(K)-2
        N=0
 300    LL=LL+1
        N=N+1
        IF(CBRLU3(LL).LT.1.)GOTO 300
C Shift branching ratios and decay modes accordingly
           DO 400 J=1,N
             CBRLU3(LL-J+2)=CBRLU3(LL-J+1)
             KDPLU3(4*(LL-J+1)+1)=KDPLU3(4*(LL-J+1)-3)
             KDPLU3(4*(LL-J+1)+2)=KDPLU3(4*(LL-J+1)-2)
             KDPLU3(4*(LL-J+1)+3)=KDPLU3(4*(LL-J+1)-1)
             KDPLU3(4*(LL-J+1)+4)=KDPLU3(4*(LL-J+1))
 400       CONTINUE
 200  CONTINUE
C
C define the final state of the new entry
C
      KDPLU3(4*(IDBLU3(NKF1+1)-1)-3)=IW(JGADB+2)
      KDPLU3(4*(IDBLU3(NKF1+1)-1)-2)=IW(JGADB+3)
      KDPLU3(4*(IDBLU3(NKF1+1)-1)-1)=IW(JGADB+4)
      KDPLU3(4*(IDBLU3(NKF1+1)-1))=IW(JGADB+5)
      GOTO 100
 97   CONTINUE
C store the card on the C list
      CALL BKFMT('GADB','I')
      CALL BLIST(IW,'C+','GADB')
  96  CONTINUE
C
C  Update the cumulated branching ratios
C
      NAMI =NAMIND('GMOB')
      IF (IW(NAMI ).EQ.0) GO TO 95
      JGMOB=NAMI +1
 101  JGMOB=IW(JGMOB-1)
      IF(JGMOB.EQ.0)GOTO 98
      NKF2=IW(JGMOB+1)
C define the branching ratios of the particle NKF2
      DO 500 JJ=2,IW(JGMOB)
        IF(JJ.EQ.2)THEN
          CBRLU3(IDBLU3(NKF2+1)+1-IW(JGMOB))=RW(JGMOB+2)
        ELSE
          CBRLU3(IDBLU3(NKF2+1)-1-IW(JGMOB)+JJ)=
     >    CBRLU3(IDBLU3(NKF2+1)-2-IW(JGMOB)+JJ)+RW(JGMOB+JJ)
        ENDIF
 500  CONTINUE
      GOTO 101
 98   CONTINUE
C store the card on the C list
      CALL BKFMT('GMOB','I,(F)')
      CALL BLIST(IW,'C+','GMOB')
  95  CONTINUE
      NAMI =NAMIND('GREP')
      IF (IW(NAMI ).EQ.0) GO TO 94
      JGREP=NAMI +1
C
C Replace decay mode by a new definition
C
 102  JGREP=IW(JGREP-1)
      IF(JGREP.EQ.0)GOTO 99
      NKF3=IW(JGREP+1)
C
C replace final state IK by another one for the particle NKF3
C
      IK = IW(JGREP+2)
      KDPLU3(4*(IDBLU3(NKF3)+IK-1)-3)=IW(JGREP+3)
      KDPLU3(4*(IDBLU3(NKF3)+IK-1)-2)=IW(JGREP+4)
      KDPLU3(4*(IDBLU3(NKF3)+IK-1)-1)=IW(JGREP+5)
      KDPLU3(4*(IDBLU3(NKF3)+IK-1))  =IW(JGREP+6)
      GOTO 102
   99 CONTINUE
C store the card on the C list
      CALL BKFMT('GREP','I')
      CALL BLIST(IW,'C+','GREP')
  94  CONTINUE
C     CALL LULIST(3)
      RETURN
 1000 WRITE (IW(6),550) NKF1,K
      CALL EXIT
  550 FORMAT(1X,'++++++KXLUBR+++++++ cannot extend decay list for',
     $ ' particle',I10,'Pointer idblu3 was',I10)
      END
#endif
