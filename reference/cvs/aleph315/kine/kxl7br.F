      SUBROUTINE KXL7BR
C------------------------------------------------------------------
C          B.Bloch-Devaux -910125
C! Modify decay scheme inside LUND7.3 for requested particles
CKEY KINE KINGAL LUND7 DECAY  /  INTERNAL
C  Bos data cards are used with the following convention:
C  GADM : define new decay channel for given particle in LUND
C  GRPL : replace a final state by another one
C  GMOB : defines the branching ratios of a LUND particle
C
C  GADM KF MXEL KDP1 KDP2 KDP3 KDP4  KDP5
C       KF Refers to the Lund7 particle code
C       MXEL is the matrix element to be used for that decay
C       KDP1-KDP5 Refer to the lund7 code of the 5 particles final state
C        to be added at the end of the list of decay modes.
C       (if less than 5 particles needed, fill others with 0  )
C        include matrix element code in MXEL, if MXEL=101  5 more
C        positions can be filled for the same decay chain
C  GRPL KF  II  MSWI  MXEL KDP1 KDP2 KDP3 KDP4  KDP5
C       KF Refers to the Lund particle code
C       II Refers to the decay mode number in the list for particle KF
C       MSWI is the switch to be used for that decay ( 0,-1,1,....5)
C       MXEL is the matrix element to be used for that decay
C       KDP1-KDP5 Refer to the lund code of the 5 particles final state
C       (if less than 5 particles needed, fill others with 0  )
C  GMOB KF  BR(1)..................BR(n)
C       KF Refers to the Lund particle code
C       BR(1)...BR(n) are the non-cumulated branching fractions
C       of the n decay modes defined for particle KF
C
C  you may have as many GADM as you want as long as you do not overfill
C  the LUDAT3 array (dimension 2000)otherwise an error message is issued
C  and the process is stopped.
C  NOTE: all GADM cards with the same KF must follow each other !!
C ------
C  GRPL cards are treated in sequence as they appear.
C  GMOB cards should define as many Branching fractions as decay modes
C  including those added through GADM cards.If not, a message is issued
C  and no modification is done.When modified a printout of the resulting
C  decay modes and branching ratios for the particle is issued.
C
C - structure : SUBROUTINE
C               User entry name :KXL7BR
C               External references:NAMIND/BKFMT/BLIST(BOS77)
C                                   KXL7ST( this library)
C                                   LUCOMP(JETSET7.3)
C               Comdecks refenced :BCS,LUN7COM
C
C - usage : CALL KXL7BR
C------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "lun7com.h"
C ----------------------------------------------------------------
      DATA NKF0/-1/
      NAMI =NAMIND('GADM')
      IF (IW(NAMI ).EQ.0) GO TO 96
      JGADM=NAMI +1
 100  JGADM=IW(JGADM-1)
      IF(JGADM.EQ.0) GOTO 97
C
C Create a new line in the original LUND particle data table
C for the particule NKF1 after copying the existing ones at the end of
C the array
C
      NKF1=IW(JGADM+1)
C  get compressed code  for particle
      KC = LUCOMP(NKF1)
      IF ( KC.GT.0) THEN
         NOLD = MDCY(KC,3)
C  Find a new free location to translate the decay tables
         IF (NKF1.NE.NKF0) THEN
            NKF0 = NKF1
            DO 200 K=L2PARF,1,-1
            DO 300 J = 1,5
               IF(KFDP(K,J).NE.0 )GO TO 400
  300       CONTINUE
  200       CONTINUE
            GO TO 1000
  400       IFREE = K+1
C   copy old modes to the new location
            IOLD = MDCY(KC,2)
            IF(NOLD.GT.0 .AND. IOLD.GT.0) THEN
               DO 401 J=1,NOLD
                  BRAT(IFREE+J-1)=BRAT(IOLD+J-1)
                  MDME(IFREE+J-1,1)=MDME(IOLD+J-1,1)
                  MDME(IFREE+J-1,2)=MDME(IOLD+J-1,2)
                  DO 402 K=1,5
                     KFDP(IFREE+J-1,K)=KFDP(IOLD+J-1,K)
  402          CONTINUE
  401          CONTINUE
            ENDIF
C   Update with new mode
            MDCY(KC,1) = 1
            MDCY(KC,2) = IFREE
         ENDIF
         MDCY(KC,3) = NOLD+1
C
C define the final state of the new entry
C
         MDME(IFREE+NOLD,1)= 1
         MDME(IFREE+NOLD,2)= IW(JGADM+2)
         KFDP(IFREE+NOLD,1)= IW(JGADM+3)
         KFDP(IFREE+NOLD,2)= IW(JGADM+4)
         KFDP(IFREE+NOLD,3)= IW(JGADM+5)
         KFDP(IFREE+NOLD,4)= IW(JGADM+6)
         KFDP(IFREE+NOLD,5)= IW(JGADM+7)
      ENDIF
      GOTO 100
 97   CONTINUE
C store the card on the C list
      CALL BKFMT('GADM','I')
      CALL BLIST(IW,'C+','GADM')
  96  CONTINUE
      NAMI =NAMIND('GRPL')
      IF (IW(NAMI ).EQ.0) GO TO 94
      JGRPL=NAMI +1
C
C Replace decay mode by a new definition
C
 102  JGRPL=IW(JGRPL-1)
      IF(JGRPL.EQ.0)GOTO 99
      NKF3=IW(JGRPL+1)
C  get compressed code  for particle
      KC = LUCOMP(NKF3)
      IF ( KC.GT.0) THEN
        NBR = MDCY(KC,3)
        IK = IW(JGRPL+2)
        IF ( IK .GT. NBR) THEN
          IF (IW(6).GT.0) WRITE(IW(6),1001) IK,NBR
 1001       FORMAT('===KXL7BR ====you try to modify a non existant',
     $       ' line',I8,' maximum is ',I10)
          GO TO 102
        ENDIF
        IENTRY = MDCY(KC,2)+IK-1
C
C replace final state IK by another one for the particle NKF3
C
        MDME(IENTRY,1) = IW(JGRPL+3)
        MDME(IENTRY,2) = IW(JGRPL+4)
        DO 110 J= 1,5
          KFDP(IENTRY,J)=IW(JGRPL+4+J)
 110    CONTINUE
      ENDIF
      GOTO 102
   99 CONTINUE
C store the card on the C list
      CALL BKFMT('GRPL','I')
      CALL BLIST(IW,'C+','GRPL')
  94  CONTINUE
C
C  Update the individual branching ratios
C
      NAMI =NAMIND('GMOB')
      IF (IW(NAMI ).EQ.0) GO TO 95
      JGMOB=NAMI +1
 101  JGMOB=IW(JGMOB-1)
      IF(JGMOB.EQ.0)GOTO 98
C define the branching ratios of the particle NKF2
      NKF2=IW(JGMOB+1)
C  get compressed code  for particle
      KC = LUCOMP(NKF2)
      IF ( KC.GT.0) THEN
        NBR = MDCY(KC,3)
        IF ( NBR .NE. IW(JGMOB)-1) THEN
          IF (IW(6).GT.0) WRITE(IW(6),1002) NFK2,NBR
 1002        FORMAT('===KXL7BR ==== BR numbers do not agree for',
     $           ' part ',I8,' should be ',I10)
          GO TO 101
        ENDIF
        IENTRY = MDCY(KC,2)
        DO 500 JJ=1,NBR
            BRAT(IENTRY+JJ-1)=RW(JGMOB+1+JJ)
 500    CONTINUE
        CALL KXL7ST(NKF2)
      ENDIF
      GOTO 101
 98   CONTINUE
C store the card on the C list
      CALL BKFMT('GMOB','I,(F)')
      CALL BLIST(IW,'C+','GMOB')
  95  CONTINUE
      RETURN
 1000 IF (IW(6).GT.0) WRITE (IW(6),550) NKF1,K
  550 FORMAT(1X,'++++++KXL7BR+++++++ cannot extend decay list for',
     $ ' particle',I10,'Pointer MDCE was',I10)
      END
#endif
