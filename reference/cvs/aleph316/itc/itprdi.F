      SUBROUTINE ITPRDI
C.
C...ITPRDI  2.10  900430  18:00                        R.Beuselinck
C.
CKEY PRINT ITC MCARLO
C.
C! Print out ITC digitisings in a readable format.
C.
C. This routine uses the IHIT (or ITHT) and IDHR banks to correlate
C. the digitisings to particular tracks.
C. This routine uses either ITDI or IDIG.
C.
C.  Calls: ITDCOR                                      (from this HLB)
C.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
      CHARACTER*108 LINE
      CHARACTER*36  LINEI(3)
      EQUIVALENCE (LINE(1:1),  LINEI(1)(1:1))
      EQUIVALENCE (LINE(37:37),LINEI(2)(1:1))
      EQUIVALENCE (LINE(73:73),LINEI(3)(1:1))
      LOGICAL NEWDI
C
      EXTERNAL NAMIND
C
      DATA IHIT,ITHT,IDHR,ITDR,ITDL,ITDI,IDIG/7*0/
#include "bmacro.h"
C ----------------------------------------------------------
C-- Get output unit #
C--
      LOUT = IW(6)
C
C--  Setup root indices to correlation banks on first entry.
C--
      IF (IHIT.EQ.0) THEN
        IHIT = NAMIND('IHIT')
        ITHT = NAMIND('ITHT')
        IDHR = NAMIND('IDHR')
        ITDR = NAMIND('ITDR')
        ITDL = NAMIND('ITDL')
        ITDI = NAMIND('ITDI')
        IDIG = NAMIND('IDIG')
      ENDIF
C
C--  Check that the necessary banks exist.
C--
      JIHI = IW(IHIT)
      JTHT = IW(ITHT)
      IF (JIHI.GT.0) JTHT = JIHI
      JDHR = IW(IDHR)
      IF (JTHT.EQ.0 .OR. JDHR.EQ.0) GO TO 990
C
C--  Determine whether we have old or new digitising banks.
C--
      JTDI = IW(ITDI)
      JDIG = IW(IDIG)
      IF (JTDI.EQ.0.AND.JDIG.EQ.0) GO TO 991
      NEWDI = JDIG.GT.0
C
C--  Recreate the tracks to digits correlations.
C--
      CALL ITDCOR
      JTDR = IW(ITDR)
      JTDL = IW(ITDL)
C
C--  Unpack old ITC digitisings into work bank for easy access.
C--
      IF (.NOT.NEWDI) THEN
        JDIG = 0
        CALL BKTOW(IW,'ITDI',0,IW,JDIG,*998)
      ENDIF
C
C--  Print out the digit numbers for each track in turn.
C--
      NTRK = LROWS(JTDR)
      WRITE(LOUT,1000)
      DO 50 I=1,NTRK
        JRO = KROW(JTDR,I)
        IT  = IW(JRO+1)
        ND  = IW(JRO+2)
        NA0 = IW(JRO+3)
        KDI = JTDL + LMHLEN + NA0 - 1
        WRITE(LOUT,1001) IT,(IW(KDI+J),J=1,ND)
   50 CONTINUE
C
      IF (NEWDI) THEN
        WRITE(LOUT,1005) LROWS(JDIG)
      ELSE
C
C--     Print the contents of the ITDI bank, first the header.
C--
        IAP = IW(JDIG+6)
        IAD = IW(JDIG+7)
        WRITE(LOUT,1002) (IW(JDIG+J),J=2,3)
     &                  ,(IW(JDIG+IAP+JJ),JJ=1,15,2)
      ENDIF
C
C--  Now print out the actual digitising together with the correlated
C--  track numbers in 3 columns across the page.
C--
      WRITE(LOUT,1003)
      IF (NEWDI) THEN
        ND = LROWS(JDIG)
      ELSE
        ND = IW(JDIG+3)
      ENDIF
      I1 = 1
   60 I2 = MIN(I1+2,ND)
      LINE = ' '
      NDO = I2 - I1 + 1
      DO 70 I=1,NDO
        J = I1 + I - 1
        IF (NEWDI) THEN
          JJD = ITABL(JDIG,J,1)
          IDI1 = IBITS(JJD,0,10)
          IDI2 = IBITS(JJD,10,9)
          IDI3 = IBITS(JJD,19,9)*2
          WRITE(LINEI(I),1004) J,IDI1,IDI2,IDI3
        ELSE
          LD = JDIG + IAD + (J-1)*3
          WRITE(LINEI(I),1004) J,(IW(LD+J1),J1=1,3)
        ENDIF
        IHA = ITABL(JDHR,J,1)
        IHB = ITABL(JDHR,J,2)
        ITR1 = ITABL(JTHT,IHA,1)
        ITR2 = ITABL(JTHT,IHB,1)
        IF (IHA.EQ.IHB) THEN
          WRITE(LINEI(I)(27:),'(I5)') ITR1
        ELSE
          WRITE(LINEI(I)(27:),'(2I5)') ITR1,ITR2
        ENDIF
   70 CONTINUE
      WRITE(LOUT,'(A)') LINE
      I1 = I2 + 1
      IF (I1.LE.ND) GO TO 60
C
C--  Drop temporary work bank.
C--
      IF (NEWDI) GO TO 999
      CALL WDROP(IW,JDIG)
      JDIG = 0
      GO TO 999
  990 WRITE(LOUT,'(A)')
     +'0+++ITPRDI+++ Cannot correlate digits without ITHT or IDHR'
      GO TO 999
  991 WRITE(LOUT,'(A)')
     +'0+++ITPRDI+++ There is no ITC digitisings bank for this event'
      GO TO 999
  998 WRITE(LOUT,'(A)')
     +'0+++ITPRDI+++ Not enough BOS space to print ITC digits.'
  999 CONTINUE
 1000 FORMAT(//T50,'ITC digitisings'//' Track : Digits...')
 1001 FORMAT(I6,' : ',(T11,20(I4)))
 1002 FORMAT(/' No. digits = ',I4,'  no. useful digits = ',I4,5X,
     +' no. digits in each layer = ',8I4)
 1003 FORMAT(3(4X,'Dig.# wire   TDC     Z tracks...'))
 1004 FORMAT(4X,2I5,2I6)
 1005 FORMAT(/' No. digits = ',I4)
      END
#endif
