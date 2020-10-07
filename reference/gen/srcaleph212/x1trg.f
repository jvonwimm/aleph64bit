      SUBROUTINE X1TRG
C ----------------------------------------------------------------------
C.
C. - Author : E. Blucher - 89/11/2 for ALPHA, JULIA
C. ----Modified version of X1TRIG by A. Putzer, C. Geweniger.
C. - Modified : C. Geweniger - 890900   for ALEPHLIB 9.9
C.
C. - Apply level1 trigger conditions using information from X1AD
C.   and XTEB banks.
C.
C? - This routine simulates the application of the level 1 trigger.
C.
C? - The subroutines called correspond to the functions applied
C?   online
C.
C. - Banks    : X1TH  return if it does not exist
C.              XTEB  book the bank with 1 row
C.
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON/X1NAMC/NAXTBN,NAXTCN,NAXTDI,NAXTEB,NAXSGE,NAXSHI,
     &              NAXSSC,NAX1AD,NAX1SC,NAX1TH,NASIXA,NASIX2,NASIFO,
     &              NAX1RG,NAX1IP,NAX1TV
      PARAMETER(JXTET1=1,JXTET2=2,JXTEL2=3,JXTEHT=4,JXTEHW=16,JXTELW=28,
     +          JXTEEW=40,JXTELT=52,JXTETE=56,JXTEIT=58,JXTETP=62,
     +          LXTEBA=65)
C.
C---Apply trigger only if threshold bank exists.
      IF (IW(NAX1TH).EQ.0) GO TO 999
C
      CALL X1INP(IERR)
      IF(IERR.EQ.1)GOTO 999
C
C--- Initialize bank XTEB
C
      IF (IW(NAXTEB).NE.0) THEN
         KXT = IW(NAXTEB)+LMHLEN
         DO 1 I=1,LXTEBA
            IW(KXT+I) = 0
 1       CONTINUE
      ELSE
         LEN=LMHLEN+LXTEBA
         CALL AUBOS('XTEB',0,LEN,KXTEB,IGARB)
         IF (KXTEB.EQ.0) GOTO 999
         IW(KXTEB+1)=LXTEBA
         IW(KXTEB+2)=1
      ENDIF
C
      CALL BLIST(IW,'E+','XTEB')
C.
      CALL X1DISC
C.
      CALL X1APTR
C.
      CALL X1OUTP
C
 999  CONTINUE
C.
      RETURN
      END
