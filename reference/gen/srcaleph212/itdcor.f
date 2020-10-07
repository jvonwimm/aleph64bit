      SUBROUTINE ITDCOR
C.
C...ITDCOR  2.00  900430  18:16                      R.Beuselinck
C.
CKEY ALEF ITC HISTORY MCARLO
C.
C!  Create ITC Track to Digits correlation banks.
C.
C.   Input banks: IHIT, ITHT, IDHR
C.  Output banks: ITDR, ITDL
C.
C-----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      EXTERNAL NAMIND
      LOGICAL INFST
      SAVE INFST, IHIT, ITHT, IDHR, ITDR, ITDL
      DATA INFST/.TRUE./
C
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
C--  Declare bank formats.
C--
      IF (INFST) THEN
        INFST = .FALSE.
        IHIT = NAMIND('IHIT')
        ITHT = NAMIND('ITHT')
        IDHR = NAMIND('IDHR')
        ITDR = NAMIND('ITDR')
        ITDL = NAMIND('ITDL')
        CALL BKFMT('ITDR','(I)')
        CALL BKFMT('ITDL','(I)')
      ENDIF
C
C--  Check whether the necessary banks exist.
C--
      JIHIT = IW(IHIT)
      JITHT = IW(ITHT)
      IF (JIHIT.GT.0) JITHT = JIHIT
      JIDHR = IW(IDHR)
      IF (JITHT.EQ.0 .OR. JIDHR.EQ.0) GO TO 999
C
C--  Book the new correlation banks.
C--
      NROW = LROWS(JIDHR)
      CALL ALBOS('ITDL',0,NROW+LMHLEN,JITDL,IGAR1)
      IW(JITDL+LMHCOL) = 1
      IW(JITDL+LMHROW) = NROW
      CALL ALBOS('ITDR',0,3*NROW+LMHLEN,JITDR,IGAR2)
      IW(JITDR+LMHCOL) = 3
      IW(JITDR+LMHROW) = 0
      IF (IGAR1+IGAR2 .NE. 0) THEN
        JIHIT = IW(IHIT)
        JITHT = IW(ITHT)
        IF (JIHIT.GT.0) JITHT = JIHIT
        JIDHR = IW(IDHR)
        JITDL = IW(ITDL)
      ENDIF
C
C--  Make an index of the HitA column of IDHR ordered by increasing
C--  hit number (=row of IHIT or ITHT) in bank ITDL.
C--
      KDH = KROW(JIDHR,1)
      KTD = KROW(JITDL,1)
      DO 10 I=1,NROW
        IW(KTD+I) = 2*(I-1) + 1
   10 CONTINUE
      IF (NROW.GT.1) CALL SORTZV(IW(KDH+1), IW(KTD+1), NROW, -1, 0, 1)
C
C--  Convert the sorted list (pointing to elements of IHDR) into a list
C--  of row numbers of IDHR.
C--  This is then the ordered list of digitisings for each track.
C--
      DO 20 I=1,NROW
        IW(KTD+I) = (IW(KTD+I) + 1) / 2
   20 CONTINUE
C
C--  Now count up the entries for each track and fill ITDR.
C--
      NTR = 0
      ITO = 999999
      DO 50 I=1,NROW
        IDIG = IW(KTD+I)
        IHTA = ITABL(JIDHR,IDIG,1)
        ITRK = ITABL(JITHT,IHTA,1)
        IF (ITRK.NE.ITO) THEN
          NTR = NTR + 1
          ITO = ITRK
          KTR = KROW(JITDR,NTR)
          IW(KTR+1) = ITRK
          IW(KTR+2) = 0
          IW(KTR+3) = I
        ENDIF
        IW(KTR+2) = IW(KTR+2) + 1
   50 CONTINUE
      IW(JITDR+LMHROW) = NTR
C
      CALL AUBPRS('ITDR')
  999 CONTINUE
      END
