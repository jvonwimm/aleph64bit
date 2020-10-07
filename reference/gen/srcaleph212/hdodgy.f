      LOGICAL FUNCTION HDODGY(KRUN)
C-----------------------------------------------------------------------
C
CKEY MUONID HCAL /  USER
C
C!  Author : G. Taylor                12-APR-1992
C!  logical is true if the hcal digital readout is bad for this event
C=======================================================================
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (NBADRU=58)
      DIMENSION IBADRU(NBADRU)
      DATA IBADRU /4510,5051,5067,5068,5070,5073,5083,5084,5095,
     +             5097,5098,5099,5100,5106,5109,5874,5880,5322,
     +             5324,5325,5326,5343,7226,7572,7573,7574,7575,
     +             7576,7577,7588,7589,7590,7815,7816,7820,7849,
     +             7850,7864,7871,7958,8220,8221,8222,8380,8537,
     +             8539,8540,8541,8542,8610,8611,8613,8827,
     +             11841,11842,11843,11844,11845/
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
C-----------------------------------------------------------------------
C
      HDODGY = .FALSE.
      DO 10 I=1,NBADRU
        IF(KRUN.EQ.IBADRU(I)) HDODGY = .TRUE.
   10 CONTINUE
      KHBAD=MDARD(IW,JUNIDB(0),'HBAD',0)
      IF(KHBAD.GT.0) THEN
       DO 20 I=1,LROWS(KHBAD)
        IF(KRUN.EQ.ITABL(KHBAD,I,1)) HDODGY = .TRUE.
   20  CONTINUE
      ENDIF
      RETURN
      END
