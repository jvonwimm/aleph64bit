      SUBROUTINE GETJUV(JUVER)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C!  - Returns the JULIA version used for reconstruction
C!
C!   Author   :- Yves A. Maumary       17-JAN-1992
C!   Modified :- Yves A. Maumary       14-AUG-1992  Bug correction
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - JUVER /I    : JULIA version
C!
C!   Libraries required: BOS77
C!
C!   Description
C!   ===========
C!   Input bank : RHAH  ( Run Header Analysis History )
C?
C!======================================================================
      SAVE FIRST,NAMRH
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      INTEGER IW
      REAL RW(10000)
      COMMON /BCS/ IW(10000)
      EQUIVALENCE (RW(1),IW(1))
      INTEGER JRHAPN,JRHAPD,JRHAPH,JRHAPV,JRHAAV,JRHADV,JRHADD,JRHANI,
     +          JRHANO,JRHACV,JRHANU,LRHAHA
      PARAMETER(JRHAPN=1,JRHAPD=3,JRHAPH=4,JRHAPV=5,JRHAAV=6,JRHADV=7,
     +          JRHADD=8,JRHANI=9,JRHANO=10,JRHACV=11,JRHANU=12,
     +          LRHAHA=12)
      CHARACTER*4 CHAINT,PRNAM
      INTEGER NAMRH,JRHAH
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+LMHCOL)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+LMHROW)
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
C-----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST = .FALSE.
        NAMRH = NAMIND('RHAH')
      ENDIF
      JRHAH = IW(NAMRH)
      IF(JRHAH.EQ.0)THEN
        CALL ALTELL('_GETJUV_ no RHAH bank',0,'RETURN')
        JUVER = -1
        RETURN
      ENDIF
C
C!.LOOP OVER ALL ROWS TO BE SURE TO PICK UP THE LAST PROCESSING
      NROWS = LROWS(JRHAH)
      DO 10 I=1,NROWS
        KRHAH = KROW(JRHAH,I)
        PRNAM = CHAINT(IW(KRHAH+JRHAPN))
        IF(PRNAM.EQ.'JULI')THEN
          JUVER = IW(KRHAH+JRHAPV)
        ENDIF
   10 CONTINUE
      END
