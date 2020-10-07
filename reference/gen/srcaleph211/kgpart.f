       INTEGER FUNCTION KGPART (IUSER)
C -----------------------------------------------------------
C - J.Boucrot - F.Ranjard - 870516
C! Return ALEPH particle # of user particle # IUSER
CKEY KINE KINGAL PART /  USER INTERNAL
C
C - structure: INTEGER FUNCTION subprogram
C              User Entry Name: KGPART
C              External References: NAMIND(BOS77)
C                                   IUCOMP(CERNLIB)
C              Comdecks referenced: BCS, BMACRO
C
C - usage   : IPART = KGPART (IUSER)
C - input   : IUSER = user generator particle #
C - Output  : KGPART = ALEPH#
C                      0 if not found
C                      -1 if KLIN bank not there
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      DATA NAKLI /0/
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
C ----------------------------------------------------------
      IF (NAKLI.EQ.0) NAKLI = NAMIND ('KLIN')
C
      JKLIN = IW(NAKLI)
      IF (JKLIN.EQ.0) THEN
         KGPART = -1
         GOTO 999
      ELSE
         NPART = LROWS (JKLIN)
         KGPART = IUCOMP (IUSER,IW(JKLIN+LMHLEN+1),NPART)
      ENDIF
C
 999  CONTINUE
      END
