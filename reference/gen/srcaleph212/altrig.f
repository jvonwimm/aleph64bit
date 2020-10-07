      INTEGER FUNCTION ALTRIG (IBITT1,IBITT2,IBITL2)
C ---------------------------------------------------------------
CKEY ALEF TRIGGER
C - F.Ranjard - 910318
C! returns the 1st 3 words of XTEB or XTRB or DTBP
C-      Modified: L.Mirabito - 910828
C       Take information in DTBP for mini DST
C       Modified: H. Meinhard       29-Apr-1993  (2)
C                 Check LUPA and SILH, if all other banks not existing
C       Modified: F.Ranjard - 940426
C       Take information from X1RG starting with 'TPR ', col# 2,3,4
C       Modified: M.N.Minard - 960207
C       Take information from X1RG starting with 'TPR ' or ' RPT'
C       to cure a problemm seen on '95 MINIs made on UNIX
C
C - Output : - IBITT1 / I  = word 1 of 1st row of XTEB or XTRB
C              IBITT2 / I  = word 2 ......
C              IBITL2 / I  = word 3 ......
C              ALTRIG / I  = 0 if no bank is there
C                            1 if XTEB is there
C                            2 if XTRB is there
C                            3 if DTBP is there
C                            4 if LUPA is there;
C                            5 if SILH is there. Note that in the latter
C                              two cases, the original level2 mask is
C                              unavailable, and IBITT2 is set equal to
C                              IBITT1
C                            6 if X1RG is there
C
C -----------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JLUPGB=1,JLUPHV=2,JLUPT1=3,JLUPL2=4,JLUPTE=5,JLUPLO=6,
     +          JLUPAM=7,JLUPMD=8,JLUPEF=9,JLUPEC=10,JLUPEW=12,
     +          JLUPXC=14,JLUPYC=16,JLUPTC=18,JLUPPC=20,JLUPAX=22,
     +          JLUPAY=24,JLUPXX=26,JLUPYY=28,JLUPXY=30,JLUPXA=32,
     +          JLUPYA=34,JLUPXD=36,JLUPYD=38,JLUPAD=40,JLUPIT=42,
     +          JLUPCT=44,JLUPTT=46,JLUPPT=48,JLUPXT=50,JLUPYT=52,
     +          JLUPE1=54,JLUPE2=56,LLUPAA=57)
      PARAMETER(JSILGB=1,JSILHV=2,JSILT1=3,JSILT2=4,JSILTE=5,JSILLO=6,
     +          JSILS2=7,JSILDW=8,JSILSC=11,JSILAM=14,LSILHA=14)
      SAVE NXTEB, NXTRB, NDTBP, NLUPA, NSILH, NX1RG, IX1RG
      DATA NXTEB / 0/
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
C ------------------------------------------------------------
C - 1st entry : get name indices
      IF (NXTEB.EQ.0) THEN
        NXTEB = NAMIND ('XTEB')
        NXTRB = NAMIND ('XTRB')
        NDTBP = NAMIND ('DTBP')
        NLUPA = NAMIND ('LUPA')
        NSILH = NAMIND ('SILH')
        NX1RG = NAMIND ('X1RG')
        IX1RG = 0
      ENDIF
C
C - next entry
      IF (IW(NX1RG).NE.0) THEN
        JTRIG = IW(NX1RG)
        IF (IX1RG .EQ. 0) THEN
           ITPR  = INTCHA ('TPR ')
      IRPT  = INTCHA (' RPT')
           DO I=1,LROWS(JTRIG)
      IF (ITABL(JTRIG,I,1).EQ.ITPR .OR.
     &            ITABL(JTRIG,I,1).EQ.IRPT ) THEN
                 IX1RG = I
                 GOTO 10
              ENDIF
           ENDDO
 10        CONTINUE
        ENDIF
        ALTRIG = 6
        ICOL  = 1
        IROW  = IX1RG
      ELSE
        IROW = 1
        ICOL = 0
        IF (IW(NXTRB).NE.0) THEN
          ALTRIG = 2
          JTRIG = IW(NXTRB)
        ELSEIF (IW(NXTEB).NE.0) THEN
          ALTRIG = 1
          JTRIG = IW(NXTEB)
        ELSEIF (IW(NDTBP).NE.0) THEN
          ALTRIG = 3
          JTRIG = IW(NDTBP)
        ELSE
          IF (IW(NLUPA) .NE. 0) THEN
            ALTRIG = 4
            JTRIG  = IW(NLUPA)
            IBITT1 = ITABL(JTRIG,1,JLUPT1)
            IBITT2 = IBITT1
            IBITL2 = ITABL(JTRIG,1,JLUPL2)
          ELSE IF (IW(NSILH) .NE. 0) THEN
            ALTRIG = 5
            JTRIG  = IW(NSILH)
            IBITT1 = ITABL(JTRIG,1,JSILT1)
            IBITT2 = IBITT1
            IBITL2 = ITABL(JTRIG,1,JSILT2)
          ELSE
            ALTRIG = 0
          END IF
          RETURN
        ENDIF
      ENDIF
C
C - get trigger words of row# IROW, col.# ICOL+1,+2,+3
      IBITT1 = ITABL (JTRIG,IROW,ICOL+1)
      IBITT2 = ITABL (JTRIG,IROW,ICOL+2)
      IBITL2 = ITABL (JTRIG,IROW,ICOL+3)
C
      END
