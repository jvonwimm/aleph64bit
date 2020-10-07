      FUNCTION CORADOC(EN,TH,PH)
C -------------------------------------------------------------
CKEY GAMPACK ECAL
C - Author: M.N Minard          930406
C - Input : EN      : Energy of cluster from 4 towers
C-          TH      : Cluster theta angle (rad)
C-          PH      : Cluster PHI angle (rad)
C - Output: CORADOC : corrected energy
C -----------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JEGCTM=1,JEGCTX=2,JEGCC1=3,JEGCC2=4,JEGCC3=5,JEGCC4=6,
     +          LEGCOA=6)
      INTEGER ALGTDB, GTSTUP
      CHARACTER DET*2,LIST*4
C
      DATA DET    / 'EC' /
      DATA LIST   / 'EGCO' /
      DATA NAEGCO / 0 /
      DATA IROLD  / 0 /
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
C ----------------------------------------------------------------
       IF ( NAEGCO.EQ.0) NAEGCO = NAMIND('EGCO')
       CORADOC = 1.
C
C- Get EGCO bank from DB
C! Get banks from DB depending on run and setup code
C
      CALL ABRUEV (IRUN,IEVT)
      IRET = 0
      IF (IRUN.NE.IROLD) THEN
        IROLD = IRUN
        IF (IRUN.LE.2000) THEN
           ITP = GTSTUP (DET,IRUN)
        ELSE
           ITP = IRUN
        ENDIF
        IRET= ALGTDB(JUNIDB(0),LIST,-ITP)
      ENDIF
C
       KEGCO = IW(NAEGCO)
       IF (KEGCO.EQ.0) RETURN
C
C-     Look for correction from EGCO bank
C
       NEGCO = LROWS(KEGCO)
       COSI = ABS(COS(TH))
       COSI = MIN (COSI,0.999999)
       IROW = 0
       DO IEGCO = 1,NEGCO
        IF (COSI.GE.RTABL(KEGCO,IEGCO,JEGCTM).AND.
     &      COSI.LT.RTABL(KEGCO,IEGCO,JEGCTX)) IROW=IEGCO
       ENDDO
       GCOR = RTABL(KEGCO,IROW,JEGCC2) -
     &        RTABL(KEGCO,IROW,JEGCC1)/SQRT(EN)
       IF(EN.LT.RTABL(KEGCO,IROW,JEGCC3))THEN
       GCOR = RTABL(KEGCO,IROW,JEGCC2)+RTABL(KEGCO,IROW,JEGCC1)
     &        *(EN-RTABL(KEGCO,IROW,JEGCC3))/(RTABL(KEGCO,IROW
     &        ,JEGCC4))
       ENDIF
C
       CORADOC = 1./GCOR
       END
