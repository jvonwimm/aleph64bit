      FUNCTION CORAD94(EN,TH,PH)
C -------------------------------------------------------------
CKEY GAMPACK ECAL
C - Author: M.N Minard          930406
C - Input : EN      : Energy of cluster from 4 towers
C-          TH      : Cluster theta angle (rad)
C-          PH      : Cluster PHI angle (rad)
C - Output: CORAD94 : corrected energy
C -----------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JEGOTM=1,JEGOTX=2,JEGOC1=3,JEGOC2=4,JEGOC3=5,JEGOC4=6,
     +          JEGOC5=7,LEGOZA=7)
      INTEGER ALGTDB, GTSTUP
      CHARACTER DET*2,LIST*4
C
      DATA DET    / 'EC' /
      DATA LIST   / 'EGOZ' /
      DATA NAEGOZ / 0 /
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
       IF ( NAEGOZ.EQ.0) NAEGOZ = NAMIND('EGOZ')
       CORAD94 = 1.
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
       KEGOZ = IW(NAEGOZ)
       IF (KEGOZ.EQ.0) RETURN
C
C-     LOOK FOR CORRECTION FROM EGOZ BANK
C
       NEGOZ = LROWS(KEGOZ)
       COSI = ABS(COS(TH))
       COSI = MIN (COSI,0.999999)
       IROW = 0
       DO IEGOZ = 1,NEGOZ
        IF (COSI.GE.RTABL(KEGOZ,IEGOZ,JEGOTM).AND.
     &      COSI.LT.RTABL(KEGOZ,IEGOZ,JEGOTX)) IROW=IEGOZ
       ENDDO
       GCOR = RTABL(KEGOZ,IROW,JEGOC2) *(1.-
     &        RTABL(KEGOZ,IROW,JEGOC1)/(EN**RTABL(KEGOZ,IROW,JEGOC3)))
       IF(EN.GT.RTABL(KEGOZ,IROW,JEGOC4))THEN
          CORAD94 = 1./GCOR
       ELSE
       ESU = RTABL(KEGOZ,IROW,JEGOC4)
       GCORS= RTABL(KEGOZ,IROW,JEGOC2) *(1.-
     &        RTABL(KEGOZ,IROW,JEGOC1)/(ESU**RTABL(KEGOZ,IROW,JEGOC3)))
       EIN = RTABL(KEGOZ,IROW,JEGOC5)
       GCORI= RTABL(KEGOZ,IROW,JEGOC2) *(1.-
     &        RTABL(KEGOZ,IROW,JEGOC1)/(EIN**RTABL(KEGOZ,IROW,JEGOC3)))
          GCOR1 = (1./(RTABL(KEGOZ,IROW,JEGOC4)-RTABL(KEGOZ,IROW,
     &    JEGOC5)))*(GCORS*(EN-RTABL(KEGOZ,IROW,JEGOC5))
     & + GCORI*(RTABL(KEGOZ,IROW,JEGOC4)-EN))
          CORAD94 = 1./GCOR1
       ENDIF
C
       RETURN
       END
