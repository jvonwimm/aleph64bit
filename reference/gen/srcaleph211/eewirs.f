      LOGICAL FUNCTION EEWIRS (IDUMMY)
C----------------------------------------------------------------------
CKEY EDIR CLASS26
C!  - select bhabha events for calibration studies
C!
C!   Author   :- E. Lancon             15-APR-1992
C!
C!   Libraries required:
C!
C!   Description : Events are flagged if two modules have ECAL wire
C!   ===========   energies (EWHE) above 30 GeV.
C!
C!   Modified :- E. Lancon              9-JUL-1993
C!                 Use uncalibrated wire energy
C?
C!======================================================================
      PARAMETER(JEWHSD=1,JEWHNU=2,LEWHEA=2)
      PARAMETER(JECHCP=1,JECHCW=2,LECHEA=2)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C------------------------------------ Energy Cut 30 GeV = 30000000 KeV
      DATA ECUT / 30000000. /
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
      EEWIRS = .FALSE.
      NAEWHE = NAMIND('EWHE')
      KEWHE  = IW(NAEWHE)
C
C?   Get Calib. coeff. applyed  on those data
C
      NAECHE = NAMIND('ECHE')
      KECHE  = IW(NAECHE)
      NECHE  = 0
      IF (KECHE.GT.0) NECHE = LROWS(KECHE)
C
      IC = 0
      IF ( KEWHE.GT.0 ) THEN
        NEWHE = LROWS (KEWHE)
        IF ( NEWHE.EQ.36 ) THEN
          DO IEWHE =  1, NEWHE
            IENE = ITABL (KEWHE, IEWHE, JEWHSD)
            IMOD = ITABL (KEWHE, IEWHE, JEWHNU)
            IF (  IMOD.LE.NECHE .AND. IMOD.GT.0 ) THEN
              COEF = RTABL(KECHE, IMOD, JECHCW)
            ELSE
              COEF = 1.
            ENDIF
            ENE = FLOAT(IENE) / COEF
            IF ( ENE.GE.ECUT ) THEN
              IC = IC  + 1
            ELSE
            ENDIF
          ENDDO
        ELSE
        ENDIF
      ELSE
      ENDIF
      IF ( IC.GE.2 ) EEWIRS = .TRUE.
  999 RETURN
      END
