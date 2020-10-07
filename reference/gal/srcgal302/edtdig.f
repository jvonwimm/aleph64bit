      SUBROUTINE EDTDIG
C-------------------------------------------------------------------
C      O.CALLOT   27-NOV-85
C! Convert signal to digit
C. - called from ECDIGI                                   this .HLB
C
C------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      PARAMETER (LPS1=6, LPS2=300)
      COMMON /ECNAMC/   NAETHT, NAEWHT, NAETTD, NAEWTD, NAETDI, NAEWDI
     &                , NAEWHE
     &                , NAETTR, NAEWTR, NAENDI
     &                , IDPSIG, IDEWTM, IDETTM
     &                , NAESHI, NAEWHI
C
      COMMON /EDCOND/ EDNOXT,EDMIXR,EDSAVG,EDZSUP
      CHARACTER*16    EDNOXT,EDMIXR,EDSAVG,EDZSUP
      COMMON /EDPARA/ EDTMCO,EDTSCO,EDTNOI(3),EDWMCO,EDWSCO,EDWNOI,
     +                EDTGAI(3),EDTGER(3),KALEDT(3),EDWGAI,EDWGER,
     +                KALEDW,EDCUT1,EDCUT2,EDWCUT,ETTNOI(12,3),
     +                ETSTWE(3), EDZTHR(3,3)
      EXTERNAL RNDM
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
      IF(IDETTM .EQ. 0 ) GOTO 980
      NETTM = LROWS(IDETTM )
      LETTM = LCOLS (IDETTM)
      KETTM = IDETTM + LMHLEN
      DO 10 I=1,NETTM
        JPHI = IW(KETTM+1)
        KTET = IW(KETTM+2)
        DO 20 L=1,3
          GAIN = EDTGAI(L) * ( 1. + EDTGER(L) * ( RNDM(L) - .5 ))
          IW(KETTM+2+L) = NINT( FLOAT(IW(KETTM+2+L)) * GAIN )
          IF(EDSAVG.EQ.'YES') RW(KETTM+8+L) = GAIN
   20   CONTINUE
        KETTM = KETTM + LETTM
   10 CONTINUE
  980 RETURN
      END
