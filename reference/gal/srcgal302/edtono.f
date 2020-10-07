      SUBROUTINE EDTONO
C------------------------------------------------------------------
C    O.CALLOT   17-MAR-86
C! Add noise to analog signal
C    Built a map of the hit zones in ECAL
C
C. - called from ECDIGI                                  this .HLB
C. - calls      EDSNOI,EDTADJ          This .HLB
C.              ALTELL,ECRWRG          ALEPHLIB
C.              RANNOR                 CERNLIB
C                VFILL,VZERO,UCOPY,NORRAN                CERNLIB
C-------------------------------------------------------------------
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
      PARAMETER (LSTCK=3,LPHI=384,LTHET=228)
      PARAMETER (LWPLA=45,LMODU=12,LCOMP=3)
      PARAMETER (NTHSG=12,NPHSG=24)
       COMMON /WRKSPC/ WSPACE(88320)
      DIMENSION    MAPECT(LPHI,LTHET)
      EQUIVALENCE (MAPECT(1,1),WSPACE(2*LTHET+1))
      DIMENSION JPMIN(LTHET),JPMAX(LTHET)
      EQUIVALENCE (JPMIN(1),WSPACE(1)),(JPMAX(1),WSPACE(LTHET+1))
      COMMON /EDPARA/ EDTMCO,EDTSCO,EDTNOI(3),EDWMCO,EDWSCO,EDWNOI,
     +                EDTGAI(3),EDTGER(3),KALEDT(3),EDWGAI,EDWGER,
     +                KALEDW,EDCUT1,EDCUT2,EDWCUT,ETTNOI(12,3),
     +                ETSTWE(3), EDZTHR(3,3)
      PARAMETER (MNTAG=1000,MXTAG=-1)
      DIMENSION LIVOI(2,10),JSTND(3)
      EXTERNAL EDSNOI
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
C
C === initializes MAPECT according to simulation condition
C
      DO 2 I=1,LTHET
        JPMIN(I) = MNTAG
        JPMAX(I) = MXTAG
   2  CONTINUE
      NBTOW = 0
      IF(EDNOXT.EQ.'ALL') THEN
        DO 5 KTET = 1,LTHET
        CALL ECRWRG (KTET,NUREG,MAXPH)
          NBTOW = NBTOW + MAXPH
      DO 3 I=1,MAXPH
        MAPECT(I,KTET) = 1
   3  CONTINUE
          JPMIN(KTET) = 1
          JPMAX(KTET) = MAXPH
    5   CONTINUE
      ENDIF
C
C === get analog signal from ETHT bank
C
      JETHT = IW(NAETHT)
      IF( JETHT .EQ. 0 ) GOTO 980
      NHIT = LROWS( JETHT )
      NWEL = LCOLS (JETHT)
      KETHT = JETHT + LMHLEN
C
C === creates map of hit zones in ECAL
C
      DO 20  L=1,NHIT
        IETIQ = IW(KETHT+1)
        KETHT   = KETHT + NWEL
        JPHI  = IBITS(IETIQ,2,9)
        KTHE  = IBITS(IETIQ,16,8)
        IF(JPMIN(KTHE).EQ.MNTAG) THEN
          DO 4 J=1,LPHI
            MAPECT(J,KTHE) = 0
   4      CONTINUE
        ENDIF
        IF(JPHI.LE.JPMIN(KTHE)) JPMIN(KTHE) = JPHI
        IF(JPHI.GT.JPMAX(KTHE)) JPMAX(KTHE) = JPHI
        IF(MAPECT(JPHI,KTHE).EQ.0) NBTOW = NBTOW + 1
        MAPECT(JPHI,KTHE) = 2
        IF(EDNOXT.EQ.'+ONE') THEN
          CALL EDTADJ(JPHI,KTHE,NBVOI,LIVOI)
          DO 10 J=1,NBVOI
            JPHIV = LIVOI(1,J)
            KTHEV = LIVOI(2,J)
          IF(JPMIN(KTHEV).EQ.MNTAG) THEN
            DO 8 JP=1,LPHI
              MAPECT(JP,KTHEV) = 0
   8        CONTINUE
          ENDIF
            IF(MAPECT(JPHIV,KTHEV).EQ.0) THEN
              IF(JPHIV.LT.JPMIN(KTHEV)) JPMIN(KTHEV) = JPHIV
              IF(JPHIV.GT.JPMAX(KTHEV)) JPMAX(KTHEV) = JPHIV
              NBTOW = NBTOW + 1
              MAPECT(JPHIV,KTHEV) = 1
            ENDIF
  10      CONTINUE
        ENDIF
  20  CONTINUE
C
C === creates temporary bank for analog signals + noise
C
      LETTM = 5
      IF(EDSAVG.EQ.'YES') LETTM = 11
      CALL WBANK (IW,IDETTM, NBTOW*LETTM+LMHLEN, *970)
      IW(IDETTM-3) = INTCHA ('ETTM')
      IW(IDETTM+LMHCOL) = LETTM
      IW(IDETTM+LMHROW) = 0
      KETHT = JETHT + LMHLEN
      LETHT = LCOLS (JETHT)
C
C === generates correlated noise
C
      CALL RANNOR(SIG,SIH)
      CORNO = EDTMCO + EDTSCO * SIG
C
C === fills the bank with analog signals and add noise
C
      NBUS = 0
      DO 60 K=1,LTHET
        IF(JPMIN(K).EQ.MNTAG) GO TO 60
        DO 70 J=JPMIN(K),JPMAX(K)
          IF(MAPECT(J,K).EQ.0) GO TO 70
          IF (LFRROW(IDETTM).LT.1) THEN
             CALL WBANK (IW,IDETTM,IW(IDETTM)+10*LETTM,*970)
          ENDIF
          KETTM = KNEXT (IDETTM)
          IW(KETTM+1) = J
          IW(KETTM+2) = K
          IETIQ = ISHFT(K,16) + ISHFT(J,2)
          IF(IETIQ.EQ.IW(KETHT+1)) THEN
            DO 50 L=2,4
              IW(KETTM+L+1) = IW(KETHT+L)
  50        CONTINUE
            NBUS = NBUS + 1
            IF(NHIT.NE.NBUS) KETHT = KETHT + LETHT
          ENDIF
C
C mixing with real event should happen here
C
          DO 85 I=1,3
            CALL RANNOR(UNCNO,SIH)
            UNCNO = UNCNO * EDSNOI(I,J,K)
            INOI  = NINT( CORNO + UNCNO )
            IW(KETTM+2+I) = IW(KETTM+2+I) + INOI
            IF(EDSAVG.EQ.'YES') IW(KETTM+5+I) = INOI
   85     CONTINUE
           IW(IDETTM+LMHROW) = LROWS(IDETTM) + 1
   70   CONTINUE
   60 CONTINUE
C
      GOTO 980
C
C - not enough space for working bank
 970  CONTINUE
      CALL ALTELL ('EDTONO not enough space for IDETTM',1,'NEXT')
C
  980 RETURN
      END
