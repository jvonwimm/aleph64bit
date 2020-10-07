      SUBROUTINE ECPRWI
C----------------------------------------------------------------
C      O.CALLOT     12-MAR-86
C
C! Print wire banks EWHT, EWDI
C
C----------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      DIMENSION IPHIT(15,3),IPDIG(15,3)
      CHARACTER*10 TITSD(3),TNAME
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
      DATA TITSD / 'End Cap A ','   Barrel ','End Cap B ' /
C ----------------------------------------------------------
      LOUT = IW(6)
C
      NHT = 0
      NDI = 0
      KEWHT = NLINK ('EWHT',0)
      IF( KEWHT .EQ. 0 ) GOTO 1
      NHT = LROWS( KEWHT )
      LHT = LCOLS (KEWHT)
      KHT = KEWHT + LMHLEN + 1
    1 CONTINUE
      KEWDI = NLINK ('EWDI',0)
      IF( KEWDI .EQ. 0 ) GOTO 2
      NDI = LROWS( KEWDI )
      LDI = LCOLS (KEWDI)
      KDI = KEWDI + LMHLEN + 1
    2 CONTINUE
      WRITE(LOUT,1000) NHT,NDI
 1000 FORMAT (/1X,'+++ECPRWI+++ EWHT, EWDI wire data print out in MEV ',
     +    I3,' hits and ',I3,' digits'/)
      SUMH = 0.
      SUMD = 0.
      DO 10 IMO=1,36
        MODE = 0
        IF(NHT.NE.0) THEN
          IF(IW(KHT).EQ.IMO) MODE = 1
        ENDIF
        IF(NDI.NE.0) THEN
          IF(IW(KDI).EQ.IMO) MODE = MODE + 2
        ENDIF
        IF(MODE.EQ.0) GO TO 10
        TNAME = TITSD((IMO-1)/12+1)
        JMOD  = MOD((IMO-1),12)+1
        IF(MODE.NE.2) THEN
          DO 20 J=1,15
            IPHIT(J,1) = NINT( .001 * IW(KHT+J))
            IPHIT(J,2) = NINT( .001 * IW(KHT+J+15))
            IPHIT(J,3) = NINT( .001 * IW(KHT+J+30))
            SUMH = SUMH + IW(KHT+J) + IW(KHT+J+15) + IW(KHT+J+30)
   20     CONTINUE
          KHT = KHT + LHT
          NHT = NHT - 1
        ENDIF
        IF(MODE.NE.1) THEN
          DO 25 J=1,15
            IPDIG(J,1) = NINT( .001 * IW(KDI+J))
            IPDIG(J,2) = NINT( .001 * IW(KDI+J+15))
            IPDIG(J,3) = NINT( .001 * IW(KDI+J+30))
            SUMD = SUMD + IW(KDI+J) + IW(KDI+J+15) + IW(KDI+J+30)
   25     CONTINUE
          KDI = KDI + LDI
          NDI = NDI - 1
        ENDIF
        IF(MODE.EQ.1) THEN
          WRITE(LOUT,1100) TNAME,JMOD,((IPHIT(I,J),I=1,15),J=1,3)
        ELSE IF(MODE.EQ.2) THEN
          WRITE(LOUT,1200) TNAME,JMOD,((IPDIG(I,J),I=1,15),J=1,3)
        ELSE
          WRITE(LOUT,1300) TNAME,JMOD,((IPHIT(I,J),I=1,15),
     +         (IPDIG(I,J),I=1,15),J=1,3)
        ENDIF
   10 CONTINUE
      ISUMH = NINT( .001 * SUMH )
      ISUMD = NINT( .001 * SUMD )
      WRITE(LOUT,2000) ISUMH,ISUMD
      RETURN
 1100 FORMAT (1X,A10,I2,'hits   1- 15 ',15I6/20X,'16- 30 ',15I6/
     +      20X,'31- 45 ',15I6)
 1200 FORMAT (1X,A10,I2,' digits 1- 15 ',15I6/20X,'16- 30 ',15I6/
     +      20X,'31- 45 ',15I6)
 1300 FORMAT (1X,A10,I2,' hits   1- 15 ',15I6/14X,'digits 1- 15 ',15I6/
     &                     20X,'16- 30 ',15I6/20X,'16- 30 ',15I6/
     &                     20X,'31- 45 ',15I6/20X,'31- 45 ',15I6)
 2000 FORMAT(//'  Grand total for hits  = ',I10,' , for digits = ',
     +        I10/)
      END
