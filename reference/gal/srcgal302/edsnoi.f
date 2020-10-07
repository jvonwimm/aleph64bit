      FUNCTION EDSNOI(ISTK,JPHI,KTET)
C----------------------------------------------------------------------
C     O.CALLOT  29-NOV-85
C!   Modified :- E. Lancon              3-DEC-1990
C! Compute noise on storeys
C. Initialise array SIGMN on first call
C----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /EDPARA/ EDTMCO,EDTSCO,EDTNOI(3),EDWMCO,EDWSCO,EDWNOI,
     +                EDTGAI(3),EDTGER(3),KALEDT(3),EDWGAI,EDWGER,
     +                KALEDW,EDCUT1,EDCUT2,EDWCUT,ETTNOI(12,3),
     +                ETSTWE(3), EDZTHR(3,3)
      DIMENSION SIGMN(3,114)
      DIMENSION CAPA0(3)
C
C?   Normalize noise to data
C
      DIMENSION XNORM (3,2)
      DATA (XNORM(I,1),I=1,3)   / 0.86, 1.42, 1.27 /
      DATA (XNORM(I,2),I=1,3)   / 0.61, 0.98, 0.95 /
      SAVE  NCALL,SIGMN
      DATA  CAPA0  /  2.  ,  3.  ,  2.  /
      DATA  NCALL  / 0 /
C
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
C  ===  definition of the function ArgsinH
      ASINH(X) = ALOG( X + SQRT( 1. + X*X) )
C ----------------------------------------------------------------------
      IF(NCALL.NE.0) GO TO 100
C     if ENNO bank exists then set XNORM from there
      JENNO = IW(NAMIND('ENNO'))
      IF (JENNO .GT. 0) THEN
         DO IREG=1,2
            DO ISAMP=1,3
               XNORM(ISAMP,IREG) = RTABL (JENNO,IREG,ISAMP+1)
            ENDDO
         ENDDO
      ENDIF
C
      ZMAX = 2315.
      YMAX = 1871.
      EPSI = YMAX/64. * ASINH(ZMAX/YMAX)
      DO 10 I=1,64
        SIGMN(1,115-I) = COSH(FLOAT(I)*EPSI/YMAX)
   10 CONTINUE
      ZE = 2550.
      ZF = 2946.
      Y0 =  565.
      PIS12 = ACOS(-1.) / 12.
      D = ASINH(Y0/ZF * PIS12 )
      ETA = ZE/45. * ( ASINH(YMAX/ZMAX) - D )
      COEF = 8. * ZE/YMAX * ETA/EPSI
      ROWN = 4.
      DO 20 I=1,50
        SIGMN(1,I) = COEF/ROWN * SINH(2.*(FLOAT(I)*ETA/ZE + D ) )
        IF(I.EQ.8) ROWN = 8.
        IF(I.EQ.24) ROWN = 12.
        IF(I.EQ.40) ROWN = 16.
        IF(I.EQ.45) COEF = COEF * EPSI/ETA * ZE/ZMAX
   20 CONTINUE
C
      IREG = 1
      DO 30 I=1,114
        IF (I.GE.51) IREG = 2
        DEP = SIGMN(1,I)
        DO 40 J=1,3
          CAPA = DEP * CAPA0(J)
          SIGMN(J,I) = SQRT( 308.*CAPA**2 + 665. ) * EDTNOI(J)
          SIGMN(J,I) = SIGMN(J,I) * XNORM(J,IREG)
   40   CONTINUE
   30 CONTINUE
      NCALL = 1
C
  100 CONTINUE
      IF(KTET.LE.114) THEN
        SIG = SIGMN(ISTK,KTET)
      ELSE
        SIG = SIGMN(ISTK,229-KTET)
      ENDIF
      EDSNOI = SIG
      RETURN
      END
