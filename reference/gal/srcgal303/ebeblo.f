      SUBROUTINE EBEBLO( GBIN , CHAR , TMAS , DEDX )
C--------------------------------------------------------------------
C      S.SNOW   02/5/89
C:
C! CALCULATE DE/DX FROM BETHE-BLOCH FORMULA
C:
C  Constants taken from the note by M Bardadin et al. ALEPH 89-2
C
C    INPUT :  GABE is gamma*beta of the particle
C          :  CHAR is the charge of the particle
C          :  TMAS is the mass   of the particle
C   OUTPUT :  DEDX is the de/dx , normalised so that m.i.p. = 1
C
C Called by  EHDEPT
C---------------------------------------------------------------------
      SAVE
C Electron mass and average ionisation energy of Xe/CO2  in KeV
C  GBMN is the minimum gamma*beta
       DATA XME,WXE,GBMN/511. ,0.403 , 0.05 /
C SQD5 normalises the function to 1.0 at the minimum
       DATA SQD5 / 0.098639 / , CTE / 4.606 / , WCUT / 3.0E+6 /
C constants to describe the density effect in Xe/co2
       DATA C,X0,X1,A,EM/ -12.566,2.0154,5.0154,0.1216,3.0/
C
        GABE = GBIN
        IF(GABE.LT.GBMN)GABE = GBMN
        AMAS = TMAS * 1000000.0
        BG2 = GABE ** 2
        GM2 = 1. + BG2
        GAMM = SQRT( GM2 )
        BETA = GABE / GAMM
C
        AABB = 2.0 * XME * BG2
        WMAX = AABB / ( 1. + 2.*GAMM*XME/AMAS + (XME/AMAS) ** 2 )
        IF(WMAX .GT. WCUT) WMAX = WCUT
        COEF = AABB * WMAX / WXE ** 2
C
        XX = ALOG10( GABE )
        IF( XX .LT. X0 ) THEN
          DEL = 0.0
        ELSEIF ( XX .LT. X1 ) THEN
          DEL = CTE * XX + C + A * ( X1 - XX ) ** EM
        ELSE
          DEL = CTE * XX + C
        ENDIF
C
        DEX = 0.5 * ( ALOG(COEF) - DEL ) - BG2 / GM2
C
        DEDX = DEX * SQD5 * ( CHAR / BETA ) ** 2
        RETURN
        END
