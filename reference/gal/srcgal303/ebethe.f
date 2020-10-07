      SUBROUTINE EBETHE(EP,GABE,EP1)
C----------------------------------------------------------
C:
C! CALCULATE DE/DX FROM BETHE-BLOCH Formula
C:                               M.N. Minard Feb. 88
C      EP   = INVERSE PARTICLE BETA ( E/P )
C      GABE = PARTICLE GAMMA*BETA   ( P/M )
C:     EP1= EP * ( DE/DX (BETHE)/ DE/DX (5 GEV))**0.5
C----------------------------------------------------------
      SAVE
C      XME , WXE in Kev
       DATA XME,WXE/511. ,0.019/
C      SQD5 is in (mev*g-1*cm2)**0.5 taken from test data
      DATA SQD5,SQ20 / 3.52,5./
       IF (EP.GT.1.E-10) THEN
        BETA = 1./EP
        COEFF = 2.*XME*(GABE**2)/WXE
        DEX = ALOG(COEFF)-BETA**2
        DEX1 = ABS(DEX)
        EP1 = EP*SQRT(DEX1)/SQD5
         IF (EP1.GT.SQ20) EP1 = SQ20
       ELSE
         EP1 = SQ20
       ENDIF
       END
