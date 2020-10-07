      SUBROUTINE EDINIP
C-------------------------------------------------------------------
C      O.CALLOT  8-NOV-85
C! Init ECAL digit constants
C. - called from ECDFPA                                  this .HLB
C-------------------------------------------------------------------
      SAVE
      COMMON /EDPARA/ EDTMCO,EDTSCO,EDTNOI(3),EDWMCO,EDWSCO,EDWNOI,
     +                EDTGAI(3),EDTGER(3),KALEDT(3),EDWGAI,EDWGER,
     +                KALEDW,EDCUT1,EDCUT2,EDWCUT,ETTNOI(12,3),
     +                ETSTWE(3), EDZTHR(3,3)
C
C  === parameters for digitsation
C
      EDTMCO    =    0.
      EDTSCO    = 1600.
      EDTNOI(1) =  170.
      EDTNOI(2) =  170.
      EDTNOI(3) =  300.
      KALEDT(1) = 4000
      KALEDT(2) = 4000
      KALEDT(3) = 7200
      EDTGAI(1) =     1./FLOAT(KALEDT(1))
      EDTGAI(2) =     1./FLOAT(KALEDT(2))
      EDTGAI(3) =     1./FLOAT(KALEDT(3))
      EDTGER(1) = .01
      EDTGER(2) = .01
      EDTGER(3) = .01
      EDWMCO    =    0.
      EDWSCO    =    0.
      EDWNOI    = 1000.
      KALEDW    =  200
      EDWGAI    =  1./FLOAT(KALEDW)
      EDWGER    = .01
C
      EDCUT1    =    3.
      EDCUT2    =    5.
      EDWCUT    =  100.
C
      DO 10 I=1,12
        ETTNOI(I,1) = 35000.
        ETTNOI(I,2) = 35000.
        ETTNOI(I,3) = 60000.
  10  CONTINUE
      DO 20 I=1,3
        ETSTWE(I) = 1.
  20  CONTINUE
      RETURN
      END
