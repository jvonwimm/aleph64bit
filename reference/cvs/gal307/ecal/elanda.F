*DK elanda
      SUBROUTINE ELANDA(T,W)
C-----------------------------------------------------------------------
C      S.SNOW   02/5/89
C
C! GET A LANDAU DISTRIBUTION FROM A LOOK-UP TABLE
C
C  INPUT : T is the length of the step in gas ( cm )
C OUTPUT : W is a number drawn from a distribution having the correct
C                     shape and a mean of 1.0
C
C The Landau distribution is taken from test beam data with
C non-interacting particles in the ECAL end cap .
C It is the energy distribution measured in a single wire plane,
C scaled to have a mean of 1 ( the rms is about 1.23 ) .
C
C  Called by EHDEPT
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
      PARAMETER( KTABL = 101 , PTAB = 100. , GAS = .32 )
      DIMENSION TABLE( KTABL )
      DATA TABLE /  0.0000,  0.0215,  0.0413,  0.0586,  0.0762,
     &  0.0912,  0.1046,  0.1194,  0.1336,  0.1477,
     &  0.1612,  0.1737,  0.1860,  0.1975,  0.2091,
     &  0.2204,  0.2313,  0.2422,  0.2522,  0.2624,
     &  0.2726,  0.2825,  0.2920,  0.3017,  0.3113,
     &  0.3211,  0.3310,  0.3415,  0.3517,  0.3612,
     &  0.3708,  0.3811,  0.3907,  0.4010,  0.4116,
     &  0.4221,  0.4323,  0.4429,  0.4540,  0.4653,
     &  0.4759,  0.4871,  0.4989,  0.5104,  0.5226,
     &  0.5349,  0.5474,  0.5602,  0.5746,  0.5877,
     &  0.6016,  0.6163,  0.6308,  0.6458,  0.6617,
     &  0.6780,  0.6946,  0.7099,  0.7262,  0.7431,
     &  0.7610,  0.7782,  0.7980,  0.8191,  0.8393,
     &  0.8591,  0.8816,  0.9037,  0.9289,  0.9545,
     &  0.9812,  1.0069,  1.0384,  1.0705,  1.1048,
     &  1.1378,  1.1751,  1.2164,  1.2569,  1.3041,
     &  1.3550,  1.4096,  1.4718,  1.5394,  1.6027,
     &  1.6834,  1.7693,  1.8728,  1.9859,  2.1244,
     &  2.2695,  2.4297,  2.6108,  2.8670,  3.1194,
     &  3.4354,  3.8409,  4.3314,  5.2005,  6.5980, 9.4919 /
C
C add the distribution to itself AIT times
C
      AIT = T / GAS
      NIT = AIT
      WWW = 0.0
      DO 1 I = 1 , NIT + 1
        REMA = AIT - FLOAT(I-1)
        IF(REMA .GT. 1. ) REMA = 1.
        RR = RNDM(W) * PTAB
        IR = RR
        F = RR - FLOAT(IR)
        WWW = WWW + REMA *
     +   (TABLE(IR+1) + F * ( TABLE(IR+2) - TABLE(IR+1) ) )
    1 CONTINUE
      W = WWW / AIT
      RETURN
      END
#endif
