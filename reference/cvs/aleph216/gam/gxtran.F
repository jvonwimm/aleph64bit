      SUBROUTINE GXTRAN (ITPIC,JFPIC,ES33,EGRAW,
     &                       F4,EST,THSC,PHSC,COREF,ICOK)
C.----------------------------------------------------------------------
CKEY GAMPACK F4    / INTERNAL
C   J.C.Brient      Creation  4/12/92
C! USE the 3 x 3 matrix to compute some varaibles such F4, etc...
C   Input :
C           ES33    ENERGY IN 3 X 3 MATRIX around the peak   REAL
C   Output:
C           F4       E4/EtotREAL
C           EST      estimator ofr any substucture           REAL
C           THSC     Best theta estimation from S-curve      REAL
C           PHSC     Best phi   estimation from S-Curve      REAL
C           COREF    scale to E4t to have best En. estim.    REAL
C   Calls: None
C   Called by GAMPEX
C.----------------------------------------------------------------------
#ifndef DOC
C
       PARAMETER ( ACOF = 1.14 , PREC = 5. , PTIT = .0001 )
       PARAMETER ( COR2OR = 1.029 )
       PARAMETER ( FLCT = .4 )
       DIMENSION ES33(3,3)
       DIMENSION E22(2,2)
       DIMENSION GERB(3)
C
       ITCR= ITPIC
       JFCR= JFPIC
C
C   Barycenter over 3 towers in pads units.
C
      EIP = 0.
      EIC = 0.
      EIM = 0.
      EJP = 0.
      EJM = 0.
      DO 2 L = 1 , 3
         EIP = EIP + ES33( 3 , L )
         EIC = EIC + ES33( 2 , L )
         EIM = EIM + ES33( 1 , L )
         EJP = EJP + ES33( L , 3 )
         EJM = EJM + ES33( L , 1 )
    2 CONTINUE
      ETT = EIP + EIC + EIM
      IF( ETT .LT. PTIT ) ETT = PTIT
      BARJ = ( EJP - EJM) / ETT
      BARI = ( EIP - EIM) / ETT
C   Analysis of 2 X 2 towers around barycenter.
      IV = 1
      JV = 1
      IF( BARI .GT. 0. ) IV = 3
      IF( BARJ .GT. 0. ) JV = 3
C   Initialisation of transverse identificators.
      X = 0.
      Y = 0.
      Z = 0.
      T = 0.
C   Calculation of transverse identificators.
C   2 X 2 central towers content.
      T = ES33(  2 , 2 )
      X = ES33( IV , 2 )
      Y = ES33(  2 ,JV )
      Z = ES33( IV ,JV )
      IF( T .LE. 0. ) T = .01
      NLIV = 0
      IF(T .GT. 0.01) NLIV=NLIV+1
      IF(X .GT. 0.01) NLIV=NLIV+1
      IF(Y .GT. 0.01) NLIV=NLIV+1
      IF(Z .GT. 0.01) NLIV=NLIV+1
C
C   Summation of 2 X 2 towers.
C
      SOM4 = T + X + Y + Z
      F4   = SOM4 / EGRAW
C
C   Transverse deviation from single gamma expectation.
C
      X1 = X / T
      Y1 = Y / T
C
C   ZTH is the expected value of Z if it is a single gamma.
C
      ZTH =  X1 * Y1 * ( 1. + ACOF * ( 1. - X1 ) * ( 1. - Y1 ) )
      IF (ZTH.LE.0) ZTH = 0.00001
      ZTH = ZTH * T
      EST  = PREC * ( Z - ZTH ) / SQRT( T + ZTH )
C------------------------------------------------------
C    Compute the S_curve correction and theoretical F4
C    assuming that the cluster is a single photon
C------------------------------------------------------
      CALL GXIMPA(ITCR,JFCR,BARI,BARJ,GERB,CTHCOR)
C
C compute theta and phi
C
      RG  =  SQRT(GERB(1)**2+GERB(2)**2)
      THSC  = ATAN2(RG,GERB(3))
      PHSC  = ATAN2(GERB(2),GERB(1))
      IF(PHSC  .LT. 0.) PHSC =PHSC +2.*3.14159265359
C
C      Renormalization of the expected F4
C

      COREF = COR2OR*CTHCOR
C
      RETURN
      END
#endif
