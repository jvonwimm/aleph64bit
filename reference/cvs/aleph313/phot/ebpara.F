      SUBROUTINE EBPARA(STAC , ETAG , ALPH , BETA , IBON)
C-----------------------------------------------------
C   AUTHOR   : J.Badier    17/04/89
C!  Calculation of the shower parameters from the stacks content.
CKEY PHOTONS SHAPE / INTERNAL
C
C   The shower is supposed to have a shape described by :
C            S ** (Alpha) * exp(-Beta)
C   The Alpha and Beta parameters are calculated from the stacks
C   limits and contents.
C
C  Input :   STAC   3 stacks contents in Gev.
C            ETAG   6 stacks limits in rad. lengthes.
C
C  Output:   ALPH   Estimated parameter Alpha.
C            BETA   Estimated parameter Beta.
C            IBON   Rejection code :
C                   = 0 : ALPH and BETA calculated.
C                   > 0 :  "        "   uncalculated.
C                   = -1: ALPH = 1. and BETA calculated.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C   Calls  EBSHOW , EBSHAP , GAUSS
C   Called by EBPRGA
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION STAC(*),ETAG(*)
C   ACCR to calculate derivatives. PREC for GAUSS routine.
C   EPS accuracy to stop iterations. KMAX number max. of iterations.
      PARAMETER( ACCR = .001 , EPS = .001 , PREC = .01 , KMAX = 10 )
C   BMIN,AMIN,AMAX limits on Beta and Alpha parameters.
      PARAMETER( BMIN = .000001 , AMIN = 1. , AMAX = 20. )
C   EPSI to avoid value=0.
      PARAMETER (EPSI = .000001)
      EXTERNAL EBSHOW
      EXTERNAL GAUSS
C ---------------------------------------------------------
      ALPH = 0.
      BETA = 0.
C
C   Tests and cuts on the signal.
C
      STOT = STAC(1) + STAC(2) + STAC(3)
C   The total energy has to be positive.
      IF(STOT .LE. EPSI) GO TO 102
      RAP1 = STAC(1) / STOT
      RAP3 = STAC(3) / STOT
C   Each stack content has to be positive.
      IF(RAP1 .LE. EPSI .OR. RAP3 .LE. EPSI) GO TO 102
C   ETAG(1) has to be positive.
      IF( ETAG(1) .LE. EPSI) GO TO 101
      DO 3 I = 2 , 6
C   The stacks limits have to be ordered.
        IF( ETAG(I) .LE. ETAG(I-1) ) GO TO 101
    3 CONTINUE
C
C   Alpha and Beta are functions of RAP1 and RAP3. They are not
C   analytical. They are estimated in a first approximation.
      S4MS2 = ETAG(6) - ETAG(2)
      CRIC = ALOG( ETAG(6) / ETAG(2) )
      CRAC = ALOG( (ETAG(6)+ETAG(4)) / (ETAG(6)-ETAG(4)) )
      CROC = ALOG( (ETAG(6)+ETAG(4)) / ETAG(6) ) -
     +       CRIC * ETAG(4) / S4MS2
      AL1 = ALOG(RAP1)
      AL3 = ALOG(RAP3)
      ALPH = (CRAC + AL3 + ETAG(4) * AL1 / S4MS2) / CROC
      BETA = 2. * (ALPH * CRIC + AL1) / S4MS2
      IF(BETA .LE. EPSI) BETA = BMIN
C
C   Alpha and Beta are calculated by iterations.
C
      KBOUC=0
    1 CONTINUE
C   EBSHAP initialisation.
      CALL EBSHAP( S , ALPH , BETA , 1 , VALE )
      F0 = GAUSS(EBSHOW,ETAG(1),ETAG(2),PREC)
      G0 = GAUSS(EBSHOW,ETAG(5),ETAG(6),PREC)
      T0 = GAUSS(EBSHOW,ETAG(3),ETAG(4),PREC) + F0 + G0
      IF(T0 .LE. EPSI) GO TO 104
      F0 = F0 / T0
      G0 = G0 / T0
      FECAL = RAP1 - F0
      GECAL = RAP3 - G0
C   Accuracy limit reached.
      IF( ABS(FECAL) .LT. EPS .AND.
     +    ABS(GECAL) .LT. EPS )     GO TO 2
C   New iteration.
      KBOUC = KBOUC + 1
C   No convergence.
      IF(KBOUC .GT. KMAX) GO TO 104
C   Linear approximation, derivatives calculation.
      ALPH = ALPH + ACCR
C   EBSHAP initialisation.
      CALL EBSHAP( S , ALPH , BETA , 1 , VALE )
      F1 = GAUSS(EBSHOW,ETAG(1),ETAG(2),PREC)
      G1 = GAUSS(EBSHOW,ETAG(5),ETAG(6),PREC)
      T1 = GAUSS(EBSHOW,ETAG(3),ETAG(4),PREC)  + F1 + G1
      IF(T1 .LE. EPSI) GO TO 104
C   Derivatives in alpha.
      DF1 = (F1 / T1 - F0) / ACCR
      DG1 = (G1 / T1 - G0) / ACCR
C
      ALPH = ALPH - ACCR
      BETA = BETA + ACCR
      CALL EBSHAP( S , ALPH , BETA , 1 , VALE )
      F2 = GAUSS(EBSHOW,ETAG(1),ETAG(2),PREC)
      G2 = GAUSS(EBSHOW,ETAG(5),ETAG(6),PREC)
      T2 = GAUSS(EBSHOW,ETAG(3),ETAG(4),PREC) + F2 + G2
      IF(T2 .LE. EPSI) GO TO 104
C   Derivatives in beta.
      DF2 = (F2 / T2 - F0) / ACCR
      DG2 = (G2 / T2 - G0) / ACCR
      BETA = BETA - ACCR
      DETER = DF1 * DG2 - DF2 * DG1
      IF(DETER .LE. EPSI) GO TO 104
C   New values.
      ALPH = ALPH + (DG2 * FECAL - DF2 * GECAL) / DETER
      BETA = BETA - (DG1 * FECAL - DF1 * GECAL) / DETER
      IF(ALPH .GT. AMAX) GO TO 105
      IF(ALPH .LT. AMIN) GO TO 106
      GO TO 1
C
C  Good event.
    2 CONTINUE
      IBON = 0
      RETURN
C ========================= error ============================
C   Bad configuration
  101 IBON = 1
      GO TO 98
C   Deposited energy < 0.
  102 IBON = 2
      GO TO 98
C   No convergence
  104 IBON = 4
      GO TO 98
C   ALPH too big
  105 IBON = 5
      GO TO 98
C   ALPH too small
  106 CONTINUE
      BETA = - ALOG ( 1.- RAP1) / ETAG(2)
      ALPH = 1.
      IBON = -1
   98 CONTINUE
      END
#endif
