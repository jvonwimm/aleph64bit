      SUBROUTINE TPRIMB(POI,ZB,NCL,TPRIME,NDEL)
C
C! Fast sim : Return the number of delta-rays produced for NCL primary
C  clusters. TPRIME is the total energy loss in these delta-rays.
C
C  Called from:  TPDRAY
C  Calls:        RNDM
C
C  Inputs:   PASSED:      --POI,   average number of collisions per cm
C                         --ZB,    charge**2/beta**2 of primary track
C                         --NCL,   the number of primary clusters in a
C                                  segment
C            TPCONS.INC:  --CRUTH, the constant on the 1/E (Rutherford)
C                                  part of the primary energy
C                                  distribution
C                         --DEMIN, the minimum energy for a delta-ray
C                         --DEMAX, the maximum energy for a delta-ray
C
C  Outputs:  PASSED:      --TPRIME, the total primary kinetic energy
C                                   (=0. if no delta rays)
C                         --NDEL,   number of delta-rays in the segment
C
C  P. Janot   05/01/88
C
C  Modifications:
C
C     1.  D.Cowen 8-Apr-88  --Use a gaussian approximation to the
C                             binomial distribution for NCL >= 100.
C
C
C  TPCONS contains physical constants for TPC simulation
C
      COMMON /DELTA/ CDELTA,DEMIN,DEMAX,DELCLU,RADFAC,CYLFAC
      PARAMETER (MXGAMV = 8)
      COMMON /TGAMM/ GAMVAL(MXGAMV),GAMLOG(MXGAMV),POIFAC(MXGAMV),
     &               POIMAX,POIMIN,CFERMI,CA,CB,POIRAT,POICON
      PARAMETER (MXBINC = 20)
      COMMON /CLUST/ EBINC(MXBINC),CONCLU,WRKFUN,MXCL,CKNMIN,CFANO,CRUTH
     &              ,POWERC
      COMMON /AVALA/ THETA,ETHETA
      COMMON /TPTIME/ NTMXSH,NTMXNO,NTMXAN,NTMXDI,NTSCAN,NTBNAS,NTBAPD
      COMMON /TPELEC/ TPRPAR,TPRSER,TPCFET,TCFEED,TMVPEL,TSIGMX,NTPBIT
C
      DATA ICALLS/0/
C
C  If this is the first call, set up the delta-ray probability limits
C  from the energy limits
C
      NDEL = 0
      IF(NCL.EQ.0) RETURN
      PRMIN = (ZB * CRUTH)*(1./DEMIN - 1./DEMAX)/POI
C
C  Calculate the number of delta-rays for NCL clusters according
C  to the binomial distribution for NCL < 100; use gaussian
C  approximation for NCL >= 100.  If the number of delta rays
C  comes out < 0, set it = 0.
C                                      Doug 8 APR 88
C
      IF (NCL .LT. 100) THEN
C
         X = RNDM(P)
C
         CNM = 1.
         PNM  = 1.
         DO 2 ICL = 1,NCL+1
            IF(ICL.EQ.(NCL+1)) THEN
               NDEL=NCL
               GOTO 3
            ENDIF
            NDEL = ICL-1
            PNM = PNM - CNM *  PRMIN   **  FLOAT(NDEL)  *
     *                       (1.-PRMIN)**FLOAT(NCL-NDEL)
            IF ( X .GE. PNM) GOTO 3
            CNM = CNM*(NCL-NDEL)/(NDEL+1)
   2     CONTINUE
   3     CONTINUE
C
      ELSE
C
         CALL RANNOR(TPRIM1,DUMMY)
         NDEL = (PRMIN*NCL)+(TPRIM1 * SQRT( NCL*PRMIN*(1.-PRMIN)) )+.5
C
      ENDIF
C
      IF (NDEL .LT. 0) NDEL = 0
C
C  Not called a delta-ray, set default energy value
C
      TPRIME = 0.
C
      IF(NDEL.NE.0) THEN
C
C  We are over the limit for the minimum energy of a delta
C  Toss a new random number to see what the energy of the delta is.
C  a different probability scale to preserve precision.
C
          DO 4 IDEL = 1, NDEL
              X = RNDM(IDEL)
              TPRIME = TPRIME + DEMIN*DEMAX/(DEMAX*(1.-X) + DEMIN*X)
 4        CONTINUE
      ENDIF
C
      RETURN
      END
