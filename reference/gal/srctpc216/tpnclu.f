      SUBROUTINE TPNCLU(NCL)
C-----------------------------------------------------------------------
C!  Return the size of a cluster from a non-delta-ray primary.
C
C  Called from:  TSDEDX
C  Calls:        RNDM
C
C  Inputs:  /TPCONS/     --EBINC,  table of probabilities of cluster
C                                  sizes, from Lapique and Puiz.
C                        --MXBINC, number of slots in EBINC
C                        --CONCLU,
C                          POWERC, constants used to calculate probabi-
C                                  lities (in CONCLU/E**POWERC) of the
C                                  cluster size distribution.
C
C  Outputs:  PASSED:     --NCL, the number of electron in the cluster
C
C  A. Caldwell
C  Modifications :
C    1.- P. Janot    18 May 1988   --Change the analytic form of cluster
C                                    size distribution from 1/E to
C                                    (1/E)**(1+x). x is determined to
C                                    reproduce TPC90 data.
C-----------------------------------------------------------------------
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
 1    X = RNDM(F)
C
      DO 2 I = 1,MXBINC
C
         IF ( X .LE. EBINC(I) ) THEN
            NCL = I
            RETURN
         ENDIF
C
 2    CONTINUE
C
      SUMPRB = EBINC(MXBINC)
C
      DO 3 I = MXBINC+1,MXCL
C
          SUMPRB = SUMPRB + CONCLU/FLOAT(I)**POWERC
C
          IF ( X .LE. SUMPRB ) THEN
              NCL = I
              RETURN
          ENDIF
C
 3    CONTINUE
C
C  We've exceeded largest cluster, go dice again
C
      GOTO 1
C
      END
