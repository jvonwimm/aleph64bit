      SUBROUTINE TCO4PA (IPADA, RPULS, PRFS2, RPH1, RPH2,IER)
C
C---------------------------------------------------------------------
C!  TPC coordinates finding : 2 COORDINATES FROM 4 PADS
C!  algorithms" (D). Uses 4 pads.
C!
C!  Author:    Gigi Rolandi 7-09-89
C!
C!  INPUT :
C!    I4/  IPADA      : indice of 1st pad of the four, in [1 .. NPADS].
C!    R4/  RPULS(ip): p heights for this cluster (in [1 ... 255] !).
C!    R4/  PRFS2    : width**2 (>0 !) of PRF.
C!
C!  OUTPUT :
C!    R4/  RPH1     : 1st coordinate, in [1..NPADS].
C!    R4/  RPH2     : 2nd coordinate, in [1..NPADS].
C!    I4/  IER      : Error code 0=O.K.
C!
C!  Called by ......
C!
C!-------------------------------------------------------------------
#ifndef DOC
C
      DIMENSION RPULS(*)
C
      IPADB = IPADA + 1
      IPADC = IPADA + 2
      IPADD = IPADA + 3
C
      C= EXP(1./PRFS2)
C
      A1= C*RPULS(IPADA)
      A2= RPULS(IPADB)
      A3= RPULS(IPADC)
      A4= C*RPULS(IPADD)
C
      SR2= 4*(A1*A1*A4*A4+A4*A2*A2*A2+A1*A3*A3*A3)-
     $     3*(A2*A3+A1*A4)*(A2*A3+A1*A4)
      IER= 1
      IF(SR2.LT.0)RETURN
      SR=  SQRT(SR2)
C
      R1= A2*A2-A1*A3
      R2= A3*A3-A2*A4
C
      CC= A2*A3-A1*A4
C
      F1= (CC+SR)/R1
      F2= (CC+SR)/R2
        IF(F1.LE.0.OR.F2.LE.0)RETURN
      IER=0
C
      RPH1= FLOAT(IPADB)+0.5+
     $      PRFS2*ALOG(F1/2.)
      RPH2= FLOAT(IPADB)+0.5-
     $      PRFS2*ALOG(F2/2.)
C
      RETURN
      END
#endif
