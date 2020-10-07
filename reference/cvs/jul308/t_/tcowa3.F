      SUBROUTINE TCOWA3 (IPADA, RPULS, PRFS2, RPHI)
C
C---------------------------------------------------------------------
C!  TPC coordinates finding : weighted-avg of three "2 pads
C!  algorithms" (E). Uses 3 pads.
C!
C!  Author:    T. Pun      12-06-86
C!  Modified:  R. Johnson   8-04-88
C!
C!  INPUT :
C!    I4/  IPADA      : indice of 1st pad of the three, in [1 .. NPADS].
C!    R4/  RPULS(ip): p heights for this cluster (in [1 ... 255] !).
C!    R4/  PRFS2    : width**2 (>0 !) of PRF.
C!
C!  OUTPUT :
C!    R4/  RPHI     : coordinate, in [1..NPADS].
C!
C!  Called by TCOALG
C!
C!-------------------------------------------------------------------
#ifndef DOC
C
      DIMENSION RPULS(*)
C
      IPADB = IPADA + 1
      IPADC = IPADA + 2
      RPHI = 0.0
C
      RP12 = FLOAT(IPADA) + 0.5 + PRFS2*
     1     ( ALOG(RPULS(IPADB)) - ALOG(RPULS(IPADA)) )
C
      RP23 = FLOAT(IPADB) + 0.5 + PRFS2*
     1     ( ALOG(RPULS(IPADC)) - ALOG(RPULS(IPADB)) )
C
      RP13 = FLOAT(IPADA) + 1.0 + 0.5*PRFS2*
     1     ( ALOG(RPULS(IPADC)) - ALOG(RPULS(IPADA)) )
C
      R412 = RPULS(IPADA) * RPULS(IPADB)
      R423 = RPULS(IPADB) * RPULS(IPADC)
      R413 = RPULS(IPADA) * RPULS(IPADC)
C
      RPHI = (R412*RP12 + R423*RP23 + R413*RP13) / (R412+R423+R413)
      RETURN
      END
#endif
