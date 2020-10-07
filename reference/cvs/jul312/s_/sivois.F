      SUBROUTINE SIVOIS(IPAD,KVOIS,NVOIS)
C----------------------------------------------------------------------
C!  - Looks for neighboors in SICAL
C!
C!   Author   : John Rander , B.Bloch-Devaux November 91
C!
C!   Inputs:    SiCAL pad address IPAD (coded as below)
C!   Outputs:   Neighboring pad addresses
C!              NVOIS          number of neighboring pads
C!              KVOIS          array of neighbor addresses
C!              coded  as for extended raw data (14 BITS)
C!
C!   Routine needs SiCAL geometry information defined by SIGEOM
C!---------------------------------------------------------------------
C!       Pad addresses are given by stack number, phi bin and radial bin
C!                                 I: 1 to 12    J: 1 to 32   K: 1 to 16
C!              and module number M: 1 = A side, 2= B side
C!---------------------------------------------------------------------
C!
C!   Description   Routine used in cluster finding
C!   ===========
C!      Neighboring pads are defined as either:
C!        - Same stack neighbors => there are typically 8 of these
C!        - Adjacent stack neighbors => there are typically 4 of these
C!                                      because of phi shifts of stacks
C!        - Diagonal neighbors on adjacent stacks are ignored
C!
C?
C!======================================================================
#ifndef DOC
#include "rcurnt.h"
      PARAMETER( NSTKSI=12,NPHISI=32,NRADSI=16)
      DIMENSION KVOIS(12),SHFPH(NSTKSI)
      DATA SHFPH / 1.,0.,-1.,1.,0.,-1.,1.,0.,-1.,1.,0.,-1./
      DATA IFIR/0/
C
      IF ( IFIR.EQ.0) THEN
        IFIR = 1
C  In May SICAL A was mounted on B side...
        IF ( IRUNRC.LT.15000 .AND. .NOT.FMCRUN ) THEN
          DO 5 I = 1,12
            SHFPH(I)= -SHFPH(I)
    5     CONTINUE
        ENDIF
      ENDIF
C
C
      NVOIS=0
C -     decode  pad address
C
      CALL SIDCOZ(IPAD,IMD,I,J,K)
C
C==> PROTECTIONS
      IF(I.LT.1.OR.I.GT.NSTKSI) GOTO 999
      IF(J.GT.NPHISI)           GOTO 999
      IF(K.LT.1.OR.K.GT.NRADSI) GOTO 999
C
C Start with same stack neighbors
      DO 20 JL = J-1,J+1
        JN=JL
        IF(JN.LT.1) JN=NPHISI
        IF(JN.GT.NPHISI) JN=1
        DO 10 KN = K-1,K+1
          IF(KN.LT.1.OR.KN.GT.NRADSI) GOTO 10
          IF(JN.EQ.J.AND.KN.EQ.K) GOTO 10
          NVOIS=NVOIS+1
          CALL SIENCZ(KVOIS(NVOIS),IMD,I,JN,KN)
   10   CONTINUE
   20 CONTINUE
C
C Then loop over two adjacent stacks
C    Neighbors in adjacent stacks have same K value (no diagonals)
      DO 30 N = 1,2
        IADJ=I+3-2*N
        IF(IADJ.GE.1.AND.IADJ.LE.NSTKSI) THEN
C Pad is shadowed by two neighbor pads in adjacent stack
C    First neighbor has same Phi index
          NVOIS=NVOIS+1
          CALL SIENCZ ( KVOIS(NVOIS),IMD,IADJ,J,K)
C    Second neighbors Phi index depends upon relative Phi-shift
          NVOIS=NVOIS+1
          J2=J-ISIGN(1,IFIX(SHFPH(IADJ)-SHFPH(I)))
          IF(J2.LT.1) J2=NPHISI
          IF(J2.GT.NPHISI) J2=1
          CALL SIENCZ ( KVOIS(NVOIS),IMD,IADJ,J2,K)
        ENDIF
   30 CONTINUE
  999 RETURN
      END
#endif
