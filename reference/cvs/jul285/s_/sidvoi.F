      SUBROUTINE SIDVOI(ICLU,IMC,IRC,JPC,KDEAD,IZDEA,NDEAD)
C----------------------------------------------------------------------
C! - Looks for dead neighbors in SICAL in 5(R) x 3 (phi) around  IPAD
C!   in any z plane
C!  Author   :  B.Bloch-Devaux June 93
C!
C!  Inputs:    Cluster number in SCLS : ICLU
C!              SiCAL pad address IMC  Module
C!                                IRC  Radial bin
C!                                JPC  Phi    bin
C!  Outputs:   Neighboring pad addresses
C!             KDEAD          array of dead neighbor addresses
C!                            coded  as for extended raw data (14 BITS)
C!             IZDEA          array of dead neighbor z planes
C!             NDEAD          number of neighboring dead pads
C!
C!---------------------------------------------------------------------
C!       Pad addresses are given by stack number, phi bin and radial bin
C!                                 I: 1 to 12    J: 1 to 32   K: 1 to 16
C!              and module number M: 1 = A side, 2= B side
C!---------------------------------------------------------------------
C?
C?   Description   Routine used in dead pad energy correction
C?   ===========
C?      Neighboring pads are defined as :
C?        - Same layer
C?        - R-2  to R+2
C?        - Phi-1 to Phi+1
C?       They must be identified as Dead pads and belong to cluster ICLU
C?
C!======================================================================
#ifndef DOC
      PARAMETER( NMOD=2,NSTKSI=12,NPHISI=32,NRADSI=16)
#include "smapjj.h"
#include "spdajj.h"
      PARAMETER(LPDEA=50)
      DIMENSION KDEAD(LPDEA),IZDEA(LPDEA)
#include "bcs.h"
      DATA IFIR/0/
#include "bmacro.h"
C
      IF ( IFIR.EQ.0) THEN
        IFIR = 1
        NASMPD = NAMIND('SMPD')
        NASMAP = NAMIND('SMAP')
        NASPDA = NAMIND('SPDA')
      ENDIF
C
      NDEAD=0
      CALL VZERO(KDEAD,LPDEA)
      CALL VZERO(IZDEA,LPDEA)
      KSMPD = IW(NASMPD)
      KSMAP = IW(NASMAP)
      KSPDA = IW(NASPDA)
C  no dead pads known
      IF ( KSMPD.LE.0) GO TO 999
      NSMPD = LROWS(KSMPD)
      IF ( NSMPD.LE.0) GO TO 999
      J = JPC
      K = IRC
      IMD = IMC
C    protections
      IF(J.GT.NPHISI)           GOTO 999
      IF(K.LT.1.OR.K.GT.NRADSI) GOTO 999
C  if no SMAP there , do nothing
      IF ( IW(NASMAP).LE.0)     GO TO 999
C Loop on all z layers
      DO 21  I = 1,NSTKSI
C Start with same layer neighbors ,3 pads in phi , 5 pads in R
      DO 20 JL = J-1,J+1
        JN=JL
        IF(JN.LT.1) JN=NPHISI
        IF(JN.GT.NPHISI) JN=1
        DO 10 KN = K-2,K+2
           IF(KN.LT.1.OR.KN.GT.NRADSI) GOTO 10
           CALL SIENCZ(IDEAD,IMD,I,JN,KN)
           ITOW = ITABL(KSMAP,IDEAD,1)
           IF ( ITOW.LE.0) GO TO 10
           KLL = KROW(KSPDA,ITOW)
           IDE = IW(KLL+JSPDSM)
C Check if this pad is a dead pad
           IF ( IDE.LE.0    ) GO TO 10
C Check if this pad belongs to cluster ICLU
           ICL = IW(KLL+JSPDSC)
           IF ( ICL.NE.ICLU ) GO TO 10
           NDEAD = NDEAD +1
           KDEAD(NDEAD) = IDEAD
           IZDEA(NDEAD) = I
   10   CONTINUE
   20 CONTINUE
   21 CONTINUE
C
  999 RETURN
      END
#endif
