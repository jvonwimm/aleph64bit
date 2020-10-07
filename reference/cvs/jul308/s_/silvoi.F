      SUBROUTINE SILVOI(IMD,IRD,IPH,IVOIS)
C----------------------------------------------------------------------
C!  - Build list of neighbours used in Energy sums for Luminosity cuts
C!   Author   : John Rander , B.Bloch-Devaux February 92 ,February 93
C!
C!   Inputs:    SiCAL pad subcomponent module(IMD),radius(IRD),phi(IPH)
C!   Outputs:   Neighbors   pad addresses  for radius IRD-1,IRD,IRD+1
C!              for 2 phi roads ( 2 phi pads width)
C!              over 5 z planes starting from 1
C!              IVOI(3,10,2) array of neighbor addresses
C!              coded  as for extended raw data
C!
C?---------------------------------------------------------------------
C?       Pad addresses are given by stack number, phi bin and radial bin
C?                                 I: 1 to 12    J: 1 to 32   K: 1 to 16
C?              and module number M: 1 = A side, 2= B side
C?---------------------------------------------------------------------
C?
C?   Description
C?   ===========
C?      Neighboring pads are defined as :
C?        - doublets of Phi pads J-1,J for road 1, J,J+1 for road 2
C?        - z stacks 1, 2, 3 ,4 ,5
C?        - radial pad I-1,1,I+1 ( within 1-16 range)
C?
C?
C!======================================================================
#ifndef DOC
C
      PARAMETER ( NPBIN = 32)
              DIMENSION IVOIS(3,10,2),JDIFF(2,10)
              DATA (JDIFF(1,K),K=1,10)/-1,0,-1,0,-1,0,-1,0,-1,0/
              DATA (JDIFF(2,K),K=1,10)/ 0,1, 0,1, 0,1, 0,1, 0,1/
              CALL VZERO(IVOIS(1,1,1),60)
C
      IRA = 0
      DO 10 IR = IRD-1,IRD+1
        IRA = IRA +1
        IF ( IR.LE.0 ) GO TO 10
        IF ( IR.GT.16) GO TO 10
              DO 20 KZ = 1,5
          DO 30 IROD = 1,2
            DO 40 JP = 1,2
                JPHI = IPH +JDIFF(IROD,2*(KZ-1)+JP)
              IF (JPHI.LT.1 ) JPHI = JPHI +NPBIN
              IF (JPHI.GT.NPBIN) JPHI = JPHI-NPBIN
C -       code  pad address
              CALL SIENCZ(IPAD,IMD,KZ,JPHI,IR)
C -       store in array
                IVOIS(IRA,2*(KZ-1)+JP,IROD) = IPAD
   40       CONTINUE
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
      RETURN
      END
#endif
