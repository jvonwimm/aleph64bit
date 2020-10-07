       SUBROUTINE HFBAHI(ISUBC,MODAQ,LAYER,LWIRE,NWCLU,MODBA,XHIT,
     * CLUW,NHIT,NNWIRE,NHITWI)
C-------------------------------------------------------------------
CKEY HCALDES HCAL BARREL WIRE CLUSTER HIT / USER
C! converts wire clusters to hits in Hbarrel
C!
C!      A.Gregorio & A.Venturi 8-oct-1993:
C!              correction of the decoding of the even modules
C!
C!      G.Capon    17-feb-1992 (extended version of old HFBHIT)
C!     1 cluster ==> 1 hit, except if dead zones or spacers
C!      fall inside the cluster
C! input :
C!      ISUBC : subcomponent number
C!      MODAQ : acquisition processor number (13-24)
C!      LAYER : streamer tubes layer number
C!      LWIRE : last wire(strip) of a cluster of consecutive hit wires
C!      NWCLU : number of consecutive hit wires in a cluster (1-16)
C! output:
C!   MODBA(N) : phys module # for hit N (a cluster may be on 2 modules)
C!    XHIT(N) : local coordinate for hit N
C!    CLUW(N) : cluster width for hit N
C!    NHIT    : number of hits for this cluster (=1 except if dead zones)
C!   NNWIRE(I): wire number of wire at position I inside cluster
C!   NHITWI(I): hit    "                "            "
C!
C--------------------------------------------------------------------
#ifndef DOC
#include "hcgeom.h"
        REAL XHIT(*),CLUW(*),XFHIT(3)
        INTEGER MODBA(*),NNWIRE(*),NHITWI(*)
        DIMENSION IFWAS(2)
C
C?      NEITSP =nb 8tubes before spacer1
C?      HCSPLT =width of spacer 1
C?      XBARR0 = coordinate of wire 0
C?      IFWAS(I) = Indes of First Wire After Spacer I
C
        NWMOD=NHCEIT*NEITHC(LAYER)
        NWMAX=2*NWMOD-1
        NHIT=0
        IF(LWIRE.GT.NWMAX)                                      GO TO 50
        DO 10 ISP=1,2
   10   IFWAS(ISP)=NHCEIT*NEITSP(LAYER,ISP)
        NFWIR=LWIRE-NWCLU+1
C
        DO 20 J=1,3
           CLUW(J) = 0.
   20   CONTINUE
C
C?           loop over wires in cluster
C
        DO 30 ICLU=1,NWCLU
        NWIRE=NFWIR+ICLU-1
        IWIRE=NWIRE
        IF(NWIRE.GE.NWMOD) IWIRE=NWMAX-NWIRE
C?             if first wire of cluster generate new hit
C?            if wire after modules jonction generate new hit
C?             if wire after spacer generate new hit
        IF (ICLU.EQ.1  .OR.  NWIRE.EQ.NWMOD  .OR.
     +     ( (IFWAS(1).EQ.IWIRE   .OR.  IFWAS(2).EQ.IWIRE)
     +     .AND.NWIRE.LT.NWMOD)   .OR.
     +     ( ((IFWAS(1)-1).EQ.IWIRE   .OR.  (IFWAS(2)-1).EQ.IWIRE)
     +     .AND.NWIRE.GE.NWMOD) ) THEN
           NHIT=NHIT+1
           XFHIT(NHIT)=XBARR0+HCSABL*FLOAT(IWIRE)
           IF(IWIRE.GE.IFWAS(1))XFHIT(NHIT)=XFHIT(NHIT)+HCSPLT(LAYER,1)
           IF(IWIRE.GE.IFWAS(2))XFHIT(NHIT)=XFHIT(NHIT)+HCSPLT(LAYER,2)
           MODBA(NHIT)=2*(MODAQ-12)-1+NWIRE/NWMOD
        ENDIF
        CLUW(NHIT)=CLUW(NHIT)+HCSABL
        NNWIRE(ICLU)=NWIRE
        NHITWI(ICLU)=NHIT
   30   CONTINUE
C
C?            compute hit center and width
C
        DO 40 IHIT=1,NHIT
          IF(MOD(MODBA(IHIT),2).NE.0)
     +      XHIT(IHIT)=XFHIT(IHIT)+(CLUW(IHIT)-HCSABL)/2.
          IF(MOD(MODBA(IHIT),2).EQ.0)
     +      XHIT(IHIT)=XFHIT(IHIT)-(CLUW(IHIT)-HCSABL)/2.
   40   CONTINUE
C
   50   RETURN
        END
#endif
