      SUBROUTINE ECTRAP(NETP1,XTRAP,ITRAP,XDER)
C-----------------------------------------------------------------------
C!     Extrapolates tracks in Ecal
C-
C-     Authors : M.N. Minard , M. Pepe       26/02/89
C-
C
C Input : NETP1          = ETP1 element of track to extrapolate
C Output: XTRAP(3,5)     = x,y,z coordinates of extrapolated point
C                          in each stack
C         ITRAP(1,5)     = number of storeys crossed by track per stack
C         ITRAP(I=2-5,5) = packed cell address of extrapolated
C                          points per stack
C         XDER (3,5)     = cosine directors of extrapolated track
C                          in each stack
C?
C!======================================================================
#ifndef DOC
#include "etp1jj.h"
#include "ecnmsk.h"
#include "alcons.h"
#include "bcs.h"
#include "ectcom.h"
#include "emskex.h"
      DIMENSION XTRAP(3,5),ITRAP(5,5),XDER(3,5)
      DIMENSION XPTRA(3),EVECT(7),EVECE(7),IEXTR(5)
      DIMENSION ITOW (3) , ITRIG(2)
      EXTERNAL EINTST
      LOGICAL  EINTST
#include "bmacro.h"
C
C-    Initialization counters & arrays
C
      CALL VZERO ( XTRAP,15 )
      CALL VZERO ( ITRAP,25 )
      CALL VZERO ( XDER ,15 )
      CALL VZERO ( IEXTR , 5 )
C
C-    Point to the track
C
      KETP1 = IW ( NAETP1 )
      JETP1 = KROW ( KETP1 , NETP1 )
      K = 1
C
C-    Check if track above momentum threshold
C
      IF ( RW(JETP1+JETPPT).LT.PMINC ) GO TO 10
C
C-    Set initial column & row address
C
      CALL EX3IJK(RW(JETP1+JETPX3),J,I,INK,NTA,IFLA,KSC,IMOD)
      IF ( .NOT.EINTST (I,J,INK) ) GOTO 10
C
C-    Determine extrapolation plane
C
      IF (I.EQ.0) THEN
        CALL UCOPY ( RW(JETP1+JETPX3),EVECT(1),3)
        R0 = SQRT ( EVECT(1)**2+EVECT(2)**2)
        CALL VSCALE( EVECT(1),1.04,EVECT(1),3)
        CALL EX3IJK(EVECT(1),J,I,INK,NTA,IFLA,KSC,IMOD)
        IF ( .NOT.EINTST (I,J,INK) ) GOTO 10
        IF (IFLA.NE.2) GO TO 10
      ENDIF
C
C-    Get storey baricenter
C
      CALL ESRBC('ALEPH',I,J,K,EVECE)
      ZEXTR = EVECE(3)
      REXTR = SQRT(EVECE(1)**2+EVECE(2)**2)
C
C-    Initialize starting point for extrapolation
C
      CALL UCOPY (RW(JETP1+JETPX3),EVECT(1),7)
C
C-    Extrapolation in electromagnetic calorimeter
C
 30   CONTINUE
C
C-    Confine extrapolation to detector volume
C-    Check subcomponent  KSC = 1 --> End cap z>0
C-                            = 2 --> Barrel
C-                            = 3 --> End cap z<0
C
      IF ( KSC.NE.2.AND.REXTR.LT.REMAX )     REXTR = REMAX
      IF ( KSC.EQ.2.AND.ABS(ZEXTR).LT.ZEMAX) ZEXTR = ZEMAX
     +    * (ZEXTR/ABS(ZEXTR))
      CALL AUHCYL(REXTR,ABS(ZEXTR),FILDEC,RW(JETP1+JETPCH)
     & ,EVECT,EVECE,IH)
C
      IF ( IH.EQ.0 ) THEN
C-    Not valid extrapolation
            GO TO 50
       ENDIF
C
C-    Convert extrapolated point into I,J,K
C
      CALL EX3IJK(EVECE,J,I,INK,NTA,IFLA,KSCN,IMOD)
      IF ( .NOT.EINTST (I,J,INK) ) GOTO 10
      IF (IFLA.GT.3)GO TO 48
C
      IF ( KSCN.EQ.KSC.AND.INK.NE.K) THEN
C-    Impact in the same module
C
          CALL RERROR('ECTRAP',1,'Extrapolation in wrong stack')
          GO TO 10
        ENDIF
C
C-    Store stack extrapolation
C
C-    Extrapolated point
        KSC = KSCN
        IEXTR ( K ) = IEXTR(K) + 1
        CALL UCOPY (EVECE(1),XTRAP(1,K),3)
C-    Direction cosines
        CALL UCOPY (EVECE(4),XDER (1,K),3)
C
C-    Calculate new impact
C
        CALL UCOPY ( EVECE(1),EVECT(1),6 )
C
C
C-    Prepare next point
C
      IF ( K.LT.3.AND.INK.NE.3) THEN
         K = K+1
C
C-    Calculate next point to extrapolate
C
         CALL ESRBC('ALEPH',I,J,K,EVECE)
         REXTR = SQRT(EVECE(1)**2+EVECE(2)**2)
         ZEXTR = EVECE(3)
         GO TO 30
      ENDIF
 50   CONTINUE
 48   CONTINUE
      CALL ECRTR ( NETP1 , ITRAP )
C
C
C-    Check extrapolation are coherent
C
      DO 52 IKJ = 1,3
      IF ( IEXTR(IKJ).EQ.0) ITRAP ( 1,IKJ ) = 0
 52   CONTINUE
C
C-    HCAL extrapolation ( no field )
C
      KH  = 1
 55   RHO =(RHCAL(1)+RHCAL(2))*0.5
      ZM  =(ZHCAL(1)+ZHCAL(2))*0.5
      CALL AULCYL(RHO,ZM,EVECT,EVECE,IH)
      IF (IH.EQ.0) GO TO 70
      CALL UCOPY (EVECE,XTRAP(1,KH+3),3)
      CALL UCOPY (EVECE(4),XDER(1,KH+3),3)
      CALL HFNDTW ( EVECE(1),'ALEPH',ITOW(2),ITOW(1),KT,ISUBC,IMOD,
     +              IPL,IDEAD)
      ITRAP ( 1,KH+3 ) = ITRAP (1,KH+3) + 1
      IF ( ITRAP (1,KH+3).LE.4 ) THEN
         IKH = 1+ ITRAP (1,KH+3)
         ITRAP ( IKH,KH+3 ) = (ITOW(2)-1)*256 + (ITOW(1)-1)*2 + KH-1
      ENDIF
      IF ( KH.EQ.1 ) GO TO 70
C
C-    Next stack
C
      KH = KH+1
      CALL UCOPY (EVECE(1),EVECT(1),3)
      GO TO 55
 70   CONTINUE
C
 10   CONTINUE
      RETURN
      END
#endif
