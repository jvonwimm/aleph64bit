*DK hcproj
      SUBROUTINE HCPROJ(POS,IS,IGO,KLEAV)
C---------------------------------------------------
C
C! Project the spots given onto the nearest sensible plane
C!
C!       Author    :F.Romano    86/06/06
C!       Modified  :G.Catanesi  86/11/01
C
C      Project the spots given by Hcpars onto the
C      nearest sensible plane. If the plane is in
C      another module the projection is performed
C      Care is taken when the shower propagates at
C      a small angle respect to the sensible planes
C      without changing the reference system.
C!
C!       input :
C!            - POS/R  : coordinates in the 'SHOWER' frame
C!            - ISS/I  : first plane crossed by the shower
C!            - IGO/I  : flag (+1=Z>0 -1=Z<0)
C!
C!       output :
C!             -POS/R  : coordinates projected on nearest
C!                       sensible plane
C!             -KLEAV/I: flags the type of spot
C!                0   spot is in the same barrel module where
C!                    the shower started
C!                1   spot is in a contiguous barrel module
C!                2   spot is in the endcap
C!
C!      Called by : HCSHOW
C!      Calls     : none
C ---------------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "alcons.h"
#include "jobcom.h"
#include "hccoun.h"
#include "hccong.h"
#include "hcgega.h"
#include "hcloc.h"
C
      DIMENSION POS(*)
C
C.....Initialize flags
C
      FHCPRJ=.FALSE.
      KLEAV=0
C
C....Determine position of sensible plane along z-axis
C
      ZSENS = HCTIRF(IHCPOR)+(IS-1)*HCSMTH+ .5*HCTUAC+HCTINS

C
      IF(ABS(SIN(HCPDIP)).LT..1) THEN
C
C....Shower axis is almost parallel to sensitive planes
C
         IF((IHCPOR.EQ.LPBAR).AND.(ABS(POS(2)).GT. (POS(3)+RHBAMN
     +   )*TAN(PIBY12))) GOTO 10
C
         IF(IGO.EQ.1) THEN
            ZUPP=ZSENS-.5*HCTUAC
            ZLOW=ZSENS-HCSMTH+.5*HCTUAC
         ELSE
            ZUPP=ZSENS+HCSMTH-.5*HCTUAC
            ZLOW=ZSENS+.5*HCTUAC
         ENDIF
C
         IF((IS-IGO).LT.1) ZLOW=-9999999.
         IF((IS-IGO).GT.NHCPLA(IHCPOR))ZUPP= 9999999.
C
         IF(POS(3).GT.ZUPP) THEN
            ZSENS=ZUPP+.5*HCTUAC
            POS(3)=ZSENS+HCPACZ*HCPFAC
            GO TO 10
         ENDIF
C
         IF(POS(3).LT.ZLOW) THEN
            ZSENS=ZLOW-.5*HCTUAC
            POS(3)=ZSENS-HCPACZ*HCPFAC
            GO TO 10
         ENDIF
         GO TO 20
      ENDIF
   10 CONTINUE
C....
C   Projection with the sensible plane is given by XN,YN,ZN
C....
      WL=(ZSENS-POS(3))/HCPACZ
      XN=POS(1)+WL*HCPACX
      YN=POS(2)+WL*HCPACY
      ZN=ZSENS
C
      IF(IHCPOR.EQ.LPBAR)THEN
C
C..starting point in barrel
C
         IF(ABS(XN).LE.ZHBAMX) THEN
C..barrel ==> barrel
            YSENS=(ZN+RHBAMN)*TAN(PIBY12)
C
Check if the projection is inside the same module
C
            IF(ABS(YN).GT.YSENS)THEN
C
C.....it is in the nearby module
C
               WLZY=SQRT((ZN-POS(3))**2+(YN-POS(2))**2)
               ALPHA=ATG(HCPACZ,HCPACY)
               IF(ALPHA.GT.PIBY2)ALPHA=PI-ALPHA
               DY = ABS(YN) - YSENS
               DLZY=SIN(PIBY6)*DY/SIN(PI-(ALPHA+PIBY6))
               DLZY=SIGN(DLZY,ZN-POS(3))
               SLZY=WLZY-DLZY
C
               POS(1)=(DLZY*POS(1)+SLZY*XN)/WLZY
               POS(2)=(DLZY*POS(2)+SLZY*YN)/WLZY
               POS(3)=(DLZY*POS(3)+SLZY*ZN)/WLZY
               KLEAV=1
            ELSE
C
               POS(1) = XN
               POS(2) = YN
               POS(3) = ZN
            ENDIF
C
         ELSE
C.
C.....barrel ==> endcap
C.
            APOS=ABS(POS(1))-ZHBAMX-HCTIRF(LPECA)-HCTUAC
C
            IF(APOS.LE.0.) THEN
               ISECAP=1
            ELSE
               ISECAP=INT(APOS/HCSMTH)+1
            ENDIF
            IF(ISECAP.GT.NHCPLA(LPECA)) GO TO 20
C
            XSENS=ZHBAMX+HCTIRF(LPECA)+(ISECAP-1)*HCSMTH +.5
     &      *HCTUAC+HCTINS
            XSENS=SIGN(XSENS,XN)
            WL=(XSENS-POS(1))/HCPACX
            POS(1)=XSENS
            POS(2)=POS(2)+WL*HCPACY
            POS(3)=POS(3)+WL*HCPACZ
            KLEAV=2
         ENDIF
C
      ELSE
C..end cap
C.....endcap ==> barrel
C         (to be implemented)
C....
C    Store projection coordinates for further analysis
C
         POS(1)=XN
         POS(2)=YN
         POS(3)=ZN
         KLEAV = 2
C
      ENDIF
C......projection found......
      FHCPRJ=.TRUE.
C
      IF(FHCDB2)THEN
       WRITE(LOUTIO,*)' HCPROJ POS KLEAV ',(POS(J),J=1,3),KLEAV
      ENDIF
      RETURN
C................................
   20 CONTINUE
      IF(FHCDB2)THEN
         WRITE(LOUTIO,*) ' HCPROJ projection not found'
         WRITE(LOUTIO,*) ' HCPROJ POS IGO ',(POS(J),J=1,3),IGO
      ENDIF
      END
#endif

