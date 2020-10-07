*DK lctrak
      SUBROUTINE LCTRAK
C--------------------------------------------------------------
C! Control m.i.p. tracking in LCal
C. - J.Dines Hansen & P.Hansen - 860417
C.                               modified by F.Ranjard - 890317
C. - Called by  LCHIT                            from this .HLB
C. - Calls      LCSTRT, LCFRAL, LCXYPA, CAHIST   from this .HLB
C.              POISSN,VSUB                      from   CERNLIB
C -----------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION XOUT(3),DXIN(3),PATH(3)
      DIMENSION XYZ(3),DXYZ(3)
#include "jqcom.h"
#include "iocom.h"
#include "lccomc.h"
#include "lcnamc.h"
#include "trkcom.h"
C -------------------------------------------------------------
C - Make sure a GEANT step has been taken inside volume
      IF (ITRKEL(8) .EQ. 1)                GOTO 999
C
C - Transform trackelement into local system
      CALL LCFRAL(IFB,MODU,XYZ,XOUT,DXIN)
C
C - Find storey and layer no. where track starts
      CALL LCSTRT(ABS(XYZ(3)),ISTMN,LAYER,LMIN,ZWIRE)
C
C - Path-length must be more than one layer
      IF (ABS(XYZ(3)-XOUT(3)).LT.ZSTPLC(ISTMN)) GOTO 999
C
C - Add the track energy to run summary bank
      KWS  = IW(NALCWS)
      IF(KWS.NE.0.AND.ISTMN.EQ.1.AND.LMIN.EQ.1) THEN
         IW(KWS+LMHLEN+4+IFB) = IW(KWS+LMHLEN+4+IFB)
     &      +INT(TRKELE(8)*1000.)
      ENDIF
C
C - Find average no. of hits pr layer
      IF (TRKELE(7).LT.1.E-6 .OR. TRKELE(13).LT.1.E-6) RETURN
      EP     = TRKELE(8)/TRKELE(7)
      GABE = TRKELE(7) / TRKELE(13)
      CALL EBETHE (EP,GABE,EP1)
      EP1=AMIN1(EP1,3.3)
      AHIT     = EELALC*EP1*EP1*16.
C
C - Assuming linear motion
C - Initial direction
      DXYZ(1) = DXIN(1)
      DXYZ(2) = DXIN(2)
      DXYZ(3) = DXIN(3)
C
C - Loop over layers
      DO 200 ISTOR = ISTMN,3
       LMAX    = LCNLAY(ISTOR)
       ZSTEP   = ZSTPLC(ISTOR)
       DO 300 LAY = LMIN,LMAX
C
C - Generate NHIT hits in this plane
        CFACT  = AMAX1(ABS(DXIN(3)),0.5)
        AVHIT  = AHIT/CFACT
        CALL POISSN(AVHIT,NHIT,IER)
        IF (IER .NE. 0)                                 GOTO 998
        IF (NHIT .LE. 0)                                GOTO 310
C
C - Deposit them on the relevant tower and plane
        CALL LCXYPA(MODU,LAYER,XYZ,IPAD)
        IF (IPAD .LE. 0)                                GOTO 310
        CALL CAHIST (NALSHI,IPAD,ISTOR,NHIT)
        CALL CAHIST (NALWHI,MODU,LAYER,NHIT)
C
C - Go to the next plane
  310   STEPL = ZSTEP/ABS(DXYZ(3))
        DO 320 I = 1,3
  320   XYZ(I) = XYZ(I) + STEPL*DXYZ(I)
        IF ( ABS(XYZ(3)).GT.ABS(XOUT(3))) GOTO 999
  300  LAYER = LAYER + 1
C
C - Go to the next storey
       LMIN = 1
  200 CONTINUE
      GOTO 999
  998 WRITE(LOUTIO,9999) IER
  999 RETURN
C
 9999 FORMAT(' +++ LCTRAK +++ error in call to POISSN',I10)
C
      END
#endif
