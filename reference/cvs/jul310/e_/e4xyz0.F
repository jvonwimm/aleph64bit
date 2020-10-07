      SUBROUTINE E4XYZ0(PAREX,IERR)
C----------------------------------------------------------------------
C!  - R2 ESTIMATOR : GIVE X Y Z EXTRAPOLATION ON THE FIRST PLANE OF ECAL
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C?
C!======================================================================
#ifndef DOC
#include "eiflag.h"
#include "etpinf.h"
#include "e4com0.h"
#include "rlunit.h"
#include "rflags.h"
#include "rparac.h"
#include "e4par0.h"
#include "e4par1.h"
#include "rconds.h"
      PARAMETER(EPSILO=0.03)
      CHARACTER*23 WRONG
      DIMENSION PAR(7),PAREX(6),DX(3)
      IBOUC=0
      RP1=RBARL0
      ZP1=ZEDCP0
      PAR(1)=X0TP
      PAR(2)=Y0TP
      PAR(3)=Z0TP
      PAR(4)=C0TP
      PAR(5)=C1TP
      PAR(6)=C2TP
      PAR(7)=PTTP
      IF(JDBDRF(JULEC).GT.8)
     &WRITE(LDEBRL,*)'PX PY PZ ',PXTP,PYTP,PZTP
CXX DETERMINATION X0 Y0 Z0 ET COS DIRECTEUR DE LA DTRE AU PLAN 1 DU
CXX MODULE D ENTREE
 1    CALL AUHCYL(RP1,ZP1,FIELRC,CHTP,PAR,PAREX,ICODE)
      IF ( ICODE.EQ.0) THEN
         IERR = 1
         GO TO 999
      ENDIF
      IF(JDBDRF(JULEC).GT.8)
     &WRITE (LDEBRL,122)PAREX,ICODE,RP1,ZP1
 122  FORMAT(1X,'PAREX',6(F8.2,1X),'ICODE',I2,'R Z',2F8.3)
      IBOUC=IBOUC+1
      DX(1)=PAREX(1)
      DX(2)=PAREX(2)
      DX(3)=PAREX(3)
      ROWJ=0
      COLI=0
      STK=0
      CALL EFNCRS(DX,ROWJ,COLI,STK,WRONG)
      TETIFL=COLI
      PHIIFL=ROWJ
      IF(JDBDRF(JULEC).GT.8)
     &WRITE(LDEBRL,100)COLI,ROWJ,STK,WRONG
 100  FORMAT(1X,/,
     &'E4XYZ0 : COLI ROWJ STK',3(1X,F8.3),4X,A13)
C
CXX    SI STK.GT.1.01  ON BOUCLE EN REDEFINISSANT RP1 ZP1
C
      IF(STK.EQ.0)THEN
       RP1=RBARL1
       ZP1=ZEDCP0
       IF(IBOUC.GE.5)THEN
          IERR = 2
          WRITE(LDEBRL,*)'E4XYZ0 >...IBOUC'
          GO TO 999
       ENDIF
       GOTO 1
       ENDIF
      IF(STK.GT.1.01.OR.STK.LT.0.99)THEN
        IF(IBOUC.GE.5)THEN
          IERR = 3
          WRITE(LDEBRL,*)'E4XYZ0 >...IBOUC'
          GO TO 999
        ENDIF
        ST=1.005
        CALL ESRPT('ALEPH',COLI,ROWJ,ST,DX)
        IF(COLI.LE.COOV12.OR.COLI.GE.COOV22)THEN
           ZP1=ABS(DX(3))+EPSILO
           RP1=RBARL1
           ELSE
           RP1=SQRT(DX(1)**2+DX(2)**2)
           ENDIF
           GOTO 1
      ENDIF
      RETURN
999   CALL RERROR ( 'E4XYZ0' , 1 , ' NO EXTRAPOLATION DONE ')
      RETURN
      END
#endif
