*DK agsi92
      SUBROUTINE AGSI92
C ----------------------------------------------------------
C - B.Bloch-Devaux                  910930
C! build SCAL geometry for crystals and ceramics
CKEY SCAL GEOM
C - called by AGSCAL
C - calls     GSROTM,GSPOS                      from GEANTLib
C - calls     ALTELL                            from ALEPHLib
C ---------------------------------------------------------
#ifndef DOC
#include "agcons.h"
#include "sigeco.h"
      DIMENSION IROT(3)
C=======================================================================
      DELTOT = ZWFRSI+(NZBNSI-1)*ZWIDSI+ZWLASI
      DSIL0= 2.*D360/NPBNSI
      IROT(1) = IAGROT-1
      IROT(2) = 0
      IROT(3) = IAGROT
      Z = 0.5*(DELTOT-ZWIDSI-2.*ZWLASI)
      X = 0.
      Y = 0.
      DO 10 I = NZBNSI-1,1,-1
C.......... modules are rotated by DELPHI,0,-DELPHI,DELPHI,0,... etc
        IR = MOD(( I-1),3)+1
        CALL GSPOS  ('SMOD',I,'SACT',X,Y,Z,IROT(IR),'ONLY')
        Z = Z - ZWIDSI
   10 CONTINUE
      OO = OVLPSI/SIN(0.5*DSIL0*DEGRA)
      ALSIL = 0.5*DSIL0
      IR = 0
      DO 20 I=1,8
         J = 2*(I-1)+1
         K = 2*I
         XSIL = - OO*COS(ALSIL*DEGRA)
         YSIL = - OO*SIN(ALSIL*DEGRA)
         CALL GSPOS('SKAP',J,'SSN1',XSIL,YSIL,0.,IR,'ONLY')
         CALL GSPOS('SCRM',J,'SSN2',0.  ,0.  ,0.,IR,'ONLY')
         IAGROT = IAGROT+1
         DSIL = DSIL0*(2*I-1)
         CALL GSROTM(IAGROT,D90,DSIL,D90,D90+DSIL,0.,0.)
         IR = IAGROT
         ALSIL = ALSIL+DSIL0
         XSIL = - OO*COS(ALSIL*DEGRA)
         YSIL = - OO*SIN(ALSIL*DEGRA)
         CALL GSPOS('SKAP',K,'SSN2',XSIL,YSIL,0.,IR,'ONLY')
         CALL GSPOS('SCRM',K,'SSN3',0.  ,0.  ,0.,IR,'ONLY')
         IF ( I.EQ.8) GO TO 20
         IAGROT = IAGROT+1
         DSIL = 2*I*DSIL0
         CALL GSROTM(IAGROT,D90,DSIL,D90,D90+DSIL,0.,0.)
         IR = IAGROT
         ALSIL = ALSIL+DSIL0
 20   CONTINUE
      RETURN
      END
#endif
