      SUBROUTINE KKDROP( NAME , KF)
C -----------------------------------------------------------------
C -  B.Bloch September 92
C! Drops banks from memory if they refer to same KF
CKEY KINE KINGAL INIT  /   INTERNAL
C
C
C ------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      CHARACTER*4 NAME
      CHARACTER*4 BLAST
      PARAMETER ( NLAST = 3 )
      DIMENSION BLAST(NLAST)
      DIMENSION NRDRP(100)
      DATA BLAST /'KMOB','KRPL','KADM'/
C----------------------------------------------------------------------
      IF ( NAME.EQ.'GADM' .OR. NAME .EQ.'GRPL' ) NN= NLAST
      IF ( NAME.EQ.'GMOB' ) NN = 1
      DO 20  I = 1,NN
         NAMI = NAMIND(BLAST(I))
C   Bank exists?
         IF ( IW(NAMI).LE.0 ) GO TO 20
         ND = 0
         IND = NAMI+1
C   Loop on banks with same name  and KF code
   18    IND = IW(IND-1)
C   Count and record extra copies
         IF ( IND.NE.0 ) THEN
            KFI=  IW(IND+1)
            IF ( KFI.NE.KF) GO TO 18
            ND = ND + 1
            NRDRP(ND) = IW(IND-2)
            GO TO 18
         ENDIF
         IF ( ND.LE.0) GO TO 20
C  Drop undesired banks
         DO 19 J = 1 ,ND
             WRITE ( IW(6) ,100) BLAST(I),NRDRP(J),KF
             JIND = NDROP(BLAST(I),NRDRP(J))
   19    CONTINUE
   20 CONTINUE
  999 RETURN
  100 FORMAT ( 1X,'++++KKDROP : DROPPING  Bank ',A10,'  NR = ',I5,
     $ '  KF=',I6,'  From KREF ...superseded by user INPUT')
      END
#endif
