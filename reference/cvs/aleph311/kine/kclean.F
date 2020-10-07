      SUBROUTINE KCLEAN(IOK)
C -----------------------------------------------------------------
C -  B.Bloch September 92
C! Clean up data cards from Data Base overwritten by possible user's
C! cards
CKEY KINE KINGAL INIT  /   INTERNAL
C    IOK = 0  ok
C    IOK = 1  error occured
C ------------------------------------------------------------------
#include "bcs.h"
C----------------------------------------------------------------------
C  Banks input to KINGAL are of different types:
C
C   Banks with an explicit NR : MSTJ,PARJ,... for JETSET 7.3
C   -------------------------   MSTA,PARA     for ARIADNE
C                               GMAS          for HERWIG
C   the last one read in from  user input  wins!
C
C   Banks with no explicit NR : GMOB,GADM,GRPL for JETSET modifications
C   --------------------------
C   The last one is taken into account but you may copy many arrays
C   before getting through.Better drop those which are overwritten
C
C   Banks with no explicit NR : GHVF,GDYM,GMIX....  for HVFL02
C   --------------------------  GARI                for Ariadne
C                               GHRW,GGSW,GBDE,GPAR for Herwig
C                               GKOR,GBEA,GTAU,GKBR for Koralz
C                               SVRT,XVRT           for general use
C   There should be only one bank with a given name , better drop the
C   first one if another has been provided by the user .Rename it to
C   NR = 0 , in case the generator makes an explicit link to it instead
C   of using NAMIND.
C----------------------------------------------------------------------
#ifndef DOC
      CHARACTER*4 BLAST,BSING
      PARAMETER ( NSING = 21 , NLAST = 3 )
      DIMENSION BLAST(NLAST),BSING(NSING)
      DIMENSION NRDRP(10)
      DATA BLAST /'GADM','GRPL','GMOB'/
      DATA BSING /'GHVF','GDYM','GMIX','GCPV','GSTA','GVBU','GSEM',
     $            'GPHO','GDSS','GBUS','SVRT','GARI','GHRW','GGSW',
     $            'GBDE','GPAR','GKOR','GBEA','GTAU','GKBR','XVRT'/
      IOK = 0
C   Single copy banks
      DO 10  I = 1,NSING
         NAMI = NAMIND(BSING(I))
C   Bank exists?
         IF ( IW(NAMI).LE.0 ) GO TO 10
         ND = 0
         IND = NAMI+1
C   Loop on banks with same name
    8    IND = IW(IND-1)
C   Count and record extra copies
         IF ( IND.NE.0 ) THEN
            ND = ND + 1
            NRDRP(ND) = IW(IND-2)
            GO TO 8
         ENDIF
         IF ( ND.LE.1) GO TO 10
C  Drop undesired banks
         DO 9 J = 1 ,ND-1
             WRITE ( IW(6) ,100) BSING(I),NRDRP(J)
             JIND = NDROP(BSING(I),NRDRP(J))
    9    CONTINUE
   10 CONTINUE
      DO 11  I = 1,NSING
         NAMI = NAMIND(BSING(I))
         IND = IW(NAMI)
         IF ( IND.GT.0) THEN
            NR = IW(IND-2)
            IF ( NR.NE.0) THEN
              WRITE( IW(6),300) BSING(I),NR
              IW(IND-2) = 0
            ENDIF
         ENDIF
   11 CONTINUE
C   Multi- copy banks to be reduced to single copy
      DO 20  I = 1,NLAST
         NAMI = NAMIND(BLAST(I))
C   Bank exists?
         IF ( IW(NAMI).LE.0 ) GO TO 20
         ND = 0
         IND = NAMI+1
C   Loop on banks with same name  and KF code
   18    IND = IW(IND-1)
C   Count and record extra copies
         IF ( IND.NE.0 ) THEN
            KF =  IW(IND+1)
            CALL KKDROP( BLAST(I),KF)
            GO TO 18
         ENDIF
   20 CONTINUE
      CALL KKMOVE
  999 WRITE( IW(6),200)
      RETURN
  100 FORMAT ( 1X,'++++KCLEAN : DROPPING  Bank ',A10,'  NR = ',I5,
     $       '  From KREF ...superseded by user INPUT')
  300 FORMAT ( 1X,'++++KCLEAN : RENAMING  Bank ',A10,'  NR = ',I5,
     $       '  To NR = 0    ')
 200  FORMAT(1X,/,'============= End of section  to handle user',
     $            ' input superseding KREF defaults ==============')
      END
#endif
