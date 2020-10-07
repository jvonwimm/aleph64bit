      SUBROUTINE KKMOVE
C -----------------------------------------------------------------
C -  B.Bloch September 92
C! Move banks from temporary to memory if needed
C    Modified December 97 : extend NMX from 100 to 200
CKEY KINE KINGAL INIT  /   INTERNAL
C
C
C ------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      CHARACTER*4 BSWP,BLAST
      PARAMETER ( NLAST = 3 , NMX = 200 )
      DIMENSION BLAST(NLAST),BSWP(NLAST)
      DIMENSION NMOVE(NMX)
      DATA BLAST /'GADM','GRPL','GMOB'/
      DATA BSWP  /'KADM','KRPL','KMOB'/
C----------------------------------------------------------------------
      DO 20  I = 1,NLAST
         NAMI = NAMIND(BLAST(I))
C   Bank exists?
         NL = -1
         IF ( IW(NAMI).LE.0 ) GO TO 21
         IND = NAMI+1
C   Loop on banks with same name
   18    IND = IW(IND-1)
C   Count and record extra copies
         IF ( IND.NE.0 ) THEN
            NL = IW(IND-2)
            GO TO 18
         ENDIF
C  move temporary banks to memory
  21     NAMJ = NAMIND(BSWP(I))
C   Bank exists?
         IF ( IW(NAMJ).LE.0 ) GO TO 20
         ND = 0
         JND = NAMJ+1
C   Loop on banks with same name
   19    JND = IW(JND-1)
C   Count and record extra copies
         IF ( JND.NE.0 ) THEN
            NJ = IW(JND-2)
            ND = ND+1
            IF ( ND.GT.NMX ) GO TO 998
            NMOVE(ND) = NJ
            GO TO 19
         ENDIF
         DO 22 L = 1,ND
            NJ = NMOVE(L)
            KIND = NSWAP(BSWP(I),NJ,BLAST(I),NL+L)
            WRITE ( IW(6) ,100) BSWP(I),NJ,BLAST(I),NL+L
   22    CONTINUE
         CALL BDROP(IW,BSWP(I))
   20 CONTINUE
      GO TO 999
  998 CONTINUE
      WRITE (IW(6) ,'(1x,''(+++KKMOVE Too many Banks Increase NMX )'')')
      CALL EXIT
  999 RETURN
  100 FORMAT ( 1X,'++++KKMOVE : Copying  Bank ',A10,'  NR = ',I5,
     $ 'to Bank ',A10,'  NR = ',I5)
      END
#endif
