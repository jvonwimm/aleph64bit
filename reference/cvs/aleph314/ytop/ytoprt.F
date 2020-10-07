      SUBROUTINE YTOPRT(IPRL)
C
C----------------------------------------------------------*
C!    Printout of reconstructed vertices
C!    To be called once per event after YTOPOL
CKEY YTOP PRINT
C!    Author :     M. Bosman   jan 30 1989
C!    Modified:    S. Wasserbaech 9 jan 1990
C!
C!
C!    Description
C!    ===========
C!    This routine prints the reconstructed primary vertex
C!    and lists the tracks that are used in the vertex
C!    IPRLV = 1 : prints only the position of the vertex
C!            2 : lists the tracks used in the vertex
C!                and the CHI2
C!
#ifndef DOC
      SAVE
#include "bcs.h"
#include "pyerjj.h"
#include "pyfrjj.h"
C!---------------------------------------------------------*
      EXTERNAL NAMIND
#include "bmacro.h"
C!---------------------------------------------------------*
      LOUT = IW(6)
      KPYER = IW(NAMIND('PYER'))
      IF(KPYER.NE.0) THEN
        NTR = (ITABL(KPYER,1,JPYEDF)+3)/2
        IF (ITABL(KPYER,1,JPYETY) .EQ. 3) NTR = NTR - 1
        WRITE(LOUT,1000)
     +    (RTABL(KPYER,1,JPYEVX+I-1),I=1,3), NTR
      ENDIF
      IF(IPRL.GE.2.AND.KPYER.NE.0) THEN
        WRITE(LOUT,1001)
     +    (RTABL(KPYER,1,JPYEVM+I-1),I=1,6)
        WRITE(LOUT,1002)
     +    RTABL(KPYER,1,JPYEC2),ITABL(KPYER,1,JPYEDF)
        WRITE (LOUT,1003) (ITABL(KPYER,1,JPYETY) .EQ. 3)
        KPYFR = IW(NAMIND('PYFR'))
        IF(KPYFR.NE.0) THEN
          WRITE(LOUT,1004)
     +      (ITABL(KPYFR,I,JPYFTN),I=1,LROWS(KPYFR))
          WRITE(LOUT,1005)
        ENDIF
      ENDIF
      RETURN
 1000 FORMAT(/,' ------YTOPOL : vertex reconstruction ------'/,
     +       ' Primary Vertex (X,Y,Z) :',3F10.5,' from ',I2,' tracks',
     +       /,1X,43('-'))
 1001 FORMAT(' Variances in sequence X:XY:Y:XZ:YZ:Z '/,6E10.3)
 1002 FORMAT(' Chi2 ',F10.3,'  for ',I2,' degrees of freedom')
 1003 FORMAT(' Beam xy position used: ',L1)
 1004 FORMAT(' List of tracks that form the primary vertex '/,5(20I3/))
 1005 FORMAT(1X,43('-'))
      END
#endif
