      SUBROUTINE SIPRGO
C.---------------------------------------------------------------------
CKEY SCALDES PRINT GEOMETRY / USER
C     B.BLOCH       October 91
C! Geometry printing routine  SICAL
C   Input : none
C   Output: none
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "sigeco.h"
      WRITE (IW(6),'(8X,''+++ SICAL Geometry package running +++'')')
      WRITE(IW(6),1000) NMODSI,NRBNSI,NPBNSI,NZBNSI,RMINSI,RMAXSI,
     $    Z0SNSI,ZWIDSI,ZWFLSI,ZWFRSI,ZWLASI,ZWBKSI,OVLPSI,
     $    ZWRFSI,ZWRLSI,PHSHFT,RADSTP,PHISTP,ISINUM,GAPXSI,DPOSSI
 1000 FORMAT (' SICAL  geometry constants : ',//,
     $   ' Number of modules                :',I5,/,
     $   ' Number of radial bins per module :',I5,/,
     $   ' Number of Phi bins per module    :',I5,/,
     $   ' Number of z layers per module    :',I5,/,
     $   ' Minimum radius of sensitive area (A and B) :',2F11.4,/,
     $   ' Maximum radius of sensitive area (A and B) :',2F11.4,/,
     $   ' Z position of first sensitive layer (A and B) :',2F11.4,/,
     $   ' Z width of a sensitive layer     :',F11.4,/,
     $   ' Z width in front of first sen lay:',F11.4,/,
     $   ' Z width in front of first module :',F11.4,/,
     $   ' Z width of last module ( #12)    :',F11.4,/,
     $   ' Z width of back plate            :',F11.4,/,
     $   ' Overlap between material of X-tal:',F11.4,/,
     $   ' Number of radiation length before first layer :',F11.3,/,
     $   ' Number of radiation length per layer          :',F11.3,/,
     $   ' PHI shifts of subsequent z planes in a triplet (A):',3F10.3,/
     $   ' PHI shifts of subsequent z planes in a triplet (B):',3F10.3,/
     $   ' Radial width of a pad            :',F11.4,/,
     $   ' Phi width of a pad               :',F11.4,/,
     $   ' Plate numbers in pile-up   sideA :',12I4,/,
     $   ' Plate numbers in pile-up   sideB :',12I4,/,
     $   ' Half X gap between two half modules (A and B) :',2F11.4,/,
     $   ' Alignment correction x y z sideA :',3F11.4,/,
     $   ' Alignment correction x y z sideB :',3F11.4)
      RETURN
      END
#endif
