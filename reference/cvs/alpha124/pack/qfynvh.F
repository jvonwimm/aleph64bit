      SUBROUTINE QFYNVH(IVRTX,INTRA,NOUTR,LISOU)
CKEY FILL VERTEX   /INTERNAL
C----------------------------------------------------------------------
C!  - Fill list of tracks from Nuclear Interaction vertices from new
C!  - tracking bank YNVH.
C!
C!   Author   :  J. Boucrot                    21-Apr-1997
C!
C!    Input  :
C!          IVRTX = Vertex number
C!   Outputs :
C!          INTRA = ALPHA number of incoming track, if any
C!          NOUTR = Number of outgoing tracks
C!          LISOU = List of ALPHA numbers of outgoing tracks
C?
C!======================================================================
#ifndef DOC
#include "qcde.h"
#include "ynvhjj.h"
      PARAMETER ( NOUTT = 50 )
      INTEGER LISOU(NOUTT)
#include "qmacro.h"
C----------------------------------------------------------------------
C--- Check if bank of nuclear interactions is present :
C
      INTRA=0
      NOUTR=0
      JYNVH=IW(NAMIND('YNVH'))
      IF (JYNVH.LE.0) GO TO 999
C
C Loop on tracks:
C
      DO 600 ITR=1,LROWS(JYNVH)
         KYNVH=KROW(JYNVH,ITR)
         ITRAK=IW(KYNVH+JYNVTN)
         IVERT=IW(KYNVH+JYNVVN)
C Vertex must be the input one:
         IF (IABS(IVERT).NE.IVRTX) GO TO 600
C If IVERT < 0 , the track is incoming to the vertex:
         IF (IVERT.LT.0) THEN
            INTRA=IABS(ITRAK)+KFCHT-1
            GO TO 600
         ENDIF
C If ITRAK < 0 : this is not a track, but a V0 from bank YLV0:
         IF (ITRAK.LT.0) THEN
            IVOL=IABS(ITRAK)
C Search vertex IVOL in the reconstructed vertices:
            DO 100 IV=KFREV,KLREV
               IF (KVTYPE(IV).NE.3) GO TO 100
               IF (KVN(IV).NE.IVOL) GO TO 100
               NOUTR=NOUTR+1
               IF (NOUTR.GT.NOUTT) GO TO 999
               LISOU(NOUTR)=KVINCP(IV)
               GO TO 600
 100        CONTINUE
         ENDIF
C ITRAK > 0 : this is an ordinary charged reconstructed track:
C Build the list of outgoing tracks:
         NOUTR=NOUTR+1
         IF (NOUTR.GT.NOUTT) GO TO 999
         LISOU(NOUTR)=IABS(ITRAK)+KFCHT-1
  600 CONTINUE
C
  999 RETURN
      END
#endif
