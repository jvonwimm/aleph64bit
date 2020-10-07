*DK hrdgal
      SUBROUTINE HRDGAL
C------------------------------------------------------------------
C! Get HCAL bank used only in Galeph and store the content in the
C                     HCCONG common
C- F.Ranjard - 880202
C- Modified by L.Silvestris     31/08/90
C              F.Ranjard        11/10/91
C              get HCOS and HGEA depending on the GEANT version number
C              use MDARD to get banks from data base
C- Called by : HCIRUN
C!          Calls     : ALTELL from Alephlib
C!                      UCOPY,HISPRE from Cernlib
C!
C -------------------------------------------------
#ifndef DOC
#include "gcbank.h"
#include "hjjpar.h"
#include "jobcom.h"
#include "iocom.h"
#include "bcs.h"
#include "hccong.h"
#include "bmacro.h"
C ----------------------------------------------------------------------
C - get GEANT version number
       IGEANT = INT (GVERSN*100.)
C - get machine row number : CRAY=2, .NOT.CRAY=1
       MAC = 1
C
       JHCOS = MDARD (IW,LRDBIO,'HCOS',IGEANT)
       IF (JHCOS.EQ.0) THEN
C      try to get HCOS,NR=1 (GEANT 313 version)
          JHCOS = MDARD (IW,LRDBIO,'HCOS',1)
       ENDIF
       JHGEA = MDARD (IW,LRDBIO,'HGEA',IGEANT)
       IF (JHGEA.NE.0) THEN
          IF (MAC.GT.LROWS(JHGEA)) MAC = LROWS(JHGEA)
C      in case the row corresponding to the machine is not the 1st one
C      copy the row # MAC into the 1st one
          IF (MAC.GT.1) THEN
             KHGEA = KROW(JHGEA,MAC)
             CALL UCOPY (RW(KHGEA+1),RW(JHGEA+LMHLEN+1),IW(JHGEA+1))
          ENDIF
       ENDIF
C
       JHTRE = MDARD (IW,LRDBIO,'HTRE',1)
C
       IF (JHCOS.NE.0) THEN
          IF (MAC.GT.LROWS(JHCOS)) MAC = LROWS(JHCOS)
          HCTUAC   = RTABL(JHCOS,MAC,JHCOTA)
          HSTREA   = RTABL(JHCOS,MAC,JHCOTS)
          HCSTDT   = RTABL(JHCOS,MAC,JHCOOL)
          HTLEMX   = RTABL(JHCOS,MAC,JHCOMT)
          HCTEFF(1)= RTABL(JHCOS,MAC,JHCOEA)
          HCTEFF(2)= RTABL(JHCOS,MAC,JHCOEA+1)
          HCTEFF(3)= RTABL(JHCOS,MAC,JHCOEA+2)
          HCADCE   = RTABL(JHCOS,MAC,JHCOCF)
          HPINDU   = RTABL(JHCOS,MAC,JHCOIF)
          HADCMX   = RTABL(JHCOS,MAC,JHCOMA)
      ELSE
         GOTO 998
      ENDIF
C
      IF(JHTRE.NE.0) THEN
         NHCFSS = ITABL(JHTRE,1,JHTRNB)
         HCFSS1 = RTABL(JHTRE,1,JHTRLE)
         HCFSS2 = RTABL(JHTRE,1,JHTRUE)
C
         CALL UCOPY(RW(JHTRE+LMHLEN+JHTRSR),HCFLSS(1),NHCFSS)
         CALL HISPRE(HCFLSS,NHCFSS)
C
      ELSE
         GOTO 998
      ENDIF
C
C  Get geometry D.B. variables used in Galeph
C
      CALL HGETDB
      RETURN
998   CONTINUE
      CALL ALTELL('HRDGAL: missing DB banks ', 0, 'STOP')
      END
#endif
