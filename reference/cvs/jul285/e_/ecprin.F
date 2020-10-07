      SUBROUTINE ECPRIN
C----------------------------------------------------------------------
C!  - Fill ECPRIC common to gather ecal statistics
C!    to be printed at end of job.
C!
C!   Modified :- E. Lancon              4-JUN-1991
C!              Add Number of electron, photons per events, wire energy
C!              statistics
C!
C!  Called By : CALREC
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rparac.h"
#include "ecnamc.h"
#include "etp1jj.h"
#include "etc2jj.h"
#include "ect1jj.h"
#include "erl3jj.h"
#include "esdajj.h"
#include "eclujj.h"
#include "egidjj.h"
#include "eidtjj.h"
#include "ewhejj.h"
#include "ecpric.h"
C
#include "bmacro.h"
C? STOREYS NUMBER
      KESDA = IW(NAESDA)
      IF (KESDA .EQ. 0) GOTO 999
      NSTO   = IW (KESDA+LMHROW)
      NSTOEV = NSTO + NSTOEV
C? STOREYS ENERGY
      ESTO=0
      DO 10 I=1,NSTO
        ESTO = ESTO + RW(KESDA+LMHLEN+LESDAA*(I-1)+JESDME)
   10 CONTINUE
      ESTOEV=ESTOEV+ESTO
C? CLUSTERS NUMBER
      KECLU = IW(NAECLU)
      IF (KECLU .EQ. 0) GOTO 999
      NCLU   = IW(KECLU+LMHROW)
      NCLUEV = NCLU + NCLUEV
C? CLUSTERS ENERGY
      DO 11 I=1,NCLU
        ECLUEV = ECLUEV + RW(KECLU+LMHLEN+LECLUA*(I-1)+JECLE4)
   11 CONTINUE
C? Find total energy of charged clusters
C? Get a  cluster  and look if charged
      KETP1 = IW(NAETP1)
      IF (KETP1 .EQ.0) GOTO 999
      NTRAK = IW(KETP1+LMHROW)
      KECT1 = IW(NAECT1)
      KERL3 = IW(NAERL3)
      DO 12 I=1,NCLU
        NR   = IW(KECT1+LMHLEN+LECT1A*(I-1)+JECTER)
        IF (NR .EQ. 0) GOTO 12
        ICLU = IW(KERL3+LMHLEN+LERL3A*(NR-1)  +JERLEC)
        ECLA = RW(KECLU+LMHLEN+LECLUA*(ICLU-1)+JECLE4)
        ECLUCH = ECLUCH + ECLA
   12 CONTINUE
C
C?   Compute wire energy
C
      KEWHE = IW(NAMIND('EWHE'))
      IF ( KEWHE.GT.0 ) THEN
        NEWHE = LROWS(KEWHE)
        DO 22 IEWHE =  1,  NEWHE
          IMODU = ITABL(KEWHE,IEWHE,JEWHNU)
          EWIRE = FLOAT(ITABL(KEWHE,IEWHE,JEWHSD)) / 1000000.
          IF ( IMODU.LE.12 ) THEN
            EWEAEV = EWEAEV + EWIRE
          ELSEIF ( IMODU.GE.13 .AND. IMODU.LE.24 ) THEN
            EWBAEV = EWBAEV + EWIRE
          ELSEIF ( IMODU.GT.25 ) THEN
            EWEBEV = EWEBEV + EWIRE
          ENDIF
   22   CONTINUE
      ELSE
      ENDIF
C
C?   Now Number of gamma and electron candidates
C
      KEIDT = IW(NAMIND('EIDT'))
      IF ( KEIDT.GT.0 ) THEN
        NELEEV = NELEEV + LROWS(KEIDT)
      ENDIF
      KEGID = IW(NAMIND('EGID'))
      IF ( KEGID.GT.0 ) THEN
        NGAMEV = NGAMEV + LROWS(KEGID)
      ENDIF
C
  999 CONTINUE
      RETURN
      END
#endif
