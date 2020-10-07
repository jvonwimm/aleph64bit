*DK mufldt
      SUBROUTINE MUFLDT (NDIGI,NHTCL,IADHT)
C
C*******************************************************************************
C
C T.Wang -860117
C
C       Routine to fill a digit-track reference of one digit
C       to BOS bank 'MUDT'
C
C       Input:
C         NDIGI -- the current # of digits
C         NHTCL -- the # of hits that contribute to the current digit(cluster)
C         IADHT -- array to contain the addresses of NHTCL hits in 'MUHT'
C
C       Output:
C         in BOS bank 'MUDT'
C         RETURN 1 if not enough space to enlarge MUDT-3
C
C       Called by MUDGTZ
C       Calls ALBOS            in this .HLB
C
C*******************************************************************************
C
#ifndef DOC
      SAVE
#include "mudgpr.h"
#include "muhtjj.h"
#include "munamc.h"
#include "bcs.h"
#include "jobcom.h"
#include "iocom.h"
      INTEGER IADHT(*),IDTRK(MXDHT)
#include "bmacro.h"
C
C       Get index for banks needed
C
      JMUHT = IW(NAMUHT)
      JMDT1 = IW(NAMUDT)
      JMDT2 = IW(JMDT1-1)
      JMDT3 = IW(JMDT2-1)
C
      KMDT1 = JMDT1 + LMHLEN
      KMDT2 = JMDT2 + LMHLEN
C
      NTRCK = 0
      DO 20 IHTCL=1,NHTCL
         KMUHT = KROW ( JMUHT,IADHT(IHTCL))
         JTRCK = IW(KMUHT + JMUHTN)
         IF(NTRCK .EQ. 0)THEN
            IDTRK(1) = JTRCK
            NTRCK = 1
         ENDIF
         DO 10 IT=1,NTRCK
            IF(JTRCK .EQ. IDTRK(IT))GOTO 20
   10    CONTINUE
         NTRCK = NTRCK + 1
         IF (NTRCK .GT. MXDHT) GOTO 98
         IDTRK(NTRCK)= JTRCK
   20 CONTINUE
      IW(KMDT1 + NDIGI) = NTRCK
      IF (NDIGI .GT. 1) IW(KMDT2+NDIGI) = IW(KMDT2+NDIGI-1)
     &                                   + IW(KMDT1+NDIGI-1)
C
      IW(KMDT1) = IW(KMDT1) + 1
      IW(KMDT2) = IW(KMDT2) + 1
C
C       Fill 'MUDT'-3
C
      IF (LFRWRD(JMDT3).LT. NTRCK) THEN
         CALL ALBOS ('MUDT',3,IW(JMDT3)+NAVTD*NTRCK,JMDT3,IGARB)
      ENDIF
      KMDT3 = JMDT3 + LMHLEN
      DO 30 IT=1,NTRCK
         IW(KMDT3 + IW(KMDT2+NDIGI) + IT) = IDTRK(IT)
   30 CONTINUE
      IW(KMDT3) = IW(KMDT3) + NTRCK
C
      RETURN
C
   98 CALL ALTELL ('MUFLDT: too many tracks for local array',2,'RETURN')
C
      END
#endif
