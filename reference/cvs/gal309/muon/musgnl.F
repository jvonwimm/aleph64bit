*DK musgnl
      SUBROUTINE MUSGNL
C
C****************************************************************************
C
C T.Wang -860108
C
C! induce signals on strips by streamers.
C
C       Called by MUHIT
C       Calls ALBOS,MUFLHT                    in this .HLB
C             BLIST                           in BOS
C
C****************************************************************************
C
#ifndef DOC
      SAVE
#include "munamc.h"
#include "mustjj.h"
#include "muhtjj.h"
#include "mugncn.h"
#include "musgcn.h"
#include "bcs.h"
#include "jobcom.h"
#include "iocom.h"
#include "bmacro.h"
C
C       Check if there is any streamer in JDMUST
C
      IF (JDMUST .EQ. 0) RETURN
      IF (LROWS(JDMUST) .EQ. 0) RETURN
      NSTRM = LROWS (JDMUST)
C
C       Fill histograms of # of segments and # of streamers
C
      IF( FHISJO(8) )THEN
        X = NSTRM
        CALL HFILL(801,X)
        MSTRM = 0
        KMUST = JDMUST + LMHLEN
        DO  5 ISTRM=1,NSTRM
        JFIRE = IW(KMUST + JMUSFF)
        IF( JFIRE .EQ. 0 )GOTO 5
        MSTRM = MSTRM + 1
    5   KMUST = KMUST + LMUSTA
        X = MSTRM
        CALL HFILL(802,X)
      ENDIF
C
C       Create or link to BOS bank 'MUHT'
C
      JMUHT = IW(NAMUHT)
      IF( JMUHT .EQ. 0 )THEN
         ND = LMHLEN + NHTIN*LMUHTA
         CALL ALBOS ('MUHT',0,ND,JMUHT,IGARB)
         CALL BLIST(IW,'E+','MUHT')
         IW(JMUHT + LMHCOL) = LMUHTA
         IW(JMUHT + LMHROW) = 0
      ENDIF
C
C       Do loop for existing stremers in JDMUST
C
      DO 50 ISTRM=1,NSTRM
         KMUST = KROW(JDMUST,ISTRM)
         JFIRE = IW(KMUST + JMUSFF)
         IF( JFIRE .EQ.  0)GOTO 50
         JPLAN = IW(KMUST + JMUSPL)
         JEIGH = IW(KMUST + JMUSET)
         JTUBE = IW(KMUST + JMUSTU)
         Y1 = RW(KMUST + JMUSYI)
         Y2 = RW(KMUST + JMUSYO)
C
C       Deal with X-strip
C
C       X-strips are aligned with eight-fold tubes
C
         NX = (JEIGH-1)*8 + JTUBE
         R = RNDM(0.)
         IF( R .GT. SNSXMU(1) )GOTO 20
         CALL MUFLHT (JPLAN,1,NX)
C
C       Deal with adjacent strips
C
         IF( JTUBE .EQ. 1 )GOTO 10
         R = RNDM(0.)
         IF( R .GT. SNSXMU(2) )GOTO 10
         NS = NX - 1
         CALL MUFLHT (JPLAN,1,NS)
         IF( JTUBE .EQ. 2 )GOTO 10
         R = RNDM(0.)
         IF( R .GT. SNSXMU(3) )GOTO 10
         NS = NX - 2
         CALL MUFLHT (JPLAN,1,NS)
   10    IF( JTUBE .EQ. 8 )GOTO 20
         R = RNDM(0.)
         IF( R .GT. SNSXMU(2) )GOTO 20
         NS = NX + 1
         CALL MUFLHT (JPLAN,1,NS)
         IF( JTUBE .EQ. 7 )GOTO 20
         R = RNDM(0.)
         IF( R .GT. SNSXMU(3) )GOTO 20
         NS = NX + 2
         CALL MUFLHT (JPLAN,1,NS)
   20    CONTINUE
C
C       Deal with Y-strip
C
         NY1 = Y1/PTYSMU + 1
         IF (NY1 .GT. NYSTMU) GOTO 50
         NY2 = Y2/PTYSMU + 1
         DO 40 NS=NY1,NY2
            R = RNDM(0.)
            IF (R .GT. SNSYMU(1)) GOTO 40
            CALL MUFLHT (JPLAN,2,NS)
C
            IF( NS .NE. NY1 )GOTO 30
            R = RNDM(0.)
            IF( R .GT. SNSYMU(2) )GOTO 30
            N = NS - 1
            CALL MUFLHT (JPLAN,2,N)
            R = RNDM(0.)
            IF( R .GT. SNSYMU(3) )GOTO 30
            N = NS - 2
            CALL MUFLHT (JPLAN,2,N)
   30       IF( NS .NE. NY2 )GOTO 40
            R = RNDM(0.)
            IF( R .GT. SNSYMU(2) )GOTO 40
            N = NS + 1
            CALL MUFLHT (JPLAN,2,N)
            R = RNDM(0.)
            IF( R .GT. SNSYMU(3) )GOTO 40
            N = NS + 2
            CALL MUFLHT (JPLAN,2,N)
   40    CONTINUE
   50 CONTINUE
C
      END
#endif
