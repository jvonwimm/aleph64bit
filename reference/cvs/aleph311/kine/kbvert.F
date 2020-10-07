      INTEGER FUNCTION KBVERT (NVX,VXYZT,NTR)
C -------------------------------------------------------------
C - Y.Kariotakis - 860200    modified by F.Ranjard - 870504
C
C! fill VERT bank
CKEY KINE KINGAL FILL BANK / USER  INTERNAL
C  Book and fill VERT,NR=NVX bank with vertex position and time of
C       flight
C  Connect vertex# NVX to track# NTR (enlarge KINE,NR=NTR bank if
C       necessary)
C  Return VERT,NR=NVX bank index
C
C - structure: INTEGER FUNCTION subprogram
C              User Entry Name: KBVERT
C              External References: AUBOS(ALEPHLIB), NLINK(BOS77)
C              Comdecks referenced: BCS, KIPARA, BMACRO, KMACRO
C
C - usage   : JVERT  = KBVERT (NVX,VXYZT,NTR)
C - input   : VYXZT  = vx,vy,vz and time of flight
C             NTR    = track origin (KINE bank # of the origin)
C             NVX    = vertex# (VERT bank # to be filled)
C - output  : KBVERT = BOS index of the VERT bank just filled
C                      0 means not enough space to book it
C                     -1       bank KINE,NR=NTR does not exist
C                     -2       not enough space to increase KINE
#ifndef DOC
      SAVE
      REAL VXYZT(*)
#include "kipara.h"
#include "bcs.h"
#include "bmacro.h"
#include "kmacro.h"
C -------------------------------------------------------------------
C
C - Connect vertex NVX to track NTR if NTR .ne. 0
C
      IF (NTR .EQ. 0) GOTO 100
C
      JKINE = NLINK ('KINE',NTR)
      IF (JKINE .LE. 0) THEN
         KBVERT = -1
         GOTO 999
      ELSE
C        check the space left
         IF (LFRVK(JKINE) .EQ. 0) THEN
            CALL AUBOS ('KINE',NTR,IW(JKINE)+LKVX,JKINE,IGARB)
            IF (JKINE.EQ.0) THEN
               KBVERT = -2
               GOTO 999
            ENDIF
         ENDIF
C        store the vertex NVX
         KNVX = KNEXVK(JKINE)
         IW(KNVX+1) = NVX
C        increase the # of vertices on this track
         IW(JKINE+3) = IW(JKINE+3) + 1
      ENDIF
C
 100  CONTINUE
C
C - Book VERT, NR = NVX , RETURN if the bank exists already
      JVERT = NLINK ('VERT',NVX)
      IF (JVERT .GT. 0) GOTO 998
      CALL AUBOS ('VERT',NVX,LHVER+LPVER+LVKI,JVERT,IGARB)
      IF (JVERT .EQ. 0) GOTO 998
C
C - Fill the bank
      IW(JVERT+1) = LHVER
      IW(JVERT+2) = LPVER
      IW(JVERT+3) = 0
C
      KVERT = JVERT + LHVER
      RW(KVERT+1) = VXYZT(1)
      RW(KVERT+2) = VXYZT(2)
      RW(KVERT+3) = VXYZT(3)
      RW(KVERT+4) = VXYZT(4)
      IW(KVERT+5) = NTR
C
C -   end
C
 998  KBVERT = JVERT
C
 999  RETURN
      END
#endif
