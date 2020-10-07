      INTEGER FUNCTION KBKINE (NTR,PXYZM,IPART,NVX)
C -------------------------------------------------------------
C - Y.Kariotakis - 860200    modified by F.Ranjard - 870504
C
C!  fill KINE bank
CKEY KINE KINGAL FILL BANK /USER   INTERNAL
C   first connect track# NTR to vertex bank VERT,NR=NVX (enlarge
C         VERT,NR=NVX bank if necessary)
C         IF the track# NTR is .LE. 0 THEN fill the corresponding
C         bank KINE,NR=NTR without connecting the track to any
C         vertex
C   then  fill KINE,NR=NTR bank (use SBANK to see the content)
C         in case the energy is not given compute the energy of the
C         particle from its momemtum and its mass found in the particle
C         data bank PART.
C
C - structure : INTEGER FUNCTION subprogram
C               User Entry Names: KBKINE
C               External References: AUBOS(ALEPHLIB), NLINK (BOS77)
C               Comdecks referenced: KIPARA, BCS, BMACRO, RMACRO
C
C - usage   : JKINE  = KBKINE (NTR,PXYZM,IPART,NVX)
C - input   : PYXZM  = px,py,pz and mass of the track
C                      if PXYZM(4)=0. get the mass taken from the PART bank
C             IPART  = particle# (row# in PART bank)
C             NVX    = vertex origin (VERT bank # of the origin)
C             NTR    = track # (KINE bank # to be filled)
C - output  : KBKINE = BOS index of the KINE bank just filled
C                      0 means not enough space to book it
C                     -1       bank VERT,NR=NVX does not exist
C                     -2       not enough space to increase VERT
C                     -3       PART bank does not exist
#ifndef DOC
      SAVE
      REAL PXYZM(*)
#include "kipara.h"
#include "bcs.h"
      DATA NAPAR /0/
#include "bmacro.h"
#include "kmacro.h"
C -------------------------------------------------------------------
#include "kiparlin.h"
C
C - Check track# : if <0 store the track without connecting to
C   any vertex
      IF (NTR .LE. 0) GOTO 10
C
C - Connect track NTR to vertex NVX
C
      JVERT = NLINK ('VERT',NVX)
      IF (JVERT .LE. 0) THEN
         KBKINE = -1
         GOTO 999
      ELSE
C        check the space left
         IF (LFRVK(JVERT) .EQ. 0) THEN
            CALL AUBOS ('VERT',NVX,IW(JVERT)+LVKI,JVERT,IGARB)
            IF (JVERT.EQ.0) THEN
               KBKINE = -2
               GOTO 999
            ENDIF
         ENDIF
C        store the track NTR
         KNTR = KNEXVK(JVERT)
         IW(KNTR+1) = NTR
C        increase the # of tracks coming from this vertex
         IW(JVERT+3) = IW(JVERT+3) + 1
      ENDIF
C
 10   CONTINUE
C
C - Book KINE, NR = NTR
      CALL AUBOS ('KINE',NTR,LHKIN+LPKIN+LKVX,JKINE,IGARB)
      IF (JKINE .EQ. 0) GOTO 998
C
C - Fill the bank
      IW(JKINE+1) = LHKIN
      IW(JKINE+2) = LPKIN
      IW(JKINE+3) = 1
C
      IF (PXYZM(4) .EQ. 0.) THEN
C     get the mass from PART bank
         JPART = IW(NAPAR)
         IF (JPART.EQ.0) THEN
            KBKINE = -3
            GOTO 999
         ELSE
            PXYZM(4) = PARMAS (IPART)
         ENDIF
      ENDIF
C
      KKINE = JKINE + LHKIN
      RW(KKINE+1) = PXYZM(1)
      RW(KKINE+2) = PXYZM(2)
      RW(KKINE+3) = PXYZM(3)
      RW(KKINE+4) = PXYZM(4)
      IW(KKINE+5) = IPART
      IW(KKINE+LPKIN+1) = NVX
C
C -   end
C
 998  KBKINE = JKINE
C
 999  RETURN
      END
#endif
