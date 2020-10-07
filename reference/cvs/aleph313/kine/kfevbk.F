      SUBROUTINE KFEVBK (VMAIN,RPARA,IPARA,MTRAK,ISTAT)
C -----------------------------------------------------------
C - J.Boucrot - B.Bloch - F.Ranjard - 870515
C! Fill event banks KINE VERT KHIS
CKEY KINE KINGAL FILL BANK /  INTERNAL
C  first KINE and VERT banks are booked and filled with parameters
C        sent as arguments (all vertices at the same position).
C  then  depending on the decay length of secondary particles , the
C        secondary vertices are displaced from the main vertex . The
C        propagation follows a straight line for neutral generating a
C        secondary vertec, and a simple helix for charged particles.
C        In case of charge particles generating a secondary vertex,
C        swim Px and Py of all secondaries up to decay vertex. Then
C        store the time of flight.
C        The magnetic field is assumed to be 15.0 Kgauss.
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KFEVBK
C              External References: KBVERT/KBKINE/KGPART/KGDECL(ALEPHLIB)
C              Comdecks referenced: BCS, ALCONS, KIPARA, BMACRO, KMACRO
C
C - Usage   : CALL KFEVBK (VMAIN,RPARA,IPARA,MTRAK,ISTAT)
C - Input   : VMAIN          = vx,vy,vz of the main vertex
C             RPARA (1-4,k)  = px,py,pz,(mass) of track(k)
C                              if RPARA(4,k)=0. then the mass is taken
C                              from the PART bank.
C             IPARA (1,k)    = vertex# of the origin of the track(k)
C                   (2,k)    = vertex# of the decay of the track(k)
C                                0 if there is no decay
C                   (3,k)    = ALEPH particle#
C             MTRAK          = # of tracks
C             ISTAT          = return code  ( 0 means OK)
C                              -1 means too many particles
C                              -2 means wrong KINE/VERT booking
#ifndef DOC
      SAVE
#include "bcs.h"
#include "alcons.h"
#include "kipara.h"
      PARAMETER (CLITS = CLGHT * 1.E+9)
      INTEGER IPARA(3,*)
      REAL RPARA(4,*),VMAIN(3)
      REAL KGDECL
      LOGICAL FDECAY,FNEUTR
      DATA NAPAR /0/
#include "bmacro.h"
#include "kmacro.h"
      FDECAY(JTR) = IPARA(2,JTR).GT.1 .AND. IPARA(2,JTR).NE.IPARA(1,JTR)
      FNEUTR(JPA) = ABS (CHARGE(JPA)) .LT. .1
C -------------------------------------------------------
      ISTAT = 0
C
C - Get 'PART' name-index at the 1st entry
      IF (NAPAR .EQ. 0) NAPAR = NAMIND ('PART')
C
C - Create main vertex bank
      IVMAI = 1
      JVERT = KBVERT (IVMAI,VMAIN,0)
C
C - Fill VERT and KINE banks
      DO 1 NT = 1,MTRAK
         JKINE = KBKINE (NT,RPARA(1,NT),IPARA(3,NT),IPARA(1,NT))
         IF (JKINE.LE.0) GOTO 998
         IF (IPARA(2,NT).GT.0) THEN
            JVERT = KBVERT (IPARA(2,NT),VMAIN,NT)
            IF (JVERT.LE.0) GOTO 998
         ENDIF
 1    CONTINUE
C
C - Propagate secondary vertices if any
C
      DO 100 NT = 1,MTRAK
         IPART = IPARA(3,NT)
         PMOD = SQRT (RPARA(1,NT)**2+RPARA(2,NT)**2+RPARA(3,NT)**2)
         TLIF = TIMLIF (IPART)
         IF (RPARA(4,NT).EQ.0.) THEN
            ZMAS = PARMAS (IPART)
         ELSE
            ZMAS = RPARA(4,NT)
         ENDIF
C
         IF (FDECAY(NT)) THEN
            DCLEN = KGDECL (PMOD,ZMAS,TLIF)
            IF (DCLEN .LE. 0.) GOTO 100
C           get the origin vertex
            IVOR = IPARA(1,NT)
            JVOR = NLINK ('VERT',IVOR)
            KVO  = KPARVK (JVOR)
C           get the decay vertex
            IVOUT = IPARA(2,NT)
            JVERT = NLINK ('VERT',IVOUT)
            KVX   = KPARVK (JVERT)
            KVTR  = KLISVK (JVERT)
C
C           straight line for neutral generating a secondary vx
            IF (FNEUTR(IPART)) THEN
               DO 102 IX = 1,3
                  RW(KVX+IX) = RW(KVO+IX) + RPARA(IX,NT)*DCLEN/PMOD
 102           CONTINUE
            ELSE
C
C          propagation according to a simple helix for charged
C
               RAD = PMOD / (CFIEL*CHARGE(IPART))
               DPSI = DCLEN / RAD
               DXDS = RPARA(1,NT) / PMOD
               DYDS = RPARA(2,NT) / PMOD
               DZDS = RPARA(3,NT) / PMOD
               CPSI = COS (DPSI)
               SPSI = SIN (DPSI)
               DX = RAD * (DXDS*SPSI + DYDS*(1.-CPSI))
               DY = RAD * (DYDS*SPSI - DXDS*(1.-CPSI))
               DZ = DCLEN * DZDS
               RW(KVX+1)  = RW(KVO+1) + DX
               RW(KVX+2)  = RW(KVO+2) + DY
               RW(KVX+3)  = RW(KVO+3) + DZ
C           swim Px and Py of all secondaries up to decay vertex
               MTVX = IW(JVERT+3)
               IF (MTVX .GT. 0) THEN
                  DO 103 N=1,MTVX
                     NS = IW (KVTR+N)
                     JKINE = NLINK ('KINE',NS)
                     IF (JKINE.EQ.0) GOTO 998
                     KTR = KPARVK (JKINE)
                     RW(KTR+1) = RPARA(1,NS)*CPSI - RPARA(2,NS)*SPSI
                     RW(KTR+2) = RPARA(1,NS)*SPSI + RPARA(2,NS)*CPSI
 103              CONTINUE
               ENDIF
            ENDIF
C           Store the time of flight
            RW(KVX+4)  = RW(KVO+4) + DCLEN/CLITS
         ENDIF
C
 100   CONTINUE
C
       GOTO 999
C
C - Error
C      unsuccessfull booking of VERT or KINE
 998   ISTAT = -2
C
C - End
 999   CONTINUE
       END
#endif
