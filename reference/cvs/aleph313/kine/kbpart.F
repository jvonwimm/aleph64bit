      INTEGER FUNCTION KBPART (IGEA,NAME,ITRTP,ZMAS,ZCHA,TLIF)
C -----------------------------------------------------------
C - J.Boucrot - B.Bloch - F.Ranjard - 870424
C
C! Complete the PART bank with new particles
CKEY KINE KINGAL FILL BANK /   INTERNAL
C  first Get the necessary name-indices and the NOtracking marker word
C        from the KRUN bank
C  then  IF it is a new particle  THEN
C           fill a new row of the PART bank
C        ELSE IF it is a particle already known by GEANT THEN
C           modify the content of the corresponding row
C        END IF
C  then  Return the particle row #
C
C - structure : INTEGER FUNCTION subprogram
C               User Entry Name: KBPART
C               External References: AUBOS/LOCTAB(ALEPHLIB), NAMIND(BOS77
C               Comdeck References: BCS, KIPARA, BMACRO
C
C - usage : IPART  = KBPART (IGEA,NAME,ITRTP,ZMAS,ZCHA,TLIF)
C - input : IGEA   = Geant#
C           NAME   = particle name (at most 12 char.)
C           ITRTP  = Geant tracking type
C           ZMAS   = particle mass
C           ZCHA   = particle charge
C           TLIF   = particle time life (if TLIF>1000. then TLIF=1.E+15)
C - output: KBPART = Aleph particle#
C                    0 means no PART bank
C                    - N means : PART is full with N particles
#ifndef DOC
      SAVE
#include "bcs.h"
#include "kipara.h"
      EXTERNAL NAMIND
      CHARACTER*(*) NAME
      PARAMETER (TLMAX=1000., TSTAB=1.E+15)
      DATA NAPAR /0/
#include "bmacro.h"
      NUPART(JBOS,NGEAN) = LOCTAB (IW(JBOS+LMHLEN+1),LCOLS(JBOS),
     &                             LROWS(JBOS),1,NGEAN)
C ---------------------------------------------------------
      KBPART = 0
C
C - The 1st time get NOtracking marker word
C
      IF (NAPAR .EQ.0 ) THEN
         NAPAR = NAMIND ('PART')
         JKRUN = IW(NAMIND ('KRUN'))
         IF (JKRUN.NE.0) THEN
            NOTRK = ITABL(JKRUN,1,2)
         ELSE
            NOTRK = 0
         ENDIF
      ENDIF
C
C - Get PART bank, return if does not exist
      JPART = IW(NAPAR)
      IF (JPART.EQ.0) GOTO 999
      LPART = IW(JPART+LMHCOL)
C
C - Normal entry
C   is it a particle already known: check if the Geant#
C   is .NE. NOTRK and already there
C
      NPART = 0
      IF (IGEA .NE. NOTRK) NPART = NUPART(JPART,IGEA)
      IF (NPART.GT.0) THEN
C     the particle is already there, update the content
         KPART = KROW (JPART,NPART)
      ELSE
C     Add the new particle at the end of the PART bank
         IF (LFRROW(JPART).LT.1) THEN
            CALL AUBOS ('PART',0,IW(JPART)+LPART*LRPART,JPART,IGARB)
            IF (JPART.EQ.0) THEN
               KBPART = -NPART
               GOTO 999
            ENDIF
         ENDIF
         NPART = LROWS(JPART)+1
         KPART = KNEXT(JPART)
         IW(JPART+LMHROW) = NPART
      ENDIF
C
C - Fill the row# NPART at the index KPART
      IW(KPART+1) = IGEA
      DO 101 L=1,3
         LL = 4*(L-1)+1
         IW(KPART+1+L) = INTCHA (NAME(LL:LL+3))
 101  CONTINUE
      IW(KPART+5) = ITRTP
      RW(KPART+6) = ZMAS
      RW(KPART+7) = ZCHA
      IF (TLIF .GT. TLMAX) TLIF = TSTAB
      RW(KPART+8) = TLIF
C
      KBPART = NPART
C
 999  CONTINUE
      END
#endif
