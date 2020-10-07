*DK ehchar
      SUBROUTINE EHCHAR(PDEP,TRLEN,IPLAN,NGTR)
C.----------------------------------------------------------------------
C  M.Rumpf      Jan 86
C! Track elem. energy
C  Input : PDEP  (NGTR)     Energy deposition point
C          TRLEN (NGTR)     Track segment length
C          IPLAN (NGTR)     Wire plane number
C          NGTR             number of points
C  Store results in temporary bank PSIG
C  UNIT for signal is 1 Kev integer
C  - Called by EHTRKE
C  - Calls     EHDEPT,WBANK
C.----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "ecstat.h"
#include "tmacrod.h"
#include "jqcom.h"
#include "jobcom.h"
#include "ehcond.h"
#include "ecnamc.h"
      REAL PDEP(3,NGTR), TRLEN(NGTR)
      INTEGER IPLAN(NGTR)
       DIMENSION MIL(4)
       DATA MIL/4HEB12,4HEBS3,4HEC12,4HECS3/
       DATA NEVSKI/0/
C
#include "bmacro.h"
C --------------------------------------------------------------
C
C    Book working bank PSIG if does not exist or extend it if too small
C
      LEN = LPS1*NGTR+LMHLEN
      IF (IDPSIG.EQ.0) THEN
         CALL WBANK (IW,IDPSIG,LEN,*999)
         IW(IDPSIG-3) = INTCHA ('PSIG')
         IW(IDPSIG+1) = LPS1
      ELSE
         IF (LEN .GT. IW(IDPSIG)) THEN
            CALL WBANK (IW,IDPSIG,LEN,*999)
         ENDIF
      ENDIF
      IW(IDPSIG+2) = 0
C
C    if FHISJO(4)=.TRUE. and if new event, then initialisation
C    of ECCONT(I) I=21,30, used for filling histograms
C
       IF(FHISJO(4)) THEN
           IDSKI=439
           SKIZ=0.
           WSKI=1
           IF(NEVTJO.NE.NEVSKI) THEN
               DO 57 J=21,30,1
                   ECCONT(J)=0.
57             CONTINUE
           ENDIF
       ENDIF
C
C
C    Loop over all points
C
      NECSG = LROWS (IDPSIG)
      IPX   = KNEXT (IDPSIG)
C
      DO 1 I=1,NGTR
C
      CALL EHDEPT (TRLEN(I),IECDE)
C
      IF (TSMEAR.EQ.'SMEARING')             THEN
C           Do something
      ENDIF
C
C   Store point in bank PSIG
C
      IF(IECDE.EQ.0)                        GOTO 1
          NECSG = NECSG + 1
C
C   Add a new entry
C
      RW(IPX+1) = PDEP(1,I)
      RW(IPX+2) = PDEP(2,I)
      RW(IPX+3) = PDEP(3,I)
          IW(IPX + 4) = IECDE
          IW(IPX + 5) = IECDE
          IW(IPX + 6) = IPLAN(I)
C
C    if FHISJO(4)=.TRUE.,then get data for filling
C
       IF(FHISJO(4)) THEN
C
C analog signal per evt ans sensible volume
C
           NUMSKI=IUCOMP(ITRKEL(5),MIL,4)
           NUMSKI=NUMSKI+20
           ECCONT(NUMSKI)=ECCONT(NUMSKI)+IECDE
C
C signal of odd planes vs signal of even planes (analog)
C
           IF(MOD(IPLAN(I),2).EQ.0)  THEN
               ECCONT(26)=ECCONT(26)+IECDE
           ELSE
               ECCONT(25)=ECCONT(25)+IECDE
           ENDIF
C
C analog signal for all the wire planes
C
           ECCONT(27)=ECCONT(27)+IECDE
C
C analog signal per wire planes
C
           ZPSKI=IPLAN(I)-0.5
           ESKI=FLOAT(IECDE)/1000.
           CALL HFILL(IDSKI+8,ZPSKI,SKIZ,ESKI)
C
C number of hits per wire planes
C
           CALL HFILL(IDSKI+9,ZPSKI,SKIZ,WSKI)
C
C analog signal deposited in towers in all stacks and each stacks
C
           IF(IPLAN(I).LE.10) THEN
               ECCONT(28)=ECCONT(28)+IECDE
           ELSE IF (IPLAN(I).LE.33) THEN
               ECCONT(29)=ECCONT(29)+IECDE
           ELSE IF (IPLAN(I).LE.45) THEN
               ECCONT(30)=ECCONT(30)+IECDE
            ENDIF
           NEVSKI=NEVTJO
       ENDIF
C
C
         IPX = IPX + LCOLS(IDPSIG)
C
 1    CONTINUE
      IW(IDPSIG+2) = NECSG
C
 999  CONTINUE
      RETURN
      END
#endif
