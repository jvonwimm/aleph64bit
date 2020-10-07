*DK ehshow
      SUBROUTINE EHSHOW (ISCMP,MODUL)
C.----------------------------------------------------------------------
C   J.Badier 11/12/85    M.Rumpf  00/01/86
C! Shower creation steering
C Generate SHOWER for track element selected by EHCUTF
C Points and signals stored in temporary bank 'PSIG'
C   - avec NSIGNL points de cordonnees POSSG
C ----- TIRAGE : 'FLUCTUATED' pour tirage standard
C -----          'VERY FAST'  pour tirage rapide
C   - Called by ECHIT
C   - Calls     EHGERB,ECSGEO,EHSITW,UCOPY
C
C.----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "ehpash.h"
#include "capano.h"
#include "jobcom.h"
#include "jqcom.h"
#include "ehcond.h"
#include "ecdbug.h"
#include "ecstat.h"
#include "iocom.h"
#include "ecnamc.h"
C     Calls EHGERB,ECSGEO,EHSITW
      REAL POSSG(3)
C
      DIMENSION XGERB(5)
      CHARACTER * 5 TYGEO
      EXTERNAL RNDM
         EXTERNAL EFNLAY
         INTEGER EFNLAY
         LOGICAL INSTK
#include "tmacrod.h"
      EQUIVALENCE (ENER,TRKNXT(8))
      PARAMETER (NMIL=14)
      CHARACTER*4 TEVOL(NMIL)
C
      DATA TEVOL / 'EBS2', 'EBS1', 'EBS3',
     +             'ECS2', 'ECS1', 'ECS3', 'ECS0',
     +             'EDS2', 'EDS1', 'EDS3', 'EDS0',
     +             'EB12', 'EC12', 'ED12' /
#include "bmacro.h"
#include "tmacrof.h"
C
C -----------------------------------------------------------
C ----- Recherche du milieu
      IF( ITRKEL(8) .NE. 2 .AND. FTINO ) GO TO 20
      DO 1 MILI = 1,NMIL
         IF (TRKVOL .EQ. TEVOL(MILI)) GOTO 2
   1  CONTINUE
      MILI = 0
   2  CONTINUE
C
      ENER =TRKNXT(8)
C  Compute Number of points for this Shower
C
      IFIN = 0
      IEMG = 0
      IHAD = 0
      IF( FTINO ) THEN
         CALL CAGERB(EMGNRJ,IEMG,XGERB,NRJDP)
         CALL CAHGRB(HADNRJ,IHAD,XGERB,NRJDP)
         CALL CAPSTO ( ITRKEL(1))
         IFIN = IEMG + IHAD
         NSIGNL = 200. * (TOTNRJ+1.)
      ELSE
         CALL EHGERB ( ENER,IFIN,XGERB,NRJDP )
C        IFIN est le nombre de points a tirer.
         NSIGNL = IFIN
      ENDIF
      IF( IFIN .LE.  0) GO TO 20
C
C  Define Readout Geometry in Shower system  'GERBE'
C
      CALL ECSGEO(TRKNXT(1),TRKNXT(4),ISCMP,MODUL)
C
      TYGEO = 'GERBE'
      NECONT(3) = NECONT(3) + 1
C
C    Book working bank PSIG if does not exist or extend it if too small
C
      LEN = LPS1*NSIGNL+LMHLEN
      IF (IDPSIG.LE.0) THEN
         CALL WBANK (IW,IDPSIG,LEN,*20)
         IW(IDPSIG-3) = INTCHA ('PSIG')
         IW(IDPSIG+LMHCOL) = LPS1
      ELSE
         IF (LEN .GT. IW(IDPSIG)) THEN
            CALL WBANK (IW,IDPSIG,LEN,*20)
         ENDIF
      ENDIF
      IW(IDPSIG+LMHROW) = 0
C
C  Fill temporary bank 'PSIG'
C
      NECSG = 0
      IPX = KNEXT(IDPSIG)
   10 CONTINUE
      IF( FTINO ) THEN
         IF( IEMG .NE.  0) THEN
            CALL CAGERB(EMGNRJ,IEMG,POSSG,NRJDP)
         ELSE
            CALL CAHGRB(HADNRJ,IHAD,POSSG,NRJDP)
         ENDIF
         IFIN = IEMG + IHAD
         IF( NSIGNL .LE.  0) GO TO 19
      ELSE
         CALL EHGERB(ENER,IFIN,POSSG,NRJDP)
         IFIN = IFIN - 1
      ENDIF
      IF (NRJDP.GT.0) THEN
      IPLAN = EFNLAY( ISCMP , MODUL , POSSG , TYGEO , INSTK )
      IF( FTINO ) THEN
         IF(  IPLAN .LT. IPLMIN(MILI)
     +     .OR. IPLAN .GT. IPLMAX(MILI)
     +     .OR. .NOT. INSTK ) THEN
             NRJDP = 0
      ENDIF
         ENDIF
      ENDIF
      IF(NRJDP.GT.0. AND. IPLAN.GT.0) THEN
         NECSG = NECSG + 1
         NSIGNL = NSIGNL - 1
         RW(IPX+1) = POSSG(1)
         RW(IPX+2) = POSSG(2)
         RW(IPX+3) = POSSG(3)
         IW(IPX+4) = NRJDP
         IW(IPX+5) = NRJDP
         IW(IPX+6) = IPLAN
         IPX = IPX + LCOLS(IDPSIG)
      ENDIF
C
      IF(IFIN .GT. 0) GO TO 10
C
 19   CONTINUE
      IW(IDPSIG+2) = NECSG
C
C  Allocate Signals to Towers ans Wires
C
      CALL EHSITW ( ISCMP,MODUL,TYGEO)
C
   20 CONTINUE
      RETURN
      END
#endif
