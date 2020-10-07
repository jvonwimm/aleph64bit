*DK asigea
      SUBROUTINE ASIGEA
C ----------------------------------------------------------------------
C. - F.Ranjard - 850306
C! interface GALEPH_GEANT3
C. - Fill GEANT3 common blocks
C. - called by    ASIRUN                               from this .HLB
C. - calls        GINIT , GZINIT                       from GEANT3.PAM
C.                GDINIT                               from GEANG3.PAM
C.                ASGPAR                               from this .HLB
C ----------------------------------------------------------------------
#ifndef DOC
#include "gctrak.h"
#include "gcbank.h"
#include "gccuts.h"
#include "gcflag.h"
#include "gcphys.h"
#include "alfgeo.h"
#include "iocom.h"
#include "jobcom.h"
#include "bcs.h"
C ----------------------------------------------------------------------
C
C - Fill GEANT3 common blocks with data read in ASREDC
C
      IGPAIR = 1
      IGCOMP = 1
      IGPHOT = 1
      IGBREM = 1
      IGHADR = 1
      IGANNI = 1
      IGDRAY = 1
      IGLOSS = 1
      IGDCAY = 1
      IGMULS = 1
      IGMUNU = 0
      IGPFIS = 0
      IGRAYL = 0
C - set AUTO flag for GEANT version above 3.140
      IF (GVERSN .GE. 3.14) THEN
         IGAUTO = 0
         IF (IW(NAMIND('AUTO')).GT.0) IGAUTO = 1
         WRITE (LOUTIO,*) ' ****ASIGEA*** IGAUTO = ', IGAUTO
C - set CUTHAD = cut on charged hadrons to 6 MeV when running with
C   GEANT 315 and no TCUT card
         IF (IW(NAMIND('TCUT')).EQ.0) TCUTJO(3) = 0.006
      ENDIF
      NGEVEN = 100000
      IGDRUN = IRUNJO
C
      IF(TCUTJO(1).GT.0.) GCUTGA=TCUTJO(1)
      IF(TCUTJO(2).GT.0.) GCUTEL=TCUTJO(2)
      IF(TCUTJO(3).GT.0.) GCUTHA=TCUTJO(3)
      IF(TCUTJO(4).GT.0.) GCUTNE=TCUTJO(4)
      IF(TCUTJO(5).GT.0.) GCUTMU=TCUTJO(5)
C
C - Initialize ZEBRA structure
C
      CALL GZINIT
C
C - Fill 'PTYP' GEANT banks
C
      CALL ASGPAR
C
#if defined(DRAW)
C - Initialize drawing package
C
      IF(LGRAIO.GT.0) CALL GDINIT
C
#endif
      RETURN
       END
#endif
