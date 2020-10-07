      SUBROUTINE HINIJO
C--------------------------------------------------------------------
C! Initialise the HCAL job
C!
C! Author G.Capon
C!
C! Modified  - Version 1.1 R.Tenchini 890126
C!
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
#include "hdeach.h"
#include "rflags.h"
#include "hdebug.h"
#include "bcs.h"
#include "hcnamc.h"
#include "hcjosu.h"
#include "bmacro.h"
C--------------------------------------------------------------------
C
C               set debug printout level
C
      IHPRIN=0
      IF(FDEBRF) IHPRIN = JDBDRF(JULHC)
C
C               fill flags for dead/missing elements
C
C               1 - missing subcomponents
C
      JHSUB=IW(NAMIND('HSUB'))
      DO 12 N=1,3
      IHSUBC(N)=1
      IF(JHSUB.NE.0) IHSUBC(N)=ITABL(JHSUB,N,1)
 12   CONTINUE
C
C               2 - missing acquisition modules (Astros) J=1,36
C
      DO 11 N=1,36
 11   MODULE(N)=1
      JHMOD=IW(NAMIND('HMOD'))
      IF (JHMOD.NE.0) THEN
         DO 13 N=1,LROWS(JHMOD)
         J=ITABL(JHMOD,N,1)
 13      MODULE(J)=0
      ENDIF
C
C               3 - missing buses (SGS boards) Iastr=101/171
C                   (moved to HINIRU R.Tenchini 13-oct-89)
C
C
C               reset variables for job summary statistics
C
      NSTOHC(1) = 0
      NSTOHC(2) = 0
      ETOTHC(1) = 0.
      ETOTHC(2) = 0.
      NCLUHC = 0
      NTUBHC = 0
      NPATHC = 0
      NHITHC = 0
C
C? Get RUN condition for HCAL DIGITAL PATTERNS
C
      CALL HPRUNC
C
C    ******   Read HNGR for Topological Noise Ananlysis ******
C
C
C First we check if there are cards that overwrite the Database
C
      NHNGR=NAMIND('HNGR')
      KHNGR=IW(NHNGR)
C
C  Database reading
C
      IF(KHNGR.EQ.0)THEN
         KHNGR=MDARD(IW,LRCONS,'HNGR',0)
      ENDIF
      IF(KHNGR.EQ.0)THEN
         CALL REPORT('HINIJO','HNGR NOT READ FROM DAF OR CARDS',0)
      ENDIF
C
C?          set name-indices for Hcal banks
C?          put banks in E list
C
      NAHTDI=NAMIND('HTDI')
      NAHWDI=NAMIND('HWDI')
      NAHSDA=NAMIND('HSDA')
      NAHSTO=NAMIND('HSTO')
      NAHCLU=NAMIND('HCLU')
      NAHTUB=NAMIND('HTUB')
      NAHCSR=NAMIND('HCSR')
      NAHPCO=NAMIND('HPCO')
      NAHPDS=NAMIND('HPDS')
      NAHSPH=NAMIND('HSPH')
      NAHSPE=NAMIND('HSPE')
      NACEXT=NAMIND('CEXT')
  999 CONTINUE
      RETURN
      END
#endif
