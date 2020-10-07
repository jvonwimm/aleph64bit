*DK asktrk
      SUBROUTINE ASKTRK
C ----------------------------------------------------------------------
C - B.Bloch-Devaux - 850315                       F.Ranjard - 860929
C! Store new tracks:
C    - in the kinematic banks if Pmod > 1.Gev  or Pmod > 0.1Gev in the
C      inner part of the apparatus.
C    - other tracks are stored in the stack.
C. - called from    GUSTEP                            from this .HLB
C. - calls          GSVERT, GSKINE, GSKING            from GEANT3 pam
C.                  IUCOMP                            from KERNLIB
C.
C -----------------------------------------------------
#ifndef DOC
      SAVE
#include "gctmed.h"
#include "gckine.h"
#include "gctrak.h"
#include "gcking.h"
#include "gcvolu.h"
#include "iocom.h"
#include "jobcom.h"
#include "jqcom.h"
#include "trkcom.h"
#include "capano.h"
#include "kvoljj.h"
      PARAMETER (NOMOR=3)
      REAL GADD(3)
      LOGICAL  UPTPC,UPECA,EMPA,HADPA,MUPA,PRIMA,TINO,BREMP,INMSK
      EXTERNAL IUCOMP
      CHARACTER*4 TCASE,CHAINT,CHAHOL
C - define central region and beam tube region
      DATA KCDET,KBTUB/4HCDET,4HBTUB/
C - define MASK and SHIELDS regions
      DATA KBMSK,KBSH1,KBSH2/4HBMSK,4HBSH1,4HBSH2/
C - define ITC endplate, TPC endplate and TPC outer wall
      DATA KITND,KTPEP,KTPWO /4HITND,4HTPEP,4HTPWO/
C - define TPC region
      DATA KTPCR / 4HTPCR/
C - particles w/ momentum below 40.Mev up to the TPC, and 100Mev
C   in the TPC are kept in STAK bank
C - brem or dray which take more than 15% of the incident particle
C   provoke the stopping of the incident particle and the creation
C   of a new one.
C - muons which are daughters of primaries are kept as primaries.
      DATA PERC /0.15/, CUTPC /0.1/, CUTIT /0.04/
C - Statment functions
#include "bmacro.h"
C                      to define e.m. particles, had. part. or muon part.
      TINO(I) = INT(GKIN(5,I)).GE.LTYPEL .AND. INT(GKIN(5,I)).LE.LTYPGA
      EMPA(I) = INT(GKIN(5,I)).LE.3
      MUPA(I) = INT(GKIN(5,I)).EQ.5 .OR. INT(GKIN(5,I)).EQ.6
      HADPA(I)= INT(GKIN(5,I)).EQ.4 .OR. INT(GKIN(5,I)).GT.6
C - define region in Tungsten mask and shield (beam pipe region)
      INMSK = (NGAMES(NGLEVE).EQ.KBMSK).OR.(NGAMES(NGLEVE).EQ.KBSH1).OR.
     &        (NGAMES(NGLEVE).EQ.KBSH2)
C - define region up to TPC
      UPTPC   = ((NGAMES(2).EQ.KBTUB) .OR.
     &          (NGAMES(2).EQ.KCDET .AND. NGAMES(3).NE.KTPCR .AND.
     &          NGAMES(4).NE.KITND)) .AND. (.NOT.INMSK)

C - define region up to ECAL
      UPECA   = UPTPC .OR. (NGAMES(3).EQ.KTPCR .AND. NGAMES(4).NE.KTPEP
     &                .AND. NGAMES(4).NE.KTPWO)
C ----------------------------------------------------------------------
C
C   IF there are secondaries THEN
C      IF a primary has created secondaries w/o loosing its identity
C         (BREM or DRAY) and the secondaries take more than 15% of
C         the enery of the primary THEN
C            stop the current particle
C            create a new particle w/ the residual energy
C      ENDIF
C      store the secondaries:
C      a particle is a primary IF its mother is a primary
C                             AND its momentum is above 40Mev up to TPC
C                              OR its momentum is above 100Mev up to ECAL
C                             AND its origin vertex is up to the outer
C                                 TPC wall but the TPC and ITC endplate
C                             AND it is not in the mask and shields part
C      store primaries in KINE and VERT banks and others in the STAK
C   ENDIF
C
      NVTX = 0
      IF (NGKINE .GT. 0) THEN
        TCASE = CHAHOL (KGCASE)
        BREMP = IGSTAK.EQ.0 .AND. UPECA. AND. GVECT(7).GT.CUTPC
     &           .AND.(TCASE.EQ.'BREM' .OR. TCASE.EQ.'DRAY')
     &           .AND.NGKINE.LT.LGKINE
        IF (BREMP) THEN
          PSUM = 0.
          DO 10 I=1,NGKINE
            PSUM = PSUM+SQRT(GKIN(1,I)**2+GKIN(2,I)**2+GKIN(3,I)**2)
   10     CONTINUE
          IF (PSUM .GT. PERC*GVECT(7)) THEN
            IGSTOP = NOMOR
            NGKINE = NGKINE+1
            DO 11 IK=1,3
                GPOS(IK,NGKINE) = GVECT(IK)
                GKIN(IK,NGKINE) = GVECT(3+IK)*GVECT(7)
   11       CONTINUE
            GKIN(4,NGKINE) = SQRT(GMASS**2+GKIN(1,NGKINE)**2+
     &                            GKIN(2,NGKINE)**2+GKIN(3,NGKINE)**2)
            GKIN(5,NGKINE) = REAL(IGPART)
          ENDIF
        ENDIF
        DO 30 I=1,NGKINE
C        store particle# I either in the STACK or in KINE banks
          PMOD  = SQRT (GKIN(1,I)**2+GKIN(2,I)**2+GKIN(3,I)**2)
          PRIMA = ((IGSTAK.EQ.0).AND.( (UPTPC.AND.(PMOD.GT.CUTIT))
     &                 .OR.(UPECA.AND.(PMOD.GT.CUTPC)).OR.MUPA(I)) )
     &                 .OR. TINO(I)
          IF (PRIMA) THEN
C           primary daughter AND
C               ( muon OR
C                 part. with Pmod>0.04Gev up to TPC  OR
C                 part. with Pmod>0.1Gev up to ECAL )
C           OR geantino
C           are stored in KINE and VERT banks
            IGFLGK(I) = 1
            NVTX = 1
          ELSE
C           other particles are in stack
            IGFLGK(I) = 0
          ENDIF
   30   CONTINUE
        CALL GSKING (0)
      ENDIF
C
C   IF  a charged primary stopped and no vertex was stored yet THEN
C      store an end vertex ==> NVTX>0
C   ENDIF
C
      IF ((GCHARG.NE.0..AND.IGSTOP.NE.0.AND.IGSTAK.EQ.0)
     &     .AND. NVTX.EQ.0) THEN
        CALL GSVERT(GVECT,IGTRA,0,0,0, NVTX)
        IF(NVTX.LE.0) GO TO 999
      ENDIF
C
C   IF a new vertex has been stored THEN
C      store vertex volume name in KVOL bank
C   ENDIF
C
      IF (NVTX .GT.0) THEN
        JKVOL = IW(NAKVOL)
        IF (LFRROW(JKVOL).LT.1) THEN
          CALL ALBOS ('KVOL',0,IW(JKVOL)*2,JKVOL,IGARB)
        ENDIF
        IW(KNEXT(JKVOL)+JKVOVN) = INTCHA(TRKVOL)
        IW(KNEXT(JKVOL)+JKVOVM) = INTCHA(TCASE)
        IW(JKVOL+LMHROW) = LROWS(JKVOL) + 1
      ENDIF
C
C - end
  999 CONTINUE
      RETURN
      END
#endif
