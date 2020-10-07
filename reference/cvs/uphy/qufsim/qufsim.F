       SUBROUTINE QUFSIM(IER)
C-----------------------------------------------------------------------
C! Fast simulation starting from MC truth
C  Output  IER : error flag ( 0 if ok )
C
C  G.Ganis  21 Mar 1991
C  Heavy modifications : Patrick Janot -- Nov 1993
C  Modif -- 29 aug 1995 ( P.Janot ) Store particles in class KRECO
C  Modif -- 13 may 1998 ( M. Kado ) Quclal (clustering of hadrons 
C                                   and photons in one step) 
C-----------------------------------------------------------------------
#include "qcde.h"
#include "qhac.h"
#include "resolu.h"
#include "parcut.h"
#include "fsimid.h"
#include "hadpho.h"
#include "psfrjj.h"
#include "ftrig.h"
#include "coddat.h"
#include "psfrfu.h"
#include "qmacro.h"
      DATA IFI /0/, ipri /0/
C ----------------------------------------------------------------------

C
C -- Initialize
C
      IER= 0
      IF ( ifi .EQ. 0 ) THEN
        CALL inifsim(ier1)
        igale = NLINK('GALE',0)
        IF ( ier1 .NE. 0 ) THEN
          ier= 1
          GOTO 999
        ENDIF
        ifi= 1
      ELSE
        ind = ndrop('PSFR',0)
      ENDIF
C      print *, '****************************************'
C      print *, 'from qufsim nch ... 1',ntrack



C
C--- Fill the correct value for qelep
C
      IF( krun .LT. 2000 ) THEN
        CALL QEEVEH(ecm,ier)
        IF( ier .EQ. 0 ) qelep= ecm
        IF( ier .EQ. 1 ) WRITE(*,*) ' EVEH does not exist'
      END IF
      IF( qelep .LT. 80. ) qelep= 91.25
C
C --- Initialize pseudo particles
C -- total 4-momentum
      ltot = kvnew(dummy)
      ltot = kvsavc(ltot,' ',kreco)
C -- photons
      lmg = kvnew(dummy)
      lmg = kvsavc(lmg,' ',kreco)
C -- neutral hadrons
      lmh = kvnew(dummy)
      lmh = kvsavc(lmh,' ',kreco)
C -- charged particles
      lch = kvnew(dummy)
      lch = kvsavc(lch,' ',kreco)
C -- V0s
      lv0 = kvnew(dummy)
      lv0 = kvsavc(lv0,' ',kreco)
C -- Lumi
      lum = kvnew(dummy)
      lum = kvsavc(lum,' ',kreco)
C -- dummies
      lkh = 0
      lkg = kvnew(dummy)
      lkg = kvsavc(lkg,' ',kreco)
C
      inew= kvnew(dummy)
      inew= kvsavc(inew,' ',kmonte)
C
C Zero the count of each kind of Eflow object added to KRECO
C
      NCHTOT = 0
      NV0TOT = 0
      NPHTOT = 0
      NHATOT = 0
      NLUTOT = 0
      NEKTOT = 0
      NHKTOT = 0

C
C -- Restore correct stability codes in exotic cases
C
      IF ( idbg .GE. 15 ) WRITE(6,*) 'CALL QUKSTA'
      CALL quksta

C      print *, 'from qufsim nch ... 2',ntrack


C
      IF ( idbg .GE. 5 ) THEN
        CALL prfkin
        IF ( idbg .GE. 10 ) THEN
          WRITE(6,*) 'Table de correspondance entre ',kfmct,' et ',klmct
          DO itk = kfmct, klmct
            IF ( kstabc(itk) .GE. 1 )
     .      WRITE(6,*) cqtpn(itk),itk,ktn(itk)
          ENDDO
        ENDIF
      ENDIF

C
C -- FTRIG  initialise tables
C
      call vzero(E_ECAL,37)
      call vzero(T_HCAL,36)
      call vzero(ITCseg,48)

C
C -- Decay the particles that usually decay in Galeph ...
C
      IF ( igale .LE. 0 ) THEN
C
        IF ( idbg .GE. 15 ) WRITE(6,*) 'CALL QUDECA'
        CALL qudeca
C
C  -- Simulate photon conversions
C



        IF ( idbg .GE. 15 ) WRITE(6,*) 'CALL QUPHCV'
        CALL quphcv
C
      ENDIF

C      print *, 'from qufsim nch ... 3',ntrack

C
C -- Determine the good charged tracks.
C
      IF ( idbg .GE. 15 ) WRITE(6,*) 'CALL QUTPCH'
      CALL qutpch(ier)
C      print *, 'Ier ....',ier
      IF ( ier .NE. 0 ) GOTO 999

C      print *, 'from qufsim nch ... 4',ntrack

C
C Find the vzero tracks in the remaining "bad" tracks.
C
      IF ( inotp .LE. 0 ) THEN
C
        IF ( idbg .GE. 15 ) WRITE(6,*) 'CALL QUTPV0'
        CALL qutpv0
C      print *, 'from qufsim nch ... 5',ntrack
C
C Perform electron and muon identification
C
        IF ( idbg .GE. 15 ) WRITE(6,*) 'CALL QUIDEM'
        CALL quidem
C       print *, 'from qufsim nch ... 6',ntrack

C
      ENDIF
C
C Store the photons
C
      IF ( idbg .GE. 15 ) WRITE(6,*) 'CALL QUPHST'
      CALL quphst
C      print *, 'from qufsim nch ... 7',ntrack


C
C Build the CAL-objects, with link to charged tracks
C
      IF ( idbg .GE. 15 ) WRITE(6,*) 'CALL QUCAOB'
      CALL qucaob
C      print *, 'from qufsim nch ... 8',ntrack

C
C Perform photon clustering
C
      IF ( idbg .GE. 15 ) WRITE(6,*) 'CALL QUCLAL'
      CALL quclal

C
C -- Build readable output
C
      IF ( idbg .GE. 15 ) WRITE(6,*) 'CALL SUMMA '
      CALL SUMMA
C
C -- Fill some histos
C
C    ...
C
C -- Trigger
C
C      CALL FASTRI( IOK )
      CALL FTRGAL( IOK )

      IF(IOK.EQ.0) THEN
        IER= 3
c       CALL fsimlist(kreco)
      ENDIF
C
999   CONTINUE
C
      CALL qlzer(kmonte)
      CALL qlzer(kreco)
      DO kpsfr = 1, npsfr
        ind = ndrop('TREX',kpsfr)
      ENDDO
C
      RETURN
      END
