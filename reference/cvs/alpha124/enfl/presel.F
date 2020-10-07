      SUBROUTINE PRESEL(KEEP)
C-----------------------------------------------------------------------
C! Preselection (charged particles)
C
C  Patrick Janot -- 18 Apr 1990
C-----------------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "parabank.h"
#include "intval.h"
#include "qcde.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
      ifrf0 = 0
      nafrf0 = namind('FRF0')
      IF ( nafrf0 .GT. 0 ) ifrf0 = IW(nafrf0)
C
      ifrf2 = nlink('FRFT',2)
C
      gdmult = 0.
      chatot = 0.
      chapo  = 0.
      chane  = 0.
      chene  = 0.
      nresca = 0
      CALL vzero(resfac(1),mxtrk)
      CALL vzero(itkres(1),mxtrk)
C-- Loop on charged particles
      DO 1 ich = kfcht , klcht
C -- More than 4 TPC points
        IF ( kfrtnt(ich) .LT. n0 ) CALL qltrk(ich)
C -- Don't count these bad tracks...
        IF ( xlock(ich) ) GOTO 1
C -- Set the momentum according to ECAL and TPC if energetic electron
        IF ( keidip(ich) .EQ. 1 .AND. keidif(ich) .EQ. 0 ) THEN
          IF ( qp(ich) .GT. 15. ) THEN
            pich = AMIN1(qp(ich),qelep/2.)
            eich = AMIN1(qeidec(ich),qelep/2.)
            sigmapt = pich**2*SQRT(1.-qct(ich)**2)
            IF ( ifrf2 .GT. 0 .AND. ifrf0 .LE. 0 ) THEN
              sigmapt = 6.0E-4 * sigmapt
            ELSE
              sigmapt = 1.0E-3 * sigmapt
            ENDIF
            sigmaec = .17*SQRT(eich) + .02*eich
            sigmatt = SQRT(sigmapt**2+sigmaec**2)
            sigmann = ABS(eich-pich)/sigmatt
            IF ( sigmann .LT. 3. ) THEN
              factor = (sigmapt**2*eich + sigmaec**2*pich)
     .               / (sigmapt**2      + sigmaec**2     )
              factor = factor/qp(ich)
              IF ( idbg .GE. 10 ) THEN
                WRITE (IW(6),*) 'Track ',ich,' rescaled by ',factor
                WRITE (IW(6),*) 'p,sigmap : ',qp(ich),sigmapt
                WRITE (IW(6),*) 'e,sigmae : ',qeidec(ich),sigmaec
                WRITE (IW(6),*) '# sigmas : ',sigmann
                WRITE (IW(6),*) '--------------------'
              ENDIF
              CALL qvscal(ich,factor)
              nresca = nresca + 1
              itkres(nresca) = ich
              resfac(nresca) = factor
            ENDIF
          ENDIF
        ENDIF
C -- Good tracks only - More stringent d0 for high momenta
C -- Also 1 ITC point -- or electron -- if high momentum
        IF ( ABS(qzb(ich)) .LT. z0 .AND. ABS(qdb(ich)) .LT. d0 .AND.
     .     ( qp(ich) .LE. 15. .OR. kfrtni(ich) .GE. 1 .OR.
     .       kfrtnt(ich) .GE. 8 .OR. keidip(ich) .EQ. 1 ) ) THEN
C -- Add up  ...
          CALL charad(ich)
          chene = chene + qe(ich)
        ELSE
          ichsam = ksame(ich)
   10     IF ( ichsam .EQ. ich ) GOTO 20
          IF ( ichsam .GE. kfdct .AND. ichsam .LE. kldct ) GOTO 1
          ichsam = ksame(ichsam)
          GOTO 10
   20     CALL qltrk(ich)
        ENDIF
    1 CONTINUE
C
C -- Preselection cuts
C
      keep = 0
      IF ( idbg .GE. 1 ) CALL looses('PRESEL  ',1)
C
      IF ( gdmult  .LT.  ntrack .OR.
     .     gdmult  .GT.  mtrack ) GOTO 999
      IF ( idbg .GE. 1 ) CALL looses('PRESEL  ',2)
C
      IF ( chene .LT. echmin )    GOTO 999
      IF ( idbg .GE. 1 ) CALL looses('PRESEL  ',3)
C
      IF ( chene .GT. echmax ) THEN
        WRITE(IW(6),*) ' Run/Event : ',krun,kevt
        WRITE(IW(6),*) ' ENFLW Warning: Charged energy = ',chene,' GeV'
      ENDIF
      IF ( idbg .GE. 1 ) CALL looses('PRESEL  ',4)
C
      keep = 1
C
 999  RETURN
      END
#endif
