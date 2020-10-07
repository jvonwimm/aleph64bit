      SUBROUTINE CREFLW(IREJET)
C----------------------------------------------------------------
C! Reconstruction of the event (charged + neutral particles)
C  using :
C
C           . Charged particles
C           . Reconstructed V0's,
C           . ELectron and muon identification,
C           . Photons identified by GAMPEC,
C           . Remaining neutral hadronic energy from PCOB.
C
C  Needs PECO,PEST,EWHE,ETDI,PHCO,PPOB,PCOB,PCRL ...etc banks
C
C  Inputs:  -- Data cards to perform a preselection on charged
C              particle.
C
C  Outputs: -- irejet = 0 if bad event (not preselected, or
C                         missing bank)
C           -- Banks EFOL and EAUX
C
C  P. Janot -- 18 April 1990
C----------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "parabank.h"
#include "intval.h"
      CHARACTER*8 prognam
      CHARACTER*4 CHAINT
      LOGICAL first
      DATA first/.TRUE./
      DATA krun0/0/
#include "qcde.h"
#include "qmacro.h"
C----------------------------------------------------------------
C  First set the ENFLW parameters using the data cards
C
      IF ( first ) THEN
        kfwvc = 7
        CALL bkfmt('FWVC','2I,(5F,2I)')
        CALL bkfmt('EFOL','2I,(5F,6I)')
        CALL bkfmt('EJET','2I,(4F)')
        CALL bkfmt('EAUX','5I,3F')
        CALL parset
        first = .FALSE.
      ENDIF
C
      irejet = 0
      ipecow = 0
      ipestw = 0
      iefol = NLINK('EFOL',3)
C
C  Get JULIA version number + correction file
C
      IF ( krun .NE. krun0 ) THEN
        krun0 = krun
        irhah = 0
        julver = 0
        julcor = 0
        libver = 0
        narhah = NAMIND('RHAH')
        IF ( narhah .GT. 0 ) irhah = IW(narhah)
        IF ( irhah .GT. 0 ) THEN
          DO istep = 1, LROWS(irhah)
            prognam(1:4) = CHAINT(ITABL(irhah,istep,1))
            prognam(5:8) = CHAINT(ITABL(irhah,istep,2))
            IF ( prognam .EQ. 'JULIA   ' ) THEN
              julver = ITABL(irhah,istep,5)
              julcor = ITABL(irhah,istep,11)
              libver = ITABL(irhah,istep,6)
            ENDIF
          ENDDO
        ENDIF
        IF ( julver .EQ. 0 ) julver = 999
        IF ( libver .EQ. 0 ) libver = 999
        IF ( idbg .GE. 1 )
     .  WRITE (IW(6),*) 'Run ',krun,' Julia version n0 : ',julver,
     .             ' Alephlib version n0 : ',libver
      ENDIF
C
C  Create temporary banks instead of using the ALPHA QVEC area
C
      IEFOL = NBANK ('EFOL', 3, lmhlen)
      IF ( IEFOL .EQ. 0 ) THEN
        WRITE (IW(6),*)
     .  ' **** CREFLW **** Unable to create bank EFOL #3'
        GOTO 997
      ENDIF
      CALL blist(iw,'S+','EFOL')
      IW (iefol + lmhcol) = lefola
      IW (iefol + lmhlen) = 0
C
      CALL bdrop(iw,'FWVC')
      CALL vzero(ifwvc(1),7)
      nfwvc = 200
      DO ipartk = 1 , 7
        IFWVC(IPARTK) = NBANK ('FWVC', ipartk, lmhlen + kfwvc*nfwvc)
        IF ( IFWVC(IPARTK) .EQ. 0 ) THEN
          WRITE (IW(6),*)
     .    ' **** CREFLW **** Unable to create temporary banks.'
          GOTO 997
        ENDIF
        IW( ifwvc(ipartk) + lmhcol ) = kfwvc
        IW( ifwvc(ipartk) + lmhlen ) = 1
      ENDDO
C
      IF ( idbg .GE. 1 ) CALL looses('ENFLW   ',1)
C
C  Preselection on good tracks (charged particles)
C
      CALL preselfw(keep)
      IF ( keep .EQ. 0 ) GOTO 999
      IF ( idbg .GE. 1 ) CALL looses('ENFLW   ',2)
C
C  Detached vertices :
C
      CALL v0sum
C
C  Initialisation of  calorimetry
C
      ipeco = 0
      ipest = 0
      ipeco = nlink('PECO',1)
      ipest = nlink('PEST',0)
      IF ( ipeco .GT. 0 ) CALL bktow(iw,'PECO',1,iw,ipecow,*10)
   10 IF ( ipest .GT. 0 ) CALL bktow(iw,'PEST',0,iw,ipestw,*11)
   11 CALL calinit(keep)
      IF ( keep .EQ. 0 ) GOTO 999
      IF ( idbg .GE. 1 ) CALL looses('ENFLW   ',3)
C
C  Electrons and muons
C
      CALL partid
C
C  Photons and neutral hadrons
C
      CALL gamixed
C
      irejet = 1
  999 CONTINUE
C
C  Build bank EFOL
C
      CALL crefol(irejet)
      IF (irejet . eq. 0 ) GO TO 998
      IF ( idbg .GE. 1 ) CALL looses('ENFLW   ',4)
C
C  Construct the bank EJET
C
      IF ( NLINK('EFLJ',0) .GT. 0 ) THEN
         CALL enfjet(irejet)
         if ( irejet . eq. 0 ) go to 998
      ENDIF
C
C  Unlock everything again
C
      CALL dlocks
C
C  Restore PECO and PEST banks from the work area
C
      IF ( ipecow .GT. 0 ) CALL bkfrw(iw,'PECO',1,iw,ipecow,*12)
   12 IF ( ipestw .GT. 0 ) CALL bkfrw(iw,'PEST',0,iw,ipestw,*13)
   13 CONTINUE
C
C  Restore the momenta of the charged particles
C
      DO jresca = 1, nresca
        factor = 1./resfac(jresca)
        itk = itkres(jresca)
        CALL qvscal(itk,factor)
      ENDDO
C
      GO TO 998
C No room for banks :
 997  irejet = 0
      CALL QWMESE(
     +'0CREFLW_ : Unable to create ENFLW banks :--> Enlarge BOS IW !!')
C drop bank EFOL , NR = 3 , if it esits , to avoid troubles in QFEFLO :
      INEF = NDROP('EFOL',3)
 998  CONTINUE
      IF ( ipecow .GT. 0 ) CALL wdrop(iw,ipecow)
      IF ( ipestw .GT. 0 ) CALL wdrop(iw,ipestw)
      RETURN
      END
#endif
