      SUBROUTINE BCALJU(IER)
C --------------------------------------------------------------------
C! Create BCPO (BCAL POT bank)
C
C     Author:  Peter Morawitz, 6.11.98 
C              Code from Gonzalo Merino incorporated into JULIA framework
C     Modified:Marco Cattaneo, 9.11.98
C              Cleaned up for JULIA
C
C     Purpose: Extract information from the raw data banks BHIT and BPTR and 
C              store it in the POT bank BCPO, for use in the BCAL++ offline
C              calibration.
C              Up to 100/9 Bcal events per ALEPH event are written to BCPO.
C
C     IER = 0  splendid
C     IER = 1  database not initialised
C     IER = 2  no BOS bank space ... should go home
C     IER = 3  BPTR bank found without corresponding BHIT bank
C --------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "bcs.h"

      INTEGER N_Events,iev

      INTEGER itrmask

      INTEGER N_Mod
      PARAMETER ( N_Mod = 4 )
      INTEGER iPmt(N_Mod),iApd(N_Mod)
      INTEGER iRmx(N_Mod),iPhmx(N_Mod)

      INTEGER i,nwords,nbhabhas

      INTEGER nwords_Max
      PARAMETER (nwords_Max = 100)

      INTEGER irun,ievt, kbptr, kbhit, kbcpo,igarb,ier,iret
      INTEGER nabptr / 0 /, nabhit / 0 / 
      INTEGER lastrun / 0 /

      INTEGER namind
      EXTERNAL namind
C-----------------------------------------------------------------------

C Routine only valid for BCAL++. Return for runs before 1997
      CALL abruev(irun,ievt)
      IF( irun .LT. 43000 ) GOTO 999

      IF (irun.NE.lastrun) THEN
        CALL bcinit( irun, iret )
        IF( iret .EQ. 0 ) THEN
          ier = 1
          RETURN
        ENDIF
        lastrun=irun
      ENDIF
c
c Get the banks pointers
      IF( nabhit .EQ. 0 ) nabhit = namind('BHIT')
      IF( nabptr .EQ. 0 ) nabptr = namind('BPTR')

      kbptr = iw(nabptr)
      IF (kbptr.NE.0) THEN
        N_Events = iw(kbptr+2)
      ELSE
        N_Events = 0
      ENDIF
      IF (N_events .EQ. 0) GOTO 999

      kbhit = iw(nabhit)
      IF( kbhit .EQ. 0 ) THEN
        IER = 3
        CALL RERROR('BCALJU',-3,'BPTR bank found without BHIT banks')
        RETURN
      ENDIF

c-> Go for the BHIT bank
      nwords = 1
      nbhabhas = 0

C -------------------------------------------------------------------
c   Create the BCPO bank with max space for N_Events
      CALL BDROP(IW,'BCPO')
      CALL AUBOS('BCPO',0,N_Events*9+LMHLEN,KBCPO,IGARB)
      IF (IGARB.EQ.2) THEN
C No space to create bank
        GOTO 1000
      ELSEIF( IGARB .EQ. 1 ) THEN
C Garbage collection, don't do anything here ...
        CONTINUE
      ENDIF
C -------------------------------------------------------------------

      DO iev = 1, N_Events
        CALL BCGETE(kbptr,kbhit,iev,itrmask,iPmt,iApd,iRmx,iPhmx)
c-> Save something only if there is some coincidence trigger...
        IF (ibits(itrmask,4,1).eq.1.or.
     .        ibits(itrmask,5,1).eq.1) then

          IF (nwords+1.gt.nwords_Max) goto 99
          nwords = nwords + 1
          IW(KBCPO+LMHLEN+nwords-1) = itrmask

c-> If there is Coin14
          IF (ibits(itrmask,4,1).eq.1) then
            IF (nwords+8.gt.nwords_Max) goto 99
            IW(KBCPO+LMHLEN+nwords-1+1) = iPmt(1)
            IW(KBCPO+LMHLEN+nwords-1+2) = iApd(1)
            IW(KBCPO+LMHLEN+nwords-1+3) = iRmx(1)
            IW(KBCPO+LMHLEN+nwords-1+4) = iPhmx(1)

            IW(KBCPO+LMHLEN+nwords-1+5) = iPmt(4)
            IW(KBCPO+LMHLEN+nwords-1+6) = iApd(4)
            IW(KBCPO+LMHLEN+nwords-1+7) = iRmx(4)
            IW(KBCPO+LMHLEN+nwords-1+8) = iPhmx(4)

            nwords = nwords + 8
            nbhabhas = nbhabhas + 1
          ENDIF

c-> If there is Coin23
          IF (ibits(itrmask,5,1).eq.1) then
             if (ibits(itrmask,4,1).eq.1) then
C ...  there was already a 14 trigger, store bhabha in new row.
                if (nwords+1.gt.nwords_Max) goto 99
                nwords = nwords + 1
C ...  repeat the trigger mask here
                IW(KBCPO+LMHLEN+nwords-1) = itrmask
             endif
             
            IF (nwords+8.gt.nwords_Max) goto 99
            IW(KBCPO+LMHLEN+nwords-1+1) = iPmt(2)
            IW(KBCPO+LMHLEN+nwords-1+2) = iApd(2)
            IW(KBCPO+LMHLEN+nwords-1+3) = iRmx(2)
            IW(KBCPO+LMHLEN+nwords-1+4) = iPhmx(2)

            IW(KBCPO+LMHLEN+nwords-1+5) = iPmt(3)
            IW(KBCPO+LMHLEN+nwords-1+6) = iApd(3)
            IW(KBCPO+LMHLEN+nwords-1+7) = iRmx(3)
            IW(KBCPO+LMHLEN+nwords-1+8) = iPhmx(3)

            nwords = nwords + 8
            nbhabhas = nbhabhas + 1
          ENDIF
c
        ENDIF
      ENDDO
c
   99 CONTINUE

C -- include BCPO header, adjust length and add bank to the E list
      IW(KBCPO+LMHROW) = nbhabhas
      IW(KBCPO+LMHCOL) = 9
      CALL AUBOS('BCPO',0,nbhabhas*9+LMHLEN,KBCPO,IGARB)
      IF (IGARB.EQ.2) THEN
C No space to create bank
        GOTO 1000
      ENDIF
      CALL BLIST(IW,'E+','BCPO')

  999 CONTINUE
      IER=0
      RETURN

 1000 CONTINUE ! fatal ...
      IER = 2
      CALL RERROR('BCALJU',-2,'No space to create necessary BOS banks.')

      RETURN
      END
#endif
