      SUBROUTINE ITRECI(IER)
C-----------------------------------------------------------------------
C! ITC Reconstruction initialisation.
C!
C!    Author      :- J. Sedgbeer 87/02/09
C!    Modified    :- J. Sedgbeer 88/06/20
C!    Modified    :- I. Tomalin  88/11/17
C!
C!   Input:
C!     need commons /ITNAMC/ for NAITCO etc.
C!                  /ITWICC/ for ITC geom.
C!                  /BCS/    for ITCO bank etc.
C!                  /IWBNKT/ for pointers to work banks
C!                  /RCURNT/ for run and event number
C!          params.  ITCOJJ
C!                   ITFTJJ
C!                   IGTLJJ
C!
C!   Output:
C!    IER /I4 : Error flag = 0 if O.K.
C!                         = 1 if no ITCO(0) bank
C!                         = 2 if no TGFT bank
C!                         = 3 if no ITCO(0) and no TGFT bank
C!                         = 4 No space to create banks
C!     fill common /IWBNKT/ with ITC work bank pointers for tracking
C!     create IUCOIW and IWHTIW banks
C!     create ITFT, IGTL AND IGCL banks
C!
C!   called by : ITCREC
C!   calls     : none
C!
C!   Libraries required: BOS
C!
C!======================================================================
#ifndef DOC
C Standard commons
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C----------------------------------------------------------------------
C I/O commons etc.
#include "bcs.h"
#include "bosext.h"
#include "rcurnt.h"
#include "itwicc.h"
#include "iwbnkt.h"
C----------------------------------------------------------------------
#include "itcojj.h"
#include "igtljj.h"
#include "itftjj.h"
#include "tgftjj.h"
C----------------------------------------------------------------------
      LOGICAL SETZ
      DATA SETZ/.TRUE./
C----------------------------------------------------------------------
#include "bmacro.h"
C----------------------------------------------------------------------
C Initialise.
      IER = 0
      IF (SETZ) THEN
        IWHTIW = 0
        IUCOIW = 0
        ILNKIW = 0
        SETZ = .FALSE.
      END IF
C
C Locate ITCO bank #0.
      KITC0  = NLINK('ITCO',0)
      NCO = 0
      IF (KITC0.GT.0) NCO = LROWS(KITC0)
C
C Create IGTL, IGCL and ITFT banks (#1), and fill their headers.
C (IGTL and ITFT banks have same number of rows as TGFT).
C
      KTGFT = IW(NAMIND('TGFT'))
      IF (KTGFT.LE.0) THEN
        IER = 2
        NTGF = 0
      ELSE
        NTGF = LROWS(KTGFT)
      END IF
C
      LIGTL = NTGF*LIGTLA + LMHLEN
      CALL AUBOS('IGTL',1,LIGTL,KIGTL,IGARB)
      IF(IGARB.EQ.2) GOTO 998
      IW(KIGTL+LMHROW) = NTGF
      IW(KIGTL+LMHCOL) = LIGTLA
C
      LIGCL = NCO + LMHLEN
      CALL AUBOS('IGCL',1,LIGCL,KIGCL,IGARB)
      IF (IGARB.EQ.2) GOTO 998
      IW(KIGCL+LMHROW) = 0
      IW(KIGCL+LMHCOL) = 1
C
C Create ITFT bank #1 (joint TPC-ITC fit). Fill header words
C
      LITFT = NTGF*LITFTA + LMHLEN
      CALL AUBOS('ITFT',1,LITFT,KITFT,IGARB)
      IF(IGARB.EQ.2) GOTO 998
      IW(KITFT+LMHCOL) = LITFTA
      IW(KITFT+LMHROW) = NTGF
C
C Relocate ITCO bank #0.
C
      KITC0  = NLINK('ITCO',0)
      IF(KITC0.LE.0) THEN
        IER = IER + 1
      ELSE
C Copy ITCO bank #0 to ITCO bank #1. The latter will always contain the
C original coords. The former will contain the ones actually used in
C the track fits.
        LEN = NCO*LITCOA + LMHLEN
        CALL AUBOS('ITCO',1,LEN,KITC1,IGARB)
        IF(IGARB.EQ.1) KITC0 = NLINK('ITCO',0)
        IF(IGARB.EQ.2) GOTO 998
        CALL UCOPY(IW(KITC0+1),IW(KITC1+1),LEN)
C
C Book some work banks needed for ITC tracking.
C
        IF (IWHTIW.GT.0) THEN
          CALL WDROP(IW,IWHTIW)
          CALL RERROR('ITRECI',3,'IWHTIW work bank still exists')
        END IF
        IF (IUCOIW.GT.0) THEN
          CALL WDROP(IW,IUCOIW)
          CALL RERROR('ITRECI',4,'IUCOIW work bank still exists')
        END IF
        LIWHT  = MWIRIT + LMHLEN
        LIUCO  = NCO + LMHLEN
        CALL WBANK(IW,IWHTIW,LIWHT,*998)
        CALL WBANK(IW,IUCOIW,LIUCO,*998)
C
C Fill work bank for hit wires
C
        IW(IWHTIW+LMHCOL)=1
        IW(IWHTIW+LMHROW)=MWIRIT
        DO 50 I=1,NCO
          IWIRL = ITABL(KITC0,I,JITCWN)
          IWIRE = MOD(IWIRL,1000)
          IW(IWHTIW+LMHLEN+IWIRE) = I
   50   CONTINUE
C
C Initialise IUCOIW (used coordinates) bank.
C
        IW(IUCOIW+LMHCOL) = 1
        IW(IUCOIW+LMHROW) = NCO
C
C Create ILNKIW bank for storing links.
C
        IF (IER.EQ.0) THEN
          IF (ILNKIW.GT.0) THEN
            CALL WDROP(IW,ILNKIW)
            CALL RERROR('ITRECI',5,'ILNKIW work bank still exists')
          END IF
          LEN = LMHLEN + 4*75
          CALL WBANK(IW,ILNKIW,LEN,*998)
          IW(ILNKIW+LMHCOL) = 4
          IW(ILNKIW+LMHROW) = 0
        ENDIF
      END IF
C
      GOTO 999
C
  998 IER = 4
      IF(IWHTIW.NE.0) CALL WDROP(IW,IWHTIW)
      IF(IUCOIW.NE.0) CALL WDROP(IW,IUCOIW)
      IF(ILNKIW.NE.0) CALL WDROP(IW,ILNKIW)
C Tidy up by dropping the banks which we just tried to create.
      KITFT = NLINK('ITFT',1)
      IF (KITFT.GT.0) IDUM = NDROP('ITFT',1)
      KIGTL = NLINK('IGTL',1)
      IF (KIGTL.GT.0) IDUM = NDROP('IGTL',1)
      KIGCL = NLINK('IGCL',1)
      IF (KIGCL.GT.0) IDUM = NDROP('IGCL',1)
      KITC1 = NLINK('ITCO',1)
      IF (KITC1.GT.0) IDUM = NDROP('ITCO',1)
      CALL RERROR('ITRECI',6,'No space to create banks.')
  999 CONTINUE
C Add banks to 'T' list
      CALL BLIST(IW,'T+','IGCLIGTLITFT')
      END
#endif
