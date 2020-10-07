      SUBROUTINE IPAKDI(IER)
C-----------------------------------------------------------------------
C! Pack ITC digits. for POT output.
C!
C!    Author     :- I. Tomalin
C!    Modified   :- J. Sedgbeer   89/02/10   Separated out from IPREDA.
C!    Modified   :- J. Sedgbeer   89/04/11
C!    Modified   :- J. Sedgbeer 91/09/10 Remove obsolete setting of bit
C!                  31 (last bit) in PIDI.
C!    Input:
C!      commons:       /BCS/  for banks  IDIG - ITC digitisings
C!                                  and  IDCR - Digit-to-Coord relation
C!                     /RCURNT/ run and event number.
C!      parameters:    PIDIJJ
C!                     IDIGJP
C!
C!    Output:
C!      IER   /I     : error flag:  = 0 if all O.K.
C!                                  = 1 if missing or empty input banks
C!                                  = 2 if not enough BOS space
C!      PIDI bank      packed ITC digitisings.
C!
C!    called by : IPREDA
C!    calls     : AUBOS
C!
C!    Libraries required: BOS
C!
C! IPAKDI: Create ITC packed digitisings bank PIDI for POT output.
C! N.B. : Only 'good' digits are packed, i.e. only digits which have
C! a corresponding coordinate in the ITCO bank. The IDCR bank is
C! used to check for the existence of a coord.
C! Hence, the PIDI bank is in 1-to-1 correspondence with the ITCO bank
C! not with the IDIG bank.
C!
C? Get IDIG bank and IDCR bank
C? Loop over digitisings
C?   If Digit has a corresponding Coord. then
C?     Store in PIDI bank
C?   Endif
C? End Loop
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
#include "rflags.h"
C-----------------------------------------------------------------------
C I/O commons and parameters etc.
#include "bcs.h"
#include "pidijj.h"
#include "idigjp.h"
#include "rcurnt.h"
      EXTERNAL NAMIND
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
      IF (FIRST) THEN
        NIDIG = NAMIND('IDIG')
        NIDCR = NAMIND('IDCR')
        FIRST = .FALSE.
      ENDIF
      IER = 0
C
C Check that IDIG and IDCR banks exist and are not empty.
C
      KIDIG = IW(NIDIG)
      KIDCR = IW(NIDCR)
      IF(KIDIG.EQ.0.OR.KIDCR.EQ.0) GOTO 910
      NDIG = LROWS(KIDIG)
      IF(NDIG.LE.0) GOTO 910
C
C Create POT bank for ITC digitisings.
C
      CALL AUBOS('PIDI',0,NDIG+LMHLEN,KPIDI,IGARB)
      IF(IGARB.EQ.2) GOTO 920
      IF(IGARB.EQ.1) THEN
        KIDIG = IW(NIDIG)
        KIDCR = IW(NIDCR)
      ENDIF
C
C-----------------------------------------------------------------------
C Loop over all digits.  If no coord. then skip this digit.
C
      NCO  = 0
      DO 100 IDIG =1,NDIG
        ICO   = IW(KIDCR+LMHLEN+IDIG)
        IF(ICO.LE.0) GOTO 100
        NCO = NCO + 1
        IW(KPIDI+LMHLEN+NCO) = IW(KIDIG+LMHLEN+IDIG)
  100 CONTINUE
C-----------------------------------------------------------------------
C
C Adjust length of PIDI bank and fill header words
C
      IW(KPIDI+LMHCOL) = 1
      IW(KPIDI+LMHROW) = NCO
      CALL AUBOS('PIDI',0,NCO+LMHLEN,KPIDI,IGARB)
      IF(IGARB.EQ.2) GOTO 920
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.3)
     +                    WRITE(LDEBRL,1000) IRUNRC,IEVTRC,NDIG,NCO
 1000 FORMAT('0IPAKDI: Run',I8,' Event',I10,' #digis',I4,' #packed',I4)
C
      GOTO 999
C-----------------------------------------------------------------------
C Error returns.
C
  910 CONTINUE
      IER = 1
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) WRITE(LDEBRL,9010)
 9010 FORMAT(' IPAKDI: Missing or empty IDIG or IDCR bank.')
      CALL RERROR('IPAKDI',1,'Missing or empty IDIG or IDCR bank.')
      GOTO 999
C
  920 CONTINUE
      IER = 2
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) WRITE(LDEBRL,9020)
 9020 FORMAT(' IPAKDI: No space to create BOS banks')
      CALL RERROR('IPAKDI',2,'No space to create necessary BOS banks.')
C
C-----------------------------------------------------------------------
  999 CONTINUE
      END
#endif
