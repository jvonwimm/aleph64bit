      SUBROUTINE IPREDA
C-----------------------------------------------------------------------
C! Prepare ITC Data.
C!
C!    Author          :- J. Sedgbeer
C!    Modified        :- I. Tomalin    88/08/25
C!    Modified        :- J. Sedgbeer   89/04/11
C!    Modified        :- J. Sedgbeer   89/06/05
C!    Modified        :- J. Sedgbeer   90/01/04 Add IFCO bank to list.
C!                                              Tidy write statements.
C!    Modified        :- J. Sedgbeer   92/01/30 Remove obsolete code
C!                                  (fixes for very old MC; IFCO bank)
C!    Input:
C!      commons:       /BCS/    for bank IDIG
C!                     /RCURNT/ run and event number
C!
C!    Output:
C!                     /ITTIME/ CPU time taken by IPREDA
C!
C!    called by : RPREDA
C!    calls     : ICRCOO (Alephlib) Create ITC coords from digis.
C!                ITCOPR            Print ITC coords.
C!                IPAKDI            Pack digits for POT output.
C!                IPSTAT            Prepare data statistics and histos.
C!                IDEADW            Simulate dead wires/channels
C!
C!    Libraries required: BOS, CERNLIB
C!
C! Prepare ITC data: Make coords from digitisings using Alephlib
C! ITC routines ICRCOO etc.
C!
C? Check for IDIG bank
C? If IDIG bank exists then
C?   If Galeph data then simulate dead wires/channels
C?   Create coordinates (ITCO and IDCR) from IDIG
C?   Add banks to BOS lists
C?   Create packed digis. bank PIDI for POT
C? Endif
C? Stats. and histos. for prepare data
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
#include "rflags.h"
C-----------------------------------------------------------------------
C I/O commons and parameters etc.
#include "bcs.h"
#include "rcurnt.h"
#include "ittime.h"
      EXTERNAL NAMIND
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
      IF( IFITTI ) CALL ALTIME ( TIME1 )
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        NIDIG = NAMIND('IDIG')
        NAJOB = NAMIND('AJOB')
      ENDIF
C
C Check that IDIG bank exists.
C If IDIG bank exists and Galeph input then simulate dead wires etc.
C
      KIDIG = IW(NIDIG)
      IF(KIDIG.EQ.0) GOTO 910
      IF(IW(NAJOB).GT.0) CALL IDEADW
C
C Create coords. from digitisings
C
      CALL ICRCOO(IER)
      GOTO (910,920,930,940) IER
C
C Add IDCR bank to list T, ITCO bank to E list.
C
      CALL BLIST(IW,'T+','IDCR')
      CALL BLIST(IW,'E+','ITCO')
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) THEN
        WRITE(LDEBRL,1003) IRUNRC,IEVTRC
 1003   FORMAT('0IPREDA:  Debug print: for run',I8,' event',I10)
        CALL IDIGPR
        CALL ITCOPR(0)
        KIDCR = IW(NAMIND('IDCR'))
        ND = IW(KIDCR+LMHROW)
        WRITE(LDEBRL,1005) (I,IW(KIDCR+LMHLEN+I),I=1,ND)
 1005   FORMAT('0IPREDA:  IDCR BANK:'//5('  Digi-Coord   ')/(5(2I6,3X)))
      ENDIF
C
C Create PIDI bank of packed digits. Add to E list.
C
      CALL IPAKDI(IER)
      IF(IER.EQ.0) CALL BLIST(IW,'E+','PIDI')
      IF(IER.EQ.2) GOTO 920
C
      GOTO 998
C-----------------------------------------------------------------------
C Errors
C
  910 CONTINUE
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.1) WRITE(LDEBRL,9010)
 9010 FORMAT(' IPREDA: No ITC digitising in input data')
      GOTO 998
C
  920 CONTINUE
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.3) WRITE(LDEBRL,9020)
 9020 FORMAT(' IPREDA: No room to create BOS banks.')
      CALL RERROR('IPREDA',2,'No room to create BOS banks.')
      GOTO 998
C
  930 CONTINUE
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.3) WRITE(LDEBRL,9030)
 9030 FORMAT(' IPREDA: ITC Digitisings bank empty')
      GOTO 998
C
  940 IF(FDEBRF.AND.JDBDRF(JULIT).GE.3) WRITE(LDEBRL,9040)
 9040 FORMAT(' IPREDA: No ITC coordinates found.')
      CALL RERROR('IPREDA',4,'No ITC coordinates found.')
C-----------------------------------------------------------------------
C Statistics and histograms
C
  998 CALL IPSTAT
C
C Note the total amount of CPU time used by IPREDA.
C
      IF( IFITTI ) CALL ALTIME ( TIME2 )
      TPREDA=TPREDA+(TIME2-TIME1)
C
      END
#endif
