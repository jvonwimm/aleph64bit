      SUBROUTINE IINTRK
C-----------------------------------------------------------------------
C! Initialise constants and parameters needed for ITC tracking.
C!
C!    Author      :- J. Sedgbeer 87/02/09
C!    Modified    :- I. Tomalin  88/08/25
C!    Modified    :- J. Sedgbeer 90/01/17 Get consts. for 'refine' stage
C!                                        Change ALGTDB to AGETDB
C!    Modified    :- J. Carr 91/02/18 Change contstants for refin
C!  Input:
C!     commons:  /RCONDS/  (for FIELRC)
C!               /RCURNT/  for current run number
C!               /BCS/
C!     banks  :  ITKP from 'DB'
C!     params.:  ITKPJJ  for ITKP bank
C!
C!   Output:
C!     fill common /ITRKCC/
C!                 /IASIGC/
C!
C!   called by : IINIRU
C!   calls     : AGETDB (Alephlib)
C!
C!   libraries:  BOS
C!
C!   Description:
C! Set up constants and parameters needed in the reconstruction program.
C! Get data from direct access file (filled from D.B.).
C! Input cards may be used to override the values in the d.a. file.
C!
C? IINTRK
C?
C? Check validity of ITKP bank - AGETDB
C? If no ITKP bank then
C?    Fatal report
C? Endif
C? If first time or ITKP bank has changed then
C?    get values from ITKP bank.
C? Endif
C?
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C Input commons and params.
#include "bcs.h"
#include "rcurnt.h"
#include "rconds.h"
#include "itkpjj.h"
C-----------------------------------------------------------------------
C Output commons
#include "itrkcc.h"
#include "iasigc.h"
#include "bosext.h"
C-----------------------------------------------------------------------
C Local arrays, data etc.
      EXTERNAL AGETDB
      INTEGER AGETDB
      REAL SMAX,ADJC
      INTEGER NHMN,NCMN(2),NSMN(2)
      LOGICAL FIRST
      DATA SMAX/3.0/, ADJC/0.85/, XISF/8.0/,FJUNC/1.0/
      DATA NHMN/4/, NCMN/2,4/, NSMN/0,0/
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C Get ILIV - live wire bank
      CALL ILIVEW('C+',IER)
C Check for validity of ITKP bank.
C
      IRETT = AGETDB('ITKP',IRUNRC)
      IF(IRETT.EQ.0) THEN
        CALL RERROR('IINTRK',-1,'Dbase bank ITKP missing')
      ENDIF
C
C Get values from ITKP bank. Fill /ITRKCC/
C
      IF(FIRST.OR.IRETT.LT.0) THEN
        KITKP = IW(NAMIND('ITKP'))
C
        RSCAIT = RTABL(KITKP,1,JITKRS)
        X0     = RTABL(KITKP,1,JITKRL)
        FRHOIT = 0.3*ABS(FIELRC)*0.001
        RLX0   = 1.0 + LOG10(X0)/9.
        FSCAIT = 0.0141*SQRT(X0)*RLX0
C
        IF(FDEBRF.AND.JDBDRF(JULIT).GE.3) THEN
          WRITE(LDEBRL,1000) FRHOIT,RSCAIT,FSCAIT
 1000     FORMAT(1H0,'IINTRK: ITC Tracking Parameters and Constants'/
     +     5X,' factor rho -> pt      ',F8.5,/
     +     5X,' Scattering radius(cm.)',F8.2,/
     +     5X,' Scattering factor     ',F8.3/)
        ENDIF
      ENDIF
C
C Check for validity of IRFC bank.
C
      IRETR = AGETDB('IRFC',IRUNRC)
C
C If first call and no bank for this run then fill /IASIGC/ with default
C                                                        values.
      IF(FIRST.AND.IRETR.EQ.0) THEN
        SMAXIA = SMAX
        ADJCIA = ADJC
        NHMNIA = NHMN
        XISFIL = XISF
        FJUNCU = FJUNC
        NCMNIA(1) = NCMN(1)
        NCMNIA(2) = NCMN(2)
        NSMNIA(1) = NSMN(1)
        NSMNIA(2) = NSMN(2)
      ENDIF
C
C Get values from new IRFC bank.  Fill /IASIGC/
C
      IF((FIRST.AND.IRETR.GT.0).OR.(IRETR.LT.0)) THEN
        KIRFC  = IW(NAMIND('IRFC'))
        SMAXIA = RTABL(KIRFC,1,4)
        ADJCIA = RTABL(KIRFC,1,5)
        NHMNIA = ITABL(KIRFC,1,6)
        NCMNIA(1) = ITABL(KIRFC,1,7)
        NCMNIA(2) = ITABL(KIRFC,1,8)
        NSMNIA(1) = ITABL(KIRFC,1,9)
        NSMNIA(2) = ITABL(KIRFC,1,10)
      ENDIF
C
      FIRST = .FALSE.
C
      END
#endif
