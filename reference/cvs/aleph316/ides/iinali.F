      SUBROUTINE IINALI(IRUN,IRET)
C-----------------------------------------------------------------------
C! Initialise ITC alignment constants.
C!
CKEY ITCDES ITC GEOM /INTERNAL
C!   Author     :-  J. Sedgbeer  88/05/05
C!   Modified   :-  J. Sedgbeer  89/08/06
C!   Modified   :-  J. Sedgbeer  92/02/04 Implement run-period
C!                               scheme for some dbase banks.
C!   Modified   :-  D. Casper    97/03/05  Handle new alignment banks
C!                                         INLI/TNOS as well as
C!                                         IALI/TPOS
C!
C!   Input:
C!    IRUN    /I : Current run number
C!    params.:    IALIJJ  for IALI bank
C!                INLIJJ  for INLI bank
C!                TPOSJJ  for TPOS bank
C!                TNOSJJ  for TNOS bank
C!    commons:    /BCS/  =>  banks  IALI and TPOS from 'DB'
C!
C!   Output:
C!    IRET    /I : Error flag: (as for AGETDB)
C!                   IRET > 0  Existing values still valid
C!                   IRET = 0  Error. One or more banks missing for this
C!                             run - leave values unchanged or, if first
C!                             time use default values.
C!                   IRET < 0  1 or more banks reloaded
C!    commons  /IALIGC/
C!             /IALIGG/
C!
C!   calls     : AGETDB (Alephlib)
C!               GTSTUP (Alephlib)
C!
C!   libraries:  BOS
C!
C!   Description:
C! Set up ITC alignment constants.
C! Get data from a direct access file ( filled from D.B.).
C! Input cards may be used to override the values in the d.a. file.
C! If no valid bank then use values already loaded into common or,
C! if first time, use default values.
C!
C? If data (run number > 2000) then
C?   run period = run number
C? else (MC)
C?   get run period from function GTSTUP
C?   if no set-up number found set run period = run number
C? endif
C?
C? Check validity of IALI bank - AGETDB
C? If (first and no IALI bank) then fill /IALIGC/ with default values
C? If (first and existing IALI bank still valid) or (new IALI bank) then
C?    get values from IALI bank. Fill /IALIGC/
C? Endif
C? Check validity of TPOS bank - AGETDB
C? If (first and no TPOS bank) then fill /IALIGG/ with default values
C? If (first and existing TPOS bank still valid) or (new TPOS bank) then
C?    get values from TPOS bank. Fill /IALIGG/
C?  Endif
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C I/O commons etc.
#include "bcs.h"
#include "ialijj.h"
#include "inlijj.h"
#include "ialigc.h"
#include "tposjj.h"
#include "tnosjj.h"
#include "ialigg.h"
C-----------------------------------------------------------------------
      EXTERNAL AGETDB,NAMIND,GTSTUP
      INTEGER AGETDB,NAMIND,GTSTUP
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA EU1,EU2,EU3/3*0./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
      IF (FIRST) THEN
        NTOAL = NAMIND('TOAL')
        NIALI = NAMIND('IALI')
        NINLI = NAMIND('INLI')
        NTPOS = NAMIND('TPOS')
        NTNOS = NAMIND('TNOS')
      ENDIF
C
C Set run-period for data/MC
C
      IF(IRUN.GT.2000) THEN
        IRUNP = IRUN
      ELSE
        IRUNP = GTSTUP('IT',IRUN)
C If no setup then just keep run number => pick up dbase bank number 1
        IF(IRUNP.EQ.-1) THEN
          IRUNP = IRUN
        ENDIF
      ENDIF
C
C Check for banks to use and their validity
C
      IF (IW(NTOAL).EQ.0) THEN
        IRETIN = AGETDB('INLI',IRUNP)
        IRETGN = AGETDB('TNOS',IRUNP)
      ELSE
        IRETIN = 0
        IRETGN = 0
      ENDIF
      IF (IRETIN.EQ.0 .OR. IRETGN.EQ.0) THEN
        IRETIO = AGETDB('IALI',IRUNP)
        IRETGO = AGETDB('TPOS',IRUNP)
        IF (IRETIO.NE.0 .AND. IRETGO.NE.0) THEN
            IRETI = IRETIO
            IRETG = IRETGO
            CALL BDROP(IW,'INLITNOS')
        ELSE
            IRETI = 0
            IRETG = 0
            CALL BDROP(IW,'IALIINLITNOSTPOS')
        ENDIF
      ELSE
        IRETI = IRETIN
        IRETG = IRETGN
        CALL BDROP(IW,'IALITPOS')
      ENDIF
      KIALI = IW(NIALI)
      KTPOS = IW(NTPOS)
      KINLI = IW(NINLI)
      KTNOS = IW(NTNOS)
C
C If first call and no bank for this run then fill /IALIGC/ with default
C                                                        values.
      IF(FIRST.AND.IRETI.EQ.0) THEN
        DXYZIA(1) = 0.
        DXYZIA(2) = 0.
        DXYZIA(3) = 0.
        ROTNIA(1,1) =  1.
        ROTNIA(1,2) =  0.
        ROTNIA(1,3) =  0.
        ROTNIA(2,1) =  0.
        ROTNIA(2,2) =  1.
        ROTNIA(2,3) =  0.
        ROTNIA(3,1) =  0.
        ROTNIA(3,2) =  0.
        ROTNIA(3,3) =  1.
      ENDIF
C
C Get values from new IALI or INLI bank. Fill /IALIGC/
C
      IF((FIRST.AND.IRETI.GT.0).OR.(IRETI.LT.0)) THEN
        IF (KINLI.GT.0) THEN
            DXYZIA(1) = RTABL(KINLI,1,JINLDX)
            DXYZIA(2) = RTABL(KINLI,1,JINLDX+1)
            DXYZIA(3) = RTABL(KINLI,1,JINLDX+2)
            EU1       = RTABL(KINLI,1,JINLDR)
            EU2       = RTABL(KINLI,1,JINLDR+1)
            EU3       = RTABL(KINLI,1,JINLDR+2)
        ELSE
            DXYZIA(1) = RTABL(KIALI,1,JIALDX)
            DXYZIA(2) = RTABL(KIALI,1,JIALDX+1)
            DXYZIA(3) = RTABL(KIALI,1,JIALDX+2)
            EU1       = RTABL(KIALI,1,JIALDR)
            EU2       = RTABL(KIALI,1,JIALDR+1)
            EU3       = RTABL(KIALI,1,JIALDR+2)
        ENDIF
        C1        = COS(EU1)
        C2        = COS(EU2)
        C3        = COS(EU3)
        S1        = SIN(EU1)
        S2        = SIN(EU2)
        S3        = SIN(EU3)
        ROTNIA(1,1) =  C3*C2*C1 - S3*S1
        ROTNIA(1,2) =  C3*C2*S1 + S3*C1
        ROTNIA(1,3) = -C3*S2
        ROTNIA(2,1) = -S3*C2*C1 - C3*S1
        ROTNIA(2,2) = -S3*C2*S1 + C3*C1
        ROTNIA(2,3) =  S3*S2
        ROTNIA(3,1) =     S2*C1
        ROTNIA(3,2) =     S2*S1
        ROTNIA(3,3) =     C2
      ENDIF
C
C If first call and no bank for this run then fill /IALIGG/ with default
C                                                        values.
      IF(FIRST.AND.IRETG.EQ.0) THEN
        GTRNIA(1) = 0.
        GTRNIA(2) = 0.
        GTRNIA(3) = 0.
        GROTIA(1,1) =  1.
        GROTIA(1,2) =  0.
        GROTIA(1,3) =  0.
        GROTIA(2,1) =  0.
        GROTIA(2,2) =  1.
        GROTIA(2,3) =  0.
        GROTIA(3,1) =  0.
        GROTIA(3,2) =  0.
        GROTIA(3,3) =  1.
      ENDIF
C
C Get values from new TPOS or TNOS bank. Fill /IALIGG/
C
      IF((FIRST.AND.IRETG.GT.0).OR.(IRETG.LT.0)) THEN
        IF (KTNOS.GT.0) THEN
          DO I=1,3
            GTRNIA(I)=RTABL(KTNOS,1,JTNOTL-1+I)
          ENDDO
          THE=RTABL(KTNOS,1,JTNORT)
          DEL=RTABL(KTNOS,1,JTNORT+1)
          PHI=RTABL(KTNOS,1,JTNORT+2)
        ELSE
          DO I=1,3
            GTRNIA(I)=RTABL(KTPOS,1,JTPOTL-1+I)
          ENDDO
          THE=RTABL(KTPOS,1,JTPORT)
          DEL=RTABL(KTPOS,1,JTPORT+1)
          PHI=RTABL(KTPOS,1,JTPORT+2)
        ENDIF
        GROTIA(1,1)= COS(DEL)*COS(PHI)
        GROTIA(1,2)=-COS(DEL)*SIN(PHI)
        GROTIA(1,3)= SIN(DEL)
        GROTIA(2,1)= SIN(THE)*SIN(DEL)*COS(PHI) + COS(THE)*SIN(PHI)
        GROTIA(2,2)= COS(THE)*COS(PHI) - SIN(THE)*SIN(DEL)*SIN(PHI)
        GROTIA(2,3)=-SIN(THE)*COS(DEL)
        GROTIA(3,1)= SIN(THE)*SIN(PHI) - COS(THE)*SIN(DEL)*COS(PHI)
        GROTIA(3,2)= SIN(THE)*COS(PHI) + COS(THE)*SIN(DEL)*SIN(PHI)
        GROTIA(3,3)= COS(THE)*COS(DEL)
      ENDIF
C
      IRET = -1
      IF(IRETI.GT.0.AND.IRETG.GT.0) IRET = 1
      IF(IRETI.EQ.0 .OR.IRETG.EQ.0) IRET = 0
      FIRST = .FALSE.
C
      END
#endif
