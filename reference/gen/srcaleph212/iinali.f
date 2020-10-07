      SUBROUTINE IINALI(IRUN,IRET)
C-----------------------------------------------------------------------
C! Initialise ITC alignment constants.
C!
CKEY ITCDES ITC GEOM /INTERNAL
C!   Author     :-  J. Sedgbeer  88/05/05
C!   Modified   :-  J. Sedgbeer  89/08/06
C!   Modified   :-  J. Sedgbeer  92/02/04 Implement run-period
C!                               scheme for some dbase banks.
C!
C!   Input:
C!    IRUN    /I : Current run number
C!    params.:    IALIJJ  for IALI bank
C!                TPOSJJ  for TPOS bank
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
      SAVE
C I/O commons etc.
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JIALID=1,JIALVR=2,JIALDX=4,JIALDR=7,JIALIS=10,LIALIA=10)
      PARAMETER (JXYZIA=3)
      COMMON/IALIGC/ROTNIA(JXYZIA,JXYZIA),DXYZIA(JXYZIA)
      PARAMETER(JTPOID=1,JTPOVR=2,JTPOTL=4,JTPORT=7,JTPOIT=10,JTPOTC=11,
     +          LTPOSA=11)
      PARAMETER (JGTRIA=3)
      COMMON/IALIGG/GROTIA(JGTRIA,JGTRIA),GTRNIA(JGTRIA)
C-----------------------------------------------------------------------
      EXTERNAL AGETDB,NAMIND,GTSTUP
      INTEGER AGETDB,NAMIND,GTSTUP
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA EU1,EU2,EU3/3*0./
C-----------------------------------------------------------------------
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C-----------------------------------------------------------------------
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
C Check for validity of IALI bank.
C
      IRETI = AGETDB('IALI',IRUNP)
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
C Get values from new IALI bank. Fill /IALIGC/
C
      IF((FIRST.AND.IRETI.GT.0).OR.(IRETI.LT.0)) THEN
        KIALI     = IW(NAMIND('IALI'))
        DXYZIA(1) = RTABL(KIALI,1,JIALDX)
        DXYZIA(2) = RTABL(KIALI,1,JIALDX+1)
        DXYZIA(3) = RTABL(KIALI,1,JIALDX+2)
        EU1       = RTABL(KIALI,1,JIALDR)
        EU2       = RTABL(KIALI,1,JIALDR+1)
        EU3       = RTABL(KIALI,1,JIALDR+2)
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
C Check for validity of TPOS bank.
C
      IRETG = AGETDB('TPOS',IRUNP)
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
C Get values from new TPOS bank. Fill /IALIGG/
C
      IF((FIRST.AND.IRETG.GT.0).OR.(IRETG.LT.0)) THEN
        KTPOS = IW(NAMIND('TPOS'))
        DO 10 I=1,3
          GTRNIA(I)=RTABL(KTPOS,1,JTPOTL-1+I)
   10   CONTINUE
        THE=RTABL(KTPOS,1,JTPORT)
        DEL=RTABL(KTPOS,1,JTPORT+1)
        PHI=RTABL(KTPOS,1,JTPORT+2)
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
