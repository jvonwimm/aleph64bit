      SUBROUTINE SIEDIR(SIARM,SIBHA)
C----------------------------------------------------------------------
CKEY EDIR CLASS SICAL
C! Compute EDIR class from SICAL
C-
C   Input  : None
C   Output : SIARM = Class 23 logical flag  single arm  A or B
C            SIBHA = Class 22 logical flag  coincidence A and B
C-
C   Called by   : SELEVT
C   Calls  : None
C   Input banks : SCLS
C-
C                                   Author: B.Bloch-Devaux September 92
C----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JSCLEC=1,JSCLRC=2,JSCLZC=3,JSCLPC=4,JSCLTC=5,JSCLWP=6,
     +          JSCLWR=7,JSCLCR=8,JSCLCP=9,JSCLCT=10,JSCLR3=11,
     +          JSCLE3=14,JSCLE4=17,JSCLOV=20,JSCLSP=21,LSCLSA=21)
      LOGICAL SIARM,SIBHA
      DATA NSCLS /0/
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
C ------------------------------------------------------------------
      SICALA = 0.
      SICALB = 0.
      SIARM = .FALSE.
      SIBHA = .FALSE.
C --
      IF (NSCLS.EQ.0) NSCLS = NAMIND('SCLS')
      KSCLS=IW(NSCLS)
      IF(KSCLS.LE.0) GOTO 999
C --
C   Loop on SCAL clusters and get separately the energy
C   of the SCAL sides A and B
C --
      NCLU  = LROWS(KSCLS)
      DO 40 NLT = 1,NCLU
        ZCLU = RTABL(KSCLS,NLT,JSCLZC)
        IF ( ZCLU.GT.0.) THEN
           SICALA = SICALA+RTABL(KSCLS,NLT,JSCLEC)
        ELSE IF ( ZCLU.LT.0.) THEN
           SICALB = SICALB+RTABL(KSCLS,NLT,JSCLEC)
        ENDIF
   40 CONTINUE
C --
      IF(SICALA.GT.20 .AND. SICALB.GT.20.) SIBHA = .TRUE.
      IF(SICALA.GT.20 .OR.  SICALB.GT.20.) SIARM = .TRUE.
C --
  999 RETURN
      END
