      FUNCTION GTT0GL (IRUN)
C -----------------------------------------------------
CKEY ALEF GET T0GL
C! Get T0 depending on run number and number of bunches
C - F.Ranjard - 920924
C               940323 : T0GL depends on setup code
C - Input   : IRUN    / I = run number
C - Output  : GTT0GL  / R = T0 or 0. in case of error
C ----------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JT0GID=1,JT0GVR=2,JT0GGT=4,JT0GA1=5,JT0GA2=6,JT0GA3=7,
     +          JT0GAW=8,JT0GOF=9,LT0GLA=9)
      PARAMETER(JRLELE=1,JRLELB=2,JRLELD=3,JRLELF=4,JRLELP=5,LRLEPA=5)
      SAVE T0, OF
      INTEGER AGETDB,GTSTUP
      DATA NT0GL, NRLEP, IRLST /3*0/
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
C -----------------------------------------------------
C - 1st entry
      IF (NT0GL.EQ.0) THEN
         NT0GL = NAMIND('T0GL')
         NRLEP = NAMIND('RLEP')
      ENDIF
C
C - next entry
      IF (IRUN.NE.IRLST) THEN
         IRLST = IRUN
         IF (IRUN.LE.2000) THEN
            ITP = GTSTUP ('TP',IRUN)
         ELSE
            ITP = IRUN
         ENDIF
         IER = AGETDB ('T0GL',-ITP)
         JT0GL = IW(NT0GL)
         IF (JT0GL.EQ.0) THEN
            T0 = 0.
            OF = 0.
            GTT0GL = 0.
            GOTO 999
         ELSE
            T0 = RTABL(JT0GL,1,JT0GGT)
            IF (LCOLS(JT0GL).GE.JT0GOF) THEN
               OF = RTABL(JT0GL,1,JT0GOF)
            ELSE
               OF = 0.
            ENDIF
         ENDIF
      ENDIF
C
      JRLEP = IW(NRLEP)
      IF (JRLEP.EQ.0) THEN
         GTT0GL = T0
      ELSE
         IF (ITABL(JRLEP,1,JRLELD).EQ.8) THEN
            GTT0GL = T0 - OF
         ELSE
            GTT0GL = T0
         ENDIF
      ENDIF
C
 999  CONTINUE
      END
