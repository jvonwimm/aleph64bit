      SUBROUTINE PEIDTJ (LIST,IER)
C
C----------------------------------------------------------------------
C! Unpack  PEID bank into EIDT bank
C!
C!    Author:  D. PALLIN - 881129
C
C     Input :   LIST      /C    BOS event list
C                               if LIST(2:2).eq.'-' drop POT banks
C!
C!    Output:   IER       /I    Error return=0 if operation successful
C                                           -1 if OK but garbage coll.
C                                            2 if not enough space
C                                            1 POT bank missing
C!
C!    Input bank  : PEID
C!    Output bank : EIDT
C!
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JEIDIF=1,JEIDR1=2,JEIDR2=3,JEIDR3=4,JEIDR4=5,JEIDR5=6,
     +          JEIDR6=7,JEIDR7=8,JEIDEC=9,JEIDIP=10,JEIDE1=11,
     +          JEIDE2=12,JEIDE3=13,JEIDFR=14,JEIDPE=15,LEIDTA=15)
      PARAMETER(JPEIIF=1,JPEIR1=2,JPEIR2=3,JPEIR3=4,JPEIR4=5,JPEIR6=6,
     +          JPEIR7=7,JPEIEC=8,JPEIET=9,JPEIP1=10,JPEIP2=11,
     +          JPEIPF=12,LPEIDA=12)
      CHARACTER*(*) LIST, PLIST*4, JLIST*4
      PARAMETER(RAT=100./65534.)
      DATA NEIDT,NPEID/0,0/
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
C?
C?
      IF(NEIDT.EQ.0)THEN
        NEIDT=NAMIND('EIDT')
        NPEID=NAMIND('PEID')
        CALL BKFMT('EIDT','2I,(I,8F,I,3F,2I)')
      ENDIF
C?
C - bank PEID exist ?
C?
      JPEID=IW(NPEID)
      IER = 1
      IF (JPEID.EQ.0) GOTO 999
      IF (LROWS(JPEID).EQ.0) GOTO 999
C
C  create EIDT bank
C?
      LEN=LROWS(JPEID)*LEIDTA+LMHLEN
      CALL AUBOS('EIDT',0,LEN,JEIDT,IER)
      IF (IER.EQ.2) GOTO 999
      JLIST = 'EIDT'
      JPEID=IW(NPEID)
C?
C  fill  EIDT bank
C?
      IW(JEIDT+LMHCOL)=LEIDTA
      IW(JEIDT+LMHROW)=LROWS(JPEID)
      DO 1 NN=1,LROWS(JPEID)
         KPEID = KROW(JPEID,NN)
         KEIDT = KROW(JEIDT,NN)
C
      IFLIPP=IW(KPEID+JPEIIF)
      IF(IFLIPP.GE.10)THEN
         IPP=1
      ELSE
         IPP=0
      ENDIF
      IW(KEIDT+JEIDIP)=IPP
      IW(KEIDT+JEIDIF)=IFLIPP-IPP*10
C
      IBN=IW(KPEID+JPEIR1)
      RR1=0
      IF(IBN.EQ.128)RR1=1000.
      IF(ABS(IBN).EQ.127)RR1=999.*SIGN(1.,FLOAT(IBN))
      IF(ABS(RR1).LT.998 )RR1=FLOAT(IBN)/10.
      RW(KEIDT+JEIDR1)=RR1
C
      IBN=IW(KPEID+JPEIR2)
      RR2=0
      IF(IBN.EQ.128)RR2=1000.
      IF(ABS(IBN).EQ.127)RR2=999.*SIGN(1.,FLOAT(IBN))
      IF(ABS(RR2).LT.998 )RR2=FLOAT(IBN)/10.
      RW(KEIDT+JEIDR2)=RR2
C
      IBN=IW(KPEID+JPEIR3)
      RR3=0
      IF(IBN.EQ.128)RR3=1000.
      IF(ABS(IBN).EQ.127)RR3=999.*SIGN(1.,FLOAT(IBN))
      IF(ABS(RR3).LT.998 )RR3=FLOAT(IBN)/10.
      RW(KEIDT+JEIDR3)=RR3
C
      IBN=IW(KPEID+JPEIR4)
      RR4=0
      IF(IBN.EQ.128)RR4=1000.
      IF(ABS(IBN).EQ.127)RR4=999.*SIGN(1.,FLOAT(IBN))
      IF(ABS(RR4).LT.998 )RR4=FLOAT(IBN)/10.
      RW(KEIDT+JEIDR4)=RR4
C
      IBN=IW(KPEID+JPEIR6)
      RR6=0
      IF(IBN.EQ.128)RR6=1000.
      IF(ABS(IBN).EQ.127)RR6=999.*SIGN(1.,FLOAT(IBN))
      IF(ABS(RR6).LT.998 )RR6=FLOAT(IBN)/10.
      RW(KEIDT+JEIDR6)=RR6
C
      IBN=IW(KPEID+JPEIR7)
      RR7=0
      IF(IBN.EQ.128)RR7=1000.
      IF(ABS(IBN).EQ.127)RR7=999.*SIGN(1.,FLOAT(IBN))
      IF(ABS(RR7).LT.998 )RR7=FLOAT(IBN)/10.
      RW(KEIDT+JEIDR7)=RR7
C
      IBN=IW(KPEID+JPEIEC)
      IF(IBN.EQ.65535)ECC=1000.
      ECC=RAT*IBN
      RW(KEIDT+JEIDEC)=ECC
C
      IBN=IW(KPEID+JPEIET)
      IF(IBN.EQ.65535)ETT=1000.
      ETT=RAT*IBN
      IBN=IW(KPEID+JPEIP1)
      EP1=FLOAT(IBN)/255.
      IBN=IW(KPEID+JPEIP2)
      EP2=FLOAT(IBN)/255.
      RW(KEIDT+JEIDE1)=EP1*ETT
      RW(KEIDT+JEIDE2)=EP2*ETT
      RW(KEIDT+JEIDE3)=(1.-(EP1+EP2))*ETT
C
      IW(KEIDT+JEIDFR)=IW(KPEID+JPEIPF)
C
 1    CONTINUE
C?
 998  CONTINUE
C - get the drop flag if any, then drop POT banks if required,
C   add JUL banks to S-list
C   POT banks are on PLIST, JUL banks on JLIST
      PLIST = 'PEID'
C! add JLIST to S-list, drop PLIST if required
      IF (LNBLNK(LIST).EQ.2) THEN
         IF (LIST(2:2).EQ.'-' .AND. LNBLNK(PLIST).GE.4) THEN
            CALL BDROP (IW,PLIST)
            CALL BLIST (IW,LIST,PLIST(1:LNBLNK(PLIST)))
         ENDIF
      ENDIF
      CALL BLIST (IW,'S+',JLIST(1:LNBLNK(JLIST)))
C
C
      IF (IER .EQ. 1) IER = -1
C?
  999 CONTINUE
      RETURN
      END
