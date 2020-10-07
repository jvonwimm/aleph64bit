      SUBROUTINE FIXPCRL
CKEY  EVENT / INTERNAL
C----------------------------------------------------------------------
C!  - restore LCAL clusters in PCRL,PCOB for POT-POT proc. Julver < 262
C!
C!   Author   :- E. Lancon             10-JUN-1992
C!
C?   Modifications.
C!       1. Patrick Janot -- 13 June 1992:
C!          o Update PCOB as well as PCRL (# of calobjects)
C!          o Update the 10th row of the LCAL PECOs (Relation PECO/PCOB)
C!          o Fill the 1st row of PCRL for the new lines (calobject
C!            number)
C!       2. E. Lancon 15-JUN-1992
C!          o order in PCRL
C!
C!  Called from QMEVNT
C!======================================================================
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      INTEGER IW
      REAL RW(10000)
      COMMON /BCS/ IW(10000)
      EQUIVALENCE (RW(1),IW(1))
      PARAMETER(JPCRPC=1,JPCRPE=2,JPCRPF=3,JPCRPH=4,JPCRPP=5,LPCRLA=5)
      INTEGER JPECER,JPECE1,JPECE2,JPECTH,JPECPH,JPECEC,JPECKD,JPECCC,
     +          JPECRB,JPECPC,LPECOA
      PARAMETER(JPECER=1,JPECE1=2,JPECE2=3,JPECTH=4,JPECPH=5,JPECEC=6,
     +          JPECKD=7,JPECCC=8,JPECRB=9,JPECPC=10,LPECOA=10)
      INTEGER JRHAPN,JRHAPD,JRHAPH,JRHAPV,JRHAAV,JRHADV,JRHADD,JRHANI,
     +          JRHANO,JRHACV,JRHANU,LRHAHA
      PARAMETER(JRHAPN=1,JRHAPD=3,JRHAPH=4,JRHAPV=5,JRHAAV=6,JRHADV=7,
     +          JRHADD=8,JRHANI=9,JRHANO=10,JRHACV=11,JRHANU=12,
     +          LRHAHA=12)
      COMMON / JUVV / IJVER
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+LMHCOL)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+LMHROW)
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
C-----------------------------------------------------------------------
C
C   Do nothing if Julia version IJVER >= 262 and if :
C?                  - No PECO
C?                  - No LCAL cluster (region code=192) in PECO
C
      KPECO = IW(NAMIND('PECO'))
      IF (IJVER.LE.0.OR.IJVER.GE.262) GO TO 999
      IF (KPECO.LE.0) GOTO 999
      NPECO = LROWS (KPECO)
      IF (ITABL(KPECO,NPECO,JPECKD).NE.192) GOTO 999
C PAJ ----------------------
C PAJ : Initialize PCOB
      KPCOB = IW(NAMIND('PCOB'))
      NPCOB = 0
      IF ( KPCOB .GT. 0 ) NPCOB = LROWS (KPCOB)
C PAJ ----------------------
C
C?   one have LCAL cluster(s) in PECO check if they are in PCRL
C
      KPCRL = IW(NAMIND('PCRL'))
      IF ( KPCRL.GT.0 ) THEN
        NPCRL = LROWS (KPCRL)
        IF ( NPCRL.GT.0 ) THEN
          IPECO = ITABL(KPCRL,NPCRL,JPCRPE)
          IF ( IPECO.NE.NPECO ) THEN
C
C?   One has to DO something !!!
C?        - Count number of lcal cluster
C?        - enlarge PCRL by the corresponding number and fill PCRL
C
            IF (FIRST) WRITE (IW(6),'(///,4(/,10x,a),///)')
     &'+------------------------------------------------------------+',
     &'|      FIXPCRL called to Fix PCRL/PCOB Banks                 |',
     &'|  !!!! Output banks are not equal to Input banks !!!!       |',
     &'+------------------------------------------------------------+'
            FIRST = .FALSE.
            NLCAL = 0
            DO IPECO =  1, NPECO
              IF ( ITABL(KPECO,IPECO,JPECKD).EQ.192 ) THEN
                NLCAL = NLCAL + 1
C PAJ -----------------
C PAJ : Update PECO
                NPCOB = NPCOB + 1
                IW (KROW(KPECO,IPECO)+JPECPC) = NPCOB
C PAJ -----------------
              ENDIF
            ENDDO
            NROWS = LROWS(KPCRL) + NLCAL
            NTOT = NROWS*LCOLS(KPCRL) + LMHLEN
            CALL AUBOS ('PCRL', 0, NTOT, KPCRL, IGARB)
C PAJ -----------------
C PAJ : Update PCOB
            IW (KPCOB+LMHROW) = NPCOB
C PAJ -----------------
            IF ( IGARB.EQ.2 ) THEN
              WRITE (IW(6),*) ' **** FIXPCRL **** unable to fill PCRL '
              GOTO 999
            ENDIF
            KPECO = IW(NAMIND('PECO'))
            IW (KPCRL+LMHROW) = NROWS
            DO ILCAL  =  1, NLCAL
              IW (KROW(KPCRL,NPCRL+ILCAL)+JPCRPE) = NPECO - NLCAL +
     &          ILCAL
C PAJ -----------------
C PAJ : Update PCRL
              IW (KROW(KPCRL,NPCRL+ILCAL)+JPCRPC) = NPCOB - NLCAL +
     &          ILCAL
C PAJ -----------------
            ENDDO
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
