       SUBROUTINE KXLUCO (LUPAR)
C -----------------------------------------------------------------
C - M.Reyrolle - 860820                   F.Ranjard - 871114
C! Set LUND parameters by data cards
CKEY KINE KINGAL LUND DECAY  /  USER INTERNAL
C  Every LUND parameter is a BOS data card keyword, the index of the
C  parameter is the bank # .
C  the list of keywords with their format is given below:
C   'MST'  (I), 'PAR'  (F), 'KTYP' (I), 'PMAS' (F), 'PWID' (F),
C   'CFR'  (F), 'DPAR' (F), 'IDB '  (I), 'CBR'  (F), 'KDP'  (I),
C   'MSTE' (I), 'PARE' (F)
C
C    KEY  i  /  ival     ====>  KEY(i) = ival
C    RKEY i  /  value    ====>  RKEY(i) = value
C  special treatment for KEY = IDB :
C    IDB  i  /  'ON'     ====>  IDB(i) stays unchanged : decay is allowe
C    IDB  i  /  'OFF'    ====>  IDB(i) = 0 : decay is inhibited
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KXLUCO
C              External References: NAMIND/BKFMT/BLIST(BOS77)
C                                   KXLUBR (this Lib)
C              Comdecks referenced: BCS,LUNDCOM
C
C - usage   : CALL KXLUCO (LUPAR)
C - input   : LUPAR = # of read data cards
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)
     &                , KDPLU3(L3KDP)
      COMMON /LUDATE/   MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
C
      PARAMETER (LKEYS=12)
      CHARACTER*4 KEY(LKEYS), CHAINT
      CHARACTER*1 FMT(LKEYS)
C
      DATA KEY / 'MST' , 'PAR' ,'KTYP', 'PMAS', 'PWID', 'CFR' , 'DPAR'
     &         , 'IDB ' , 'CBR' , 'KDP', 'MSTE', 'PARE' /
      DATA FMT / 'I','F','I','F','F', 'F', 'F','I','F', 'I', 'I','F' /
C -------------------------------------------------------------------
C
      LUPAR = 0
      DO 40 I=1,LKEYS
         NAMI = NAMIND (KEY(I))
         IF (IW(NAMI) .EQ. 0) GOTO 40
         KIND = NAMI + 1
  15     KIND = IW(KIND-1)
            IF (KIND .EQ. 0) GOTO 39
            LUPAR = LUPAR + 1
            J = IW(KIND-2)
            GOTO (21,22,23,24,25,26,27,28,29,30,31,32) I
 21         MSTLU1(J) = IW(KIND+1)
         GOTO 15
 22         PARLU1(J) = RW(KIND+1)
         GOTO 15
 23         KTYPL2(J) = IW(KIND+1)
         GOTO 15
 24         PMASL2(J) = RW(KIND+1)
         GOTO 15
 25         PWIDL2(J) = RW(KIND+1)
         GOTO 15
 26         CFRLU2(J) = RW(KIND+1)
         GOTO 15
 27         DPARL3(J) = RW(KIND+1)
         GOTO 15
 28         IF (CHAINT(IW(KIND+1)) .EQ. 'OFF') THEN
               IDBLU3(J) = 0
            ENDIF
         GOTO 15
 29         CBRLU3(J) = RW(KIND+1)
         GOTO 15
 30         KDPLU3(J) = IW(KIND+1)
         GOTO 15
 31         MSTELE(J) = IW(KIND+1)
         GOTO 15
 32         PARELE(J) = RW(KIND+1)
         GOTO 15
 39      CONTINUE
         CALL BKFMT (KEY(I),FMT(I))
         CALL BLIST (IW,'C+',KEY(I))
 40   CONTINUE
C
C     look for more modifications in the decay schemes
C
      CALL KXLUBR
      RETURN
      END
