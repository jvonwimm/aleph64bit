      SUBROUTINE FYTOKI
C-------------------------------------------------------
CKEY FYXX  / INTERNAL
C - J. Hilgart 14/06/88
C                         modified by - F.Ranjard - 881107
C                                       B.Bloch   - 901010
C! Convert from FVER,FKIN,FPOL,FZFR to KINE,VERT,KHIS,KVOL,KPOL,KZFR
C  drop banks FKIN, FVER ,FPOL,FZFR
C  add KINE, VERT, KVOL, KHIS ,KPOL,KZFR to the E-list
C - Calls: AUBOS                        from ALEPHLIB.HLB
C
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFKIPX=1,JFKIPY=2,JFKIPZ=3,JFKIMA=4,JFKIPA=5,JFKIOV=6,
     +          JFKIEV=7,JFKIHC=8,LFKINA=8)
      PARAMETER(JFVEVX=1,JFVEVY=2,JFVEVZ=3,JFVETO=4,JFVEIP=5,JFVEIS=6,
     +          JFVENS=7,JFVEVN=8,JFVEVM=9,LFVERA=9)
      PARAMETER(JFPOIP=1,JFPOIS=2,LFPOIA=2)
      PARAMETER(JFPOKI=1,JFPOHX=2,JFPOHY=3,JFPOHZ=4,LFPOLA=4)
CD FLCOJJ
      PARAMETER(JFLCIL=1,LFLCOA=1)
CD FLTRJJ
      PARAMETER(JFLTIL=1,LFLTRA=1)
CD FSCOJJ
      PARAMETER(JFSCEN=1,JFSCIH=2,JFSCE1=3,JFSCE2=4,JFSCE3=5,JFSCH1=6,
     +          JFSCH2=7,JFSCTH=8,JFSCPH=9,JFSCIP=10,LFSCOA=10)
CD FSTRJJ
      PARAMETER(JFSTPX=1,JFSTPY=2,JFSTPZ=3,JFSTIQ=4,JFSTIH=5,JFSTCH=6,
     +          JFSTNH=7,JFSTD0=8,JFSTPH=9,JFSTZ0=10,JFSTAV=11,
     +          JFSTS2=12,JFSTNW=13,JFSTM2=14,JFSTNM=15,JFSTNC=16,
     +          JFSTMC=17,JFSTIC=18,LFSTRA=18)
CD FTCMJJ
      PARAMETER(JFTCIP=1,LFTCMA=1)
CD FTOCJJ
      PARAMETER(JFTOIP=1,LFTOCA=1)
CD FTTMJJ
      PARAMETER(JFTTIP=1,LFTTMA=1)
CKEY KINE KINGAL HAC
      PARAMETER(JKEVRN=1,JKEVNT=2,JKEVNV=3,JKEVPI=4,JKEVWT=5,JKEVSR=6,
     +          JKEVTR=7,LKEVHA=7)
      PARAMETER(JKHIHC=1,LKHISA=1)
      PARAMETER(JKJOJD=1,JKJOJT=2,JKJOAV=3,JKJODV=4,JKJODC=5,LKJOBA=5)
      PARAMETER(JKLIGN=1,LKLINA=1)
      PARAMETER(JKPOKI=1,JKPOHX=2,JKPOHY=3,JKPOHZ=4,LKPOLA=4)
      PARAMETER(JKRUGI=1,JKRUNO=2,JKRURT=3,JKRUFS=15,JKRUSS=16,
     +          LKRUNA=16)
      PARAMETER(JKVOVN=1,JKVOVM=2,LKVOLA=2)
      PARAMETER (LHVER=3,LPVER=5,LHKIN=3,LPKIN=5)
      DATA IONC /0/
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
C ============================================================
      IF (IONC.EQ.0) THEN
         NFKIN = NAMIND('FKIN')
         NFVER = NAMIND('FVER')
         NFZFR = NAMIND('FZFR')
         NFPOL = NAMIND('FPOL')
         NKEVH = NAMIND('KEVH')
         IONC = 1
         CALL BKFMT ('VERT','3I,4F,(I)')
         CALL BKFMT ('KINE','3I,4F,(I)')
         CALL BKFMT ('KVOL','2I,(A)')
         CALL BKFMT ('KHIS','I')
         CALL BKFMT ('KPOL','2I,(I,3F)')
         CALL BKFMT ('KZFR','2I,(F)')
      ENDIF
      JFKIN = IW(NFKIN)
      JFVER = IW(NFVER)
      JKEVH = IW(NKEVH)
      IF (JFKIN.EQ.0.OR.JFVER.EQ.0) GO TO 999
C Store vertices

      DO 10 IV = 1, LROWS(JFVER)
         KFVER = KROW(JFVER,IV)
         IOF = IW(KFVER + JFVEIS)
         NOUT = IW(KFVER + JFVENS)
         CALL AUBOS('VERT',IV,LHVER+LPVER+NOUT,JVERT,IGARB)
         IF (JVERT.EQ.0) GO TO 999
         IF (IGARB.NE.0) THEN
            JFVER = IW(NFVER)
            KFVER = KROW(JFVER,IV)
            JFKIN = IW(NFKIN)
         ENDIF
         IW(JVERT + 1) = LHVER
         IW(JVERT + 2) = LPVER
         IW(JVERT + 3) = NOUT
         CALL UCOPY (RW(KFVER+1),RW(JVERT+LHVER+1),4)
         IW(JVERT+LHVER+5) = IW(KFVER+JFVEIP)
         DO 12 IT = 1,NOUT
            IW(JVERT+LHVER+LPVER+IT) = IOF + IT
   12    CONTINUE
   10 CONTINUE
C
C Store vertex volume names
      LE = LROWS(JFVER)*LKVOLA + LMHLEN
      CALL AUBOS('KVOL',0,LE,JKVOL,IGARB)
      IF (JKVOL.EQ.0) GO TO 999
      IF (IGARB.NE.0)  JFVER = IW(NFVER)
      IW(JKVOL+LMHCOL) = LKVOLA
      IW(JKVOL+LMHROW) = LROWS(JFVER)
      KKVOL = JKVOL + LMHLEN
      DO 20 IV = 1, LROWS(JFVER)
         IW(KKVOL+JKVOVN) = ITABL(JFVER,IV,JFVEVN)
         IF (LCOLS(JFVER).GE.JFVEVM) THEN
            IW(KKVOL+JKVOVM) = ITABL(JFVER,IV,JFVEVM)
         ELSE
            IW(KKVOL+JKVOVM) = INTCHA ('    ')
         ENDIF
 20   KKVOL = KKVOL + LCOLS(JKVOL)
C
C Store particles
      IPAR = LROWS(JFKIN)
      DO 30   IP = 1, IPAR
         KFKIN = KROW(JFKIN,IP)
         IPART = IW(KFKIN+JFKIPA)
         IVOUT = IW(KFKIN+JFKIEV)
         IVOR = IW(KFKIN+JFKIOV)
         NVX = 1
         IF (IVOUT .GT. 0) NVX = 2
         CALL AUBOS('KINE',IP,LHKIN+LPKIN+NVX,JKINE,IGARB)
         IF(JKINE.EQ.0) GO TO 999
         IF (IGARB.NE.0) THEN
            JFKIN = IW(NFKIN)
            KFKIN = KROW(JFKIN,IP)
         ENDIF
         IW(JKINE + 1) = LHKIN
         IW(JKINE + 2) = LPKIN
         IW(JKINE + 3) = NVX
         CALL UCOPY(RW(KFKIN+1),RW(JKINE+LHKIN+1),4)
         IW(JKINE+LHKIN+5) = IPART
         IW(JKINE+LHKIN+LPKIN+1) = IVOR
         IF (NVX.EQ.2) IW(JKINE+LHKIN+LPKIN+2) = IVOUT
   30 CONTINUE
C
C Store history information for the tracks produced by ev. gen.
      IF (JKEVH .GT. 0) THEN
         NITRK = ITABL(JKEVH,1,JKEVNT)
      ELSE
         NITRK = LROWS(JFKIN)
      ENDIF
      LE = NITRK + LMHLEN
      CALL AUBOS('KHIS',0,LE,JKHIS,IGARB)
      IF (JKHIS.EQ.0) GO TO 999
      IF (IGARB.NE.0) JFKIN = IW(NFKIN)
      IW(JKHIS+LMHCOL) = LKHISA
      IW(JKHIS+LMHROW) = NITRK
      KKHIS = JKHIS + LMHLEN
      DO 40   IP = 1, NITRK
         IW(KKHIS+1) = ITABL(JFKIN,IP,JFKIHC)
 40   KKHIS = KKHIS + LCOLS(JKHIS)
C
C Store fragmentation information for the tracks produced by ev. gen.
C and polarisation as well
      CALL BSWAP(IW,'FZFR','KZFR')
      CALL BSWAP(IW,'FPOL','KPOL')
C
C - Add new banks to the E-list
C
      CALL BLIST (IW,'E+','KINEVERTKVOLKHISKPOLKZFR')
C
C - Drop old banks
C
      CALL BDROP (IW,'FKINFVERFPOLFZFR')
      CALL BLIST (IW,'E-','FKINFVERFPOLFZFR')
C
  999 RETURN
      END
