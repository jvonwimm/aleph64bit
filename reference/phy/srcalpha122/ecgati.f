      SUBROUTINE ECGATI(NRUN)
C ---------------------------------------------------------------------
C - Author: M.N Minard          940121
C!    Fix Gampek photon
C     Called from FIXGAEN
C-    For all processing JULIA before ALEPHLIB 156
C-    find Gain correction either from ECMC or Bank file
C ---------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      INTEGER IW
      REAL RW(10000)
      COMMON /BCS/ IW(10000)
      EQUIVALENCE (RW(1),IW(1))
C------------------ /QCNAMI/ --- name indices -------------------------
      COMMON /QCNAMI/ NAAFID,NAAJOB,NAASEV,NABCNT,NABHIT,NABOMB,
     1 NABOME,NABOMP,NABOMR,NABPTR,NADCAL,NADCRL,NADECO,NADEID,NADENF,
     2 NADEVT,NADEWI,NADFMC,NADFOT,NADGAM,NADHCO,NADHEA,NADHRL,NADJET,
     3 NADMID,NADMJT,NADMUO,NADNEU,NADPOB,NADRES,NADTBP,NADTMC,NADTRA,
     4 NADVER,NADVMC,NAECRQ,NAECTE,NAEGID,NAEGPC,NAEIDT,NAEJET,NAERRF,
     5 NAETDI,NAETKC,NAEVEH,NAEWHE,NAFICL,NAFKIN,NAFPOI,NAFPOL,NAFRFT,
     6 NAFRID,NAFRTL,NAFSTR,NAFTCL,NAFTCM,NAFTOC,NAFTTM,NAFVCL,NAFVER,
     7 NAFZFR,NAHCCV,NAHCTE,NAHINF,NAHMAD,NAHPDI,NAHROA,NAHSDA,NAHTUB,
     8 NAIASL,NAIPJT,NAIRJT,NAITCO,NAITMA,NAIXTR,NAIZBD,NAJBER,NAJEST,
     9 NAJSUM,NAKEVH,NAKINE,NAKJOB,NAKLIN,NAKPOL,NAKRUN,NALIDT,NALOLE,
     A NALUPA,NAMCAD,NAMHIT,NAMTHR,NAMUDG,NAMUEX,NAOSTS,NAPART,NAPASL,
     B NAPCHY,NAPCOB,NAPCOI,NAPCPA,NAPCQA,NAPCRL,NAPECO,NAPEHY,NAPEID,
     C NAPEMH,NAPEPT,NAPEST,NAPEWI,NAPFER,NAPFHR,NAPFRF,NAPFRT,NAPHCO,
     D NAPHER,NAPHHY,NAPHMH,NAPHST,NAPIDI,NAPITM,NAPLID,NAPLPD,NAPLSD,
     E NAPMSK,NAPNEU,NAPPDS,NAPPOB,NAPPRL,NAPRTM,NAPSCO,NAPSPO,NAPSTR,
     F NAPT2X,NAPTBC,NAPTCO,NAPTEX,NAPTMA,NAPTML,NAPTNC,NAPTST,NAPVCO,
     G NAPYER,NAPYFR,NAPYNE,NAQDET,NAQFPA,NAQLIN,NAQMTL,NAQMTS,NAQPAR,
     H NAQPBT,NAQPLI,NAQTRA,NAQVEC,NAQVRT,NAQWRK,NAQZER,NAREVH,NARHAH,
     I NARTLO,NARTLS,NARUNH,NARUNR,NASFTR,NATEXS,NATGMA,NATMTL,NATPCO,
     J NAVCOM,NAVCPL,NAVDCO,NAVDHT,NAVDXY,NAVDZT,NAVERT,NAVFHL,NAVFLG,
     K NAVFPH,NAVHLS,NAVPLH,NAX1AD,NAX1SC,NAX1TI,NAX2DF,NAX3EC,NAX3EW,
     L NAX3HC,NAX3IT,NAX3L2,NAX3LU,NAX3TM,NAX3TO,NAX3TP,NAX3X3,NAXTBN,
     M NAXTBP,NAXTCN,NAXTEB,NAXTOP,NAXTRB,NAYV0V,NAZPFR,NAEFOL,NAMUID,
     N NAPGID,NAPGPC,NAPGAC,NAPMSC,NAPTHR,NANBIP,NAPDLT,NAPMLT,NAPLJT
C--------------------- end of QCNAMI ----------------------------------
      INTEGER JRHAPN,JRHAPD,JRHAPH,JRHAPV,JRHAAV,JRHADV,JRHADD,JRHANI,
     +          JRHANO,JRHACV,JRHANU,LRHAHA
      PARAMETER(JRHAPN=1,JRHAPD=3,JRHAPH=4,JRHAPV=5,JRHAAV=6,JRHADV=7,
     +          JRHADD=8,JRHANI=9,JRHANO=10,JRHACV=11,JRHANU=12,
     +          LRHAHA=12)
      SAVE NAECMC,NAEGAC,IFIRST,IPRRI
      COMMON / ECCMF / ECCM(36) ,NRMIN,NRMAX
      DATA IROLD / -99/
      DATA NMCMX / 2000 /
      DATA IFIRST,IPRRI  / 0 , 0 /
      DATA ILVGCH / 157 /
      EXTERNAL CHAINT
      CHARACTER*4 CHAINT
      CHARACTER*4 NAMJUL
      CHARACTER*8 PRNAM
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
C ---------------------------------------------------------------------
C
C-    Initialise pointers
C
      IF(IFIRST.EQ.0 ) THEN
        NAECMC = NAMIND('ECMC')
        NARHAH = NAMIND('RHAH')
        NAEGAC = NAMIND('EGAC')
        IFIRST = 1
      ENDIF
C
C-    Check if ALEPHLIB version has the correction
C

      IACOR = 0
      KRHAH = IW(NARHAH)
        IMVER = MINGTV(DUM)
        NRHAH=LROWS(KRHAH)
        DO 10 IRHAH = 1, NRHAH
          PRNAM(1:4) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN))
          PRNAM(5:8) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN+1))
          IF (PRNAM.EQ.'JULIA') THEN
            IJVER = ITABL(KRHAH,IRHAH,JRHAPV)
            ILVER = ITABL(KRHAH,IRHAH,JRHAAV)
          ENDIF
   10   CONTINUE
C
C-    Look if new run
C
      IMC = 0
      IF(NRUN.LT.NMCMX) IMC = 1
C
C-    Look for ECMC bank
C
      IF (IMC.EQ.1) THEN
        DO IC = 1, 36
        ECCM(IC) = 1.
        ENDDO
        NRMIN = 1
        NRMAX = 2000
      ELSE
        IF (ILVER.GT.ILVGCH ) THEN
        DO IC = 1, 36
        ECCM(IC) = 1.
        ENDDO
        IROLD = NRUN

        NRMIN = NRUN
        NRMAX = NRUN
        ELSE
        IF ( NRUN.NE.IROLD ) THEN
C
C-    ECMC bank exist
C
          KECMC = IW(NAECMC)
          NECMC = 0
          IF ( KECMC.GT.0) THEN
            NECMC = LROWS(KECMC)
            DO IECMC = 1,NECMC
              ECCM(IECMC) = RTABL(KECMC,IECMC,1)
            ENDDO
            CALL CALYEAR(NRUN)
            IROLD = NRUN
            NRMIN = NRUN
            NRMAX = NRUN
          ELSE
C
C-     Look for EGAC bank
C
          IF(NRUN.LT.NRMIN.OR.NRUN.GT.NRMAX) THEN
            KEGAC = IW(NAEGAC)
            NEGAC = 0
            IF ( KEGAC.EQ.0) THEN
              DO IC=1,36
                ECCM (IC) = 1.
              ENDDO
              IROLD=NRUN
              NRMIN=NRUN
              NRMAX=NRUN
              IPRRI=IPRRI+1
              IF (IPRRI.LE.10) WRITE (IW(6),'(///,5(/,10X,A),///)')
     &'+------------------------------------------------------------+',
     &'|  Data Processed no gain correction for photon available    |',
     &'|                                                            |',
     &'+------------------------------------------------------------+'
             CALL CALYEAR(NRUN)
            ELSE
C
C-   Search for new correction factors
C
             NEGAC = LROWS(KEGAC)
             IRMIN = 99999
             IRMAX = 0
             DO IEGAC = 1,NEGAC-1
               IF ( NRUN.GE.ITABL(KEGAC,IEGAC,1).AND.
     &              NRUN.LT.ITABL(KEGAC,IEGAC+1,1)) THEN
               IRMAX = IEGAC
               DO IL =1,36
               ECCM (IL) = RTABL(KEGAC,IEGAC,IL+2)
               NRMIN = ITABL(KEGAC,IEGAC,1)
               NRMAX = ITABL(KEGAC,IEGAC+1,1) -1
               ENDDO
               IROLD = NRUN
               CALL CALYEAR(NRUN)
               ENDIF
             ENDDO
             IF ( IRMAX.EQ.0) THEN
               IPRRI=IPRRI+1
               IF (IPRRI.LE.10) WRITE (IW(6),'(///,5(/,10X,A),///)')
     &'+------------------------------------------------------------+',
     &'|  Data Processed no gain correction for photon available    |',
     &'|                                                            |',
     &'+------------------------------------------------------------+'
              DO IC=1,36
              ECCM (IC) = 1.
              ENDDO
             ENDIF
           ENDIF
         ENDIF
       ENDIF
      ENDIF
      ENDIF
      ENDIF
      RETURN
      END
