      SUBROUTINE VDDIGI
C!----------------------------------------------------------------------
C! Generate Vdet digitizings
CKEY VDET DIGITIZE
C!
C!  Author         A. Bonissent  15-Jan-1994
C!
C!  Description
C!  ===========
C!  Steering routine for the Vdet digitizings.
C!  Calls appropriate routines to :
C!        compute hit positions in local wafer coordinates  VDMKLO
C!        generate charge on fired strips                   VDFIRW
C!        compute charge on readout channels                VDROCH
C!        generate noise                                    VDNOIS
C!        find clusters                                     VDCLU
C!        fill final output banks                           VDFOOB
C!
C! Input :  VDHT bank
C!
C! Output : VPLS and VHLS banks (IN VDFOOB)
C!
C-----------------------------------------------------------------------
C
      SAVE NAVDLH,NAVWCX
C
C Max number of modules and wafers in one detector
      PARAMETER (MAXMOD=24, MAXWAF=3)
C
      DIMENSION IHF(-MAXMOD:MAXMOD,MAXWAF),IHL(-MAXMOD:MAXMOD,MAXWAF)
      DIMENSION IARG(3)
      DIMENSION MODHIT(-MAXMOD:MAXMOD)
      PARAMETER (ICDIM=10)
      DIMENSION CCOEF(2,0:ICDIM),MXCPL(2),COEFN(2,ICDIM)
      DIMENSION MUXL(2),MAXC(2),MAXS(2),NROS(2)
      DIMENSION NAVWSX(2),ISOFF(2),IPICH(2),CSTRP(2)
      DIMENSION NAVWCX(2)
      PARAMETER(JVWCCC=1,JVWCSN=2,JVWCVT=3,LVWC1A=3)
      PARAMETER(JVWSSC=1,JVWSVT=2,LVWS1A=2)
      PARAMETER(JVDLXI=1,JVDLXO=4,JVDLMO=7,JVDLWA=8,JVDLCO=9,JVDLER=10,
     +          JVDLTR=11,JVDLHT=12,LVDLHA=12)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER VPHSTM
      LOGICAL FIRST
      DATA FIRST /.TRUE./
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
C
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVDLH=NAMIND('VDLH')
        NAVWCX(1)=NAMIND('VWC1')
        NAVWCX(2)=NAMIND('VWC2')
      ENDIF
C
C Build local hits bank
C
      CALL VDMKLO
      KVDLH=IW(NAVDLH)
      IF(KVDLH.EQ.0) GO TO 999
      NVDLH = LROWS(KVDLH)
      NMAX = (2*MAXMOD+1)*MAXWAF
C
C Reset arrays at each event (fired wafers and modules
C
      CALL VZERO(IHF,NMAX)
      CALL VZERO(IHL,NMAX)
      CALL VZERO(MODHIT(-MAXMOD),MAXMOD*2+1)
C
C Before we start, reset possible old banks
C
      CALL BDROP(IW,'VDTEVPLHVHLSVWS1VWS2VWC1VWC2VTSKVTRS')
C
C Loop over hits, and see which modules and wafers are hit
C
      DO 10 IVDLH=1,NVDLH
        MODUL=ITABL(KVDLH,IVDLH,JVDLMO)
        MODHIT(MODUL)=1
        IWAF=ITABL(KVDLH,IVDLH,JVDLWA)
C--  the row number for the first hit in the wafer
        IF(IHF(MODUL,IWAF).EQ.0)THEN
          IHF(MODUL,IWAF)=IVDLH
        ENDIF
C--  the last hit in the wafer
        IHL(MODUL,IWAF)=IVDLH
   10 CONTINUE
C
C Get  max number of readout channels in one module,
C mux. length and readout frequency ipich
C
      CALL VFNDMC(1,MAXC,NROS,MUXL,IPICH)
C
C Create channels array (bank)
C
      CALL ALBOS('VWC1',0,MAXC(1)*LVWC1A+LMHLEN,KVWC1,IGARB)
      IW(KVWC1+LMHROW)=MAXC(1)
      IW(KVWC1+LMHCOL)=LVWC1A
      CALL ALBOS('VWC2',0,MAXC(2)*LVWC1A+LMHLEN,KVWC2,IGARB)
      IW(KVWC2+LMHROW)=MAXC(2)
      IW(KVWC2+LMHCOL)=LVWC1A
C
C
C Create strip array (bank)
C First get wafer size in number of strips
C
      DO IV=1,2
         IBID = VPHSTM(IV,MAXS(IV),PPPP)
      ENDDO
      CALL ALBOS('VWS1',0,MAXS(1)*LVWS1A+LMHLEN,KVWS1,IGARB)
      IW(KVWS1+LMHROW)=MAXS(1)
      IW(KVWS1+LMHCOL)=LVWS1A
      CALL ALBOS('VWS2',0,MAXS(2)*LVWS1A+LMHLEN,KVWS2,IGARB)
      IW(KVWS2+LMHROW)=MAXS(2)
      IW(KVWS2+LMHCOL)=LVWS1A
C
C loop over all hit modules, and for each over all hit wafers,
C VDFIRW will put signal in fired strips for the current wafer
C
      DO 20 IMOD = -MAXMOD,MAXMOD

        IF(MODHIT(IMOD).EQ.1)THEN
C
C Get some module quantities
C
          CALL VFNDMP
     $       (IMOD,MAXS,IPICH,MAXC,MUXL,CCOEF,
     $       COEFN,CSTRP,ICDIM,MXCPL)
C
C Reset readout channels array
C
          DO 30 IV=1,2
            KBNK = IW(NAVWCX(IV))
            NDATA = IW(KBNK)-LMHLEN
            CALL VZERO(IW(KBNK+LMHLEN+1),NDATA)
   30     CONTINUE

          NHITM = 0
          DO 21 IWAF = 1,MAXWAF
            IFIRS = IHF(IMOD,IWAF)
            IF(IFIRS.NE.0)THEN
              ILAST = IHL(IMOD,IWAF)
              NHITW = ILAST-IFIRS+1
              CALL VDFIRW(IFIRS,ILAST,IMOD,IWAF)
              NHITM=NHITM+NHITW
C
C accumulate signal in readout channels (and reset wafer map)
C
              CALL VDROCH(IMOD,IWAF,NVDLH)
              IARG(1)=0
              IARG(2)=IFIRS
              IARG(3)=ILAST
              CALL VDFILL('WAFER',IARG)
            ENDIF
   21     CONTINUE
C
C compute noise in current module
C
          CALL VDNOIS(IMOD)
          IARG(1)=IMOD
          IARG(2)=0
          IARG(3)=0
          CALL VDFILL('MODULE',IARG)
C
C Perform clustering on current module
C
          CALL VDCLU(IMOD,NHITM)
C
C Fill final output banks
C
          CALL VDFOOB(IMOD,NVDLH)
        ENDIF
   20 CONTINUE
C
C Adjust size of output banks
C
      CALL AUBPRS('VPLH')
      CALL AUBPRS('VHLS')
      CALL AUBPRS('VTRS')
C
C Now, we delete "work banks"
C
      CALL BDROP(IW,'VDTEVWS1VWS2VWC1VWC2VTSK')
C
C Put banks in E list
C
      CALL BLIST(IW,'E+','VHLSVPLHVTRS')
      CALL VDFILL('EVENT',IARG)
  999 CONTINUE
      RETURN
      END
