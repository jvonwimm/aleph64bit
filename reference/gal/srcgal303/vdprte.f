      SUBROUTINE VDPRTE(IMOD,IWAF,IVDHT,NHITW)
C!----------------------------------------------------------------------
C! Process one track element in Vdet
CKEY VDET DIGITIZE
C!
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C!  Algorithm copied from VDFIRS routine by F.Forti,G.Triggiani
C!                                            and P.Cattaneo
C!  For each small segment along the track :
C!      get the energy released
C!      for each view
C!        compute the parameters of the electronic cloud on wafer face
C!            taking into account magnetic and electric field,
C!                and diffusion coefficients.
C!         get the list of fired strips from routine VDFIRS
C!         insert them in the strips arrays
C!
C!
C! Input :  IMOD  : module                                        I
C!          IWAF  : wafer                                         I
C!          IVDHT : VDHT hit number                               I
C!          NHITW : number of hits in the wafer                   I
C! Input :  VDTE bank
C!
C! Output : VWS1,VWS2 banks
C!
C-----------------------------------------------------------------------
C
      INTEGER VDFIRS
      SAVE NAVDTE,NAVWSX
C
      PARAMETER (MXSTR=100)
      DIMENSION ESTRI(MXSTR)
      DIMENSION XCOOR(3)
      DIMENSION VMU(2),VTE(2),VDTHL(2),VFPOS(2)
      DIMENSION VDIFF(2),TNORM(2),NAVWSX(2),XLOC(2)
      PARAMETER(JVWSSC=1,JVWSVT=2,LVWS1A=2)
      PARAMETER(JVDTXB=1,JVDTRE=4,LVDTEA=4)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      LOGICAL FIRST
      LOGICAL VINSEN
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
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVDTE = NAMIND('VDTE')
        NAVWSX(1) = NAMIND('VWS1')
        NAVWSX(2) = NAMIND('VWS2')
      ENDIF
C
C Get wafer constants
C
      CALL VGWCNS(IMOD,IWAF,VDEPL,VMU,VTE,VDTHL,VFPOS,VDAPP)
      BFILD = VMMFLD(IMOD)
      IFILD = SIGN(1.,BFILD)
C
C Compute drift constants for each view
C
      DO 9 IV=1,2
        VDMUC = VMU(IV)*(1.-(ABS(BFILD)*VMU(IV))*1.E-5)**2
        VDIFF(IV) = VTE(IV) * VDMUC
C     constants for drift time calculations         JJD 16/4/91
        TNORM(IV) = VWTHCK()**2/(2.*VDEPL*VDMUC)
 9    CONTINUE
      VPLUS=VDAPP+VDEPL
      KVDTE=IW(NAVDTE)
      NVDTE=LROWS(KVDTE)
C
C Loop over small track segments
C
      DO 10 IVDTE=1,NVDTE
        DO 11 IDIM=1,3
C
C center coordinates
C
          XCOOR(IDIM)=RTABL(KVDTE,IVDTE,JVDTXB-1+IDIM)
 11     CONTINUE
C
C Released energy
C
        ER = RTABL(KVDTE,IVDTE,JVDTRE)
        DO 12 IV=1,2
          YF = ABS(VFPOS(IV)-XCOOR(3))/VWTHCK()
          TICOL = -TNORM(IV)*LOG(1.-2.*VDEPL*YF/VPLUS)
C
C sigma of electron cloud
C
          QSIG = SQRT(2.*VDIFF(IV)*TICOL)
C
C Displacement along phi, because of Hall effect
C There is no - sign because the charge carriers
C are the holes for the xy view
C
          XHALL = -(VFPOS(IV)-XCOOR(3))*VDTHL(IV)*IFILD
          XLOC(1)=XCOOR(1)
          XLOC(2)=XCOOR(2)+XHALL
C if xloc is not in the active region, do nothing
          IF (.NOT.VINSEN(IV,XLOC(1),XLOC(2))) GOTO 12
C
C find list of fired strips, and energy in each strip
C
          IBID = VDFIRS(XLOC,
     >                  QSIG,IV,ISTF,NSF,ESTRI,MXSTR,IERF)
          IF(IERF.EQ.0)THEN
C
C update array of strips, and update truth relations
C
            KVWSX = IW(NAVWSX(IV))
            DO 20 IST=1,NSF
              ISTG = ISTF+IST-1
              EOLD = RTABL(KVWSX,ISTG,JVWSSC)
              IROL = ITABL(KVWSX,ISTG,JVWSVT)
              KLINE = KROW(KVWSX,ISTG)
              ERS = ESTRI(IST)*ER
              RW(KLINE+JVWSSC)=EOLD+ERS
              CALL VINSTS(ISTG,IVDHT,ERS,IROL,NHITW,IRNW)
              IW(KLINE+JVWSVT)=IRNW
 20         CONTINUE
          ENDIF
 12     CONTINUE
 10   CONTINUE
 999  RETURN
      END
