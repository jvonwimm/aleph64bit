      SUBROUTINE VDNOIS(IMOD)
C!----------------------------------------------------------------------
C! Generate noise on fired strips
C! noise outside of hits not implemented yet
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C!  Algorithm copied from VDNOFS from P. Cattaneo :
C!  Compute electronic noise for fired readout channel
C!  and their neighbours, but no more than once per channel
C!  then, compute the noise on each channel due to capacitive couplings
C!  Finally, compute the parallel noise for each channel where there is
C!  signal
C!
C! Input :  VWC1, VWC2 banks
C! Input :  Module number
C! Output : VWC1, VWC2 banks
C!
C-----------------------------------------------------------------------
C
      SAVE NAVWCX
C
      DIMENSION NBCLN(2),NVDCR(2),VDSLP(2),VDPAL(2),VDELC(2)
      DIMENSION MXCNO(2),MXCSI(2),VELGV(2),VDLCO(2),IOFSET(2),NBITSH(2)
      DIMENSION NAVWCX(2)
      PARAMETER (ICDIM=10)
      DIMENSION CCOEF(2,0:ICDIM),MXCPL(2),COEFN(2,ICDIM)
      DIMENSION MUXL(2),MAXC(2)
      DIMENSION MAXS(2),ISOFF(2),IPICH(2),CSTRP(2)
      PARAMETER(JVWCCC=1,JVWCSN=2,JVWCVT=3,LVWC1A=3)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
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

      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVWCX(1) = NAMIND('VWC1')
        NAVWCX(2) = NAMIND('VWC2')
      ENDIF
C
C Get some module quantities
C
      CALL VFNDMP
     $   (IMOD,MAXS,IPICH,MAXC,MUXL,CCOEF,
     $   COEFN,CSTRP,ICDIM,MXCPL)
C
C Get electronic constants
C
      CALL VFNDEL
     $   (IMOD,NBCLN,NVDCR,VDSLP,VDPAL,VDELC,
     $    MXCSI,MXCNO,VELGV,VDLCO,IOFSET,NBITSH)
C
C  Loop on 2 views
C
      DO 100 IV=1,2
C
C First, compute the correlated noise :
C
        KVWCX = IW(NAVWCX(IV))
C
C skip in case of nonexistent bank
C
        IF(KVWCX.EQ.0)GO TO 100
C
        NVWCX = LROWS(KVWCX)
C
C For each readout channel
C     If there is signal then
C       loop over 9 neighbours
C       For each neighbour
C         If no signal in second column of VWCX bank then
C           Compute series noise from random number
C           put it in second column of VWCX bank
C         Endif
C       end neighbours
C     Endif
C
        DO 10 IVWCX = 1,NVWCX
          SIG = RTABL(KVWCX,IVWCX,JVWCCC)
          IF(SIG.NE.0)THEN
            NNEIG = NBCLN(IV)
C
C Minimum and maximum readout channels for this strip
C
          IMIN = MAX(IVWCX - NNEIG,1)
          IMAX = MIN(IVWCX + NNEIG,NVWCX)
C
            DO 11 INEIG=IMIN,IMAX
              SERN = RTABL(KVWCX,INEIG,JVWCSN)
              IF(SERN.EQ.0)THEN
                CALL RANNOR(RDM,DUM)
C
C Compute the noise due to the amplifier on this strip.
C This is a voltage (expressed in e/pf)
C
                RW(KROW(KVWCX,INEIG)+JVWCSN) =
     $            VDSLP(IV)*RDM
              ENDIF
 11         CONTINUE
          ENDIF
 10     CONTINUE
C
C For each readout channel
C     If there is noise in second column then
C        Channel noise = Vdot(noise(channel-2),coefficients,NVDCR(=5))
C        Channel signal = channel signal + channel noise
C     endif
C Endloop
C
        DO 20 IVWCX = 1,NVWCX
          CNOI = RTABL(KVWCX,IVWCX,JVWCSN)
          IF(CNOI.NE.0)THEN
            DO 21 INEIG=1,NVDCR(IV)
              SOLD = RTABL(KVWCX,IVWCX,JVWCCC)
              IF (IVWCX-INEIG .GT. 0) THEN
                SNM = RTABL(KVWCX,IVWCX-INEIG,JVWCSN)
              ELSE
                SNM = 0.0
              ENDIF
              IF (IVWCX+INEIG .LE. NVWCX) THEN
                SNP = RTABL(KVWCX,IVWCX+INEIG,JVWCSN)
              ELSE
                SNP = 0.0
              ENDIF
              RW(KROW(KVWCX,IVWCX)+JVWCCC) = SOLD+
     $           (SNM+SNP)*COEFN(IV,INEIG)*VELGV(IV)
 21         CONTINUE
C
C Now, compute the contribution to the strip itself
C This is (like above) voltage multiplied by strip capacitance,
C then transformed into Gev
C
            RW(KROW(KVWCX,IVWCX)+JVWCCC) = RTABL(KVWCX,IVWCX,JVWCCC)+
     >                                     RTABL(KVWCX,IVWCX,JVWCSN)*
     >                                     CSTRP(IV)*VELGV(IV)
          ENDIF
 20     CONTINUE
C
C parallel noise
C For each readout channel
C     If there is signal in first column
C        compute random noise for the channel
C        add it to signal in first column
C     endif
C Endloop
C
        DO 30 IVWCX = 1,NVWCX
          SIG = RTABL(KVWCX,IVWCX,JVWCCC)
          IF(SIG.NE.0)THEN
            CALL RANNOR(RDM,DUM)
            RW(KROW(KVWCX,IVWCX)+JVWCCC) = SIG +
     $         VDPAL(IV)*RDM*VELGV(IV)
          ENDIF
 30     CONTINUE
 100  CONTINUE
      RETURN
      END
