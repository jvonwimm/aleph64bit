*CD lccomc
      COMMON /LCCOMC/ ADCOLC,    COHNLC,    DPR1LC,    DPR2LC,
     *                DPR3LC,    DPR4LC,    DPR5LC,    ECRTLC,
     *                ECUTLC,    EELALC,    GVARLC,    LCADCO,
     *                LCBHTR,    LCHBOK,    LCNLAY(3), LCNWPL,
     *                LCMATE(2), LCPRNT,    LCSTRH(3), CHTOE(3),
     *                PAR1LC,    PAR2LC,    PAR3LC,    PAR4LC,
     *                PAR5LC,    PAR6LC,    PAR7LC,    PAR8LC,
     *                RADLLC(2), SNOILC(3), SCONLC,    SSAMLC,
     *                SSTPLC(3), TNOILC(3), WNOILC(3),
     *                ZMATLC(2), ZREFLC(3), ZSTPLC(3), Z123LC(3),
     *                XYZOLC(3,2),DWIRLC
#if defined(DOC)
C     The common /LCCOMC/ contains constants to be used in the
C     LCAL set of subroutines
C
C     ADCOLC    = MeV per ADC-count (LCIRUN)
C     COHNLC    = coherent noise level (LCNOIS)
C     DPR1-5LC  = parameters in the shower devellopment (LCSHOW)
C     GVARLC    = gainvariation parameter (LCGAIN)
C     ECRTLC    = parameter in the shower devellopment (LCSHOW)
C     ECUTLC    = energies below are deposited at shower peak
C                 (LCSHOW)
C     EELALC    = parameter in hit-probability (LCTRAK)
C     LCADCO    = ADCOLC
C     LCBHTR    = trigger threshold for Bhabha-triggers (LCBHAB)
C     LCNLAY(3) = number of planes in each storey
C     LCNWPL    = total number of planes
C     LCMATE(2) = pointer to materials in storeyes
C     LCPRNT    = print flag
C     LCSTRH(3) = storey treshold for readout (LCNOIS,LCROC)
C     CHTOE(3) = hit to energy conversion factors
C     PAR1-8LC  = parameters in the shower devellopment (LCSHOW)
C     RADLLC(2) = radiation lengths of light and heavy materials
C     SNOILC(3) = incoherent noise level per tower in each storey
C                 (LCNOIS)
C     SCONLC    = Constant term to be added to the energy fluctuations
C                 DE/E that are already there from the sampling algo
C     SSAMLC    = Extra term SSAMPL/sqrt(E) to be added to the
C                 DE/E that are already there from the sampling algo
C     SSTPLC(3) = step length in radians length in each storey
C     TNOILC(3) = incoherent noise per trigger segment
C                 in each storey (LCNOIS)
C     WNOILC(3) = incoherent noise level in wireplanes
C     ZMATLC(2) = average atomic number of light and heavy material
C     ZREFLC(3) = reference z-value for clusters in each storey
C     ZSTPLC(3) = steplength in cm in each storey
C     Z123LC(3) = length of each storey in cm
C     XYZOLC(3,2) origin of endcap at start of the sensitive area
C     DWIRLC    = Z-distance from wire-plane to end-of-layer
#endif
