      PARAMETER (MADSP=200, MNOSP=70)
      PARAMETER (MHISP=17, MH1SP=13)
      PARAMETER (MRDSP=11, MCASP=12, MCBSP=13, MTASP=14, MTBSP=15,
     +           MPCSP=16, MPDSP=17,
     +           MDASP=21, MDBSP=22, MPASP=23, MPBSP=24,
     +           MTCSP=25, MTDSP=26, MBCSP=27, MBDSP=28,
     +           MSASP=31, MSBSP=32)
      PARAMETER (MN1SP=41, MS1SP=42, MC1SP=43, MR1SP=44,
     +           MN2SP=45, MS2SP=46, MC2SP=47, MR2SP=48)
      PARAMETER (MM1SP=51, MK1SP=52, MF1SP=53, MK2SP=54, MF2SP=55)
#if defined(DOC)
C
C!         SATR reconstruction parameters
C
C MADSP    : dimension of address array in SRZONE
C MNOSP    : dimension of node arrays in SRSELT
C MHISP    : number of histograms
C MH1SP    : number of more histograms
C MRDSP    : hist. number: raw data per event
C MCASP    : hist. number: coordinates on side A per event
C MCBSP    : hist. number: coordinates on side B per event
C MTASP    : hist. number: tracks on side A per event
C MTBSP    : hist. number: tracks on side B per event
C MPCSP    : hist. number: patch parameters on side A per event
C MPDSP    : hist. number: patch parameters on side B per event
C MDASP    : hist. number: thetas of tracks on side A
C MDBSP    : hist. number: thetas of tracks on side B
C MPASP    : hist. number: phis of tracks on side A
C MPBSP    : hist. number: phis of tracks on side B
C MTCSP    : hist. number: thetas of patch parameters on side A
C MTDSP    : hist. number: thetas of patch parameters on side B
C MBCSP    : hist. number: phis of patch parameters on side A
C MBDSP    : hist. number: phis of patch parameters on side B
C MSASP    : hist. number: scatterplot of ay versus ax on side A
C MSBSP    : hist. number: scatterplot of ay versus ax on side B
C MN1SP    : hist. number: no of hits per track
C MS1SP    : hist. number: layers of hits in track fits
C MC1SP    : hist. number: chisquare probabilities of track fits
C MR1SP    : hist. number: residuals of track fits
C MN2SP    : hist. number: no of hits per track, only best tracks
C MS2SP    : hist. number: layers of hits in track fits, only best tr
C MC2SP    : hist. number: chisq prob of track fits, only best track
C MR2SP    : hist. number: residuals of track fits, only best track
C MM1SP    : hist. number: measured distances coordinate - wire
C MK1SP    : hist. number: distance to wire of coordinates in fits
C MF1SP    : hist. number: distance between fit and wire
C MK2SP    : hist. number: distance to wire of coordinates in best fit
C MF2SP    : hist. number: distance between best fit and wire
C
#endif
