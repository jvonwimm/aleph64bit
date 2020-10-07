      PARAMETER (NFCPHI=19,NFCRAD=8,NFCZED=6,NFCVAL=4)
      PARAMETER (NSPACE=NFCVAL*NFCPHI*NFCRAD*NFCZED)
      PARAMETER (NGMAX=33)
      PARAMETER (NVZ = NSPACE/2)
      COMMON/TFCORR/ RLOWFC,RHIGFC,DRFCOR,NRFCOR,
     &               PLOWFC,PHIGFC,DPFCOR,NPFCOR,
     &               ZLOWFC,ZHIGFC,DZFCOR,NZFCOR,
     &               INDCOR(4),
     &               FSPACE(NSPACE),NAFCOR(3),AFCORR(NGMAX),
     &               ZSPACE(NVZ)
#if defined(DOC)
C
C! Constants for TPC coordinate drift field corrections
C
C RLOWFC = lower limit of the radial coordinate of the grid
C RHIGFC = upper limit of the radial coordinate of the grid
C DRFCOR = grid spacing in the radial coordinate
C NRFCOR = number of grid points in the radial coordinate
C PLOWFC = lower limit of the azimuthal coordinate of the grid
C PHIGFC = upper limit of the azimuthal coordinate of the grid
C DPFCOR = grid spacing in the azimuthal coordinate
C NPFCOR = number of grid points in the azimuthal coordinate
C ZLOWFC = lower limit of the z coordinate of the grid
C ZHIGFC = upper limit of the z coordinate of the grid
C DZFCOR = grid spacing in the z coordinate
C NZFCOR = number of grid points in the z coordinate
C INDCOR = pointer to first value in FSPACE for each correction:
C           1    radial displacement for z>0
C           2    radial displacement for z<0
C           3    phi displacement for z>0
C           4    phi displacement for z<0
C FSPACE = array of coordinate corrections
C NAFCOR = array of numbers of points for (r,phi,z) on grid
C AFCORR = description of correction grid
C ZSPACE =
C
C-----------------------------------------------------------------------
#endif
