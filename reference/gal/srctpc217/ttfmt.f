      SUBROUTINE TTFMT(KEND,ISECT,JCHN,IBIN1,NLEN,NHITS,IERR)
C--------------------------------------------------------------------
C!  Format MC Trigger Pad data to emulate trigger readout
C
C  Called from:  TSRESP
C  Calls:        AUBOS, BLIST, BKFMT
C
C  Inputs:   PASSED:      --KEND,   "compress banks" flag
C                         --ISECT,  sector number
C                         --JCHN,   channel number
C                         --IBIN1,  first time bin of this signal
C                         --NLEN,   number of bins in this signal
C                         --NHITS,  hit number or number of hits
C                         --NDI,    number of bins in dig bank
C            TPCBOS.INC:  --DIGNAM, names of hl and dig banks
C                         --NDIDEF, default lengths of hl and dig banks
C                         --NDIEXT, default extensions
C                         --ITMADC, id for single digitized pulse
C  Output:   IERR:        -- 0 for normal completion, 1 if insufficient
C                            space to increase work bank length
C--------------------------------------------------------------------
C  Format not known yet
      DATA NDI/0/
      IERR = 0
      RETURN
      END
