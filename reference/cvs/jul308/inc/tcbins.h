      PARAMETER (LNBBIG=10,LNBSML=1,LMXBIN=LNBBIG*LNBSML)
      PARAMETER (NMXWRP=LNBSML/2,LNWRAP=LMXBIN+4*NMXWRP+2)
      PARAMETER (JNDBCO=1,JNDBFW=2,JNDBBW=3,LNDBIN=3)
      COMMON/TCBINS/ IBNPTR(LMXBIN,LTPDRO),INDBIN,IXWRAP(LNWRAP),
     &               TCBINW,TCDELT,IBLAST,TC2DEL,TCOFFS
C
#if defined(DOC)
C
C!    This is a common internal to the TRKFND pattern recognition
C     module.  It is used for double binning of the TPCO coordinates.
C
C     LNBBIG          = number of big phi bins for TRKFND
C     LNBSML          = number of small phi bins within each large bin
C     LMXBIN          = total number of small phi bins for TRKFND
C     LNWRAP          = dimension for the array IXWRAP
C     IBLAST          = next row available in the list IBNPTR
C     TCBINW          = phi bin width for binning coordinates
C     TCDELT          = worst case assumed resolution (=TCBINW/4)
C     TC2DEL          = 2*TCDELT
C     TCOFFS          = constant used in calculation of bin number
C     IXWRAP          = pointers for indexing into IBNPTR in order to
C                       allow wrapping around at the cut (phi=0 or 2pi)
C     IBNPTR(bin,row) = pointer to first coordinate for each bin of
C                       each TPC row.  The pointer points into a
C                       workbank called INDBIN.
C     INDBIN          = pointer to workbank of linked coordinate lists
C
C    ---------------------------------------------------------------
C    |   +--------+                                                |
C    |   | INDBIN |    1.  Number of words per coordinate.         |
C    |   +--------+    2.  Number of coordinates.                  |
C    |-------------------------------------------------------------|
C    |  CO  1.   Pointer to the coordinate in the TPCO bank        |
C    |  FW  2.   Pointer to the next coordinate in this list       |
C    |           (zero if there are none, negative if this entry   |
C    |            has been removed from the list)
C    |  BW  3.   Pointer to previous pointer in this list          |
C    ---------------------------------------------------------------
C
#endif
