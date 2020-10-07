      COMMON/TKRWWB/ ITWPLW,ITWTAW,ITRKSW(3),ITWPUW
      PARAMETER (JTPUCE=1,JTPUZP=2,JTPUTW=3,LENTPU=3)
      PARAMETER (JWPLNP=1,JWPLOF=2,LENWPL=2)
      PARAMETER (JTWWCE=1,JTWWSL=2,JTWWZR=3,JTWWRP=4,
     &           JTWWTN=5,LENTWW=5)
#if defined(DOC)
C!  Workbank indices for track-wire association.  This common should not
C  be referenced outside of the module TRKWIR.  Note that TRKWIR and
C  this comdeck are obsolete.  They are replaced by TRKWRA and TRKWWC.
C
C  ITWPUW= Workbank for list of unpacked wire pulses.  Pulses on
C          a given wire will be consecutive.
C
C     ------------------------------------------------------------
C     | +--------+                                               |
C     | | ITWPUW |            Number of words per pulse=3        |
C     | +--------+            Number of pulses                   |
C     |----------------------------------------------------------|
C     | JTPUCE  1.   inte     charge estimate                    |
C     | JTPUZP  2.   real     z position in sector frame         |
C     | JTPUTW  3.   real     pointer to ITWTAW (<0 if bad pulse)|
C     ------------------------------------------------------------
C
C  ITWPLW= Workbank definition for associating wire numbers with pulses:
C
C     ------------------------------------------------------------
C     | +--------+                                               |
C     | | ITWPLW |            Number of words per row = 2        |
C     | +--------+            Number of rows (# wires in sector) |
C     |----------------------------------------------------------|
C     | JWPLNP  1.   inte     Number of pulses on wire           |
C     | JWPLOF  2.   inte     Offset in TWRR of first pulse      |
C     ------------------------------------------------------------
C
C  ITWTAW= Workbank definition for temporary wire association
C
C     ------------------------------------------------------------
C     | +--------+                                               |
C     | | ITWTAW |            Number of words per pulse=2        |
C     | +--------+            Number of associated pulses        |
C     |----------------------------------------------------------|
C     | JTWWCE  1.   real     Calibrated charge estimate         |
C     | JTWWSL  2.   real     Sample length in cm.               |
C     | JTWWZR  3.   real     Z residual from pulse to track     |
C     | JTWWRP  4.   real     Radial position of the hit         |
C     | JTWWTN  5.   inte     Track number                       |
C     ------------------------------------------------------------
C
C     Workbank definitions for the track-sector list:
C
C  ITRKSW(1)= list of track and chain numbers (negative for chain)
C  ITRKSW(2)= list of sector numbers
C  ITRKSW(3)= indices for sorting by sector number
C
C-----------------------------------------------------------------------
#endif
