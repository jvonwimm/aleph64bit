      PARAMETER (MXZVAL=6)
      COMMON/TPRZDP/ NZVLDP,ZZVLDP(MXZVAL),TPRFQ1(MXZVAL),TPRFA1(MXZVAL)
C
#if defined(DOC)
C!  Constants filled by TPRFIN at run beginnings for calculation of
C  the z dependence of the square of the TPC pad response function.
C  Here z means simply the drift length (z in sector frame of ref.).
C
C  NZVLDP =     Number of different drift lengths
C  ZZVLDP =     Values of the drift lengths
C  TPRFZ2 =     Division point in z to change from 1st line to the 2nd
C  TPRFQ1 =     Constant term for the interpolating line for each region
C  TPRFA1 =     Slope of the interpolating line for each region
C               Where there are NZVLDP-1 regions.
C----------------------------------------------------------------------
#endif
