      COMMON /TWRPRM/ NPREWP,NPSTWP,IPEDWP,MXSTWP,RTHRWP,JPCKWP,
     &                JTHRWP,MNLNWP,MXLNWP,IALGWP,TPCKWP,MXABWP
#if defined(DOC)
C
C!  Parameters for wire reduction, filled by TWRRED and used by TWPANA
C!  This common should not be used elsewhere!
C
C   NPREWP = number of pre samples
C   NPSTWP = number of post samples
C   IPEDWP = pedestal
C   MXSTWP = maximum number of saturated samples
C   RTHRWP = fractional threshold for the time
C   JPCKWP = constant for packing the charge
C   JTHRWP = average threshold
C   MNLNWP = minimum pulse length
C   MXLNWP = maximum pulse length
C   MXABWP = maximum number of samples above threshold plus pedestal
C   IALGWP = algorithm to be used
C   TPCKWP = constant for packing the time
#endif
