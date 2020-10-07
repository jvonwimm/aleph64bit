C
C Number of histograms in |cos(theta)|, p and both.
      INTEGER NCHIS,NPHIS,NCPHIS
      PARAMETER(NCHIS=3,NPHIS=3)
      PARAMETER(NCPHIS=NCHIS*NPHIS)
C
C Number of track types for theta/p binning.
      INTEGER NTTSB
      PARAMETER(NTTSB=1)
C
C Theta/p bin limits
      REAL PBIN(NPHIS-1), TBIN(NCHIS-1)
      DATA TBIN/0.3, 0.6/
      DATA PBIN/1.0, 3.0/ 
C
C Variables for the smearing/deletion histograms numbers
      INTEGER KDEDIFF, IQHIS
      PARAMETER(KDEDIFF=3, IQHIS=120000)
