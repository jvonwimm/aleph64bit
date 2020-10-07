C! VDET Electronics channels arrays
C
C  electronics channels; 3 wafers; 3 wafer flags
C  index is electronics channel starting from 1
      INTEGER EFLAG
      PARAMETER(EFLAG=7)
      INTEGER IELCHP(1024,eflag)
      INTEGER IELCHZ(1024,eflag)
      COMMON/VELCHN/IELCHP, IELCHZ
C ------------------------------------------------
