C! HCAL calibrations and normalization constants
      COMMON/HTOWBA/CALBA(24)
      COMMON/HTOWEC/CALEC(12)
      COMMON/HTOTIM/CALTBA,CALTEC
      COMMON/HTOT0N/CT0MCB,CT0MCE,CT0DAB,CT0DAE,CT0DWB(4),CT0DWE(4)
#if defined(DOC)
C     CALBA -  Barrel Intermodule calibrations
C     CALEC -  Endcaps Intermodule calibrations
C     CALTBA - Barrel time dependent constant
C     CALTEC - Endcaps time dependent constant
C     CT0MCB - Barrel normalization (MC)
C     CT0MCE - Endcaps normalization (MC)
C     CT0DAB - Barrel normalization (data)
C     CT0DAE - Endcaps normalization (data)
C     CT0DWB - Barrel normalization (data) wagon i i = 1,4
C     CT0DWE - Endcaps normalization (data) wagon i i =1,4
#endif
