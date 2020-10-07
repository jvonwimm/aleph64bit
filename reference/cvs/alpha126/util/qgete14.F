      SUBROUTINE QGETE14(e14ec,e14hc,lveto)
C-----------------------------------------------------------------------
C User-access to  the results of the E14 algorithm
C J. Boucrot 9 Oct 2000
C-----------------------------------------------------------------------
      COMMON / E14VARS / E14ECA,E14HCA,ILCCVT
C-----------------------------------------------------------------------
      e14ec=e14eca
      e14hc=e14hca
      lveto=ilccvt
      return
      end 
