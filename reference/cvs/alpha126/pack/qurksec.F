      SUBROUTINE QURKSEC(XSEC,XSECE,NEVPR,XPROD,XPRODE)
C-----------------------------------------------------------------------
C User access to the MCarlo cross-sections generated for the
C dataset presently read 
C Output arguments:
C  xsec   = cross section (in pb) generated for the present run
C         = -1. if not found
C  xsece  = error on xsecru (in pb) , =-1. if not found
C  nevpr  = total number of events for the whole prouction
C         = 0 if not found
C  xprod  = cross section (in pb) for the whole production
CC        = -1. if not found
C  xprode = error on xsecpr (in pb), =-1. if not found 
C
C Author J. Boucrot 26-10-2000
C-----------------------------------------------------------------------
      common / XKSECS / xsecru,xseceru,nevpro,xsecpr,xsecepr
C ----------------------------------------------------------------------
      xsec=xsecru
      xsece=xseceru
      nevpr=nevpro
      xprod=xsecpr
      xprode=xsecepr
      return
      end 
