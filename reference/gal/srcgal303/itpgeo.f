      SUBROUTINE ITPGEO
C.
C...ITPGEO  1.11  930721  18:08                        R.Beuselinck
C.
C! Print ITC read out geometry
C.
C-----------------------------------------------------------------------
      SAVE
      COMMON /ITELEC/TDCRIT,TOFFIT,TFINIT(8),ITTDCL(8),ITTDCH(8),
     +               TDCZIT,ITZTZB,ZEXPIT,ZPARIT(3),ITZTDL(8),
     +               ITZTDH(8),ZLOFIT(8),ZRESIT(2,8)
C
      COMMON/ITEXPC/CLOKIT,CLGOIT,TSTRIT,TBINIT,PLENIT(8),EXPFIT(8)
C
      COMMON/ITPARC/DVELIT(8,5),HWEFIT
      REAL DVELIT,HWEFIT
C
      COMMON /ITRESC/RESITP(5,8),RESITN(5,8)
      REAL RESITP,RESITN
C
      COMMON/ITROTC/EULRIT(3),DXYZIT(3),ROTITC(3,3),ITSHFT,WSAGIT
      REAL EULRIT,DXYZIT,ROTITC,WSAGIT
      LOGICAL ITSHFT
C
      COMMON/ITWIRC/RWIRIT(8),NWIRIT(8),IWIRIT(8),PHWRIT(8),CELHIT(8),
     +              CELWIT(8),WZMXIT
C
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      WRITE(LOUTIO,1000) WZMXIT,
     + (I,RWIRIT(I),NWIRIT(I),IWIRIT(I),PHWRIT(I),CELHIT(I),CELWIT(I),
     +  I=1,8)
      WRITE(LOUTIO,1001) WSAGIT, EULRIT, DXYZIT
      WRITE(LOUTIO,1002) HWEFIT
      WRITE(LOUTIO,1004) (I,DVELIT(I,1),DVELIT(I,2),DVELIT(I,3),
     +                    DVELIT(I,4),DVELIT(I,5),I=1,8)
      WRITE(LOUTIO,1005) 'Positive',(I,(RESITP(J,I),J=1,5),I=1,8)
      WRITE(LOUTIO,1005) 'Negative',(I,(RESITN(J,I),J=1,5),I=1,8)
      WRITE(LOUTIO,1006) (I,(ZRESIT(J,I),J=1,2),I=1,8)
      WRITE(LOUTIO,1003) CLOKIT, TSTRIT, TBINIT,
     + (I,PLENIT(I),EXPFIT(I),I=1,8)
C
 1000 FORMAT(///50X,'ITC geometry'//' Readout geometry:',10X,
     + 'Sense wire half-length (cm) =',F6.1//
     + /' Layer   Wire   No. of  Numbering   Phi     Cell    Cell'
     + /' Number  Radii  Wires    Offset    Offset  Height   Width'
     + /(1X,I3,5X,F5.2,2X,I4,4X,I5,6X,F6.4,2X,F6.2,3X,F5.2))
 1001 FORMAT(//' Alignment constants:',10X,'Wire sag at centre (cm) = ',
     + F5.4//' Chamber rotation (Euler angles) = ',3F11.7
     + //' Displacement of ITC origin w.r.t. ALEPH = ',3F10.4)
 1002 FORMAT(//' General readout characteristics:'/
     + /' Wire inefficiency at theta = 90 degrees. : ', F6.3)
 1003 FORMAT(//' Time expansion constants for Z-scalars:'/
     + /' Clock pulse interval for Z coincidence test (ns).: ', F6.2
     + /' Start time for calculation of ITC theta bin (ns).: ', F6.0
     + /' Time width of one ITC theta bin (ns).            : ', F6.0
     +//' Layer   Pulse   Expansion'
     + /' Number  Length   Factor'
     + /(1X,I3,T9,F6.2,T19,F6.2))
 1004 FORMAT(//
     + ' Layer    <---- Drift Time Coeffs. ------------------>'
     + /' Number       C1        C2        C3        C4        C5'
     + /(I4,T11,5(2PE10.1)))
 1005 FORMAT(//
     + ' Layer    <----- R-Phi ',A,'-phi Resol. Coeffs. ----->'
     + /' Number       C1        C2        C3        C4        C5'
     + /(I4,T11,5(2PE10.1)))
 1006 FORMAT(//
     + ' Layer    <--- Z Res. Coeffs. --->'
     + /' Number       C1        C2'
     + /(I4,T11,2(2PE10.1)))
      END
