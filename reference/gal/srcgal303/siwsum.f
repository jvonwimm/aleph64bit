      SUBROUTINE SIWSUM
C--------------------------------------------------------------
C! - Job summary for SCal
C. - B.Bloch-Devaux     910115
C. - Called by  ASCJOB                           from this .HLB
C -----------------------------------------------
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      PARAMETER ( NSIST = 15)
      DOUBLE PRECISION ESICOU
      COMMON/SISTAT/NSIPRT,NSICOU(NSIST),ESICOU(NSIST)
C ------------------------------------------------------------
      IF ( NSICOU(1).LE.0) THEN
         WRITE(LOUTIO,900)
      ELSE
         WRITE(LOUTIO,1000)
         WRITE(LOUTIO,1100) NSICOU(1),NSICOU(2),NSICOU(3)
         ET = 0.
         EP = 0.
         EA = 0.
         EB = 0.
         EN = 0.
         ET = SNGL(ESICOU(1))/NSICOU(1)
         EN = FLOAT(NSICOU(4))/FLOAT(NSICOU(1))
         WRITE(LOUTIO,1400)   ET
         ET = SNGL(ESICOU(4))/NSICOU(1)
         EP = SNGL(ESICOU(7))/NSICOU(1)
         ESICOU(10) = SNGL(ESICOU(10))/FLOAT(NSICOU(1))
         IF ( NSICOU(2).GT.0) EA = SNGL(ESICOU(2))/NSICOU(2)
         IF ( NSICOU(3).GT.0) EB = SNGL(ESICOU(3))/NSICOU(3)
         WRITE(LOUTIO,1500)   EA,EB
         WRITE(LOUTIO,1600)   EN
         IF ( NSICOU(2).GT.0) EA = SNGL(ESICOU(5))/NSICOU(2)
         IF ( NSICOU(3).GT.0) EB = SNGL(ESICOU(6))/NSICOU(3)
         WRITE(LOUTIO,1700)   ET
         WRITE(LOUTIO,1800)   EA,EB
         IF ( NSICOU(2).GT.0) EA = SNGL(ESICOU(8))/NSICOU(2)
         IF ( NSICOU(3).GT.0) EB = SNGL(ESICOU(9))/NSICOU(3)
         WRITE(LOUTIO,1900)   EP
         WRITE(LOUTIO,1950)   EA,EB
         ETT = SNGL(ESICOU(1)+ESICOU(4))/FLOAT(NSICOU(1))
         DE = 0.
C        IF (ETT.GT.0.) DE = SQRT(0.001*(1.E06*ESICOU(10)-ETT**2)/ETT)
         WRITE(LOUTIO,1960)   DE
      ENDIF
  999 RETURN
C
  900 FORMAT(//,'1+++ SCAL SUMMARY +++  No events entering SICAL')
 1000 FORMAT(//,'1+++ SCAL SUMMARY +++')
 1100 FORMAT(' No. of events with particles in SCAL:',10X,I10,
     $       ' SCA , SCB',2I10)
 1200 FORMAT(' No. of events satisfying Bhabha-trigger condition',I10,
     *       ' MeV is',I10)
 1300 FORMAT('  Average trigger  energy in SCA,SCB',2F10.1,' MeV')
 1400 FORMAT('  Average Trelem   energy in SCA+SCB', F10.1,' MeV')
 1500 FORMAT('  Average Trelem   energy in SCA,SCB',2F10.1,' MeV')
 1600 FORMAT('  Average number of pad hit / event ',2F10.1,' Pad')
 1700 FORMAT('  Average Parametrized energy in SCA+SCB', F10.1,' MeV')
 1800 FORMAT('  Average parametrized energy in SCA,SCB',2F10.1,' MeV')
 1900 FORMAT('  Average Parametrized lost energy total', F10.1,' MeV')
 1950 FORMAT('  Average parametrized lost energy in SCA,SCB',2F10.1,
     $          ' MeV')
 1960 FORMAT('  Delta = sqrt((<E**2>-<E>**2)/<E>)  Gev**1/2', F10.4)
      END
