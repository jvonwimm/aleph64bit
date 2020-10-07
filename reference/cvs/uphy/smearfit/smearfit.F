      SUBROUTINE SMEARFIT(SMBIN, RTBIN,  NMCFIL, MCFILE, NDAFIL,
     +     DAFILE, CAFILE, HIFILE)
C-----------------------------------------------------------------------
C! Fit for the smearing function necessary for making the data 3-d
C! impact parameter significance look like the Monte Carlo.  Dave Brown
C!                                                    13-3-93
C! Debugged - I.Tomalin 10/11/95
C! Smearing function made symmetric - I.Tomalin 29/2/96
C! Smear as function of p, theta or both - I.Tomalin 21/5/96
C! Updated for the reprocessing - P. Spagnolo 29/6/99
C! Added track deletion - P. Spagnolo 22/7/99
C! New histogram naming convention, 
C!     deletion factor in p/theta - P.-F. Giraud 17/08/99
C!
C! Arguments :
C!     CHAR*1   SMBIN     = 'N' no binning in theta/p for smearing.
C!                        = 'T' binning in theta for smearing.
C!                        = 'P' binning in p for smearing.
C!                        = 'B' binning in both theta/p for smearing.
C!     CHAR*1   RTBIN     = 'N' no binning in theta/p for deletion.
C!                        = 'T' binning in theta for deletion.
C!                        = 'P' binning in p for deletion.
C!                        = 'B' binning in both theta/p for deletion.
C!     INTE     NMCFIL    = number of MC files
C!     CHAR*(*) MCFILE(*) = MC file names
C!     INTE     NDAFIL    = number of data files
C!     CHAR*(*) DAFILE(*) = data file names
C!     CHAR*(*) CAFILE    = output card file name
C!     CHAR*(*) HIFILE    = output histogram file name
C-----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
C
C Calling arguments
      CHARACTER*1 SMBIN, RTBIN
      INTEGER NMCFIL, NDAFIL
      CHARACTER*(*) MCFILE(*), DAFILE(*), CAFILE, HIFILE
C
C Smearing include file
#include"qsmear.h"
C
C HBOOK/PAW stuff
      INTEGER*4 HBSIZE
      PARAMETER(HBSIZE=100000)
      COMMON/PAWC/HMEMOR(HBSIZE)
      REAL*4 HMEMOR
      LOGICAL HEXIST
      INTEGER NPRIME/1024/
      INTEGER LREC
      INTEGER ICYCLE/0/
C
C Common containing histogram data.
      INTEGER NBIN
      PARAMETER (NBIN=100)
      REAL*4 DTHIST(NBIN),DTERR(NBIN),MCHIST(NBIN),MCERR(NBIN),
     +     TOTDA,TOTMC
      REAL*8 DCENT(NBIN),WIDTH(NBIN)
      INTEGER HID,IRANGE(2),NDOF
      LOGICAL TRK2D
      COMMON/SMFIT/DTHIST,DTERR,MCHIST,MCERR,TOTDA,TOTMC,DCENT,WIDTH,
     +             HID,IRANGE,NDOF,TRK2D
      CHARACTER*60 CHTIT
C
C Common containing random numbers.
      INTEGER KG,NGAUSS,NRAND
      PARAMETER(NGAUSS=2,NRAND=500)
      REAL*4 RANX,RANY
      COMMON/RANNUM/RANX(NRAND,NBIN,NGAUSS),RANY(NRAND,NBIN,NGAUSS)
C
C Fit variables
      INTEGER NPAR,NTYPE,IPAR,IVAR,NPARI,NPARX,IER,NDEL
      PARAMETER (NPAR=2*NGAUSS,NTYPE=9,NDEL=1)
      REAL*8 DUMMY(10),AMPLI,SIGMA,VAL(NPAR),ERROR(NPAR),BND1,BND2,
     +       CHIMIN,FEDM,ERRDEF
      CHARACTER*10 CHNAM

C
C  Local variables
      REAL PI,DMIN_BINS(NBIN+1),XEDGE(2),RLINX,RLINY,
     +     SMIR(NPAR,0:NCPHIS,NTYPE)
      PARAMETER(PI=3.1415927)
      INTEGER IRAN,KOFF,IFILE,ISTAT,IBIN,HJD,I,J,INDEX
      CHARACTER*43 FILE
      CHARACTER*6 SMBK
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C Track deletion variables
*      REAL*8 ENTRDT(12), ENTRMC(12), EFFDT1(9), EFFMC1(9)
*      REAL*8 EFFDT2(9), EFFMC2(9), DEL1(9), DEL2(9)
      REAL*8 ENTRDTD(NTYPE, 0:NCPHIS), ENTRMCD(NTYPE, 0:NCPHIS)
      REAL*8 EFFDT(NTYPE, 0:NCPHIS), EFFMC(NTYPE, 0:NCPHIS)
      REAL*8 DEL(NTYPE, 0:NCPHIS)
      REAL*8 NEVTMC, NEVTDT, NTRKMC, NTRKDT, REVT, RTRK, RERR
      INTEGER NBHIS , NMHIS , NVHIS , NUSE , IVHIS,  ITYPE
      INTEGER NBHIS2, NMHIS2, NVHIS2, NUSE2, IVHIS2, ITYPE2
      CHARACTER*6 DEBK

C  Card file info
      REAL SMINFO(3)
      INTEGER ISMINFO
C
C  QIPBTAG track types which are 2-dimensional.
      LOGICAL TYPE2D(NTYPE)/4*.FALSE.,5*.TRUE./
C
C  Externals
C
      REAL HI
      REAL HIE
      EXTERNAL SMEARIT
      EXTERNAL HI
C-----------------------------------------------------------------------
C Determine how many theta/p bins are being used, for the smearing.
      IF (SMBIN.EQ.'N') THEN
        NMHIS = 0
        NBHIS = 0
        NVHIS = 0
      ELSE
        NMHIS = 1
        NBHIS = NCPHIS
        IF (SMBIN.EQ.'T') THEN
          NVHIS = NCHIS
        ELSE IF (SMBIN.EQ.'P') THEN
          NVHIS = NPHIS
        ELSE IF (SMBIN.EQ.'B') THEN
          NVHIS = NCPHIS
        END IF
      END IF
      SMBK = ' Q'//SMBIN//'SM '
C
C Determine how many theta/p bins are being used, for the deletion.
      IF (RTBIN.EQ.'N') THEN
        NMHIS2 = 0
        NBHIS2 = 0
        NVHIS2 = 0
      ELSE
        NMHIS2 = 1
        NBHIS2 = NCPHIS
        IF (RTBIN.EQ.'T') THEN
          NVHIS2 = NCHIS
        ELSE IF (RTBIN.EQ.'P') THEN
          NVHIS2 = NPHIS
        ELSE IF (RTBIN.EQ.'B') THEN
          NVHIS2 = NCPHIS
        END IF
      END IF
      DEBK = ' Q'//RTBIN//'DE '
C
C Generate some random numbers to do the smearing.
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        DO KG = 1,NGAUSS
C Linear distribution.
          CALL RANLUX(RANX(1,1,KG),NRAND*NBIN)
          CALL RANLUX(RANY(1,1,KG),NRAND*NBIN)
C Normal distribution.
C         CALL RNORML(RANX(1,1,KG),NRAND*NBIN)
C         CALL RNORML(RANY(1,1,KG),NRAND*NBIN)
C
          DO IRAN= 1,NRAND
            DO IBIN = 1,NBIN
              RLINX = 2.0*RANX(IRAN,IBIN,KG) - 1.0
              RLINY = 2.0*RANY(IRAN,IBIN,KG) - 1.0
C Exponential distribution (linear input).
              RANX(IRAN,IBIN,KG) = SIGN(LOG(MAX(ABS(RLINX),1.E-30))
     $             ,RLINX)
              RANY(IRAN,IBIN,KG) = SIGN(LOG(MAX(ABS(RLINY),1.E-30))
     $             ,RLINY)
C Breit-Wigner distribution (linear input).
C     RANX(J,IBIN,KG) = TAN(0.5*PI*RLINX)
C     RANY(J,IBIN,KG) = TAN(0.5*PI*RLINY)
            END DO
          END DO
        END DO
C
C Initialize HBOOK
C
        CALL HLIMIT(HBSIZE)
      ENDIF
C     
C Read in the calibration histograms (MC)
C
      DO 30 IFILE = 1,NMCFIL
        FILE = MCFILE(IFILE)
        LREC = 0
        CALL HROPEN(89,'MCCAL',FILE,' ',LREC,ISTAT)
        IF (ISTAT.NE.0) THEN
          WRITE(6,10)
 10       FORMAT(/,' FATAL ERROR: Unable to open MC histogram file.')
          STOP
        ELSE
          WRITE(6,20) FILE,LREC
 20       FORMAT(/,' Opened MC histogram file ',A80,I6)
        END IF
C
C Get the number of track types from info histogram
        IF (IFILE.EQ.1) THEN
          CALL HRIN(IQHIS+100, 999999, 0)
          CALL HUNPAK(IQHIS+100, SMINFO, ' ', 0)
          IF (SMBIN.EQ.'N') THEN
            NUSE = NINT(SMINFO(3))
          ELSE
            NUSE = NTTSB
          ENDIF
          IF (RTBIN.EQ.'N') THEN
            NUSE2 = NINT(SMINFO(3))
          ELSE
            NUSE2 = NTTSB
          ENDIF
          CALL HDELET (IQHIS+100)
        ENDIF
C
C Input the histograms used for the smearing parameters
        KOFF = 0
        IF (IFILE.GT.1) KOFF = 99999
        DO ITYPE=1,NUSE
          DO IVHIS=NMHIS,NBHIS
            CALL HRIN(IQHIS+10*IVHIS+ITYPE,999,KOFF)
          END DO
        END DO
C
C Input the histograms used for the deletion parameters
        DO IVHIS2=0, NBHIS2
          CALL HRIN(IQHIS+10*IVHIS2, 999, KOFF)
        ENDDO
        CALL HREND('MCCAL')
        CLOSE(89)
 30   CONTINUE

C
C deletion: MC efficiency
C
      CALL SMEARADD(RTBIN, 0, NMHIS2, NBHIS2, 0)
      NEVTMC = HI(IQHIS,1)
      NTRKMC = HI(IQHIS,2)

      IF (NEVTMC .LE. 0. .OR. NTRKMC .LE. 0.) THEN
        WRITE(6,*) 'WARNING: ERROR IN THE EFFICIENCY PLOT!'   
        STOP            
      END IF

      DO IVHIS2=NMHIS2, NVHIS2
        DO ITYPE2=1, NUSE2
          ENTRMCD(ITYPE2,IVHIS2) = HI(IQHIS+10*IVHIS2,ITYPE2+KDEDIFF)
          EFFMC(ITYPE2, IVHIS2) = ENTRMCD(ITYPE2, IVHIS2)/NTRKMC
        ENDDO
      ENDDO   

      DO IVHIS2=0, NVHIS2
        CALL HDELET(IQHIS+10*IVHIS2)
      ENDDO
C
C smearing: offset MC histograms by 1000 and combine theta/p histograms.
C
      CALL SMEARADD(SMBIN, NUSE, NMHIS, NBHIS, 1000)

C
C
C Read in the calibration histograms (Data)
C
      DO 60 IFILE = 1,NDAFIL
        FILE = DAFILE(IFILE)
        LREC = 0
        CALL HROPEN(89,'DACAL',FILE,' ',LREC,ISTAT)
        IF (ISTAT.NE.0) THEN
          WRITE(6,40)
   40     FORMAT(/,' FATAL ERROR: Unable to open data histogram file.')
          STOP
        ELSE
          WRITE(6,50) FILE,LREC
   50     FORMAT(/,' Opened data histogram file ',A80,I6)
        END IF
C
C Get the bank number from the info histogram
        IF (IFILE.EQ.1) THEN
          CALL HRIN (IQHIS+100, 999999, 0)
          CALL HUNPAK(IQHIS+100, SMINFO, ' ', 0)
          ISMINFO = 100*NINT(SMINFO(1))+NINT(SMINFO(2))
          CALL HDELET(IQHIS+100)
        ENDIF
C
C Input the histograms used for the smearing parameters
        KOFF = 0
        IF (IFILE.GT.1) KOFF = 99999
        DO ITYPE=1,NUSE
          DO IVHIS=NMHIS,NBHIS
            CALL HRIN(IQHIS+10*IVHIS+ITYPE,999,KOFF)
          END DO
        END DO
C
C Input the histograms used for the deletion parameters
        DO IVHIS2=0, NBHIS2
          CALL HRIN(IQHIS+10*IVHIS2, 999, 0)
        ENDDO
        CALL HREND('DACAL')
        CLOSE(89)
   60 CONTINUE
C
C Track deletion: data efficiency
C
      CALL SMEARADD(RTBIN, 0, NMHIS2, NBHIS2, 0)
      NEVTDT = HI(IQHIS,1)
      NTRKDT = HI(IQHIS,2)

      IF (NEVTDT .LE. 0. .OR. NTRKDT .LE. 0.) THEN
        WRITE(6,*) 'WARNING: ERROR IN THE EFFICIENCY PLOT!'   
        STOP            
      END IF

      DO IVHIS2=NMHIS2, NVHIS2
        DO ITYPE2=1, NUSE2
          ENTRDTD(ITYPE2,IVHIS2) = HI(IQHIS+10*IVHIS2,ITYPE2+KDEDIFF)
          EFFDT(ITYPE2, IVHIS2) = ENTRDTD(ITYPE2, IVHIS2)/NTRKDT
        ENDDO
      ENDDO   

      DO IVHIS2=0, NVHIS2
        CALL HDELET(IQHIS+10*IVHIS2)
      ENDDO
C
C Estimate the systematic error on the deletion factors
      REVT = NEVTDT/NEVTMC
      RTRK = NTRKDT/NTRKMC
      RERR = ABS(REVT-RTRK)/REVT
      WRITE(6,70) 100*RERR
 70   FORMAT('Systematic error on the deletion factors : ', F5.3, '%')
      IF (RERR.GT.0.01) WRITE(6,75)
 75   FORMAT('WARNING : Error greater than 1% !')
C
C Compute de deletion factor          
      DO ITYPE2=1, NUSE2
        DO IVHIS2=NMHIS2, NVHIS2
          IF(EFFMC(ITYPE2, IVHIS2) .GT. 0) THEN 
            DEL(ITYPE2, IVHIS2) = 1.0 - EFFDT(ITYPE2, IVHIS2)
     +           /EFFMC(ITYPE2, IVHIS2)
          ELSE 
            DEL(ITYPE2, IVHIS2) = 0.00   
          END IF 
        ENDDO
      ENDDO
C
C Combine theta/p data histograms.
C
      CALL SMEARADD(SMBIN, NUSE, NMHIS, NBHIS, 0)
C
C Find the bins used in the histograms.
C
      DO IBIN=1,NBIN
        CALL HIX(IQHIS+10*NVHIS+1,IBIN,XEDGE(1))
        CALL HIX(IQHIS+10*NVHIS+1,IBIN+1,XEDGE(2))
        DCENT(IBIN) = DBLE(XEDGE(1)+XEDGE(2))/2.0D0
        WIDTH(IBIN) = DBLE(XEDGE(2)-XEDGE(1))
        DMIN_BINS(IBIN) = XEDGE(1)
      END DO
      DMIN_BINS(NBIN+1) = XEDGE(2)
C
C Book histograms.
C
      DO ITYPE=1,NUSE
        DO IVHIS=NMHIS,NVHIS
          HID = 10*IVHIS+ITYPE
C
          WRITE(CHTIT,200) ITYPE, IVHIS, 'Smeared MC'
          CALL HBOOKB(HID+2000,CHTIT,NBIN,DMIN_BINS,0.0)
C
          WRITE(CHTIT,200) ITYPE, IVHIS, '(DATA - Smeared MC)/Error'
          CALL HBOOKB(HID+3000,CHTIT,NBIN,DMIN_BINS,0.0)
C
          WRITE(CHTIT,200) ITYPE, IVHIS, 'DATA/Smeared MC'
          CALL HBOOKB(HID+4000,CHTIT,NBIN,DMIN_BINS,0.0)
C
          WRITE(CHTIT,200) ITYPE, IVHIS, '(DATA - MC)/Error'
          CALL HBOOKB(HID+5000,CHTIT,NBIN,DMIN_BINS,0.0)
C
          WRITE(CHTIT,200) ITYPE, IVHIS, 'DATA/MC'
          CALL HBOOKB(HID+6000,CHTIT,NBIN,DMIN_BINS,0.0)
C
          WRITE(CHTIT,200) ITYPE, IVHIS, '1D Smearing Function'
          CALL HBOOK1(HID+7000,CHTIT,100,-20.0,20.0,0.0)
        END DO
      END DO
      DO KG = 1,NGAUSS
        CALL HBOOK1(200+KG,'Random distribution',100,-5.0,5.0,0.0)
      END DO
      CALL HBARX(0)
C
C Initialize MINUIT
C
      CALL MNINIT(5,6,7)
C
C Set MINUIT printout level.
      DUMMY(1) = 2.0
      CALL MNEXCM(SMEARIT,'SET PRI',DUMMY,1,IER,0)
C
C Loop over the track types
C
      DO 100 ITYPE=1,NUSE
C
C Is this a 2D track ?
        TRK2D = TYPE2D(ITYPE)
C
C Loop over theta/p histograms.
C
        DO 90 IVHIS=NMHIS,NVHIS
C
C Output some information
          WRITE(6,80) IVHIS, ITYPE, SMBK
 80       FORMAT(/,' *************************************************',
     +         '*********',/,
     +         ' *** Processing bin #',I1,' of track type ',I1,
     +         ' for bank ',A,'. ***',/,' ****************************',
     +         '******************************',/)
C
C Unpack the histograms
C
          HID = IQHIS+10*IVHIS+ITYPE
          CALL HUNPAK(HID,DTHIST,' ',0)
          CALL HUNPAK(HID+1000,MCHIST,' ',0)
          CALL HUNPKE(HID,DTERR,' ',0)
          CALL HUNPKE(HID+1000,MCERR,' ',0)
C
C Find number of entries in histograms
          TOTDA = 0.0
          TOTMC = 0.0
          DO IBIN=1,NBIN
            TOTDA = TOTDA + DTHIST(IBIN)*WIDTH(IBIN)/WIDTH(1)
            TOTMC = TOTMC + MCHIST(IBIN)*WIDTH(IBIN)/WIDTH(1)
          END DO
C
C If there are no entries ...
          IF (TOTDA .EQ. 0 .OR. TOTMC .EQ. 0) THEN
            WRITE(6,*) 'no entry for histogram type', ITYPE  
            SMIR(1,IVHIS,ITYPE) = 1.0
            SMIR(2,IVHIS,ITYPE) = 0.0
            SMIR(3,IVHIS,ITYPE) = 5.0
            SMIR(4,IVHIS,ITYPE) = 0.0
            GOTO 90
          ENDIF
C
C  Now the errors; why can't HBOOK have a routine to do this???
C  ALSO normalize the histograms, and find the quick range
          IRANGE(1) = 1
          IRANGE(2) = NBIN
          DO IBIN=NBIN,1,-1
            DTHIST(IBIN) = DTHIST(IBIN)/TOTDA
            MCHIST(IBIN) = MCHIST(IBIN)/TOTMC
C If bins have an entry to small, put it to 0
            IF (DTHIST(IBIN).LT.1.0E-8)  DTHIST(IBIN)=0  
            IF (MCHIST(IBIN).LT.1.0E-8)  MCHIST(IBIN)=0  
C If bins have no entries, set error assuming one entry,
            IF (DTHIST(IBIN).EQ.0.0) DTERR(IBIN) = WIDTH(1)/
     +                                             WIDTH(IBIN)
            IF (MCHIST(IBIN).EQ.0.0) MCERR(IBIN) = WIDTH(1)/
     +                                             WIDTH(IBIN)
            DTERR(IBIN) = DTERR(IBIN)/TOTDA
            MCERR(IBIN) = MCERR(IBIN)/TOTMC
            IF(DTHIST(IBIN).EQ.0.0 .OR. MCHIST(IBIN).EQ.0.0)THEN
              IF (IBIN.GT.10) IRANGE(2) = IBIN - 1
            END IF
          END DO
C
C Re-set and re-pack the histograms
C
          CALL HRESET(HID,'DATA')
          CALL HPAK(HID,DTHIST)
          CALL HPAKE(HID,DTERR)
          CALL HRESET(HID+1000,'MC')
          CALL HPAK(HID+1000,MCHIST)
          CALL HPAKE(HID+1000,MCERR)
C
C Initialize the fit parameters
C
          CALL MNEXCM(SMEARIT,'CLEAR',DUMMY,0,IER,0)
          SIGMA = 1.0D0
          AMPLI = 0.1D0
          DO KG = 1,NGAUSS
            CALL MNPARM(2*KG-1,'Sigma',SIGMA,0.3D0*SIGMA,
     +                  0.01D0,1.0D1*SIGMA,IER)
            CALL MNPARM(2*KG  ,'Amplitude',AMPLI,0.3D0*AMPLI,
     +                  0.0D0,1.0D0,IER)
            SIGMA = 5.0*SIGMA
            AMPLI = 0.1*AMPLI
          END DO
C
C Initialize function
          DUMMY(1) = 1.0D0
          CALL MNEXCM(SMEARIT,'CALL',DUMMY,1,IER,0)
C
C First, just fit amplitudes and keep sigma fixed.
C (If fit gives bad chi**2, commenting out next 6 lines may help).
C
          DO KG = 1,NGAUSS   
            DUMMY(KG) = DBLE(2*KG-1)
          END DO     
          CALL MNEXCM(SMEARIT,'FIX',DUMMY,NGAUSS,IER,0)        
          CALL MNEXCM(SMEARIT,'SIMPLEX',DUMMY,0,IER,0)
          CALL MNEXCM(SMEARIT,'RESTORE',DUMMY,0,IER,0)        
C
C Then fit all parameters simultaneously.
C
          CALL MNEXCM(SMEARIT,'SIMPLEX',DUMMY,0,IER,0)
          CALL MNEXCM(SMEARIT,'MIGRAD',DUMMY,0,IER,0)
          CALL MNEXCM(SMEARIT,'HESSE',DUMMY,0,IER,0)
C          CALL MNEXCM(SMEARIT,'MINOS',DUMMY,0,IER,0)
C
C Plot the output and the smearing function
C
          DUMMY(1) = 3.0D0
          CALL MNEXCM(SMEARIT,'CALL',DUMMY,1,IER,0)
C
C Print out the smearing function values
C
          DO IPAR=1,NPAR
            CALL MNPOUT(IPAR,CHNAM,VAL(IPAR),ERROR(IPAR),
     +                  BND1,BND2,IVAR)
            WRITE(6,*) CHNAM,IPAR,VAL(IPAR),ERROR(IPAR)
C
C Add these numbers to the correct row of the SMIR bank
            SMIR(IPAR,IVHIS,ITYPE) = VAL(IPAR)
C
          END DO
          CALL MNSTAT(CHIMIN,FEDM,ERRDEF,NPARI,NPARX,ISTAT)
          WRITE(6,*) 'Fit type is ',ITYPE
          WRITE(6,*) 'Chisquared = ',CHIMIN,' NDOF = ',NDOF-NPAR
          WRITE(6,*) 'Status = ',ISTAT
          WRITE(6,*) 'Fit range = ',IRANGE
C
 90     CONTINUE
 100  CONTINUE
C
C Output cards
C
      FILE = CAFILE
      OPEN(UNIT=32,STATUS='UNKNOWN',FORM='FORMATTED',FILE=FILE)
      WRITE(6,120) FILE
  120 FORMAT(/,' Opened output card file ',A80)
C
C
C Smearing cards :
C
      WRITE(32,130) SMBK, ISMINFO, NPAR, (MAX(1,NVHIS)*NUSE)
 130  FORMAT(A,I4,' /',2I3)
C
      DO ITYPE = 1,NUSE
          WRITE(32,140) ((SMIR(IPAR,IVHIS,ITYPE),IPAR=1,NPAR),
     +                                           IVHIS=NMHIS,NVHIS)
  140   FORMAT(2('    ',2(F7.3,F7.4,:)))
      END DO
C
C
C Deletion cards :
C
      WRITE(32,170) DEBK, ISMINFO, NDEL, (MAX(1,NVHIS2)*NUSE2)
 170  FORMAT(A,I4,' /',2I3)
C
      DO ITYPE2 = 1,NUSE2
        DO IVHIS2 = NMHIS2, NVHIS2
          WRITE(32,180)  DEL(ITYPE2, IVHIS2)
 180      FORMAT('     ',F7.3)
        ENDDO
      ENDDO
C
C
      CLOSE(32)
C
C Output histograms
C
      DO ITYPE=1,NUSE
        DO IVHIS=NMHIS,NVHIS
          HID = 10*IVHIS+ITYPE
          WRITE(CHTIT,200) ITYPE, IVHIS, 'DATA'
          CALL HCOPY(HID+IQHIS, HID, CHTIT)
          CALL HDELET(HID+IQHIS)
C
          WRITE(CHTIT,200) ITYPE, IVHIS, 'MC'
          CALL HCOPY(HID+IQHIS+1000, HID+1000, CHTIT)
          CALL HDELET(HID+IQHIS+1000)
        ENDDO
      ENDDO

      FILE = HIFILE
      OPEN(UNIT=31,FILE=FILE,FORM='UNFORMATTED',RECL=NPRIME,
     1  ACCESS='DIRECT',SHARED,STATUS='UNKNOWN')
      WRITE(6,150) FILE
 150  FORMAT(/,' Opened output histogram file ',A80)
      CALL HRFILE(31,'SMEAR','N')
C
C Save histograms to file in the right order
      DO ITYPE=1,NUSE
        DO IVHIS=NMHIS,NVHIS
          HID = 10*IVHIS+ITYPE
          DO INDEX = 0, 7
            CALL HROUT(HID+1000*INDEX,ICYCLE,' ')
          ENDDO
        ENDDO
      ENDDO
      DO KG = 1,NGAUSS
        CALL HROUT(200+KG,ICYCLE,' ')
      END DO
C
C Save any additional histogram
      CALL HROUT(0,ICYCLE,' ')
      CALL HREND('SMEAR')
      CLOSE(31)
C
      CALL HDELET(0)
C
C Format :
 200  FORMAT('Type: ',I1,' Bin: ',I1,'; ',A)
C
      END
C
C
C
C
      SUBROUTINE SMEARIT(NPAR,GRAD,CHISQ,XVAL,IFLAG,IDUM)
C-----------------------------------------------------------------------
C! This is used by subroutine SMEARFIT, which generates the Monte Carlo
C! track smearing cards for QIPBTAG. It calculates the chi**2 used by 
C! MINUIT in the fit to produce the cards. The calling arguments
C! are the standard MINUIT fcn ones.
C! Author - I.Tomalin 10/11/95
C! Mod.   - P.-F. Giraud 17/08/99
C-----------------------------------------------------------------------
      IMPLICIT NONE
#include "qsmear.h"      
C
C Inputs
C
      REAL*8 XVAL(*),GRAD(*),CHISQ
      INTEGER IFLAG,NPAR,IDUM
C
C Common containing histogram data.
      INTEGER NBIN
      PARAMETER (NBIN=100)
      REAL*4 DTHIST(NBIN),DTERR(NBIN),MCHIST(NBIN),MCERR(NBIN),
     +       TOTDA,TOTMC
      REAL*8 DCENT(NBIN),WIDTH(NBIN)
      INTEGER HID,HJD,IRANGE(2),NDOF
      LOGICAL TRK2D
      COMMON/SMFIT/DTHIST,DTERR,MCHIST,MCERR,TOTDA,TOTMC,DCENT,WIDTH,
     +             HID,IRANGE,NDOF,TRK2D
C
C Common containing random numbers.
      INTEGER KG,NGAUSS,NRAND
      PARAMETER(NGAUSS=2,NRAND=500)
      REAL*4 RANX,RANY
      COMMON/RANNUM/RANX(NRAND,NBIN,NGAUSS),RANY(NRAND,NBIN,NGAUSS)
C
C Local variables
C
      REAL*8 AMP(0:NGAUSS),SIG(NGAUSS)
      INTEGER J,IBIN,JBIN,KBIN,IRAN,MINB,MAXB
      REAL*8 SMHIST(NBIN),SMERR(NBIN),CHIBIN(NBIN)
      REAL*8 DELX,DELY,XL,XH,XLSMIR,XHSMIR,YLSMIR,YHSMIR,DLSMIR,DHSMIR,
     +       FRAC,FACTOR,ERRDIF
      REAL*4 DUMMY(NBIN),SMEAR,DUMRAN,ATOT
C
C Externals
      EXTERNAL RNDM
      REAL*4 RNDM
C-----------------------------------------------------------------------
C
C Unpack the parameters
C
      AMP(0) = 1.0D0
      DO KG = 1,NGAUSS
        SIG(KG) = XVAL(2*KG-1)
        AMP(KG) = XVAL(2*KG)
        AMP(0)  = AMP(0) - AMP(KG)
      END DO
C
      HJD=HID-IQHIS
C
C Initialize variables
C
      CHISQ = 0.0D0
      NDOF = 0
C
C Punish MINUIT if it tries setting one of the amplitudes -ve.
      IF (AMP(0).LT.0.0) THEN
        CHISQ = 1.0D6*AMP(0)**2
        AMP(0) = 0.0D0
      END IF
C
      DO KBIN=1,NBIN
        SMHIST(KBIN) = AMP(0)*MCHIST(KBIN)
      END DO
C
C Smear histogram.
C
C Loop over bins of original MC histogram.
      DO 100 IBIN=1,NBIN
C
C Skip empty bins.
        IF (MCHIST(IBIN).EQ.0.0) GOTO 100
C
C Sum over Gaussians contributing to smearing.
        DO 75 KG = 1,NGAUSS
C Skip Gaussians with zero amplitude.
          IF (AMP(KG).EQ.0.0D0) GOTO 75
C
C Randomly smear this bin and see where it goes.
          DO 50 IRAN = 1,NRAND
            DELX = SIG(KG)*DBLE(RANX(IRAN,IBIN,KG))
            DELY = SIG(KG)*DBLE(RANY(IRAN,IBIN,KG))
C Is this just a 2D track ?
            IF (TRK2D) DELY = 0.0D0
C Where is lower edge of bin smeared to ?
            XL = DCENT(IBIN) - 0.5*WIDTH(IBIN)
            XLSMIR = XL + DELX
            YLSMIR =      DELY
            DLSMIR = SQRT(XLSMIR**2 + YLSMIR**2)
C Which bin does that correspond to ?
            MINB = 1 + INT(DLSMIR/WIDTH(1))
            IF (MINB.GT.50) MINB = 51 + INT((DLSMIR - 4.0D0)/WIDTH(51))
            IF (MINB.GT.90) MINB = 91 + INT((DLSMIR - 36.0D0)/WIDTH(91))
C Did impact parameter change sign ?
            IF (XLSMIR.LT.0.0) THEN
              DLSMIR = -DLSMIR
              MINB = -MINB
            END IF
C Where is upper edge of bin smeared to ?
            XH = DCENT(IBIN) + 0.5*WIDTH(IBIN)
            XHSMIR = XH + DELX
            YHSMIR =      DELY
            DHSMIR = SQRT(XHSMIR**2 + YHSMIR**2)
C Which bin does that correspond to ?
            MAXB = 1 + INT(DHSMIR/WIDTH(1))
            IF (MAXB.GT.50) MAXB = 51 + INT((DHSMIR - 4.0D0)/WIDTH(51))
            IF (MAXB.GT.90) MAXB = 91 + INT((DHSMIR - 36.0D0)/WIDTH(91))
C Did impact parameter change sign ?
            IF (XHSMIR.LT.0.0) THEN
              DHSMIR = -DHSMIR
              MAXB = -MAXB
            END IF
C
C Loop over target bins and add up contribution to each one.
            DO 25 J = MINB,MAXB
              IF (J.EQ.0) GOTO 25
              JBIN = ABS(J)
              IF (JBIN.GT.NBIN) GOTO 25
C Find lower and upper limits of this bin.
              DLSMIR = SIGN(DCENT(JBIN),DBLE(J)) - 0.5*WIDTH(JBIN)
              DHSMIR = SIGN(DCENT(JBIN),DBLE(J)) + 0.5*WIDTH(JBIN)
C Determine what fraction of IBIN contributes to this bin.
              XLSMIR = SIGN(SQRT(MAX(DLSMIR**2-YLSMIR**2,0.0D0)),DLSMIR)
              XL     = XLSMIR - DELX
              XL     = MAX(XL,DCENT(IBIN) - 0.5*WIDTH(IBIN))
              XHSMIR = SIGN(SQRT(MAX(DHSMIR**2-YHSMIR**2,0.0D0)),DHSMIR)
              XH     = XHSMIR - DELX
              XH     = MIN(XH,DCENT(IBIN) + 0.5*WIDTH(IBIN))
              FRAC   = (XH - XL)/WIDTH(IBIN)
C Add up contribution of IBIN to this smeared bin, correcting for
C relative bin widths.
              FACTOR = AMP(KG)*FRAC*(WIDTH(IBIN)/WIDTH(JBIN))/
     +                 FLOAT(NRAND) 
              SMHIST(JBIN) = SMHIST(JBIN) + MCHIST(IBIN)*FACTOR
              IF (SMHIST(JBIN).LT.1.0E-8)  SMHIST(JBIN)=0  
   25       CONTINUE
   50     CONTINUE
   75   CONTINUE
C
  100 CONTINUE
C
C Estimate statistical errors squared on smeared histogram.
C
      DO 150 JBIN = 1,NBIN
        IF (MCHIST(JBIN).GT.0.0) THEN
          SMERR(JBIN) = MCERR(JBIN)*SQRT(SMHIST(JBIN)/MCHIST(JBIN))
        ELSE
          SMERR(JBIN) = SQRT(SMHIST(JBIN)*(WIDTH(1)/WIDTH(JBIN))/TOTMC)
        END IF
  150 CONTINUE
C
C Compare the smeared histogram with the data
C
      DO 200 KBIN = 1,NBIN
        CHIBIN(KBIN) = 0.0D0
        IF(KBIN.GE.IRANGE(1).AND.KBIN.LE.IRANGE(2))THEN
          ERRDIF = SQRT(SMERR(KBIN)**2 + DTERR(KBIN)**2)
          IF (ERRDIF.GT.0.0) THEN
            NDOF = NDOF + 1
            CHIBIN(KBIN) = (DTHIST(KBIN) - SMHIST(KBIN))/ERRDIF
            CHISQ = CHISQ + CHIBIN(KBIN)**2
          END IF
        END IF
  200 CONTINUE
C
C If finished then create some histograms.
C
      IF(IFLAG.EQ.3)THEN
C Histogram of smeared MC.
        DO KBIN = 1,NBIN
          DUMMY(KBIN) = SNGL(SMHIST(KBIN))
        END DO
        CALL HPAK(HJD+2000,DUMMY)
        DO KBIN = 1,NBIN
          DUMMY(KBIN) = SNGL(SMERR(KBIN))
        END DO
        CALL HPAKE(HJD+2000,DUMMY)
C
C Histogram of chi per bin in data minus smeared MC.
        DO KBIN = 1,NBIN
          DUMMY(KBIN) = SNGL(CHIBIN(KBIN))
        END DO
        CALL HPAK(HJD+3000,DUMMY)
C
C Histogram of data/smeared MC.
C
        CALL HOPERA(HID,'/E',HJD+2000,HJD+4000,1.0E6,1.0E6)
C
C Histogram of chi per bin in data minus original MC.
C
        DO KBIN = 1,NBIN
          DUMMY(KBIN) = 0.0
          IF(KBIN.GE.IRANGE(1).AND.KBIN.LE.IRANGE(2))THEN
            ERRDIF = SQRT(MCERR(KBIN)**2 + DTERR(KBIN)**2)
            IF(ERRDIF.GT.0.0)THEN
              DUMMY(KBIN) = (DTHIST(KBIN) - MCHIST(KBIN))/ERRDIF
            END IF
          END IF
        END DO
        CALL HPAK(HJD+5000,DUMMY)
C
C Histogram of data/original MC.
        CALL HOPERA(HID,'/E',HID+1000,HJD+6000,1.0E6,1.0E6)
C
C Histogram of 1D smearing function.
        DO IRAN = 1,NRAND
          DO IBIN = 1,NBIN
            SMEAR = 0.0
            DUMRAN = RNDM(FLOAT(IRAN + IBIN))
            ATOT = AMP(0)
            DO KG = 1,NGAUSS
              IF (DUMRAN.GT.ATOT.AND.DUMRAN.LT.ATOT+AMP(KG))
     +          SMEAR = SNGL(SIG(KG))*RANX(IRAN,IBIN,KG)
              ATOT = ATOT + AMP(KG)
            END DO
            CALL HFILL(HJD+7000,SMEAR,0.0,0.5)
          END DO
        END DO
C
C Plot component random distributions.
        DO IRAN = 1,NRAND
          DO IBIN = 1,NBIN
            DO KG = 1,NGAUSS
              CALL HFILL(200+KG,RANX(IRAN,IBIN,KG),0.0,1.0)
              CALL HFILL(200+KG,RANY(IRAN,IBIN,KG),0.0,1.0)
            END DO
          END DO
        END DO
      END IF
 
      RETURN
      END


      SUBROUTINE SMEARADD(SMBIN, NUSE, NMHIS, NBHIS, IOFF)
C-----------------------------------------------------------------------
C! This is used by subroutine SMEARFIT, which generates the Monte Carlo
C! track smearing cards for QIPBTAG. It takes histograms binned in
C! both p and theta, and by adding them together produces histograms
C! binned in only p or only theta, or without binning.
C!
C! Author - I.Tomalin 10/11/95
C! Mod.   - P.-F. Giraud 17/08/99
C!
C! Arguments :
C!     CHAR*1 SMBIN: 'N' no binning in theta/p
C!                   'T' binning in theta
C!                   'P' binning in p
C!                   'B' binning in both theta/p 
C!     INTE    NUSE: Number of track types in use if smearing,
C!                   or zero if deleting.
C!     INTE   NMHIS: = 1 if binning in p/theta or = 0 if no binning.
C!     INTE   NBHIS: = number of p/theta bins, or = 0 if no binning.
C!     INTE    IOFF: Extra offset of output histograms w.r.t. input.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUSE, NMHIS, NBHIS, IOFF
      CHARACTER*1 SMBIN
      INTEGER ITYPE, IVHIS, HID, JVHIS, HJD
      LOGICAL HEXIST
#include"qsmear.h"
C-----------------------------------------------------------------------
      DO ITYPE = MIN(NUSE,1), NUSE
        DO IVHIS = NMHIS, NBHIS
C  Determine input histogram number.
          HID = IQHIS+10*IVHIS+ITYPE
C  Offset by IOFF and redetermine input histogram number.
          IF (IOFF.NE.0) THEN
            CALL HCOPY(HID,HID+IOFF,' ')
            CALL HDELET(HID)
            HID = IQHIS+IOFF+10*IVHIS+ITYPE
          ENDIF
C
C  Determine output histogram number.
          IF (SMBIN.EQ.'T') THEN
            JVHIS = 1 + MOD(IVHIS-1,NCHIS)
          ELSE IF (SMBIN.EQ.'P') THEN
            JVHIS = 1 + INT((IVHIS-1)/NCHIS)
          ELSE 
            JVHIS = IVHIS
          END IF
          HJD = IQHIS+IOFF+10*JVHIS+ITYPE
C 
          IF (HID.NE.HJD) THEN
            IF (HEXIST(HJD)) THEN
              CALL HOPERA(HID,'+',HJD,HJD,1.0,1.0)
            ELSE
              CALL HCOPY(HID,HJD,' ')
            END IF
            CALL HDELET(HID)
          END IF
        END DO
      END DO
      END






