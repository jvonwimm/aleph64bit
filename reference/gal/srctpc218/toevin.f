      SUBROUTINE TOEVIN(LUN,IFL)
C-----------------------------------------------------------------------
C!  Read BOS banks created by GALEPH job and assign them to
C!  'C' or 'E' list as appropriate
C
C  Called from:  TORDEV
C  Calls:        BLIST, BREAD, NAMIND, NLIST
C
C  Input:   PASSED:      --LUN, unit number to read from
C           READ:        --BOS read of unknown banks, either track
C                          elements or constants
C
C  Output:  BANKS:       --list 'C', containing constants
C                          (if necessary) and list 'E', containing
C                          track elements
C  M. Mermikides
C  D. Cowen    12 Feb 88 --Use BREAD instead of BOSRD so we can
C                          specify EPIO vs. FORT (native) formats.
C                          An earlier BUNIT call sets these formats.
C
C                        --Append TSIM bank to C list after RUNH,
C                          EVEH banks have already been put there.
C                          Done so JULIA can read TPCSIM output.
C
C  P. Janot    23 Mar 88 --Keep event even if it doesn't get
C                          through the TPC ! The test is now
C                          done with EVEH instead of TPTE.
C
C  D. Cowen    28 Mar 88 --Extract the magnetic field from GALEPH
C                          bank AFID if possible;
C
C  P. Janot    29 Mar 88 --Write out C list if not empty.
C
C  P. Janot    06 May 88 --Load TPC geometry each new run.
C
C  D. Cowen     07 JUL 88 --Clear C list early on.
C-----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER(CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
C  Additional constants for TPCSIM
C  Units -- Mev,Joules,deg Kelvin,Coulombs
C
      REAL ELMASS,CROOT2,CKBOLT,CROOMT,ECHARG
      PARAMETER (ELMASS = 0.511)
      PARAMETER (CROOT2 = 1.41421356)
      PARAMETER (CKBOLT = 1.380662E-23)
      PARAMETER (CROOMT = 300.)
      PARAMETER (ECHARG = 1.602189E-19)
C
C  TPCOND  conditions under which this simulation
C  will be performed
C
      COMMON /DEBUGS/ NTPCDD,NCALDD,NTPCDT,NCALDT,NTPCDA,NCALDA,
     &                NTPCDC,NCALDC,NTPCDS,NCALDS,NTPCDE,NCALDE,
     &                NTPCDI,NCALDI,NTPCSA,NCALSA,NTPCDR,NCALDR,
     &                LTDEBU
      LOGICAL LTDEBU
      COMMON /SIMLEV/ ILEVEL
      CHARACTER*4 ILEVEL
      COMMON /GENRUN/ NUMRUN,MXEVNT,NFEVNT,INSEED(3),LEVPRO
      COMMON /RFILES/ TRKFIL,DIGFIL,HISFIL
      CHARACTER*64 TRKFIL,DIGFIL,HISFIL
      COMMON /TLFLAG/ LTWDIG,LTPDIG,LTTDIG,LWREDC,FTPC90,LPRGEO,
     &                LHISST,LTPCSA,LRDN32,REPIO,WEPIO,LDROP,LWRITE
      COMMON /TRANSP/ MXTRAN,CFIELD,BCFGEV,BCFMEV,
     &                        DRFVEL,SIGMA,SIGTR,ITRCON
      COMMON /TPCLOK/ TPANBN,TPDGBN,NLSHAP,NSHPOF
      COMMON /AVLNCH/ NPOLYA,AMPLIT,GRANNO(1000)
      COMMON /COUPCN/ CUTOFF,NCPAD,EFFCP,SIGW,SIGH,HAXCUT
      COMMON /TGCPCN/ TREFCP,SIGR,SIGARC,RAXCUT,TCSCUT
      COMMON /DEFAUL/ PEDDEF,SPEDEF,SGADEF,SDIDEF,WPSCAL,NWSMAX,THRZTW,
     &                LTHRSH,NPRESP,NPOSTS,MINLEN,
     &                LTHRS2,NPRES2,NPOST2,MINLE2
      COMMON /SHAOPT/ WIRNRM,PADNRM,TRGNRM
C
      LOGICAL LTWDIG,LTPDIG,LTTDIG,LPRGEO,
     &        LWREDC,LTPCSA,LHISST,FTPC90,LRND32,
     &        REPIO,WEPIO,LDROP,LWRITE
C
      LOGICAL LTDIGT(3)
      EQUIVALENCE (LTWDIG,LTDIGT(1))
C
      REAL FACNRM(3)
      EQUIVALENCE (WIRNRM,FACNRM(1))
C
C
      CHARACTER*4 NAME,NLIST,BLANK
C
      DATA LBASE/4/,LOUT/6/
      DATA NREC/0/
      DATA BLANK/'    '/
C
C  Read BOS record in list R
C
      IFL = 0
C
      CALL BLIST(IW,'C=','0')
 1    NREC = NREC + 1
      CALL BLIST(IW,'R=','0')
      CALL BREAD(IW,LUN,'R',*801, *802)
C
C  See what kind of banks we have
C
      IDTRK = 0
      NITRK = NAMIND('EVEH')
      IF(NITRK.GT.0) IDTRK =  IW(NITRK)
      IBANK = 0
C
C  If this isn't an event, step through banks that we have read and
C  put them on the C list.  When we have gone through the entire list
C  from this read, go and do another read to get an event block.
C
      IF ( IDTRK .EQ. 0 ) THEN
C
C Extract magnetic field from GALEPH bank AFID if possible.
C Override previous (default or card-read) value.
C
         IF (NREC .NE. 1) GOTO 11
         NARUNH = NAMIND('RUNH')
         IF(NARUNH .GT. 0) THEN
           KRUNH = IW(NARUNH)
           IF(KRUNH .GT. 0) NUMRUN = IW(KRUNH + 2)
C
C  Load standard TPC geometry in commons. Also extra parameters
C  for breaking track elements at sector boundaries.
C
           IF (FTPC90) THEN
              CALL TRDT90
           ELSE
              CALL TRDDAF (LBASE,NUMRUN,IRET)
              IF (IRET .EQ. 0) THEN
C  Stop if at least one bank is missing
              WRITE(LOUT,'(/3X,''+++TOEVIN+++ missing bank on ADBS -'',
     &                       ''CALL EXIT'')')
              CALL EXIT
              ELSE IF (IRET .LT. 0) THEN
C        at least one bank has been reloaded
                 IF (LPRGEO) CALL TGEPRI (LOUT)
              ENDIF
           ENDIF
           CALL TPBGEO
         ENDIF
         NAAFID = NAMIND('AFID')
         IF (NAAFID .GT. 0) THEN
            KAFID = IW(NAAFID)
            IF (KAFID .GT. 0) THEN
               CFIELD = RW(KAFID+2+3)
               BCFGEV = CLGHT*CFIELD*1.E-5
               BCFMEV = BCFGEV*1000.
               WRITE(6,99)  CFIELD
99             FORMAT(//10X,'Magnetic Field read from',
     &                   1X,'GALEPH BANK AFID',
     &                /,10X,' --Present value is ',F7.2,
     &                   1X,'KGauss',//)
            ELSE
               WRITE(6,100) CFIELD
            ENDIF
         ELSE
            WRITE(6,100) CFIELD
         ENDIF
  100    FORMAT(//,10X,' Magnetic Field',
     &              1X,'not read from GALEPH BANK AFID!!!',
     &           /,10X,' --Using value of ',F7.2,' KGauss',//)
C
C  Fill C_list
C
 11      IBANK = IBANK + 1
         NAME = NLIST(IW,IBANK,'R')
         IF ( NAME .EQ. BLANK ) THEN
C
C  Append TSIM to C list after other banks in first record
C  Then write out and clear C list
C
           IF (NREC.EQ.1) CALL BLIST(IW,'C+','TSIM')
           IF (LWRITE) CALL BWRITE(IW,25,'C')
           CALL BLIST(IW,'C=','0')
           GOTO 1
         ENDIF
         IF ( NAME .EQ. 'TSIM' ) GOTO 11
         CALL BLIST(IW,'C+',NAME)
         GO TO 11
      ELSE
C
C  Event bank. Step through banks and transfer to E list; when there
C  are no more banks, quit
C
 22      IBANK = IBANK + 1
         NAME = NLIST(IW,IBANK,'R')
         IF ( NAME .EQ. BLANK ) GO TO 999
         CALL BLIST(IW,'E+',NAME)
         GO TO 22
C
      ENDIF
C
C  Read error
C
 801  WRITE(6,901) NREC
      IFL = 1
      RETURN
C
C  End of file
C
 802  WRITE(6,902) NREC
      IFL = 1
      RETURN
C
C  Normal successful completion; clear R list.
C
 999  CALL BLIST(IW,'R=','0')
C
      RETURN
C
 901  FORMAT(' ++TOEVIN++ Read Error , nrec =',I8,' QUIT')
 902  FORMAT(' ++TOEVIN++ EOF after record number: ',I8,' QUIT')
C
      END
