      SUBROUTINE MINVMC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill vertex MC bank DVMC for Mini-DST.
C
C     Author: Stephen Haywood      21-Nov-90
C
C     Input  : FVER bank
C     Output : DVMC bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFVEVX=1,JFVEVY=2,JFVEVZ=3,JFVETO=4,JFVEIP=5,JFVEIS=6,
     +          JFVENS=7,JFVEVN=8,JFVEVM=9,LFVERA=9)
      PARAMETER(JDVMVX=1,JDVMVY=2,JDVMVZ=3,JDVMIP=4,JDVMVN=5,JDVMVM=6,
     +          LDVMCA=6)
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
C
C++   Speed of light in cm/s.
C
      PARAMETER (CCMPS=1.E9*CLGHT)
C
C++   Labels for vertex mechanism.
C
      PARAMETER (NVMEC=31)
      DIMENSION VMEC(NVMEC),IVMEC(NVMEC)
      CHARACTER*4 VMEC
      DATA VMEC /
     &  'NEXT','MULS','LOSS','FIEL','DCAY','PAIR','COMP','PHOT',
     &  'BREM','DRAY','ANNI','HADR','ECOH','EVAP','FISS','ABSO',
     &  'ANNH','CAPT','EINC','INHE','MUNU','TOFM','PFIS','SCUT',
     &  'RAYL','NONE','PRED','LOOP','NULL','STOP','SHOW' /
      LOGICAL FIRST
      SAVE FIRST,IVMEC,NULL,CTMAX
      DATA FIRST,IVMEC,NULL / .TRUE.,NVMEC*0,0 /
      DATA CTMAX / 1000. /
C
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
C++   Pick up FVER bank.
C
      KFVER = NLINK('FVER',0)
      IF (KFVER.LE.0) RETURN
      NFVER = LROWS(KFVER)
      IF(NFVER.LE.0) RETURN
C
C++   Initialisation for vertex mechanism labels.
C
      IF(FIRST) THEN
         DO I=1,NVMEC
            IVMEC(I) = INTCHA(VMEC(I))
         ENDDO
         NULL = INTCHA('    ')
         FIRST = .FALSE.
      ENDIF
C
C++   Create the DVMC bank.
C
      LEN = LMHLEN + LDVMCA * NFVER
      CALL AUBOS('DVMC',0,LEN, KDVMC,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINVMC: Cannot create DVMC bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KFVER = NLINK('FVER',0)
      ENDIF
      IW(KDVMC+LMHCOL) = LDVMCA
      IW(KDVMC+LMHROW) = NFVER
C
C++   Copy FVER to DVMC, integerising positions.
C++   Time of flight is stored as a distance.
C++   The track relation information is dropped since it is contained
C++   in DTMC.
C
      DO I=1,NFVER
         IW(KROW(KDVMC,I)+JDVMVX) = NINT(DFACTM * RTABL(KFVER,I,JFVEVX))
         IW(KROW(KDVMC,I)+JDVMVY) = NINT(DFACTM * RTABL(KFVER,I,JFVEVY))
         IW(KROW(KDVMC,I)+JDVMVZ) = NINT(DFACTM * RTABL(KFVER,I,JFVEVZ))
C        CT = CCMPS * RTABL(KFVER,I,JFVETO)
C        IF(CT.GT.CTMAX) CT = CTMAX
C        IW(KROW(KDVMC,I)+JDVMTO) = NINT(DFACTM * CT)
         IW(KROW(KDVMC,I)+JDVMIP) = ITABL(KFVER,I,JFVEIP)
         IW(KROW(KDVMC,I)+JDVMVN) = ITABL(KFVER,I,JFVEVN)
C
C++      Represent the vertex mechanism by an integer corresponding to
C++      list VMEC. New mechanisms are represented by the integer
C++      form of their character string.
C
         IF(LCOLS(KFVER).GE.JFVEVM) THEN
            LABEL = ITABL(KFVER,I,JFVEVM)
            IF(LABEL.EQ.NULL) THEN
               JVMEC = 0
            ELSE
               INDX = IUCOMP(LABEL,IVMEC,NVMEC)
               IF(INDX.GT.0) THEN
                  JVMEC = INDX
               ELSE
                  JVMEC = LABEL
               ENDIF
            ENDIF
         ELSE
            JVMEC = 0
         ENDIF
         IW(KROW(KDVMC,I)+JDVMVM) = JVMEC
      ENDDO
C
C++   Add the bank to the Mini list.
C++   Since the format includes an 'A' it is explicitly envoked.
C
      CALL MINLIS('DVMC')
      CALL BKFMT('DVMC','2I,(4I,A,I)')
C
      RETURN
      END
