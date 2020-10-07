      SUBROUTINE GETLEP(IRUN,IFOUN,IFILL,NV,ELEP,XYZ,DXYZ)
C----------------------------------------------------------------------
C! Gets the LEP energy and beam position for run IRUN
C  Author  J.Boucrot  15-June-1990
CKEY ALEF LFIL
C Input argument :
C  IRUN = run number to be searched in above bank
C Output arguments :
C   IFOUN = 0 if no information found for run IRUN
C         = 1 if information found in 'LFIL' bank of ADBSCONS DAF
C         = 2 if information found in the RUN Header banks
C             ( WARNING ! in this case , the quantities described
C               below have PROVISIONAL  values ! )
C   ELEP  = LEP center-of-mass energy for this run , in GEV/C2
C
C The following arguments are defined only if IFOUN.GT.0  :
C   IFILL = LEP fill number  for this run :
C     NV  = number of hadronic events used to compute XYZ , DXYZ
C    XYZ  = X,Y,Z values of beam crossing for this run , in cm
C   DXYZ  = Errors on XYZ , in cm
C
C Description:
C get the information from the LFIL data base bank.
C if unsuccessful , get it from the SOR bank : JSUM.
C
C        ENTRY GETOFS :
C        SUBROUTINE GETOFS(IRUN,OFSET)
C
C  Must be called after GETLEP
C  Output argument :
C    OFSET = Average D0 for this run ( Last word of bank LFIL )
C            used for systematic effects on vertex position
C----------------------------------------------------------------------
      REAL XYZ(*),DXYZ(*)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JJSUNT=1,JJSUNV=2,JJSUNZ=3,JJSUNL=4,JJSUNB=5,JJSUVT=6,
     +          JJSUVV=7,JJSUVZ=8,JJSUVL=9,JJSUVB=10,JJSUTT=11,
     +          JJSUTZ=12,JJSUTB=13,JJSULI=14,JJSUIZ=15,JJSULO=16,
     +          JJSULZ=17,JJSUXV=18,JJSUYV=19,JJSUZV=20,JJSUXS=21,
     +          JJSUYS=22,JJSUZS=23,JJSUKB=24,JJSUKW=25,JJSUTN=26,
     +          JJSUTS=27,JJSUTV=28,JJSUA0=29,JJSUA1=30,JJSUA2=31,
     +          JJSUA3=32,JJSUA4=33,JJSUA5=34,JJSUA6=35,JJSUA7=36,
     +          JJSUA8=37,JJSUA9=38,JJSUB0=39,JJSUB1=40,JJSUB2=41,
     +          JJSUB3=42,JJSUB4=43,JJSUB5=44,JJSUB6=45,JJSUB7=46,
     +          JJSUB8=47,JJSUB9=48,LJSUMA=48)
      PARAMETER(JLFILF=1,JLFIFR=2,JLFILR=3,JLFINV=4,JLFILE=5,JLFIBX=6,
     +          JLFIBY=7,JLFIBZ=8,JLFIEX=9,JLFIEY=10,JLFIEZ=11,
     +          JLFIOF=12,LLFILA=12)
      SAVE OFST
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
C----------------------------------------------------------------------
      IFOUN = 0
C
C get LEP energy and the row # in LFIL bank for run # IRUN
C
      ELEP = ALEFIL (IRUN,JLFIL,IROW)
      JLFIL = IABS (JLFIL)
      IF (JLFIL.GT.0 .AND. IROW.GT.0) THEN
C
C    'LFIL' bank found : get the output arguments
         IFOUN=1
         KLFIL=KROW(JLFIL,IROW)
         IFILL=IW(KLFIL+JLFILF)
         NV   = IW(KLFIL+JLFINV)
         DO 100 IX=1,3
            XYZ(IX) = RW(KLFIL+JLFIBX+IX-1)
            DXYZ(IX) =RW(KLFIL+JLFIEX+IX-1)
 100     CONTINUE
         OFST=RW(KLFIL+JLFIOF)
      ELSE
C
C     No 'LFIL' bank . Try to find the run header bank 'JSUM' :
C
         JJSUM=NLINK('JSUM',IRUN)
         IF (JJSUM.EQ.0) GO TO 999
         IFOUN=2
         NV= ITABL (JJSUM,1,JJSUNV)
         IF (NV .GE. 2)  THEN
C        compute XYZ and errors :
            V = REAL (NV)
            XYZ(1) = RTABL(JJSUM,1,JJSUXV) / V
            XYZ(2) = RTABL(JJSUM,1,JJSUYV) / V
            XYZ(3) = RTABL(JJSUM,1,JJSUZV) / V
            DXYZ(1)= (RTABL(JJSUM,1,JJSUXS) / V - XYZ(1)**2) / (V - 1.)
            DXYZ(2)= (RTABL(JJSUM,1,JJSUYS) / V - XYZ(2)**2) / (V - 1.)
            DXYZ(3)= (RTABL(JJSUM,1,JJSUZS) / V - XYZ(3)**2) / (V - 1.)
            DXYZ(1)= SQRT( MAX(DXYZ(1),0.0) )
            DXYZ(2)= SQRT( MAX(DXYZ(2),0.0) )
            DXYZ(3)= SQRT( MAX(DXYZ(3),0.0) )
            OFST=0.
         ENDIF
      ENDIF
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY GETOFS(IRUN,OFSET)
      OFSET=OFST
C
 999  RETURN
      END
