      SUBROUTINE GETLUM(IRUN,IFOUN,IRQFL,NZ,NB,RLUMI,BK,BT)
C----------------------------------------------------------------------
C! Gets the luminosity and # of Z0s and Bhabhas for a run
C  Author  J.Boucrot  15-June-1990
C  Modified 28-Sep-1992 for runs with SICAL
C  Modified 19-May-1993 to get informations from Sical bank 'SLUM'
CKEY ALEF LFIL LUMINOSITY
C
C Input   : IRUN    / INTE = run number
C
C Output  : IFOUN   / INTE = return code
C                            0   no information for run IRUN
C                            1   information from 'RLUM' bank
C                            2   information from the RUN Header banks
C                                JSUM and LUMI  ( LCAL)
C                            3   information from the RUN Header banks
C                                JSUM and SLUM  ( Sical )
C            The following arguments are defined only if IFOUN.GT.0  :
C           NZ      / INTE = number of Z0 --> Hadrons
C           NB      / INTE = number of Bhabhas
C           RLUMI   / REAL = Best estimate of Luminosity , in nb**-1
C             The following arguments are defined only if IFOUN=1
C           IRQFL   / INTE = Run_Quality flag for this run
C                     Bit 0 set = 'PERF'    1 = 'MAYB'     2 = 'DUCK'
C           BK      / REAL = Number of background Bhabha events
C           BT      / REAL = Bhabha trigger efficiency
C
C Description:
C  get the RLUM data base bank which contains the run IRUN
C  if the bank does not exist get the information from the SOR banks
C  JSUM and LUMI.
C----------------------------------------------------------------------
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
      PARAMETER(JLUMHI=1,JLUMLO=2,JLUMCP=3,JLUMSA=4,JLUMTR=5,JLUMF1=6,
     +          JLUMF2=7,JLUMNS=8,JLUMF3=9,JLUMF4=10,JLUMBH=11,
     +          JLUMNP=12,JLUMB1=13,JLUMB2=14,JLUMLL=15,JLUMVH=16,
     +          JLUMNV=17,JLUMLU=18,JLUMSE=19,JLUMSY=20,LLUMIA=20)
      PARAMETER(JRLURN=1,JRLURQ=2,JRLUNZ=3,JRLUNB=4,JRLULU=5,JRLUBK=6,
     +          JRLUBT=7,LRLUMA=7)
      PARAMETER(JSLUSI=1,JSLUSU=2,JSLUDP=3,JSLUTI=4,JSLUTO=5,JSLULI=6,
     +          JSLULO=7,JSLUVR=8,JSLUHR=9,JSLULU=10,JSLULV=11,
     +          JSLULH=12,JSLUSE=13,JSLUSY=14,LSLUMA=14)
      INTEGER ALGTRO
C - runs with SiCAL > 16600
      DATA NRSIC / 16600 /
C - runs with LUMI from Sical only ( 1993 , 1994 and 1995 LEP I )
      DATA NR93  / 20000 /
C - High energy runs with LUMI from LCAL only :
      DATA NLP2  / 40000 /
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
      IFOUN=0
      NZ=0
      NB=0
      IRQFL=0
      RLUMI=0.
      JRLUM = ALGTRO ('RLUM',IRUN,JRLURN,JROWR)
      IF (JRLUM.NE.0 .AND. JROWR.GT.0) THEN
C     RLUM bank found : get the output arguments
         JRLUM = IABS (JRLUM)
         IFOUN=1
         KRLUM=KROW(JRLUM,JROWR)
         IRQFL=IW(KRLUM+JRLURQ)
         NZ   = IW(KRLUM+JRLUNZ)
         NB   = IW(KRLUM+JRLUNB)
         RLUMI= RW(KRLUM+JRLULU)
         BK   = RW(KRLUM+JRLUBK)
         BT   = RW(KRLUM+JRLUBT)
      ELSE
C  No 'RLUM' bank . Try to find run header banks 'JSUM' , 'LUMI' , 'SLUM
         JJSUM = NLINK('JSUM',IRUN)
         IF (JJSUM.GT.0) THEN
            NZ = ITABL (JJSUM,1,JJSUVZ)
         ENDIF
C For runs before NR93 , or after NLP2 : take bank 'LUMI'
         IF (IRUN.LT.NR93.OR.IRUN.GT.NLP2) THEN
            JLUMI=NLINK('LUMI',IRUN)
            IF (JLUMI.EQ.0) GO TO 999
C Take method # 5  for LEP I runs without SICAL
C      method # 9  for LEP I runs with SICAL :
C      method # 10 for LEP II runs
            IMETH = 5
            IF ( IRUN.GT.NRSIC) IMETH = 9
            IF ( IRUN.GT.NLP2 ) IMETH = 10
            NB = RTABL (JLUMI,IMETH,JLUMB2)
            RLUMI = RTABL (JLUMI,IMETH,JLUMLU)
            IFOUN=2
C For runs after NR93 : take bank 'SLUM' , method 2 ,
C if not available try  'LUMI' , method 10 :
         ELSE
            JSLUM=NLINK('SLUM',IRUN)
            IF (JSLUM.GT.0) THEN
               IMETH=2
               NB = RTABL (JSLUM,IMETH,JSLUTI)
               RLUMI = RTABL (JSLUM,IMETH,JSLULU)
               IFOUN=3
            ELSE
               JLUMI=NLINK('LUMI',IRUN)
               IF (JLUMI.EQ.0) GO TO 999
               IMETH=10
               NB = RTABL (JLUMI,IMETH,JLUMB2)
               RLUMI = RTABL (JLUMI,IMETH,JLUMLU)
               IFOUN=2
            ENDIF
         ENDIF
      ENDIF
C
 999  RETURN
      END
