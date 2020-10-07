#if defined(YDEBUG)
C! YTOP debug common
      COMMON/YDUMTO/ IDUMPP(40),NCLROU(12),ICLROU(12)
      EQUIVALENCE (NCTOP,NCLROU(1)),(NCPRI,NCLROU(2))
      EQUIVALENCE (NCPOS,NCLROU(3)),(NCBLD,NCLROU(4))
      EQUIVALENCE (NCSDT,NCLROU(5)),(NCAHX,NCLROU(6))
      EQUIVALENCE (NCFHX,NCLROU(7)),(NCFVM,NCLROU(8))
      EQUIVALENCE (NCPRM,NCLROU(9)),(NCSTO,NCLROU(10))
      EQUIVALENCE (NCCON,NCLROU(11)),(NCVZR,NCLROU(12))
      EQUIVALENCE (ICTOP,ICLROU(1)),(ICPRI,ICLROU(2))
      EQUIVALENCE (ICPOS,ICLROU(3)),(ICBLD,ICLROU(4))
      EQUIVALENCE (ICSDT,ICLROU(5)),(ICAHX,ICLROU(6))
      EQUIVALENCE (ICFHX,ICLROU(7)),(ICFVM,ICLROU(8))
      EQUIVALENCE (ICPRM,ICLROU(9)),(ICSTO,ICLROU(10))
      EQUIVALENCE (ICCON,ICLROU(11)),(ICVZR,ICLROU(12))
C
#if defined(DOC)
C
C  IDUMPP: PRINTOUT CONTROL FOR VERTEX TOPOLOGY PROGRAMS
C
C IDUMPP DESCRIPTION
C     1,2  ............... YTOPOL
C     3,4 ................ YPRIVX
C     5,6 ................ YVPOSS
C     7 .................. YBLDVX
C     9 .................. YSDVTR
C     11,12 .............. YADHVX
C     13,14 .............. YFTHVX
C     15,16 .............. YFUMSC
C     17,18 .............. YPRTOP
C     19,20 .............. YSTTOP
C     21,22 .............. YTSTRK
C     23,24 .............. YTCONV
C     25,26 .............. YTRV0S
C     27,28 .............. YTOSVT
C     29,30 .............. YPIDAS
C     31,32 ..............(YCFVTR)
C     33,34 .............. YFTVTR
C     35,36 .............. YFMVTR
C     37,38 .............. YTPAR

C  NCLROU: NUMBER OF CALLS TO ROUTINES
C
#endif
#endif
