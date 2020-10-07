       SUBROUTINE READBS (FCARD,LUNIT,TAB,COL,OUT)
C ---------------------------------------------------------
C - F.Ranjard - 900320
C! read BS file and build BS tables
C - Input    : FCARD   / CHA4  = data card name
C              LUNIT   / INTE  = logical unit number
C              TAB     / CHA4  = Table bank name
C              COL     / CHA4  = Column bank name
C - Output   : OUT     / CHA4  = output bank name
C ---------------------------------------------------------
#ifndef DOC
      CHARACTER*(*) FCARD,TAB,COL,OUT
      CHARACTER*4 TTAB,TCOL,NAME,CHAINT,FMT
      CHARACTER*16 NAMO,NACOL,DIM
      CHARACTER*4 NAFLOW(10)
      INTEGER IC(2)
#include "bcs.h"
#include "cobsjj.h"
#include "tabsjj.h"
#include "tcoljj.h"
#include "ttabjj.h"
#include "tdocjj.h"
#include "tstcjj.h"
      DATA NAFLOW / 'STC ','LTC ','DOC ','RAW ','KIN ','GAL ','JUL ',
     &              'POT ','MDST','DST '/
#include "bmacro.h"
C
C ------------------------------------------------------------------
C - Get the .BS file in read mode on LUNIT
C
       CALL AGTFIL (FCARD,'READ',LUNIT,IERS)
       NRTAB = NDANR(LUNIT,TAB,'GE',-1)
       JTAB  = MDARD(IW,LUNIT,TAB,NRTAB)
       IF (JTAB.NE.0) THEN
          WRITE(IW(6),*) TAB,' NR= ',NRTAB,JTAB,(IW(JTAB+M),M=0,2)
       ELSE
          WRITE(IW(6),*) '  no .Tab on .BS file ',FCARD
       ENDIF
       NRCOL = NDANR(LUNIT,COL,'GE',-1)
       JCOL  = MDARD(IW,LUNIT,COL,NRCOL)
       IF (JCOL.NE.0) THEN
          WRITE(IW(6),*) COL,' NR= ',NRCOL,JCOL,(IW(JCOL+M),M=0,2)
          JCOLTI = LCOLS(JCOL)
       ELSE
          WRITE(IW(6),*) '  no .Col on .BS file ',FCARD
       ENDIF
       IF (JCOL.EQ.0 .OR. JTAB.EQ.0) RETURN
C
CCC       CALL PRTABL (TAB,NRTAB)
CCC       CALL PRTABL (COL,NRCOL)
C
C - Create DOC.DAF banks
C
       NROW = LROWS(JCOL)
       TCOL = OUT
       CALL AUBOS (TCOL,0,NROW*LTCOLA+LMHLEN,JTCOL,IGARB)
       IF (JTCOL.EQ.0) THEN
          WRITE(IW(6),*) ' NOT enough space for ',TCOL, '- STOP'
          CALL EXIT
       ENDIF
       IW(JTCOL+LMHCOL) = LTCOLA
       IW(JTCOL+LMHROW) = 0
       CALL BKFMT (TCOL,'2I,(5A,I,2A)')
C
       NTAB = LROWS(JTAB)
       TTAB = 'T'//OUT(2:4)
       CALL AUBOS (TTAB,0,NTAB*LTTABA+LMHLEN,JTTAB,IGARB)
       IF (JTTAB.EQ.0) THEN
          WRITE(IW(6),*) ' NOT enough space for ',TTAB, ' -STOP'
          CALL EXIT
       ENDIF
       IW(JTTAB+LMHCOL) = LTTABA
       IW(JTTAB+LMHROW) = 0
       CALL BKFMT(TTAB,'2I,(A,I)')
       IF (IGARB.GT.0) THEN
          JTCOL = NLINK(TCOL,0)
          JTAB  = NLINK(TAB,NRTAB)
          JCOL  = NLINK(COL,NRCOL)
       ENDIF
       JDEB = NLINK('DEBU',0)
C
C - loop over columns in COBS table
C
       ICHAR0 = ICHAR('0')
       JNAID = 0
       ID    = 0
       DO 100 I=1,LROWS(JCOL)
C       COBS row# I index
         KCOL = KROW(JCOL,I)
         IF (CHAINT(ITABL(JCOL,I,JCOBTY)).EQ.'IMPL') GOTO 100
C       table id
          NID  = ITABL(JCOL,I,JCOBTI)
C       row # in TAB
          IF (NID .NE. JNAID) THEN
             JNAID = NID
             IF (ITABL(JTAB,ID+1,1) .EQ. NID) THEN
                ID = ID+1
             ELSE
                ID = LOCTAB(IW(JTAB+LMHLEN+1),IW(JTAB+LMHCOL),
     &                      IW(JTAB+LMHROW),1,NID)
                IF (ID.LE.0) THEN
                   WRITE (IW(6),*) '  READBS error ',FCARD, ' row# ',I
                   RETURN
                ENDIF
             ENDIF
          ENDIF
C       column name
          CALL ALSTIN (IW(KCOL+JCOBNC),JCOBDI-JCOBNC,NACOL)
C       column dimension
          NDIM = 1
          IF (CHAINT(IW(KCOL+JCOBDI)).NE.'====') THEN
             CALL ALSTIN (IW(KCOL+JCOBDI),JCOBTY-JCOBDI,DIM)
             LDIM = LNBLNK(DIM)
             KF = 0
 90          KL=INDEX(DIM(KF+1:),',') +KF
             IF (KL.EQ.KF) KL = LDIM
             NDIM = NDIM * ICDECI (DIM,KF+1,KL)
             KF = KL
             IF (KL.LT.LDIM) GOTO 90
          ENDIF
C       fill next row in TCOL: table name, column name, dim, format
          KTCOL = KNEXT(JTCOL)
C         table name
          IW(KTCOL+JTCONA)=ITABL(JTAB,ID,JTABNA)
C         column name
          CALL UCOPY (IW(KCOL+JCOBNC),IW(KTCOL+JTCONC),JTCONI-JTCONC)
C         the format of the column
          FMT = CHAINT(IW(KCOL+JCOBFO))
          IF (FMT(1:1).EQ.'A') THEN
C            number of columns of a character variable
             N = ICHAR(FMT(2:2))-ICHAR0
             IF (FMT(3:3).NE.' ')N = 10*N+(ICHAR(FMT(3:3))-ICHAR0)
             NX = N/4
             FMT = 'A4'
          ELSE
             IF (FMT(1:2).EQ.'Z8') FMT='I11'
             NX=1
          ENDIF
          IW(KTCOL+JTCONI) = NDIM*NX
          CALL UCOPY (IW(KCOL+JCOBFO),IW(KTCOL+JTCOFO),LTCOLA-JTCOFO+1)
          IW(KTCOL+JTCOFO) = INTCHA(FMT)
          IW(JTCOL+LMHROW) = LROWS(JTCOL)+1
 100   CONTINUE
C
       CALL AUBPRS(TCOL)
       IF (JDEB.GT.0) CALL PRTABL(TCOL,0)
C
C - fill TTAB bank
C
       NCOUT = LROWS(JTCOL)
       INAMO = 0
       DO 200 I=1,NCOUT
          INAME = ITABL(JTCOL,I,JTCONA)
          IF (INAME.NE.INAMO) THEN
             INAMO = INAME
             KTTAB = KNEXT(JTTAB)
             IW(KTTAB+JTTANA) = INAME
             IW(KTTAB+JTTARC) = I
             IW(JTTAB+LMHROW) = LROWS(JTTAB)+1
          ENDIF
 200   CONTINUE
C
C - add table without any attribute at the end of TTAB
C
       IF (LROWS(JTTAB).LT.LROWS(JTAB)) THEN
          J=0
          DO 201 I=1,LROWS(JTAB)
             J = J+1
             NAME = CHAINT(ITABL(JTAB,I,JTABNA))
             NAMO = CHAINT(ITABL(JTTAB,J,JTTANA))
             IF (NAME.NE.NAMO) THEN
                KTTAB = KNEXT(JTTAB)
                IW(KTTAB+JTTANA)= ITABL(JTAB,I,JTABNA)
                IW(KTTAB+JTTARC)= 0
                IW(JTTAB+LMHROW)= LROWS(JTTAB)+1
                IF (LROWS(JTTAB).EQ.LROWS(JTAB)) GOTO 202
                J = J-1
              ENDIF
 201       CONTINUE
       ENDIF
 202   CONTINUE
C
       CALL AUBPRS(TTAB)
       CALL SORTIQ(IW(JTTAB+LMHLEN+1),LCOLS(JTTAB),LROWS(JTTAB),JTTANA)
       IF (JDEB.GT.0) THEN
          CALL PRTABL(TTAB,0)
          DO 1001 I=1,LROWS(JTAB)
            NAME = CHAINT(ITABL(JTAB,I,JTABNA))
            NAMO = CHAINT(ITABL(JTTAB,I,JTTANA))
            IR   = ITABL(JTTAB,I,JTTARC)
            NACOL= CHAINT(ITABL(JTCOL,IR,JTCONA))
            WRITE (IW(6),'(1X,I4,1X,A4,3X,A4,I6,3X,A4)')I,NAME,NAMO,
     &      IR,NACOL
 1001    CONTINUE
       ENDIF
C
       END
#endif
