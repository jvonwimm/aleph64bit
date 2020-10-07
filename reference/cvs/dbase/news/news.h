C! last changes
#if defined(DOC)
   DOC:DBSLIB.HLB, .FOR, .OLB, _D.OLB, .NEWS

   ----------------------------------------------------------
 ! 991001 - DBSLIB version 22 date 991001
   mods in: owdaf.h
            mods previously introduced for Linux was misplaced

   -----------------------------------------------------------
 ! 990301 - DBSLIB version 21 date 990301
   mods in: owdaf.h
            to include ALEPH_LINUX
            limit size of TFIL in BDABF calling sequence
            remove references to ADA32 or ADA31: there is
            only one version of ADAMO which is 32
 
   --------------------------------------------------------- 
 ! 981203 - DBSLIB version 20 date 981203
   mods in: eptoda, cardaf, dafcar, epiofmt, crtdaf, upddaf
            to increase the number of characters allowed in 
            filename due to /afs/cern.ch prefix
  
   ----------------------------------------------------------
 ! 980817 - DBSLIB version 19 date 980817
   add    : sbkmain.F for non UNIX platforms.
  
   ---------------------------------------------------------- 
 ! 980227 - DBSLIB version 18 date 980225
   mods in: eptoda.F to drop E-list between each bank reading
            followed by a garbage collection.

   -----------------------------------------------------------
 ! 970414 - DBSLIB version 17 date 970414
   mods in: lbos.h to increase BCS array size and maximum number
            of banks is now 1500.
            eptoda.F epio file has the ALEPH record length
            = 32040 (no longer 3600) 
            epiofmt.F epio file is written with ALEPH record
            length = 32040 (no longer 3600)

   ------------------------------------------------------------
 ! 960208 - DBSLIB version 16 date 960208
   mods in: BCS and SBKPAR to increase the maximum number of
            banks to 1300.

   ----------------------------------------------------------
 ! 951016 - DBSLIB version 15 date 951016
   mods in: BCS Increase LBOS parameter to 700000

   ----------------------------------------------------------
 ! 951011 - DBSLIB version 14 date 951011
            correct a bug in CRTFMT

   ----------------------------------------------------------
 ! 950404 - DBSLIB version 13 date 950404
   mods in: BCS and SBKPAR to increase the maximum number of
            banks to 1200.

   ----------------------------------------------------------
 ! 950125 - DBSLIB version 12 date 950125
   mods in: COBSJJ, TABSJJ, UPDDAF, READBS to cope with
            ADAMO 3.2

   ---------------------------------------------------------
 ! 941011 - DBSLIB version 11 date 941011
   mods in: BCS increase length
            CARDAF, EPTODA, DAFCAR, EPIOFMT
            introduce environment variable to open data cards
   ----------------------------------------------------------
 ! 930525 - DBSLIB version 10 date 930525
   mods in: CTRFMT allow format with more than 40 characters
                   maximum is set to 120 characters.

   ----------------------------------------------------------
 ! 930121 - DBSLIB version 9 date 930121
            include a HP, RS6K, SGI, DECS version.
            DAFB data card can have 2 words:
             word 1 : total number of records
             word 2 : number of directory records
            OUTB is an alias for DAFB
   mods in: OWDAF, EPTODA, CARDAF, CRTDAF, UPDDAF

   -----------------------------------------------------------
 ! 930112 - DBSLIB version 8 date 930112
            create a UNIX version of SBANK
            look at SBKNEWS for more details

   ----------------------------------------------------------
 ! 921015 - DBSLIB version 7 date 921015
            correct a bug in STREFER called by
            CREATE_SBANK_INDEX

   ----------------------------------------------------------
 ! 921009 - DBSLIB version 6 date 921009
            use the data card NOTA in CRTFMT to suppress
            le miniheader in the format for non-tabular banks.
            give a miniheader of 3 to KINE and VERT

   -----------------------------------------------------------
 ! 920626 - DBSLIB version 5 date 920626
            introduce a data card NOTA in CARDAFto give the
            list of non-tabular banks which have no mini-header
            Such banks cannot be checked for consistency
            NOTA 'ABCD'  'GHJI' .....

   ----------------------------------------------------------
 ! 920527 - DBSLIB version 4 date 920527
            increase the number of banks in BCS to 1000

   ----------------------------------------------------------
 ! 920422 - DBSLIB version 3 date 920422
            contains execs, programs and subroutines related
            to the data base ADBSCONS
            It contains the following sets:

            - set EXEC,EXECNEWS.EXECEND
              contains VAX command files
              define DBS AL1$USER3:[RANJARD.DBASE]
              BANKAL : DBS:BANKAL.COM
              DBSCOM : DOC:DBSCOM.COM
              NEWADBS: DBS:UPDATE_NEWADBS.COM
              UPDADBS: DBS:UPDATE_ADBS.COM
              UPDBNK : DBS:UPDATE_BANK.COM
              UPDSBK : DOC:UPDATE_SBANK.COM
              XCARDAF: DBASE:CARDAF.COM
              XCHKCRD: DBASE:CHKCRD.COM
              XDAFCAR: DBASE:DAFCAR.COM

            - set HAC,HACNEWS.HACEND
              contains the HAC parameters used in the library

            - set CD,CDNEWS.CDEND
              contains common blocks and macros used in the
              library.

            - set PROG,PROGNEWS.PROGEND
              contains programs which can be executed from DBSCOM

            - set SUBR,SUBRNEWS.SUBREND
              contains routines used by several programs.

            - set SBANK,SBKNEWS.SBKEND
              contains SBANK, CREATE_SBANK, HACPAR programs
              and related comdecks.
   -----------------------------------------------------------
#endif
