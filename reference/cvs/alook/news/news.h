   ALE:LOOK.OLB, _D.OLB, .EXE, .NEWS, ALOOK:[LOOK]*.F, ALOOK:[INC]*.h
   with $ define ALOOK $2$DKB200:[general_a.ale.look]

   /aleph/gen/liblook.a, _dbx.a, /bin/alook, /src/alook


C! last changes
   -----------------------------------------------------------
 ! 981106 - LOOK version 25 date 981106
   mods in: LKSCAN
            continue reading after a BOS error 5.
            LKBANK
            calling inconsistencies.
            LKPRROW
            put DATA statments at the end of declarations and 
            remove calling inconsistencies.
            LKREAD0
            put the keyboard input at the same line as the prompt
            on UNIX.
            LKLOOK
            missing/coooected declarations.

   -------------------------------------------------------------
 ! 980525 - LOOK version 24 date 980525
   mods in: LKHELP
            add definition of DROP command

   ------------------------------------------------------------
 ! 970829 - LOOK version 23 date 970829
   mods in: LKNICE
            to correct a bug introduced with the last correction
            (L/KINE was not working anylonger)
            LKLOOK, LKHELP
            LOOK/XEDIT works now on ALPHA/OSF and ALPHA/VMS

   ------------------------------------------------------------
 ! 970818 - LOOK version 22 date 970818
   mods in: LKGTPAR, LKLOOK
            to please LINUX
            LKLOOK, LKHELP
            to introduce LOOK/XEDIT on VMS only
            LKNICE
            to correct a bug in LOOK/FORMAT

   new      lkattprms.f lkeditb.f  lkscenter.f
   routines:lkalptab.f  lkdcomm2.f lkrmvsp.f   lkxedit.F

   new incl:phead.h

   -----------------------------------------------------------         
 ! 960901 - LOOK version 21 date 960901

   +++++++++++++++++ BECAREFUL ++++++++++++++++++++++++++
   this is the 1st version maintained with CVS
       On UNIX  
   the source file is avalaible on :
       $ALROOT/alook/look/*.F, $ALROOT/alook/inc/*.h
   compiled with 
       f77 $FCOPT [-g] -I$ALROOT/look/inc $ALROOT/look/*.*F

       on ALWS:
       $ define ALOOK $2$DKB200:[general_a.ale.alook]
       $ fpp:== $AL1$USER3:[RANJARD]fpp_axp.exe
       $ ufor:== "@AL1$USER3:[RANJARD.COM]ufor.com"
   the source is avalaible on:
       ALOOK:*.F and ALOOK:*.h
   compiled with:
       $ ufor ALOOK:xxx.F -IALOOK [\deb]

   ++++++++++++++++++++++++++++++++++++++++++++++++++++++

   mods in: LKCHKRU
            add FSEQ as 1st argument.
            IF run in increasing order (FSEQ=T) and selected
            run < current run THEN rewind EDIR/DATA file.
            ELSE look forward for the selected run.
            IF eof on EDIR file THEN try to read another FILM
            card (call LKOPRD1 entry point in LKOPRD).
            LKRREC
            get /NSEQ option early enough to call LKCHKRU with
            FSEQ as 1st argument.
            when eof on EDIR file call LKOPRD1 (entry point in
            LKOPRD) to see if there is another FILM card on the
            same EDIR file.
            LKOPRD
            add an entry point LKOPRD1 to skip resetting of units
            and opening a new file when looking for another FILM
            card on the same EDIR file.
            LKREAC
            change a comment.
            LKNWSEQ
            check /NSEQ option in  OPEN/data_type command.
            LKK7RU
            remove call to LKCHKRU.
   ------------------------------------------------------------- 
 ! 960418 - LOOK version 20 date 960418
            use cpp preprocessor to compile 
            The flag "#if defined(VAX)" has been replaced by 
            "#if defined(ALEPH_DEC) || ! defined(UNIX)"  
   ------------------------------------------------------------
 ! 950808 - LOOK version 19 date 950808
   mods in: LKPRROW
            do not stop printing banks when number of rows = 0
   ------------------------------------------------------------
 ! 950606 - LOOK version 18 date 950606
   mods in: LKK7RU, LKNWSEQ
            adapt to new RUNCARTSLIST format
   -------------------------------------------------------
 ! 950321 - LOOK version 17 date 950321
   mods in: LKNWSEQ, LKHELP
            introduce QUERY option: O/CART/QUERY AB1234.1.SL
            to know if a cartridge is already staged.
            LKRREC
            compare wanted event no. to last event read
            LKRWND
            rewind data file when edir is rewounded
            LKSHOW
            don't use IW(3) to find index of 1st bank but
            use nameindices of EVEH,RUNR or RUNH
   ----------------------------------------------------------
 ! 950307 - LOOK version 16 date 950307
   mods in: LKCHKRU
            look for run# NRUN in forward or backward direction
            LKRREC
            if wanted event# is < current event# call LKRWND.
            LKRWND
            if the current data file is the 1st one in the EDIR
            file do not reopen it.
   -------------------------------------------------------------
 ! 940707 - LOOK version 15 date 940707
   mods in: LKGTEVT
            When reading without EDIR,if CLASS 25 is selected
            (slow control record) then skip events.
            remember to read a class: R/CLASS n will select class n
            for following reads. R/CLASS 0 returns to no selection.
   ------------------------------------------------------------
 ! 940621 - LOOK version 14 date 940621
   mods in: LKFILI
            declare INTEGER ACARD1 ... to please ALPHA/OSF1
            LKK7RU
            stage the K7 everytime an EPIO file is referenced.
   ---------------------------------------------------------
 ! 931108 - LOOK version 13 date 930811
   mods in: LKLOOK, LKGTPAR
            On VAX and AXP/VMS declare INTEGER  system routines
            such as LIB$SPAWN , SMG$... to please the AXP debugger.
   ----------------------------------------------------------------
 ! 931012 - LOOK version 12 date 931012
   mods in: LKFILI
            redefine the length of FILI and FILO bank using
            NBANK.
            LKNICE
            print HLWI and HWDI with a special routine only
            when format is not given
            LKPRROW
            does not change the input argument NUM (bank
            number to be printed)
   -------------------------------------------------------
 ! 931001 - LOOK version 11 date 931001
   mods in: LKFILI
            give the right length of FILI and FILO bank
 ! 930615 - LOOK version 10 date 930615
   mods in: LKMAIN
            CALL TIMEST(1000000.) to make sure that TIMEL
            returns always enough time in interactive mode
            on VAX.
   -------------------------------------------------------
 ! 930527 - LOOK version 9 date 930527
   mods in: LKINIT
            increase size of BOS format to 120 characters.
   mods in: LKLOOK
            replace function calls GETENV and SYSTEM by
            subroutine calls to please HP.

   -------------------------------------------------------
 ! 920922 - LOOK version 8 date 920922

    +++++++++++++++ BECAREFUL ++++++++++++++++++++++++++++
    this version NEEDS BOS77 no. 1889 and ALEPHLIB 14.0
    ++++++++++++++++++++++++++++++++++++++++++++++++++++++

            new options implemented:
          - UNIX Historian flag
          - use the new ALEPHLIB function ALGTENV.
          - data cards are retrieved using the environment
            name LOOKCARDS.
          - OPEN/CART tapeno : will generate the following
            FILI card: FILI 'ALDATA | EPIO | CART tapeno.1.SL'
          - OPEN/EDIR tapeno : will generate
            On VAX: FILI ' AL$EDIR:tapeno.EDIR |EDIR'
            On IBM: FILI ' tapeno EDIR * | EDIR'
          - OPEN/EPIO tapeno : will generate
            On VAX: FILI ' AL$DATA:tapeno.EPIO |EPIO'
            On IBM: FILI ' tapeno EPIO * | EPIO'

   mods in: LKLOOK
            wrong keyword
            LKHELP
            add few commands and put them in alphabetic order
            LKLOOK, LKGTPAR, LKREAD0
            UNIX flag
            LKFILI
            suppress entry points LKFILO, LKFIL2, LKSEVT
            LKINIT
            use ALGTENV
            LKNWSEQ, LKOPRD
            implement /CART option

 -----------------------------------------------------------------

 ! 920115 - LOOK version 7 date 920115

            new options implemented:
          - By default the first event is read in when opening
            a new file or after a rewind.
            To skip this fast reading use the command 'SLOW'
            before the command 'OPEN' or 'REWIND'. To go back
            to the default give the command 'DROP SLOW'.
            'SLOW' can be a data card of the 'LOOK CARDS' file.
          - open a file containning a given run number for a given
            data type : RAW/POT/DST/MINI/NANO
                 OPEN/RAW  nrun
                 OPEN/POT  nrun
                 OPEN/DST  nrun
                 OPEN/MINI nrun
                 OPEN/NANO nrun
             The first time the file RUNCARTS.LIST is opened and
             read. If the run and data type is part of the list
             the tape number is returned.
             On VAX
               For DST or MINI
                   a file is looked for with the following name:
                     AL$EDIR:tapeno.EDIR if not there then
                     AL$DATA:tapeno.EPIO
                     AL$EDIR:D(/M)000nrun.EDIR
                     AL$DATA:D(/M)000nrun.EPIO
               For other data type
                     AL$DATA:tapeno.EPIO
             On IBM

   mods in: LKLOOK,LKHELP,LKREAC
            to implement SLOW command.
            LKNICE
            nice printout of HWDI and HLWD.
            LKOPWR,LKPRBNK,LKPRDDL,LKPRIN
            the print file LOOK_OUT.PRINT is opened with LREC=132
            LKFILI,LKGTPAR,LKINIT,LKOPRD,LKOPWR,LKPRCOM,LKRECAL
            LKSTAT
            replace LENOCC (which is obsolete) by LNBLNK

 ---------------------------------------------------------------
 ! 911120 - LOOK version 6 date 911120
            all deck names have been changed and start with LK

             new option implemented:
             3 kinds of output files are avalaible
             WRITE name nr * will write bank NAME,NR onto
             LOOK_OUT.CARDS , LREC=72
             WRITE/OUTPUT list will write BOS list onto
             LOOK_OUT.EPIO  , LREC=32040
             LOOK/PRINT name nr will print bank NAME,NR onto
             LOOK_OUT.PRINT , LREC=120

   mods in : LKFIL,LKMAIN,LKLOOK,LKINIT,LKPREVE,LKPRIN,LKPRDDL
             LKPRBNK,LKHELP,LKSTAT,LKWRCAR,LKWRLIS
             LKRREC
             read in BATCH mode when requiring CLASS 25 on EDIR.
             LKSCAN
             in case of EDIR file read the EDIR file only in BATCH.
             LKSHOW
             the handling of 'C' and 'R' lists is done in ABRREC.
             LKRWND
             call AOPERD to reopen the right data file in case
             of EDIR file.
             drop all existing lists and reset them to '0'.
             then if EDIR file read it till the 1st event in BATCH
             mode
             LKOPRD
             in case of a new file close the previous one and reset
             logical units.
             open input file. Read it till the 1st event in BATCH
             mode
   new
   routines: LKOPWR
             to open a new output file.
             LKREAC
             to read new input file or file rewound until the 1st
             event in BATCH mode to be sure to get the run record(s)
             LKSTUNP (ULIST)
             to set the unpack list used in AUNPCK
             the 1st call to LKSTUNP sets the default unpack list
             ENTRY LKRSUNP
             to reset the unpack list to the default
             LKSTOUT (LOUT)
             to set the terminal output unit to LOUT.
             the 1st call to LKSTOUT sets the default terminal
             output unit.
             ENTRY LKRSOUT
             to reset the terminal output unit to the default.
             LKGTEVT importation of GETEVT from ALEPHLIB

   ---------------------------------------------------------
 ! 911015 - LOOK version 5 date 911015
   mods in : LKLOOK, LKRREC, LKHELP, LKRWND
             GETEVT has been moved to ALEPHLIB
             new options implemented:
             READ/CLASS [class-bit]
             class-bit = 0 means reset read class word
             class-bit < 0 means set reset class word and set bit
             class-bit > 0 means add the new class-bit
             REWIND/EDIR   means rewind edir file
                   /DATA   means rewind epio/native file
             CLASS         means print class word

   ---------------------------------------------------------------
 ! 910926 - LOOK version 4 date 910926
   mods in : LKRREC, GETEVT, LKHELP
             implement option Read/Run which was forseen but
             not implemented.
                 READ/RUN nrun
             if runs are not in increasing order on the file it
             is quite possible that the search stops before the
             run is found. So it is possible to say
                 READ/RUN/NSEQ nrun

   --------------------------------------------------------
 ! 910910 - LOOK version 3 date 910910
   mods in : LKMAIN, LKINIT, LKWRCAR, LKWRLIS
             update of the options WRITE and WRITE/OUTPUT=..
             - Write NAME NR filename
             writes a single bank NAME,NR on file filename
             in CARD format (lrec=72)
             if filename = '*' the default file name is
             LOOK_OUT.CARDS
             it is not possible to change the filename during
             a LOOK session.
             - Write/OUT="fname ftype fmod" list
             will write the content of the list on the file fname
             in the format specified by the ftype.
             if ftype is neither EPIO or NATIVE it is assumed
             to be CARD.
             if fname is not given the default file name will be
             LOOK_OUT EPIO A.

   ------------------------------------------------------------
 ! 910723 - LOOK version 2 date 910723
   mods in : LKRREC
             drop E-list and reset it to 0 before reading next
             record.

   -------------------------------------------------------------
 ! 910703 - LOOK version 2 date 910703
   is maintained under Historian with flags VAX and IBM.

   It is possible to read a LOOK CARDS file on IBM and VAX.
   On VAX it is no longer possible to give parameters such as
   data base and input file name on the LOOK command.

   It is possible to read event directories and sequential
   EPIO or NATIVE files.
   on IBM the filename filetype filemode must be given between ""
   as in:
            O/EDIR "D0011044 EDIR *"
   A GIME must be issued before through the COMmand facility if
   necessary.
   on VAX:
           O/EPIO AL$DATA:D001144.EPIO
   on VAX it is not possible to read an EPIO file sequentially
   when the previous EPIO file has been opened in direct access.

   All bank formats are read from the BANKAL FMT file.

   It is possible to overwrite the format of bank at printing
   time:
           L/HEX X1AD 0
   will print the X1AD bank in hexadecimal.
           L/F="2I,(J)" X1AD 0
   will print it in 16-bits words.
           L/F=" " X1AD 0
   will restore the original format for X1AD.
   ---------------------------------------------------------

