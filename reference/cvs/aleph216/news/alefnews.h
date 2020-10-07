CKEY ALEF UTILITY BANK
C! general utilities
   this sets contains various subprograms directly callable by the USER.

 ! ALEPHLIB 21.6
    ALELEP - Replace previous code by call to identical routine ALEFIL 
             (M.Cattaneo 10/02/1997)
    ALSECS - Remove test on century for leap year (year 2000 is a leap year)
             (M.Cattaneo 27/02/1997)

 ! ALEPHLIB 21.4
    ALSECS - New routine to convert "Aleph format" date/time to integer
             number of seconds elapsed since 1-JAN-1988  (O.Schneider)

 ! ALEPHLIB 21.3
    ALKJOB - Use kjobjj.h instead of kgjjpar.h
    RQBUNC - Set NWAG and INBU for '96. Avoid trying to get the info from the
             unreliable LZZT as the bunch train scheme doesn't change 
             within the year.

 ! ALEPHLIB 21.1
    RQBUNC - tagging of level 3 was not working at the beginning of 95
             so set IBUN=1, IQUA=3 for runs in 4x1 configuration and
             leave as before those in 4x2 configuration .

 ! ALEPHLIB 21.0
    GETLUM - get LCAL luminosity for High Energy runs ( LEP II )
    GETS4B - new routine : Sical lumi per bunch for multibunch runs

 ! ALEPHLIB 20.9
    ALFMT  - IF called for ALL banks : ALFMT (LUFMT,'ALL ',FMT)
             after the 1st call, it will check ALL formats and redefine
             those which are different from those stored on BANKAL.FMT

 ! ALEPHLIB 20.8
    ADBRJJ, ADBRLIST - add one column for beam position periods.
    ALLEP1 - logical function ALLEP1(irun) is true if energy of
             run # irun is .lt. 100GeV

 ! ALEPHLIB 20.7
   ALTRIG - when X1RG is there take trigger bits from row starting
            with 'TPR '. (that was not the case for MC events.)

 ! ALEPHLIB 20.6
   ALFMT  - increase the number of bank formats to 1500.
   BKTOBK - new routine to copy a bank to another bank
            CALL BKTOBK ('ABCD',NR1,'EFGH',NR2)
            'EFGH' format is taken as 'ABCD' format.
   ALK7FIL, ALK7OP, ALK7TRU, ALK7FRU -
            Adapt to new RUNCARTSLIST format

 ! ALEPHLIB 20.5
   GETLEP - add a protection against negative sqrt.

 ! ALEPHLIB 20.4
   ALGTDB : make sure that the required bank is there.

 ! ALEPHLIB 20.3
   CAFIHT : remove a bug

 ! ALEPHLIB 20.2
   ALFIEL : give the correct value of the mag field for runs 25261
            and 25265.
   BKRHAL - get computer name on which the job is run.(P.Comas)
   BKRHAW - change the RHAH bank format.

 ! ALEPHLIB 20.1
   NAMERU : new FUNCTION to get the row# of NAME bank containing
            a given run.
   ALK7COM: Increase number of segments to 3 which allows 7500 runs
            instead of 5000.
 ! ALEPHLIB 20.0
   ALFIEL : '92 offset is valid for 92 and 93 only

 ! ALEPHLIB 15.8
   ALTRIG : use X1RG
   ALTRHV : new INTEGER FUNCTION to return HV word from X1RG or XTCN

 ! ALEPHLIB 15.8
   remove *CA ALRCOM from ALPHARD set, introduce an entry point AOPEWW
   in AOPEWR to get IFWITH flag from ALREAD set into ALPHARD set,
   move some routines from ALREAD to ALEF or to ALPHARD to be
   prepared to cut ALEPHLIB into ALEPHIO and ALEPHLIB.
   ALEPHIO will contain the machine dependant code : ALREAD, EPIO, PRESS
   ALEPHLIB will be machine independant

 ! ALEPHLIB 15.7
   TRACBCK : CALL EXIT after the traceback on IBM.
   ALFMT : limit the test on the first 3 characters of NAME to find
           'ALL'
   ALFIEL : the 92 current offset is valid for 92,93,94....
   GTT0GL : T0GL depends on TPC setup code.
   UINDXS : call SORTI instead of SORTX deleted from CERNLIB 94A.

 ! ALEPHLIB 15.6
   ALFMT : add a SAVE

 ! ALEPHLIB15.5
   ALEFIL, ALELEP : check that the row # returned by LFILRU is > 0
   GETLEP : check that the row # returned by ALEFIL is > 0

 ! ALEPHLIB 15.2
   ALGTRO : bug correction
   ALTRIG : look for LUPA or SILH bank to get trigger bits when no other
            bank is present.
   ALVSN :  add a protection against slow control records (IRUN=0)
   GETLUM : modified to get the lumi / nb of bhabhas from Sical in 1993
   GETSLU : modified to get the lumi from bank 'SLUM' if necessary
   ALFMT  : increase size of bank formats to 75 characters to fit LPZT
            bank format. That is the maximum length allowed on a file
            with a record length of 80.

 ! ALEPHLIB 15.1
   LFILRU : remove a bug which happened when reading '90 '91' '92
            data with a DBAS.BANK which contains '92 banks.

 ! ALEPHLIB 14.5
   New routine GETSLU : gets SICAL Lumi and nb of Bhabhas for a given
                        run.

