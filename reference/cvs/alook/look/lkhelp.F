      SUBROUTINE LKHELP
C -------------------------------------------------------------
C! list all possible commands
C -------------------------------------------------------------
#ifndef DOC
#include "lkfil.h"
C -------------------------------------------------------------
      WRITE(LOUT,1000)' C list contains RUN header banks.'
      WRITE(LOUT,1000)' E list contains EVENT banks.'
      WRITE(LOUT,1000)' S list contains UNPACKED banks.'
      WRITE(LOUT,1000)' T list contains DATABASE banks.'
      WRITE(LOUT,1000)
      WRITE(LOUT,1000)
     +' Banks                          : Display bank names in '//
     +'C,E,S and T lists'
      WRITE(LOUT,1000)
     +'    /Length                     : Display banks names, '//
     +'numbers and lengths'
      WRITE(LOUT,1000)
     +'    /All                        : Display banks using BOS'//
     +' routine'
      WRITE(LOUT,1000)
     +'    /Daf                        : Display banks on Direct'//
     +' Access File'
      WRITE(LOUT,1000)
     +' Class                          : Display class_bits set'
      WRITE(LOUT,1000)
     +' Command [line]                 : Issue system command '//
     +'(shell on VAX)'
      WRITE(LOUT,1000)
     +' Decompress                     : Decompress banks in E '//
     +'and C lists'
      WRITE(LOUT,1000)
     +' Drop [bank] [nr]               : Drop the bank from memory '
      WRITE(LOUT,1000)
     +' Exit                           : Leave program'
      WRITE(LOUT,1000)
     +' Help                           : This screen'
      WRITE(LOUT,1000)
     +' Look [bank] [nr]               : Print bank in tabular form'
      WRITE(LOUT,1000)
     +'    /Bos                        : Print bank in normal bos form'
      WRITE(LOUT,1000)
     +'    /Daf                        : get the bank from the DAF '//
     +' before printing it'                    
      WRITE(LOUT,1000)
     +'    /PRINT [bank][nr][file]     : Write output to file'//
     +' if file=* then default file name is LOOK_OUT PRINT'
      WRITE(LOUT,1000)
     +'    /Rows=n1-n2                 : Print rows n1 to n2'
      WRITE(LOUT,1000)
     +'    /Hex                        : Print in hexadecimal'
      WRITE(LOUT,1000)
     +'    /Format=user format         : Print with a user format'
      WRITE(LOUT,1000)
     +'    /Kine                       : Special printing for '//
     +'KINE and FKIN'
      WRITE(LOUT,1000)
     +'    /Part                       : Special printing for PART'
      WRITE(LOUT,1000)
     +' Formats                        : Display bank formats'
      WRITE(LOUT,1000)
     +' Open [filename]                : Open input file, option is '//
     +'MANDATORY'
      WRITE(LOUT,1000)
     +' Open/QUERY [filename]          : stage query input file'
      WRITE(LOUT,1000)
     +'    /CART                       : Open CARTridge in EPIO'
      WRITE(LOUT,1000)
     +'    /DAF                        : Open new DAF file'
      WRITE(LOUT,1000)
     +'    /EDIR                       : Open new EDIR file'
      WRITE(LOUT,1000)
     +'    /EPIO                       : Open EPIO file'
      WRITE(LOUT,1000)
     +'    /NATIve                     : Open NATIVE file'
      WRITE(LOUT,1000)
     +'    /NODEc                      : Run header event will '//
     +'be read without decompressing'
      WRITE(LOUT,1000)
     +'    /DST [run number]          : Open the DST EDIR file '//
     +'which contains this run'
      WRITE(LOUT,1000)
     +'    /MINI [run number]         : Open the MINI EDIR file '//
     +'which contains this run'
      WRITE(LOUT,1000)
     +'    /NANO [run number]         : Open the NANO EPIO file '//
     +'which contains this run'
      WRITE(LOUT,1000)
     +'    /POT [run number]          : Open the POT EPIO file '//
     +'which contains this run'
      WRITE(LOUT,1000)
     +'    /RAW [run number]          : Open the RAW EPIO file '//
     +'which contains this run'
      WRITE(LOUT,1000)
     +' Read [event_nr]                : Read next event or one '//
     +'specified'
      WRITE(LOUT,1000)
     +'    /RUN [run_nr]               : Read run specified or ' //
     +'next event in current run'
      WRITE(LOUT,1000)
     +'    /RUN/NSEQ [run_nr]          : Read run specified without '//
     +'assuming increasing order'
      WRITE(LOUT,1000)
     +'    /CLASS [class_bit]          : Read specified class '//
     +'if class_bit=0 then reset the class word'
      WRITE(LOUT,1000)
     +'    /Daf [bank_name]            : Read bank from Direct '//
     +'Access File'
      WRITE(LOUT,1000)
     +'    /Card [filename]            : Read from data card file'
      WRITE(LOUT,1000)
     +'    /Ulist[="xx yy"]            : Define unpacking list for POT'
      WRITE(LOUT,1000)
     +'    /Nodec                      : Read without decompressing'
      WRITE(LOUT,1000)
     +' RECall                         : VMS RECALL command (almost)'
      WRITE(LOUT,1000)
     +' Rewind                         : Rewind input file '
      WRITE(LOUT,1000)
     +'    /EDIR                       : Rewind EDIR file'
      WRITE(LOUT,1000)
     +'    /DATA                       : Rewind DATA file'
      WRITE(LOUT,1000)
     +' SCan                           : read input file to get'//
     +' number of run,event and slow control records'
      WRITE(LOUT,1000)
     +' SLow                           : Read every records'
      WRITE(LOUT,1000)
     +' Write [name nr file]           : Write bank in card format'//
     +' if file=* then default file name is LOOK_OUT CARDS'
      WRITE(LOUT,1000)
     +' Write /Output [list] [file]    : Write banks contained in '//
     +'list to output file .'
      WRITE(LOUT,1000)
     +'                                  if file=* default file name'//
     +' is LOOK_OUT EPIO'
      WRITE(LOUT,1000)
     +' Write/Out/Epio [list] [file]   : Write list in EPIO onto file'
      WRITE(LOUT,1000)
     +' Write/Out/Native [list] [file] : Write list in NATIVE onto file'
#if defined(ALEPH_DEC)
      WRITE(LOUT,1000)
     +' XEdit [name nr]                : look at bank with Xdesigner'
      WRITE(LOUT,1000)
     +'    /Daf                        : get the bank from the DAF '//
     +' before printing it'                    
#endif
 1000 FORMAT(A)
      RETURN
      END
#endif
