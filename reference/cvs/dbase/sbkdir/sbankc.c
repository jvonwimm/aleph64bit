/********************************************************************/  
/*                                                                  */  
/* main.c                     Helge Meinhard   11-Jan-1993          */  
/*                                                                  */  
/* Main program for the sbank tool on UNIX systems                  */  
/* Format: sbank [-nb] [-nc] [-nr] [-nd] [-u] [-o file] [bank]      */  
/*         -nb        no booking                                    */  
/*         -nc        no contents                                   */  
/*         -nr        no references                                 */  
/*         -nd        no description (= comment)                    */  
/*         -u         undocumented                                  */  
/*         -o file    output to file (default sbank.out)            */  
/*         bank       bank name                                     */  
/*------------------------------------------------------------------*/  
/* Programmer's note:                                               */  
/* After decoding the command line options, this program calls the  */  
/* FORTRAN routine SBKDEC with the following arguments (all input   */  
/* to SBKDEC):                                                      */  
/* XBOOK    /L      .true. if booking to be shown                   */  
/* XCONT    /L      .true. if contents to be shown                  */  
/* XREFE    /L      .true. if references to be shown                */  
/* XCOMM    /L      .true. if comment (description) to be shown     */  
/* XUNDO    /L      .true. if undocumented banks to be shown        */  
/* XOUTP    /L      .true. if an output disk file to be written     */  
/* CHOUT    /C*80   name of output disk file, if any                */  
/* CHARG    /C*80   parameter for sbank search (bank name), if any  */  
/* SBKDEC is supposed to store the parameters in some convenient    */  
/* place. Afterwards, main.c calls the FORTRAN routine PRSBANK      */  
/* without arguments, which is supposed to do the actual sbank      */  
/* work.                                                            */  
/*                                                                  */  
/* This file contains also a routine, callable from FORTRAN as      */  
/* CALL UNIXLINES(LINESPP,CLEAR,HOME,S_IN,S_OUT), which returns    */  
/* the following information in its arguments (all output of        */  
/* UNIXLINES):                                                     */  
/* LINESPP   /I*2   number of lines per terminal page               */  
/* CLEAR     /C*80  steering sequence for clear screen              */  
/* HOME      /C*80  steering sequence for cursor home               */  
/* S_IN      /C*80  steering sequence for reverse video on          */  
/* S_OUT     /C*80  steering sequence for reverse video off         */  
/*                                                                  */  
/* Assumptions made:                                                */  
/* 1) Correspondance between C and FORTRAN data types:              */  
/*    long int x   -    LOGICAL X   (0 = .FALSE., -1 = .TRUE.)      */  
/*    short int x  -    INTEGER*2 X                                 */  
/*    char x[80]   -    CHARACTER X*80                              */  
/* 2) All external FORTRAN names are lowercased, and an underscore  */  
/*    postpended, in order to obtain the same name in C.            */  
/* 3) In the FORTRAN code: There is a format specifier '$' which    */  
/*    prevents the cursor from moving to the beginning of a new     */  
/*    line after a record has been written out.                     */  
/* 4) There is an environment variable ALDOC which points to the    */  
/*    area where the data files all.assign, sbank.adr, sbank.lbf    */  
/*    are stored; if this variable is missing, ALEPH must be        */  
/*    defined, and the files are in $ALEPH/doc.                     */  
/********************************************************************/  
    
#include <stdio.h>  
#include <stdlib.h> 
#include <string.h> 
#define STRLEN 80   
    
static char *sc_clear, *sc_home, *sc_s_in, *sc_s_out;   
static int sc_height;   
    
main(argc, argv)
    
int argc;   
char *argv[];   
    
{   
  char chout[STRLEN], charg[STRLEN];
  int i, j; 
  long int xbook, xcont, xrefe, xcomm, xundo, xoutp;
  void sbkdec_(), prsbank_();   
    
  myterm();    /* get the terminal characteristics */   
    
  xbook = -1;                             /* Set the defaults */
  xcont = -1;   
  xrefe = -1;   
  xcomm = -1;   
  xundo =  0;   
  xoutp =  0;   
  strcpy(chout,"sbank.out");
  strcpy(charg,"    "); 
    
  if (argc == 2 && argv[1][0] == '?') goto usage;   
    
  i = 0;
  while (++i < argc) {  
    if (argv[i][0] == '-') {              /* We have an option */   
      if (argv[i][1] == 0) goto usage;    /* Hyphen only       */   
      switch(argv[i][1]) {  
        case 'n':   
          switch(argv[i][2]) {  
            case 'b':   
              xbook = 0;
              break;
            case 'c':   
              xcont = 0;
              break;
            case 'r':   
              xrefe = 0;
              break;
            case 'd':   
              xcomm = 0;
              break;
            default:
              goto usage;   
          } 
          if (argv[i][3] != 0) goto usage;  
          break;
        case 'u':   
          xundo = -1;   
          if (argv[i][2] != 0) goto usage;  
          break;
        case 'o':   
          xoutp = -1;   
          if (argv[i][2] != 0) goto usage;  
          if (argc < i+2) goto usage;   
          if (argv[i+1][0] == '-') goto usage;  
          strcpy(chout,argv[++i]);  
          break;
        default:
          goto usage;   
      } 
    }   
    else {  
      if (argc > i+1) goto usage;   
      strcpy(charg,argv[i]);
    }   
  } 
    
  sbkdec_(&xbook, &xcont, &xrefe, &xcomm, &xundo, &xoutp, chout, charg);
  prsbank_();   
    
  exit(0);  
    
  usage: {  
    printf("Usage: %s [-nb] [-nc] [-nr] [-nd] [-u] [-o file] bank\n",   
      argv[0]); 
    exit(1);
  } 
    
}   
    
myterm()
{   
  char *tgetstr(), termbuf[2048], *term, *getenv(), *sp;
  static char sbuf[1024];   
    
  if ((term = getenv("TERM")) == 0) term = "unknown";   
  if (tgetent(termbuf, term) <= 0) strcpy(termbuf, "dumb:co#80:hc:");   
  sp = sbuf;
  sc_height = tgetnum("li");
  sc_clear  = tgetstr("cl", &sp);   
  sc_home   = tgetstr("ho", &sp);   
  sc_s_in   = tgetstr("so", &sp);   
  sc_s_out  = tgetstr("se", &sp);   
}   
    
unixlines_(linespp, clear, home, s_in, s_out)  
short int *linespp; 
char clear[STRLEN], home[STRLEN], s_in[STRLEN], s_out[STRLEN];  
{   
  *linespp = sc_height; 
  strcpy(clear, sc_clear);  
  strcpy(home, sc_home);
  strcpy(s_in, sc_s_in);
  strcpy(s_out, sc_s_out);  
}   