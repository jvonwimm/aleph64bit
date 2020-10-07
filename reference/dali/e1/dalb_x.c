/*
 * dalb_x.c contains general graphics routines for DALI.
 * Mark Parsons made the first translation of the UIS based DALB_UIS.FOR
 * into X-based c-code.
 * Bjorn S. Nilsson has continued maintaining and changing the code.
 * OpenVMS Alpha version from Jan 1993.
 *
 * The porting to UNIX (Ultrix) was started in February, 1993.
 * OSF/1 porting done in Jan. 1994. IRIX and HP-UX were done in fall 1995.
 * Linux support was added in spring 1996.
 */

#ifdef __hpux
#define _HPUX_SOURCE
#define unix
#define __unix__
#endif /* HP */
#if defined(__unix) && !defined(__unix__)
#define __unix__
#endif /* For SGI */
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <string.h>

#ifdef VMS
#include <iodef.h>
#include <trmdef.h>
#include <ssdef.h>
#include <decw$include/Intrinsic.h>
#include <decw$include/keysym.h>
#include <decw$include/StringDefs.h>
#include <decw$include/Shell.h>
#define No_Signals
#else
#include <sys/time.h>
#include <sys/types.h>
#include <sys/termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <X11/Intrinsic.h>
#include <X11/keysym.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#endif /* VMS */

#include "dali_defs.h"
#include "commons.h"
#include "ps_abbrevs.h"

/*
 * Global X variables and structures.
 */
Display *disp;
#define MAX_WINDOWS 5
Pixmap iconpix, winpix[MAX_WINDOWS];
Arg wargs[10];
Widget toplevel[MAX_WINDOWS], area[MAX_WINDOWS];
int window_num = 0;
Drawable drawin;
GC grcon, grconclip, gc253, gc255;
XGCValues grcon_val, grconclip_val, gc253_val, gc255_val;
Colormap cmap;
int our_visual_class, our_screen;
Font font[2][2], mainfont, helpfont, greekfont, boldhelpfont;
XComposeStatus compstat;
XtAppContext Dali_context;
Cursor cursor;
int evflag[7];
int winpix_copied = -1;
char fontname_n[6][80], fontname_s[6][80];
logical Allow_func_keys = False;
logical isGray = False; /* Color or grayscale-black&white */
logical isColor = False; /* Color or not */
int fgindex;

extern logical debug_mask[30];
extern char m_line[257];
extern float PSwidth_unit;
extern char tmeta[80];
extern FILE *fmeta;
extern int tot_meta_chars;
extern int meta_BW;
extern int meta_COL;
extern Widget tophelp;
Pixmapsize pixdata[MAX_WINDOWS];

/*
 * Static variables holding info private to dalb_x.
 */
static logical verticaltext = False;
static logical fgks;
static unsigned long colourstore_dglevs;
static unsigned char keybuffer[80];
static int keybufsize = 0;
static XFontStruct *mainfontinfo, *helpfontinfo, *boldhelpfontinfo;
static XFontStruct *greekfontinfo;
static int xptrco, yptrco;	/* N.B. Pointer position is UIS orientation. */
static int reqnumchars = 0;
static logical dgdfcu_first_call = True;
static logical cursormodified = False;
static unsigned long pixels[256], plane_mask[4];
static Status status;
static logical convert_to_upper = True;
static XFontStruct *fontinfo[2][2];
static logical dgtlnx_on = False;
static Window terminal_window;
static Time Focus_Time;
static int revert_to;
static char delete_string[] = {0x08, 0x20, 0x08, 0x00};
static XtInputId KeybInputId;
static void get_nonX_input();
static int FillShape = Convex;
static XErrorHandler old_Xerror;

#ifdef VMS
#ifdef __VAXC
extern void VAXC$CRTL_INIT();
#endif /* VAXC */
#define QIO_BUFSIZE 80
static unsigned char QIO_buf[QIO_BUFSIZE];
static unsigned short QIO_iosb[4];
FortranString QIO_dsc = {QIO_BUFSIZE, 0, 0, QIO_buf};
unsigned long QIO_Status, SYS$ASSIGN(), SYS$QIO(), SYS$QIOW(), SYS$CLREF(),
	LIB$FREE_EF(), LIB$RESERVE_EF();
void SYS$CANCEL();
static short QIO_chan;
unsigned long QIO_event_flag = 23;
void Init_QIO_Read();
#endif /* VMS */

/*
 * Function prototypes - only those explicitly required appear below.
 */
XtEventHandler displaywinpix(Widget w, Pixmapsize *data, XEvent *junk);
void dgalgr(int *iwind, int *lev, char *tlet, int idir, float h1, float v1,
	float h2, float v2);
XtEventHandler keyinput(Widget w, caddr_t junk, XKeyEvent *cdata);
void dgpusg();
void drawtextvert(float *htxt, float *vtxt, int xoffset, int yoffset,
	char *text, int *n, int fontflag);
void dgrotg(int np, float x[], float y[], float h, float v);
void setXcursor(char data[], int fg, int bg, int hsx, int hsy);
void text_to_PS();
void mprintf();
void write_colors_to_meta();
void dgbtit(),dgcome();
void mdgclpa(), mdgalph(), mdgpoin(), mdgplot(), mdgdraw(), mdgtext(),
	mdgtxtg1(), mdgtxtg2(), mdgtdir(), mdglevl(), mdglevs(), mdgarcl(),
	mdgarea(), mdgcler(), make_PS_postamble(), mdgalgr(), mdgdash(),
	dgsppt(), dgooba(), dgswpt();
int make_PS_prolog();
int tty_reset(void);
void tty_end(void);

/*
 * Data for the icon image.
 */
#define dali_width 50
#define dali_height 50

static char dali_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x40, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0xe0, 0x00, 0x00,
   0x1c, 0xe0, 0x00, 0x02, 0xf0, 0x00, 0x00, 0x1e, 0xc0, 0x01, 0x01, 0xf8,
   0x00, 0x00, 0x3f, 0x80, 0x83, 0x00, 0xf8, 0x01, 0x80, 0x3f, 0x00, 0x47,
   0x00, 0xfc, 0x03, 0xc0, 0x7f, 0x00, 0x2e, 0x00, 0xfc, 0x07, 0xc0, 0xff,
   0x00, 0x14, 0x00, 0xfc, 0x0f, 0xe0, 0xff, 0x01, 0x3a, 0x00, 0xfc, 0x1f,
   0xe0, 0xff, 0x03, 0x71, 0x00, 0xf8, 0x3f, 0xe0, 0xff, 0x87, 0xe0, 0x00,
   0xf8, 0x7f, 0xe0, 0xff, 0x47, 0xc0, 0x01, 0xf0, 0xff, 0xe0, 0xff, 0x2f,
   0x80, 0x03, 0xe0, 0xff, 0xc1, 0xff, 0x0f, 0x00, 0x00, 0xc0, 0xff, 0x83,
   0xff, 0x0f, 0x00, 0x00, 0x80, 0xff, 0x07, 0xff, 0x0f, 0x1f, 0x00, 0x80,
   0xff, 0x0f, 0xff, 0x0f, 0x21, 0x00, 0xc0, 0xff, 0x9f, 0xff, 0x07, 0x41,
   0x00, 0xc0, 0xff, 0xff, 0xff, 0x07, 0x41, 0x00, 0xe0, 0xf9, 0xff, 0xf3,
   0x03, 0x41, 0x00, 0xe0, 0xf1, 0xff, 0xe1, 0x01, 0x21, 0x00, 0xf0, 0xe1,
   0xff, 0x01, 0x00, 0x1f, 0x00, 0xf0, 0xc1, 0xff, 0x03, 0x00, 0x00, 0x00,
   0xf8, 0x83, 0xff, 0x07, 0x00, 0x00, 0x00, 0xf8, 0x03, 0xff, 0x0f, 0x00,
   0x08, 0x00, 0xfc, 0x07, 0xfe, 0x1f, 0x00, 0x14, 0x00, 0xfc, 0x0f, 0xfc,
   0x3f, 0x00, 0x14, 0x00, 0xfc, 0x1f, 0xf8, 0x7f, 0x00, 0x22, 0x00, 0xfc,
   0x3f, 0xf0, 0xff, 0x00, 0x3e, 0x00, 0xfc, 0x7f, 0xe0, 0xff, 0x01, 0x41,
   0x00, 0xf8, 0xff, 0xc0, 0xff, 0x03, 0x41, 0x00, 0xf8, 0xff, 0x81, 0xff,
   0x03, 0x00, 0x00, 0xf0, 0xff, 0x03, 0xff, 0x07, 0x00, 0x00, 0xf0, 0xff,
   0x03, 0xfe, 0x07, 0x01, 0x00, 0xf0, 0xff, 0x07, 0xfc, 0x07, 0x01, 0x00,
   0xf0, 0xff, 0x07, 0xf8, 0x07, 0x01, 0x00, 0xf0, 0xff, 0x07, 0xf0, 0x03,
   0x01, 0x00, 0xf0, 0xff, 0x03, 0xe0, 0x03, 0x01, 0x00, 0xf8, 0xff, 0x03,
   0xe0, 0x01, 0x01, 0x00, 0xfc, 0xff, 0x01, 0xe0, 0x00, 0x7f, 0x00, 0xf8,
   0xff, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7f, 0x00, 0x20, 0x04, 0xcf, 0x11,
   0x01, 0x08, 0x00, 0x20, 0x04, 0x41, 0x12, 0x01, 0x08, 0x00, 0x50, 0x04,
   0x41, 0x12, 0x01, 0x08, 0x00, 0x70, 0x04, 0xc7, 0xf1, 0x01, 0x08, 0x00,
   0x88, 0x04, 0x41, 0x10, 0x01, 0x08, 0x00, 0x88, 0x3c, 0x4f, 0x10, 0x01,
   0x7f, 0x00};

/*
 * Catch X errors and print only _real_ messsages.
 */
int catch_Xerror(Display *dpy, XErrorEvent *err)
{
/* Next from Xproto.h */
#define X_SetInputFocus 42
	if ((err->error_code == BadMatch) && (err->request_code == X_SetInputFocus)) {
		printf("\n\n*** You have iconized the DALI window! ***");
		printf ("\n                                                 ..:");
		return 0;
	}
/*
	printf("\n type, error_code, request_code, minor_code: %d %d %d %d\n",
		err->type, err->error_code, err->request_code, err->minor_code);
*/
	old_Xerror(dpy, err);
	printf ("\n                                                ..:");
    return 0;
}
#ifndef No_Signals
static void Ctrl_C_Inp(int sig, int code, struct sigcontext *scp)
{
/*
 * Receive Ctrl_C input.
 */
	unsigned short ic1 = 3;
	(void) signal(SIGINT, Ctrl_C_Inp);
	dgooba(ic1);

}
static void Ctrl_Z_Inp(int sig, int code, struct sigcontext *scp)
{
/*
 * Receive Ctrl_Z input.
 */
	unsigned short ic1 = 26;
	XKeyEvent event;

	(void) signal(SIGTSTP, Ctrl_Z_Inp);
/*
 * Simulate a Ctrl_Z X event.
 */
	event.type = KeyPress;
	event.serial = 0;
	event.send_event = True;
	event.display = disp;
	event.window = XtWindow(toplevel[0]);
	event.x = 10;
	event.y = 10;
	event.state = ControlMask;
	event.keycode = XKeysymToKeycode(disp, XK_Z);
	status = XSendEvent(disp, XtWindow(toplevel[0]), True, KeyPressMask, &event);
	XFlush(disp);
}

static void ReallyBadProblem ()
{
/*
 * Disable signal capture and abort.
 */
	signal(SIGINT,  0);
	signal(SIGBUS,  0);
	signal(SIGSEGV, 0);
	signal(SIGILL,  0);
	signal(SIGTSTP, 0);
	abort();
}

static void BadProblem(int sig, int code, struct sigcontext *scp)
{
/*
 * Signal handling for severe errors.
 */
	(void) tty_reset();
	fprintf(stderr, "\n\nCongratulations! You have found a bug which has not been spotted\n");
	fprintf(stderr, "by our development team. You can help us fix the bug if you\n");
	fprintf(stderr, " a) Keep the DALI.LOG.x file from this execution.\n");
	fprintf(stderr, " b) If a core file was dumped in your directory, keep that too.\n");
	fprintf(stderr, " c) Inform Bjorn S. Nilsson in person (Bat.2 R-025, ext. 3883) or\n");
	fprintf(stderr, "by email (send to nilsson@alws.cern.ch)\n");
	fprintf(stderr, "\n...now crashing...\n\n");
	ReallyBadProblem();
}
#endif /* Not VMS, No_Signals */

void dgsync(int *flag)
/*
 * Swith on/off X synchronization for *flag =0 / !=0
 */
{
	if (*flag)
		XSynchronize(disp, True);
	else
		XSynchronize(disp, False);
}

/*
 * Start of functions converted from DALB_UIS.
 */
void dgexec()
{
	int mfive = -5;

/*
 * Switch back to normal pointer.
 */
	if(!dtvccc.fblwdt) dgswpt(&mfive);
	if (dtvccc.fpridt) {
		duiscc.fopsdu = False;
	} else {
		duiscc.fuisdu = False;
	}
}

void dginit(
	float* h1,
	float* v1,
	float* h2,
	float* v2,
	float* h1cm,
	float* v1cm,
	float* h2cm,
	float* v2cm
)
/*
 * Main initializing routine.
 */
{
	logical debugf;
	char **fontlist, c1;
	int index, index1, store1, store2, num_fonts;
	int argc = 0;
	static char **argv = NULL;
	XColor colors[128];
	static int ocarray[] = {
		0, 0, 0, 0, 0, 0, -2,
		0, -2, -2, -2, -2, -2, -2,
		-2, 0, -2, -2, -2, -2, -2,
		-2, -2, 0, -2, -2, -2, -2,
		-2, -2, -2, 0, -2, -2, -2,
		-2, -2, -2, -2, 0, -2, -2,
		-2, -2, -2, -2, -2, 0, -2,
		0, -2, 0, -2, -2, -2, -2,
		-2, 0, -2, 0, -2, -2, -2,
		0, 0, -2, -2, -2, -2, -2,
		-2, -2, 0, 0, -2, -2, -2,
		-2, -2, -2, -2, 0, 0, -2,
		0, 0, 0, 0, -2, -2, -2,
		-2, -2, -2, -2, -2, -2, 0
	};
#include "x_resources.h"
	int default_depth, default_gray;
	XColor black, white;
	XVisualInfo info_visual;
	int sync_flag = 0;
	static char *dpyerr[] = {
"Unable to open X-display. Make sure that:",
"(1) You are displaying on a machine using X or on an X-terminal.",
"(2) You have set the security correctly using the session manager.",
#ifdef vms
"(3) You have redirected X output using \"SET DISPLAY/CREATE/NODE=machine name\""
#else
"    On UNIX, you can also use \"xhost remote_node\" on the local node for this.",
"(3) You have redirected X output correctly. Check with \"printenv DISPLAY\".",
"    Define the display with \"setenv DISPLAY node:0\" if you use TCP/IP.",
"    For DECNET, use \"setenv DISPLAY node ::0\" (note the double colon)."
#endif /* VMS */
	};
	static char mainfontname[][80] = {
		"                                                                     ",
		"-DEC-Terminal-Medium-R-Normal--14-140-75-75-C-80-ISO8859-1",
		"-Adobe-Courier-Medium-R-Normal--12-120-75-75-M-70-ISO8859-1",
		"-DEC-Terminal-Medium-R-Normal--14-1*0-100-100-C-80-ISO8859-1",
		"-Adobe-Courier-Medium-R-Normal--14-100-100-100-M-*-ISO8859-1",
		" "
	};
	static char helpfontname[][80] = {
		"                                                                     ",
		"-DEC-Terminal-Medium-R-Normal--14-140-75-75-C-80-ISO8859-1",
		"-Adobe-Courier-Medium-R-Normal--12-120-75-75-M-70-ISO8859-1",
		"-DEC-Terminal-Medium-R-Normal--14-1*0-100-100-C-80-ISO8859-1",
		"-Adobe-Courier-Medium-R-Normal--14-100-100-100-M-*-ISO8859-1",
		" "
	};
	static char fontname00[][80] = {
		"                                                                     ",
		"-DEC-Terminal-Medium-R-Normal--28-280-75-75-C-160-ISO8859-1",
		"-Adobe-Courier-Medium-R-Normal--24-*-75-75-M-150-ISO8859-1",
		"-DEC-Terminal-Medium-R-Normal--28-2*0-100-100-C-16*-ISO8859-1",
		"-Adobe-Courier-Medium-R-Normal--25-*-100-100-M-150-ISO8859-1",
		" "
	};
	static char fontname01[][80] = {
		"                                                                     ",
		"-DEC-Terminal-Bold-R-Normal--28-280-75-75-C-160-ISO8859-1",
		"-Adobe-Courier-Bold-R-Normal--24-240-75-75-M-150-ISO8859-1",
		"-DEC-Terminal-Bold-R-Normal--28-2*0-100-100-C-16*-ISO8859-1",
		"-Adobe-Courier-Bold-R-Normal--25-*-100-100-M-150-ISO8859-1",
		" "
	};
	static char greekfontname[][80] = {
		"                                                                     ",
		"-ADOBE-Symbol-Medium-R-Normal--14-1*0-*-*-P-85-ADOBE-FONTSPECIFIC",
		" "
	};
#ifdef __VAXC
/*
 * Initialize the VAXC RTL functions. This is needed because the main program
 * is not in C (but it doesn't seem to do anything really).
 */
	VAXC$CRTL_INIT();
#endif /* VAXC */
/*
 * Initialize the segment identifiers and list of erased segments.
 */
	for (index=0; index<=mpnwdu; index++)
		duiscc.idsgdu[index] = 0;
	duiscc.nersdu = 0;
	for (index=0; index<mersdu; index++)
		duiscc.idesdu[index] = 0;
/*
 * Initialize the graphics system name and hence identify the file we read
 * for info in daliq.for.
 */
	dgrdet.tgradg[0] = 'X';
	dgrdet.tgradg[1] = '1';
	dgrdet.tgradg[2] = '1';
	for (index=0; index<MAX_WINDOWS; index++)
		toplevel[index] = 0;
/*
 * Initialize connection to X-server and application context - create display.
 */
	XtToolkitInitialize();
	Dali_context = XtCreateApplicationContext();

	disp = XtOpenDisplay(Dali_context, NULL, "xdali", "XDali", NULL, 0,
		&argc, argv);
	if (disp == NULL) {
#ifdef vms
		printf("\n%s\n%s\n%s\n%s\n", dpyerr[0], dpyerr[1], dpyerr[2], dpyerr[3]);
#else
		printf("\n\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n", dpyerr[0], dpyerr[1],
			dpyerr[2], dpyerr[3], dpyerr[4], dpyerr[5], dpyerr[6]);
#endif /* VMS */
		fflush(stdout);
		exit(0);
	} else {
		dtvccc.fx11dt = True;
		duiscc.fmetdu = False;
		printf("\nWelcome to the X Windows version of DALI.\n");
		printf("\n");
		printf("To pop up the HELP window, type the character <");
		fflush(stdout);
	}
/*
 * Synchronize when debugging.
 */
	if (sync_flag) dgsync(&sync_flag);

/*
 * Initialize an error handler.
 */
	old_Xerror = XSetErrorHandler(catch_Xerror);
/*
 * Find terminal window ident if possible. We may need it when spawning.
 */
	XGetInputFocus(disp, &terminal_window, &revert_to);
#ifdef Testing
	printf("\nTerminal window id: %0x8\n"); fflush(stdout);
#endif
/*
 * Create the pixmap that is displayed when the dali window is iconified.
 */
	iconpix = XCreateBitmapFromData(disp, XDefaultRootWindow(disp),
		dali_bits, dali_width, dali_height);
/*
 * Determine visual used and colormap.
 */
	our_screen = XDefaultScreen(disp);
	our_visual_class = XDefaultVisual(disp, our_screen)->class;
	cmap = XDefaultColormap(disp, our_screen);
/*
 * Create the container window.
 */
	index = 0;
	XtSetArg(wargs[index], XtNinput, True); index++;
	XtSetArg(wargs[index], XtNiconPixmap, iconpix); index++;
	XtSetArg(wargs[index], XtNx, 1); index++;
	XtSetArg(wargs[index], XtNy, 1); index++;
	toplevel[0] = XtAppCreateShell("xdali", "XDali",
		applicationShellWidgetClass, disp, wargs, index);
/*
 * Get the resources. These are most conveniently defined in a file
 * a) VMS:  DECW$USER_DEFAULTS:XDALI.DAT
 * b) UNIX: XDali in the directory pointed to by XAPPLRESDIR or
 *          file with full pathname defined by XENVIRONMENT  or
 *          records in ~.Xdefaults-host
 * Resources are defined as
 *   XDali.name:   value
 */
	XtGetApplicationResources(toplevel[0], &defdata, resources,
		XtNumber(resources), NULL, 0);
	strcpy(mainfontname[0],defdata.mainfont);
	strcpy(helpfontname[0],defdata.helpfont);
	strcpy(fontname00[0],defdata.font00);
	strcpy(fontname01[0],defdata.font01);
	strcpy(greekfontname[0],defdata.greekfont);
	strcpy(fontname_n[0],defdata.fontn1);
	strcpy(fontname_n[1],defdata.fontn2);
	strcpy(fontname_n[2],defdata.fontn3);
	strcpy(fontname_n[3],defdata.fontn4);
	strcpy(fontname_n[4],defdata.fontn5);
	strcpy(fontname_n[5],defdata.fontn6);
	strcpy(fontname_s[0],defdata.fonts1);
	strcpy(fontname_s[1],defdata.fonts2);
	strcpy(fontname_s[2],defdata.fonts3);
	strcpy(fontname_s[3],defdata.fonts4);
	strcpy(fontname_s[4],defdata.fonts5);
	strcpy(fontname_s[5],defdata.fonts6);
	sscanf(defdata.metawidthunit, "%f", &PSwidth_unit);
	sscanf(defdata.displaydepth, "%d", &default_depth);
	sscanf(defdata.displayingray, "%d", &default_gray);
/*
 * Decode debug mask. Characters in defdata.debug set the corresponding
 * element in debug_mask to True.
 * Characters used now:
 *  d  some X-debugging, like inserting XFlush here and there.
 *  f  print out fontnames as fonts are loaded.
 *  g  use general methods, no DEC server specific.
 *  h  raise Help window pop when new information is written in it.
 *  i  do not invoke XtAppAddInput for stdin input.
 *  l  dglinm (moving lines)
 *  m  insert some debugging comments in a PostScript metafile.
 *  p  more output around pointer positioning etc.
 */
	for (index = 0; index<30; index++)
		debug_mask[index] = False;
	for (index = 0; index<strlen(defdata.debug); index++) {
		c1 = _tolower(defdata.debug[index]);
		index1 = (int) (c1 - 'a');
		if (index1 >= 0 && index1 < 30)
			debug_mask[index1] = True;
	}
/*
 * Read input from stdin in callback of XtAppAddInput, unless debug flag i is
 * set. Note that VMS uses an event flag as source for XtAppAddInput.
 */
#ifdef VMS
	if (!debug_mask[(int)('i'-'a')]) {
/*
 * The present mask is for characters CEHILR
 */
		char out_of_band_chars[] = {'C', 'E', 'H', 'I', 'L', 'R', '\0'};
		int astmsk[2] = {0,0};

		for (index = 0; out_of_band_chars[index] != '\0'; index++) {
			astmsk[1] |=
				(int) pow(2.0, (float)((int)(out_of_band_chars[index] - 'A') + 1));
		}
		strcpy((char *) QIO_buf, "TT:");
		QIO_dsc.dsc$w_length = strlen((char *) QIO_buf);
		QIO_Status = SYS$ASSIGN(&QIO_dsc, &QIO_chan, 0, 0);
		if ((QIO_Status&1) == 0) {
			printf("\nStatus %d returned when setting up for XtAppAddInput,\n",
				QIO_Status);
			exit(0);
		}
		QIO_Status = LIB$FREE_EF(&QIO_event_flag);
		QIO_Status = LIB$RESERVE_EF(&QIO_event_flag);
		KeybInputId = XtAppAddInput(Dali_context, QIO_event_flag, QIO_iosb,
			get_nonX_input, NULL);
/*
 * Setup for out-of-band AST on Ctrl-C and then initalise read on stdin.
 */
		QIO_Status = SYS$QIOW(0, QIO_chan, IO$_SETMODE|IO$M_OUTBAND, QIO_iosb,
			0, 0, dgooba, astmsk, 3, 0, 0, 0);
		QIO_Status = SYS$QIOW(0, QIO_chan, IO$_SETMODE|IO$M_CTRLCAST, QIO_iosb,
			0, 0, dgooba, 3, 3, 0, 0, 0);
		Init_QIO_Read();
	}
#else
	if (!debug_mask[(int)('i'-'a')]) {
		KeybInputId = XtAppAddInput(Dali_context, fileno(stdin),
			(XtPointer)XtInputReadMask, get_nonX_input, NULL);
		KeybInputId = XtAppAddInput(Dali_context, fileno(stdin),
			(XtPointer)XtInputExceptMask, get_nonX_input, NULL);
		if (tty_set()) {
			printf("\nCould not reinitalise stdin.\n");
			exit(0);
		}
		if (atexit(tty_end)) {
			printf("\nCould not register an atexit function.\n");
			fflush(stdout);
		}
	}
#ifdef No_Signals
printf("\n Remember: All signals are disabled at present.\n");
#else
	signal(SIGINT,  Ctrl_C_Inp);
	signal(SIGTSTP, Ctrl_Z_Inp);
#ifdef Ctrl_Signals_Only
printf("\n Remember: Hard signals are disabled at present.\n");
#else
	signal(SIGBUS,  BadProblem);
	signal(SIGSEGV, BadProblem);
	signal(SIGILL,  BadProblem);
#endif /* Hard signals */
#endif /* No signals */
#endif /* VMS */

/*
 * Create the drawing area inside the container.
 *
 * We add one pixel extra for x since DALI starts in (1,1) while X has
 * origin in (0,0).
 */
	pixdata[0].x = (int) (*h2 - *h1) + 2;
	pixdata[0].y = (int) (*v2 - *v1) + 1;
	index = 0;
	XtSetArg(wargs[index], XtNheight, pixdata[0].y); index++;
	XtSetArg(wargs[index], XtNwidth, pixdata[0].x); index++;
	area[0] = XtCreateManagedWidget("area", widgetClass, toplevel[0],
		wargs, index);
	XtAddEventHandler(area[0], ExposureMask, False,
		(XtEventHandler)displaywinpix, &pixdata);
	XtAddEventHandler(toplevel[0], KeyPressMask, False,
		(XtEventHandler)keyinput, NULL);
/*
 * Realize the window - XtWindow works from here onwards.
 */
	XtRealizeWidget(toplevel[0]);
/*
 * Create the pixmap and graphics contexts to be used for all drawing
 * operations. (Pixmap will act like backing store).
 */
	winpix[0] = XCreatePixmap(disp, XtWindow(area[0]), pixdata[0].x,
		pixdata[0].y, XDefaultDepthOfScreen(XtScreen(area[0])));
	drawin = winpix[0];
	grcon_val.foreground = XWhitePixel(disp, our_screen);
	grcon_val.background = XBlackPixel(disp, our_screen);
	grcon_val.function = GXcopy;
	grcon = XCreateGC(disp, winpix[0],
		GCFunction|GCForeground|GCBackground, &grcon_val);
	grcon_val.foreground = XBlackPixel(disp, our_screen);
	grcon_val.foreground = XWhitePixel(disp, our_screen);
	XChangeGC(disp, grcon, GCForeground, &grcon_val);
	grconclip_val.foreground = XWhitePixel(disp, our_screen);
	grconclip_val.background = XBlackPixel(disp, our_screen);
	grconclip_val.function = GXcopy;
	grconclip = XCreateGC(disp, winpix[0],
		GCFunction|GCForeground|GCBackground, &grconclip_val);
	gc253_val.foreground = XWhitePixel(disp, our_screen);
	gc253_val.background = XBlackPixel(disp, our_screen);
	gc253_val.function = GXcopy;
	gc253 = XCreateGC(disp, winpix[0],
		GCFunction|GCForeground|GCBackground, &gc253_val);

	dxpntr.nxcudx = 1;
/*
 * Load in the fonts that will be used.
 */
	debugf = debug_mask[(int)('f'-'a')];
	for (index = 0; mainfontname[index][0] != ' '; index++) {
		fontlist = XListFonts(disp, mainfontname[index], 1, &num_fonts);
		if (fontlist == NULL) continue;
		mainfontinfo = XLoadQueryFont(disp, *fontlist);
		if (mainfontinfo != NULL) break;
		XFreeFontNames(fontlist);
	}
	if (mainfontinfo == NULL) {
		printf("\nNo main font could be loaded.\n"); fflush(stdout);
		exit(0);
	} else {
		mainfont = mainfontinfo->fid;
		if (debugf) {
			printf("\nMain font: %s", *fontlist); fflush(stdout);
		}
		XFreeFontNames(fontlist);
	}
	for (index = 0; helpfontname[index][0] != ' '; index++) {
		fontlist = XListFonts(disp, helpfontname[index], 1, &num_fonts);
		if (fontlist == NULL) continue;
		helpfontinfo = XLoadQueryFont(disp, *fontlist);
		if (helpfontinfo != NULL) break;
		XFreeFontNames(fontlist);
	}
	if (helpfontinfo == NULL) {
		printf("\nNo help font could be loaded.\n"); fflush(stdout);
		exit(0);
	} else {
		helpfont = helpfontinfo->fid;
		if (debugf) {
			printf("\nHelp font: %s", *fontlist); fflush(stdout);
		}
		boldhelpfont = 0;
		{
/*
 * Also get a bold variant of the help font.
 */
			char boldname[120], *cp=*fontlist, *cp1;
			int i;
			for (i=0; i<=strlen(cp);i++)
				boldname[i] = _tolower(cp[i]);
			if(!(cp = strstr(boldname, "medium"))) goto after_help;
			*cp = 'b'; cp++;
			*cp = 'o'; cp++;
			*cp = 'l'; cp++;
			*cp = 'd'; cp++;
			cp1 = cp + 2;
			while (*cp1 != '\0') {
				*cp = *cp1;
				cp++; cp1++;
			}
			*cp = '\0';
			XFreeFontNames(fontlist);
			fontlist = XListFonts(disp, boldname, 1, &num_fonts);
			if (fontlist == NULL) goto after_help;
			boldhelpfontinfo = XLoadQueryFont(disp, *fontlist);
			if (boldhelpfontinfo == NULL) goto after_help;
			boldhelpfont = boldhelpfontinfo->fid;
			if (debugf) {
				printf("\nBold help font: %s", *fontlist); fflush(stdout);
			}
		}
	}
after_help:
	if (!boldhelpfont) {
		printf("\nNo appropriate bold help font could be loaded.\n");
		fflush(stdout);
		boldhelpfont = helpfont;
	}
	XFreeFontNames(fontlist);
	grcon_val.font = mainfont;
	XChangeGC(disp, grcon, GCFont, &grcon_val);
	grconclip_val.font = mainfont;
	XChangeGC(disp, grcon, GCFont, &grconclip_val);
	for (index = 0; greekfontname[index][0] != ' '; index++) {
		fontlist = XListFonts(disp, greekfontname[index], 1, &num_fonts);
		if (fontlist == NULL) continue;
		greekfontinfo = XLoadQueryFont(disp, *fontlist);
		if (greekfontinfo != NULL) break;
		XFreeFontNames(fontlist);
	}
	if (greekfontinfo == NULL) {
		printf("\nNo greek font could be loaded.\n"); fflush(stdout);
		exit(0);
	} else {
		greekfont = greekfontinfo->fid;
		if (debugf) {
			printf("\nGreek font: %s", *fontlist); fflush(stdout);
		}
		XFreeFontNames(fontlist);
	}
	for (index = 0; fontname00[index][0] != ' '; index++) {
		fontlist = XListFonts(disp, fontname00[index], 1, &num_fonts);
		if (fontlist == NULL) continue;
		fontinfo[0][0] = XLoadQueryFont(disp, *fontlist);
		if (fontinfo[0][0] != NULL) break;
		XFreeFontNames(fontlist);
	}
	if (fontinfo[0][0] == NULL) {
		printf("\nNo font00 could be loaded.\n"); fflush(stdout);
		exit(0);
	} else {
		font[0][0] = fontinfo[0][0]->fid;
		if (debugf) {
			printf("\nfont00: %s", *fontlist); fflush(stdout);
		}
		XFreeFontNames(fontlist);
	}
	for (index = 0; fontname01[index][0] != ' '; index++) {
		fontlist = XListFonts(disp, fontname01[index], 1, &num_fonts);
		if (fontlist == NULL) continue;
		fontinfo[0][1] = XLoadQueryFont(disp, *fontlist);
		if (fontinfo[0][1] != NULL) break;
		XFreeFontNames(fontlist);
	}
	if (fontinfo[0][1] == NULL) {
		printf("\nNo font01 could be loaded.\n"); fflush(stdout);
		exit(0);
	} else {
		font[0][1] = fontinfo[0][1]->fid;
		if (debugf) {
			printf("\nfont01: %s", *fontlist); fflush(stdout);
		}
		XFreeFontNames(fontlist);
	}
/*
 * Write the window title.
 */
	dgbtit();
/*
 * Setup colors.
 */
	duiscc.indvcm = (int) pow(2.0,
		(float) XDefaultDepthOfScreen(XtScreen(area[0])));
	if (default_depth > 0)
		duiscc.indvcm = (int) pow(2.0, default_depth);
	if (duiscc.indvcm == 2) {
/*
 * Monochrome display.
 */
		dtvccc.fmondt = True;
		dtvccc.fblwdt = True;
		isGray = False;
		for (index = 0; index <= dusdac.areadu; index++) {
			ddefco.ltabdd[index] = XWhitePixel(disp,our_screen);
		}
		for (index = (dusdac.areadu + 1); index < ddefcn.numcdd; index++) {
			ddefco.ltabdd[index] =
				XBlackPixel(disp,our_screen);
		}
		dusdac.rescdu = 0.0;
	} else {
/*
 * Colour or intensity.
 */
		dtvccc.fmondt = False;
		dtvccc.fblwdt = False;
		isGray = False;
		if (our_visual_class < 2) isGray = True;
		if (default_gray == 0) isGray = False;
		if (default_gray == 1) isGray = True;
		isColor = !isGray;
		if (duiscc.indvcm > 16) {
/*
 * More than 16 colours (> 4 planes). We use two sets of ddefcn.numcdd colours:
 * one for the basic colours and one for inversion for rubberbands etc.
 */
			dtvccc.f256dt = True;
			if ((duiscc.indvcm<2*ddefcn.numcdd) || ddefcn.numcdd>64) {
				printf("\r\nToo many colours (%d) requested\n", ddefcn.numcdd);
				exit(0);
			}
			duiscc.indvcm = 2*ddefcn.numcdd;
		} else {
/*
 * Depth 4.
 */
			dtvccc.f256dt = False;
		}
		if (dtvccc.f256dt) {
			int nplanes=1, ncol=ddefcn.numcdd, cont_col;
/*
 * Allocate numcdd colours and one plane mask.
 */
			if ((our_visual_class != TrueColor) &&
			 !XAllocColorCells(disp, cmap, True, plane_mask, nplanes, pixels, ncol)) {
				printf("\r\nUnable to allocate the minimum number of colours required.\n");
				printf("If you have a WWW browser running, this is probably using many colours. You\n");
				printf("should therefore stop it and try  to start DALI again before restarting the\n");
				printf("browser.  In particular,  if you use Netscape,  consider running  it with a\n");
				printf("reduced number of colours (-ncols option).\n");
				fflush(stdout);
				exit(0);
			}
			if (debug_mask[(int)('d'-'a')] && (our_visual_class != TrueColor)) {
				int i;
				printf("\rPixels allocated:");
				for (i=0; i<ncol; i++) {
					if (i%20==0) printf("\n");
					printf("%4d", pixels[i]);
				}
				printf("\n"); fflush(stdout);
			}
/*
 * Define the RGB values.
 */
			if (!isGray) {
/*
 * Real colour (not gray/grey).
 */
				for (index = 0; index < ncol; index++) {
					index1 = 127 - index;
					ddefco.ltabdd[index     ] = (int) pixels[index];
					ddefco.ltabdd[index+ncol] = (int) pixels[index] | plane_mask[0];
					colors[index     ].pixel = pixels[index];
					colors[index+ncol].pixel = pixels[index] | plane_mask[0];
					colors[index].red = (unsigned short)
						(65535.0 * ddefco.rdcodd[index]);
					colors[index].green = (unsigned short)
						(65535.0 * ddefco.grcodd[index]);
					colors[index].blue = (unsigned short)
						(65535.0 * ddefco.blcodd[index]);
					colors[index].flags = DoRed | DoGreen | DoBlue;
					colors[index+ncol].red = (unsigned short)
						(65535.0 * ddefco.rdcodd[index1]);
					colors[index+ncol].green = (unsigned short)
						(65535.0 * ddefco.grcodd[index1]);
					colors[index+ncol].blue = (unsigned short)
						(65535.0 * ddefco.blcodd[index1]);
					colors[index+ncol].flags = DoRed | DoGreen | DoBlue;
				}
			} else {
/*
 * Grayscale.
 */
				float gray;
				unsigned short igray;

				for (index = 0; index < ncol; index++) {
					ddefco.ltabdd[index     ] = (int) pixels[index];
					ddefco.ltabdd[index+ncol] = (int) pixels[index] | plane_mask[0];
					colors[index     ].pixel = pixels[index];
					colors[index+ncol].pixel = pixels[index] | plane_mask[0];
/*
 * Hardwire some gray values.
 */
					gray = 1.0;
					if (index == 0) gray = 0.95;
					if (index == 6) gray = 0.80;
					if (index > 6)  gray = 0.0;
					ddefco.rdcodd[index] =
					ddefco.grcodd[index] =
					ddefco.blcodd[index] = gray;
					igray = (unsigned short) (65535.0 * gray);
					colors[index].red   =
					colors[index].green =
					colors[index].blue  = igray;
					colors[index].flags = DoRed | DoGreen | DoBlue;
					gray = igray == 0 ? 1.0 : 0.0;
					ddefco.rdcodd[index+ncol] =
					ddefco.grcodd[index+ncol] =
					ddefco.blcodd[index+ncol] = gray;
					igray = (unsigned short) (65535.0 * gray);
					colors[index+ncol].red   =
					colors[index+ncol].green =
					colors[index+ncol].blue  = igray;
					colors[index+ncol].flags = DoRed | DoGreen | DoBlue;
				}
			}
			if (our_visual_class != TrueColor) {
				XStoreColors(disp, cmap, colors, duiscc.indvcm);
			} else {
				for (index = 0; index < ncol; index++) {
					index1 = 127 - index;
					status = XAllocColor(disp, cmap, &colors[index     ]);
					status = XAllocColor(disp, cmap, &colors[index+ncol]);
					pixels[index     ] = colors[index     ].pixel;
					pixels[index+ncol] = colors[index+ncol].pixel;
					ddefco.ltabdd[index     ] = (int) pixels[index     ];
/* The next could be ored with a plane mask */
					ddefco.ltabdd[index+ncol] = (int) pixels[index+ncol];
				}
			}
		} else {
/*
 * 16 colours. Create a new, virtual colourmap.
 */
			cmap = XCreateColormap(disp, XtWindow(toplevel[0]),
				XDefaultVisualOfScreen(XtScreen(toplevel[0])),
				AllocAll);
			for (index = 0; index < 16; index++) {
				ddefco.ltabdd[index] = index;
				colors[index].red = (unsigned short)
					(65535.0 * ddefco.rdcodd[index]);
				colors[index].green = (unsigned short)
					(65535.0 * ddefco.grcodd[index]);
				colors[index].blue = (unsigned short)
					(65535.0 * ddefco.blcodd[index]);
				colors[index].pixel = (unsigned long) index;
				colors[index].flags = DoRed | DoGreen | DoBlue;
			}
			ddefco.ltabdd[8] = defdata.fg;
			ddefco.ltabdd[defdata.fg] = 8;
			colors[8].pixel = defdata.fg;
			colors[defdata.fg].pixel = 8;
			for (index = 0; index < 16; index++) {
				if (ddefco.ltabdd[1] == colors[index].pixel)
					store1 = index;
				if (ddefco.ltabdd[defdata.bg] == colors[index].pixel)
					store2 = index;
			}
			ddefco.ltabdd[1] = store2;
			ddefco.ltabdd[defdata.bg] = colors[store1].pixel;
			index = colors[store1].pixel;
			colors[store1].pixel = colors[store2].pixel;
			colors[store2].pixel = index;
			XStoreColors(disp, cmap, colors, 16);
			XSetWindowColormap(disp, XtWindow(toplevel[0]), cmap);
		}
	}
/*
 * Now redefine background and foreground colours as DALI wants them.
 */
	grcon_val.foreground = ddefco.ltabdd[0];
	XChangeGC(disp, grcon, GCForeground, &grcon_val);
	XFillRectangle(disp, winpix[0], grcon, 0, 0, pixdata[0].x, pixdata[0].y);
	grcon_val.foreground = ddefco.ltabdd[8];
	grcon_val.background = ddefco.ltabdd[0];
	XChangeGC(disp, grcon, GCForeground|GCBackground, &grcon_val);
/*
 * Set up initial values of variables stored in duiscc.
 */
	duiscc.lersdu = mersdu;
	duiscc.fersdu = 0;
	duiscc.fpuidu = 0;
	for (index = 0; index < 7 * (mpnwdu + 1); index++)
		duiscc.iocldu[index] = ocarray[index];
/*
 * Set up XOR gc for rubber band drawing.
 */
	fgindex = 1;
	gc255_val.foreground = ddefco.ltabdd[1];
	gc255_val.background = ddefco.ltabdd[8];
	if (dtvccc.f256dt) {
		gc255_val.plane_mask = plane_mask[0];
		gc255_val.foreground = plane_mask[0];
		fgindex = plane_mask[0];
	}
	if (our_visual_class == TrueColor) {
#ifdef Testing
		gc255_val.foreground = 0xffffff;
		fgindex = 0xffffff;
#else
		XColor col_tmp;
		(void) XParseColor(disp, cmap, "green", &col_tmp);
		status = XAllocColor(disp, cmap, &col_tmp);
/*
 * Keep only the most significant green bit for the XOR mask.
 */
		fgindex = col_tmp.pixel ^ ((col_tmp.pixel>>1) & col_tmp.pixel);
		gc255_val.foreground = fgindex;
#endif
	}
	gc255_val.function = GXxor;
	gc255 = XCreateGC(disp, XtWindow(area[0]),
		GCFunction|GCForeground|GCBackground, &gc255_val);
	Focus_Time = CurrentTime;
	XFlush(disp);
}

void dgnsiz(
	float* h1,
	float* v1,
	float* h2,
	float* v2
)
/*
 * Change size of the DALI window.
 */
{
	int index;
	Dimension w_ret, h_ret;
	XtGeometryResult status;

	pixdata[0].x = (int) (*h2 - *h1) + 2;
	pixdata[0].y = (int) (*v2 - *v1) + 1;
	index = 0;
	XtSetArg(wargs[index], XtNheight, pixdata[0].y); index++;
	XtSetArg(wargs[index], XtNwidth,  pixdata[0].x); index++;
	XtSetValues(toplevel[0], wargs, index);
	status = XtMakeResizeRequest(area[0],
		(Dimension)pixdata[0].x, (Dimension)pixdata[0].y, &w_ret, &h_ret);
	XClearWindow(disp, XtWindow(area[0]));
	XtAddEventHandler(area[0], ExposureMask, False,
		(XtEventHandler)displaywinpix, &pixdata);
	XtAddEventHandler(toplevel[0], KeyPressMask, False,
		(XtEventHandler)keyinput, NULL);
/*
 * Then recreate the backing pixmap. Hopefully we need not recreate
 * the GC:s.
 */
	XFreePixmap(disp, winpix[0]);
	winpix[0] = XCreatePixmap(disp, XtWindow(area[0]), pixdata[0].x,
		pixdata[0].y, XDefaultDepthOfScreen(XtScreen(area[0])));
	drawin = winpix[0];
	grcon_val.foreground = ddefco.ltabdd[0];
	XChangeGC(disp, grcon, GCForeground, &grcon_val);
	XFillRectangle(disp, winpix[0], grcon, 0, 0, pixdata[0].x, pixdata[0].y);
	grcon_val.foreground = ddefco.ltabdd[8];
	grcon_val.background = ddefco.ltabdd[0];
	XChangeGC(disp, grcon, GCForeground|GCBackground, &grcon_val);
}

void dgcwin(
	float* h1,
	float* v1,
	float* h2,
	float* v2,
	int*   xco,
	int*   yco,
	int*   winno,
	FortranString *title,
#ifdef vms
	FortranString *name
#else
	FortranString *name,
	int l_title,
	int l_name
#endif /* VMS characters */
)
/*
 * Create another window with the similar properties as the main DALI window.
 * The upper left corner should be at (xco,yco). The number of the window
 * is winno. The number of the main DALI window is 0.
 */
{
	int index, n, i;
	char txt[80];

	if (toplevel[*winno]) {
		printf("\nWARNING: Window %d already exists. Windows are not switched.",
			*winno);
		return;
	}
	if (*winno > MAX_WINDOWS) {
		printf("\nWARNING: You cannot create mode than %d windows. No action taken.",
			*winno);
		return;
	}
	window_num = *winno;
	pixdata[window_num].x = (int) (*h2 - *h1) + 2;
	pixdata[window_num].y = (int) (*v2 - *v1) + 1;
#ifdef vms
	n = (sizeof(txt) > name->dsc$w_length) ? name->dsc$w_length : sizeof(txt)-1;
	strncpy(txt, (char *)name->dsc$a_pointer, n);
	txt[n] = '\0';
#else
	strncpy(txt, (char *) name, l_name);
#endif
	if (txt[0] == ' ') sprintf(txt, "DALI %d", window_num);
	for (i=strlen(txt)-1; i>0; i--) {
		if(txt[i] != ' ') break;
		txt[i] = '\0';
	}
	index = 0;
	XtSetArg(wargs[index], XtNinput, True); index++;
	XtSetArg(wargs[index], XtNiconPixmap, iconpix); index++;
	XtSetArg(wargs[index], XtNx, *xco); index++;
	XtSetArg(wargs[index], XtNy, *yco); index++;
	toplevel[window_num] = XtCreatePopupShell(txt,
		topLevelShellWidgetClass, toplevel[0], wargs, index);
	index = 0;
	XtSetArg(wargs[index], XtNheight, pixdata[window_num].y); index++;
	XtSetArg(wargs[index], XtNwidth,  pixdata[window_num].x); index++;
	area[window_num] = XtCreateManagedWidget(txt, widgetClass,
		toplevel[window_num], wargs, index);
	XtAddEventHandler(area[window_num], ExposureMask, False,
		(XtEventHandler)displaywinpix, &pixdata);
	XtAddEventHandler(toplevel[window_num], KeyPressMask, False,
		(XtEventHandler)keyinput, NULL);
	XtRealizeWidget(toplevel[window_num]);
	winpix[window_num] = XCreatePixmap(disp, XtWindow(area[window_num]),
		pixdata[window_num].x, pixdata[window_num].y,
		XDefaultDepthOfScreen(XtScreen(area[0])));
	drawin = winpix[window_num];
#ifdef vms
	n = (sizeof(txt) > title->dsc$w_length) ? title->dsc$w_length:sizeof(txt)-1;
	strncpy(txt, (char *)title->dsc$a_pointer, n);
	txt[n] = '\0';
#else
	strncpy(txt, (char *) title, l_title);
#endif
	if (txt[0] == ' ') sprintf(txt, "DALI %d", window_num);
	for (i=strlen(txt)-1; i>0; i--) {
		if(txt[i] != ' ') break;
		txt[i] = '\0';
	}
	XStoreName(disp, XtWindow(toplevel[window_num]), txt);
	XtPopup(toplevel[window_num], XtGrabNone);
	grcon_val.foreground = ddefco.ltabdd[0];
	XChangeGC(disp, grcon, GCForeground, &grcon_val);
	XFillRectangle(disp, winpix[window_num], grcon, 0, 0,
		pixdata[window_num].x, pixdata[window_num].y);
	XFlush(disp);
}

void dgdwin(
	int *winno,
	int *newwin
)
/*
 * Delete window number winno.
 */
{
	*newwin = window_num;
	if (*winno == 0) {
		printf("\nWARNING: You cannot delete the main window.");
		return;
	}
	if ((*winno>=MAX_WINDOWS) || (toplevel[*winno] == 0)) {
		printf("\nWARNING: Window %d does not exist and cannot be deleted.",
			*winno);
		return;
	}
	XtRemoveEventHandler(area[*winno], ExposureMask, False,
		(XtEventHandler)displaywinpix, &pixdata);
	XtRemoveEventHandler(toplevel[*winno], KeyPressMask, False,
		(XtEventHandler)keyinput, NULL);
	XFreePixmap(disp, winpix[*winno]);
	XClearWindow(disp, XtWindow(area[*winno]));
	XtPopdown(toplevel[*winno]);
	XtUnrealizeWidget(toplevel[*winno]);
	toplevel[*winno] = 0;
	area[*winno] = 0;
	if (window_num == *winno) {
		window_num = 0;
		drawin = winpix[0];
	}
	*newwin = window_num;
}

void dgswin(
	int *winno,
	int *newwin
)
/*
 * Select window number winno for output and put in on top of the window stack.
 */
{
	if (toplevel[*winno] == 0) {
		printf("\nWARNING: Window %d does not exist. No action taken.",
			*winno);
		*newwin = window_num;
		return;
	}
	window_num = *winno;
	drawin = winpix[window_num];
	XRaiseWindow(disp, XtWindow(toplevel[window_num]));
	*newwin = window_num;
}

void dgqinf(
	int *winkind,
	int *winno
)
/*
 * Return id of window with input focus.
 * winkind is 0 for a DALI window, 1 for the terminal window and 2 for
 * a help window. A negative number is returned if input focus is outside
 * DALI or if it cannot be determined.
 * winno is [0:MAX_WINDOWS-1] for DALI graphics windows, 0 otherwise.
 */
{
	Window focus, tmpwin1, tmpwin2;
	int i;

	*winkind = -1;
	*winno = -1;
	XGetInputFocus(disp, &focus, &revert_to);
	for (i=0; i<MAX_WINDOWS; i++) {
		if (toplevel[i] == 0) continue;
		tmpwin1 = XtWindow(toplevel[i]);
		tmpwin2 = XtWindow(area[i]);
		if((focus == tmpwin1) || (focus == tmpwin2)) {
			*winkind = 0;
			*winno = i;
			return;
		}
	}
	if(focus == terminal_window) {
		*winkind = 1;
		*winno = 0;
		return;
	}
	tmpwin1 = XtWindow(tophelp);
	if(focus == tmpwin1) {
		*winkind = 2;
		*winno = 0;
		return;
	}
	return;
}

void dgsetc()
/*
 * Define colours.
 */
{
	int index, index1, store1, store2;
	int ncol = duiscc.indvcm/2;
	XColor colors[128];
	typedef struct {
		Pixel fg, bg;
	} ApplicationData, *ApplicationDataPtr;
	static XtResource resources[] = {
		{XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
			XtOffset(ApplicationDataPtr, fg), XtRString, "Black"},
		{XtNbackground, XtCBackground, XtRPixel, sizeof(Pixel),
			XtOffset(ApplicationDataPtr, bg), XtRString, "White"}
	};
	ApplicationData defdata;

	if (dtvccc.fmondt) return;
	if (dtvccc.f256dt) {
/*
 * More than 16 colours.
 */
		for (index = 0; index < ncol; index++) {
			index1 = 127 - index;
			ddefco.ltabdd[index     ] = (int) pixels[index];
			ddefco.ltabdd[index+ncol] = (int) pixels[index] | plane_mask[0];
			colors[index     ].pixel = pixels[index];
			colors[index+ncol].pixel = pixels[index] | plane_mask[0];
			colors[index].red = (unsigned short)
				(65535.0 * ddefco.rdcodd[index]);
			colors[index].green = (unsigned short)
				(65535.0 * ddefco.grcodd[index]);
			colors[index].blue = (unsigned short)
				(65535.0 * ddefco.blcodd[index]);
			colors[index].flags = DoRed | DoGreen | DoBlue;
			colors[index+ncol].red = (unsigned short)
				(65535.0 * ddefco.rdcodd[index1]);
			colors[index+ncol].green = (unsigned short)
				(65535.0 * ddefco.grcodd[index1]);
			colors[index+ncol].blue = (unsigned short)
				(65535.0 * ddefco.blcodd[index1]);
			colors[index+ncol].flags = DoRed | DoGreen | DoBlue;
		}
		if (our_visual_class != TrueColor) {
			XStoreColors(disp, cmap, colors, duiscc.indvcm);
		} else {
			for (index = 0; index < ncol; index++) {
				index1 = 127 - index;
				status = XAllocColor(disp, cmap, &colors[index     ]);
				status = XAllocColor(disp, cmap, &colors[index+ncol]);
				pixels[index     ] = colors[index     ].pixel;
				pixels[index+ncol] = colors[index+ncol].pixel;
				ddefco.ltabdd[index     ] = (int) pixels[index     ];
/* The next could be ored with a plane mask */
				ddefco.ltabdd[index+ncol] = (int) pixels[index+ncol];
			}
		}
	} else {
/*
 * 16 colours or intensities.
 */
		for (index = 0; index < 16; index++) {
			ddefco.ltabdd[index] = index;
			colors[index].red = (unsigned short)
				(65535.0 * ddefco.rdcodd[index]);
			colors[index].green = (unsigned short)
				(65535.0 * ddefco.grcodd[index]);
			colors[index].blue = (unsigned short)
				(65535.0 * ddefco.blcodd[index]);
			colors[index].pixel = (unsigned long) index;
			colors[index].flags = DoRed | DoGreen | DoBlue;
		}
		XtGetApplicationResources(toplevel[0], &defdata, resources,
			XtNumber(resources), NULL, 0);
		ddefco.ltabdd[8] = defdata.fg;
		ddefco.ltabdd[defdata.fg] = 8;
		colors[8].pixel = defdata.fg;
		colors[defdata.fg].pixel = 8;
		for (index = 0; index < 16; index++) {
			if (ddefco.ltabdd[1] == colors[index].pixel)
				store1 = index;
			if (ddefco.ltabdd[defdata.bg] == colors[index].pixel)
				store2 = index;
		}
		ddefco.ltabdd[1] = store2;
		ddefco.ltabdd[defdata.bg] = colors[store1].pixel;
		index = colors[store1].pixel;
		colors[store1].pixel = colors[store2].pixel;
		colors[store2].pixel = index;
		XStoreColors(disp, cmap, colors, 16);
	}
	if (meta_COL > 0) write_colors_to_meta();

}

void dgclpa(
	int*   lclp,
	float* h1cl,
	float* v1cl,
	float* h2cl,
	float* v2cl
)
/*
 * Define a clipping rectangle.
 */
{
	XRectangle cliparea;

	grconclip_val.foreground = (unsigned long) ddefco.ltabdd[*lclp];
	XChangeGC(disp, grconclip, GCForeground, &grconclip_val);
	cliparea.x = (short) *h1cl;
	cliparea.y = (short) pixdata[window_num].y - (short) *v2cl;
	cliparea.width = (unsigned short) (*h2cl - *h1cl);
	cliparea.height = (unsigned short) (*v2cl - *v1cl);
	XSetClipRectangles(disp, grconclip, 0, 0, &cliparea, 1, Unsorted);

	if (duiscc.fmetdu) mdgclpa(lclp, h1cl, v1cl, h2cl, v2cl);
}

void dgalef(
	int   *leval,
	float *h1,
	float *v1,
	float *h2,
	float *v2
)
{
	dgalgr(NULL, leval, "ALEPH", 0, *h1, *v1, *h2, *v2);
}

void dgalph(
	int   *leva,
	float *halef,
	float *valef
)
{
	int height, dirctn, ascent, descent;
	XCharStruct overall;

	gc253_val.foreground = (unsigned long) ddefco.ltabdd[*leva];
	gc253_val.font = font[0][1];
	XChangeGC(disp, gc253, GCForeground|GCFont, &gc253_val);
	XTextExtents(fontinfo[0][1], "ALEPH", 5, &dirctn, &ascent,
		&descent, &overall);
	height = ascent;
	XDrawString(disp, drawin, gc253, (int) *halef,
#ifdef Previous
		pixdata[window_num].y - (int) *valef + height, "ALEPH", 5);
#else
		pixdata[window_num].y - (int) *valef, "ALEPH", 5);
#endif

	if (duiscc.fmetdu) mdgalph(leva, halef, valef, &ascent);
}

void dgpoin(
	float *hpoi,
	float *vpoi
)
/*
 * Plot point at (hpoi,vpoi)
 */
{
	if (dtvccc.fretdt) return;
	XDrawPoint(disp, drawin, grcon, (int) *hpoi, pixdata[window_num].y - (int) *vpoi);

	if (duiscc.fmetdu) mdgpoin(hpoi, vpoi);
}

void dgplot(
	int   *n,
	float h[],
	float v[]
)
/*
 * Plot N points at positions (h[],v[])
 */
{
	int index;

	if (dtvccc.fretdt) return;
	for (index = 0; index < *n; index++)
		XDrawPoint(disp, drawin, grcon, (int) h[index],
			pixdata[window_num].y - (int) v[index]);

	if (duiscc.fmetdu) mdgplot(n, h, v);
}

void dgdash(
	int *n,
	int dashes[]
)
/*
 * Interface to XSetDashes. The dashes array should contain the lengths
 * of the n components measured in pixels. The sign of n has a meaning:
 * n = 0 no dashes, LineSolid
 * n > 0 LineOnOffDash
 * n < 0 LineDoubleDash
 */
{
	if (*n == 0) {
		grcon_val.line_style = LineSolid;
		grcon_val.dashes = 4;
		XChangeGC(disp, grcon, GCLineStyle | GCDashList, &grcon_val);
	} else {
		int ns = abs(*n);
		char *dash_list = (char *) calloc(ns, sizeof(char));
		int index;

		if (*n > 0) {
			grcon_val.line_style = LineOnOffDash;
		} else {
			grcon_val.line_style = LineDoubleDash;
		}
		XChangeGC(disp, grcon, GCLineStyle, &grcon_val);
		for (index=0; index<ns; index++) {
			dash_list[index] = (char) dashes[index];
		}
		XSetDashes(disp, grcon, 0, dash_list, ns);
		free(dash_list);
	}
	if (duiscc.fmetdu) mdgdash(n, dashes);
}

void dgdraw(
	int   *n,
	float h[],
	float v[]
)
/*
 * Draw polyline through the n points (h[],v[])
 */
{
	int index;
	int ih, iv, ih2, iv2, ihd, ivd;

	if (dtvccc.fretdt) return;
	for (index = 1; index < *n; index++) {
		ih = (int) h[index - 1];
		iv = pixdata[window_num].y - (int) v[index - 1];
		ih2 = (int) h[index];
		iv2 = pixdata[window_num].y - (int) v[index];
		XDrawLine(disp, drawin, grcon, ih, iv, ih2, iv2);
		if (ddeco3.dlindd > 1.0) {
			ihd = abs(ih - ih2);
			ivd = abs(iv - iv2);
			if (ihd > ivd) {
				XDrawLine    (disp, drawin, grcon, ih, iv+1, ih2, iv2+1);
				if (ddeco3.dlindd > 2.0)
					XDrawLine(disp, drawin, grcon, ih, iv-1, ih2, iv2-1);
				if (ddeco3.dlindd > 3.0)
					XDrawLine(disp, drawin, grcon, ih, iv+2, ih2, iv2+2);
				if (ddeco3.dlindd > 4.0)
					XDrawLine(disp, drawin, grcon, ih, iv-2, ih2, iv2-2);
			} else {
				XDrawLine    (disp, drawin, grcon, ih+1, iv, ih2+1, iv2);
				if (ddeco3.dlindd > 2.0)
					XDrawLine(disp, drawin, grcon, ih-1, iv, ih2-1, iv2);
				if (ddeco3.dlindd > 3.0)
					XDrawLine(disp, drawin, grcon, ih+2, iv, ih2+2, iv2);
				if (ddeco3.dlindd > 4.0)
					XDrawLine(disp, drawin, grcon, ih-2, iv, ih2-2, iv2);
			}
		}
	}

	if (duiscc.fmetdu) mdgdraw(n, h, v);
}

void dgtext(
	float *htxt,
	float *vtxt,
	FortranString *text,
	int   *n
)
/*
 * Write n characters from text at position h,v
 */
{
	int x, y;

	if (*n == 0 || dtvccc.fretdt) return;

	if (verticaltext) {
#ifdef vms
		drawtextvert(htxt, vtxt, dtxth, dtxtv, (char *) text->dsc$a_pointer,
#else
		drawtextvert(htxt, vtxt, dtxth, dtxtv, (char *) text,
#endif /* VMS characters */
			n, 0);
	} else {
		x = (int) *htxt + dtxth;
		y = pixdata[window_num].y - ((int) *vtxt + dtxtv -
			(mainfontinfo->descent + mainfontinfo->ascent));
#ifdef vms
		XDrawString(disp, drawin, grcon, x, y, (char *) text->dsc$a_pointer,
#else
		XDrawString(disp, drawin, grcon, x, y, (char *) text,
#endif /* VMS characters */
			*n);
	}
	if (duiscc.fmetdu) mdgtext(htxt, vtxt, text, n, &mainfontinfo->ascent);
}

void dgtxtg(
	float *htxt,
	float *vtxt,
	FortranString *text,
	int *n,
	int *nposgr,
	int *ntotgr
)
/*
 * Decodes text (n characters), with greek characters at nposgr[ntotgr]
 */
{
	logical chup;
	int x, y;
	int index, ig;
	char txchar;
	unsigned char *textptr;
	char *tlet = (char *) calloc(6, sizeof(char));
	float charw;
#ifdef vms
	FortranString cout_dsc;

	cout_dsc.dsc$w_length = 1;
#else
	unsigned char *cout_dsc;

#endif /* VMS characters */

	if (dtvccc.fretdt) return;
	if (duiscc.fmetdu) mdgtxtg1(htxt, vtxt, text, n, nposgr, ntotgr);

	chup = False;
	if (verticaltext)
		chup = True;
	x = (int) *htxt;
	y = (int) *vtxt;

	ig = 0;
#ifdef vms
	textptr = text->dsc$a_pointer;
#else
	textptr = (unsigned char *) text;
#endif /* VMS characters */
	for (index = 0; index < *n; index++) {
#ifdef vms
		cout_dsc.dsc$a_pointer = textptr;
#else
		cout_dsc = textptr;
#endif /* VMS characters */
		txchar = (char) *textptr;
		textptr++;
		if (index == nposgr[ig]-1 && ig < *ntotgr) {
			if (txchar == 'F' || txchar == 'f') {
				charw = XTextWidth(greekfontinfo, "f", 1);
				strcpy(tlet, "PHI  ");
			}
			if (txchar == 'R' || txchar == 'r') {
				charw = XTextWidth(greekfontinfo, "r", 1);
				strcpy(tlet, "RHO  ");
			}
			if (txchar == 'J' || txchar == 'j') {
				charw = XTextWidth(greekfontinfo, "q", 1);
				strcpy(tlet, "THETA");
			}
			if (chup) {
				dgalgr(NULL, NULL, tlet, 1, (float) x,
					(float) y, (float) x, (float) y);
			} else {
				dgalgr(NULL, NULL, tlet, 0, (float) x,
					(float) y, (float) x, (float) y);
			}

			ig++;
			if (ig >= *ntotgr) ig = 0;
		} else {
			charw = XTextWidth(mainfontinfo, &txchar, 1);
			if (verticaltext) {
				int i = 1;
				float xf;
				float yf;

				xf = (float) x;
				yf = (float) y;
				drawtextvert(&xf, &yf, dtxth, dtxtv, &txchar, &i, 0);
			} else {
				XDrawString(disp, drawin, grcon, x + dtxth,
					pixdata[window_num].y - (y + dtxtv - (mainfontinfo->descent +
					mainfontinfo->ascent)), &txchar, 1);
			}
#ifdef vms
			if (duiscc.fmetdu) mdgtxtg2(&x, &y, &cout_dsc,
				&mainfontinfo->ascent);
#else
			if (duiscc.fmetdu) mdgtxtg2(&x, &y, cout_dsc,
				&mainfontinfo->ascent);

#endif /* VMS. Is there really a difference? */
		}
		if (chup) {
			y += charw;
		} else {
			x += charw;
		}
	}
	free(tlet);
}

void dgtdir(
	int *jdir
)
/*
 * Set text direction (horizontal/vertical).
 */
{
	if (*jdir == 90) {
		verticaltext = True;
	} else {
		verticaltext = False;
	}

	if (duiscc.fmetdu) mdgtdir(jdir);
}

void dglevl(
	int *l
)
/*
 * Set colour l
 */
{
	verticaltext = False;
	grcon_val.foreground = (unsigned long) ddefco.ltabdd[*l];
	XChangeGC(disp, grcon, GCForeground, &grcon_val);

	if (duiscc.fmetdu) mdglevl(l);
}

void dglevs(
	int *l0
)
{
	if (*l0 >= 0) {
		colourstore_dglevs = grcon_val.foreground;
		grcon_val.foreground = (unsigned long) ddefco.ltabdd[*l0];
		XChangeGC(disp, grcon, GCForeground, &grcon_val);
	} else {
		grcon_val.foreground = colourstore_dglevs;
		XChangeGC(disp, grcon, GCForeground, &grcon_val);
	}

	if (duiscc.fmetdu) mdglevs(l0, &grcon_val.foreground);
}

void dgfish(
	int *n
)
/*
 * Define FillShape. Default is Convex.
 * n = 0   : Complex
 *     1   : Nonconvex
 *     2   : Convex
 */
{
	switch (*n) {
	case 0:
		FillShape = Complex;
		break;
	case 1:
		FillShape = Nonconvex;
		printf("\nWARNING: Changing fillshape to Nonconvex may hang some DEC X-servers.");
		fflush(stdout);
		break;
	case 2:
	default:
		FillShape = Convex;
		break;
	}
}

void dgarcl(
	int *n,
	float h[],
	float v[]
)
/*
 * Draw a clipped polygon.
 */
{
	int narc, index;
	XPoint *points;

	if (dtvccc.fretdt) return;
	narc = *n;
/*
 * The following was needed in UIS.
	if (dopr1c.ipicdo >= 29 && dopr1c.ipicdo <= 33 && *n == 5) {
		h[5] = 0.5 * (h[0] + h[2]);
		h[5] = 0.5 * (v[0] + v[2]);
		narc = 6;
	}
 *
 */
	points = (XPoint *) calloc(narc, sizeof(XPoint));
	for (index = 0; index < narc; index++) {
		points[index].x = (short) h[index];
		points[index].y = (short) pixdata[window_num].y - (short) v[index];
	}
	XFillPolygon(disp, drawin, grconclip, points, narc, FillShape,
		CoordModeOrigin);
	free(points);
	if (duiscc.fmetdu) mdgarcl(n, h, v, &narc);
}

void dgarea(
	int *n,
	float h[],
	float v[]
)
/*
 * Fill area determined by the n points (h[],v[])
 */
{
	int index;
	XPoint *points = (XPoint *) calloc(*n, sizeof(XPoint));

	if (dtvccc.fretdt) return;
	for (index = 0; index < *n; index++) {
		points[index].x = (short) h[index];
		points[index].y = (short) pixdata[window_num].y - (short) v[index];
	}
	XFillPolygon(disp, drawin, grcon, points, *n, FillShape, CoordModeOrigin);
	free(points);
	if (duiscc.fmetdu) mdgarea(n, h, v);
}

void dgcler(
	float *hc1,
	float *hc2,
	float *vc1,
	float *vc2
)
/*
 * Clear the rectangular area determined by the corner points
 * (hc1,vc1) and (hc2,vc2)
 */
{
	unsigned long temp_fg;
	int index, pxt, pyt, ihc1, ihc2, ivc1, ivc2;

	if (dtvccc.fretdt) return;
	temp_fg = grcon_val.foreground;
	grcon_val.foreground = grcon_val.background;
	XChangeGC(disp, grcon, GCForeground, &grcon_val);
	XFillRectangle(disp, drawin, grcon, (int) *hc1, pixdata[window_num].y - (int) *vc2,
		(int) (*hc2 - *hc1), (int) (*vc2 - *vc1));
	grcon_val.foreground = temp_fg;
	XChangeGC(disp, grcon, GCForeground, &grcon_val);
/*
 * Also remove a possibly remaining rubber band box if at all inside the area.
 */
	if (astcm1.fbox) {
		if (*hc1 <= *hc2) {
			ihc1 = (int) *hc1;
			ihc2 = (int) *hc2;
		} else {
			ihc1 = (int) *hc2;
			ihc2 = (int) *hc1;
		}
		ivc1 =  pixdata[window_num].y - (int) *vc1;
		ivc2 =  pixdata[window_num].y - (int) *vc2;
		if (ivc1 <= ivc2) {
			ivc1 =  pixdata[window_num].y - (int) *vc2;
			ivc2 =  pixdata[window_num].y - (int) *vc1;
		}
		for (index = 0; index < 4; index++) {
			pxt = (int) astcm1.pxx[index];
			pyt = pixdata[window_num].y - (int) astcm1.pyy[index];
			if (((pxt >= ihc1) && (pxt <= ihc2)) &&
				((pyt >= ivc1) && (pyt <= ivc2))) {
				astcm1.fbox = False;
				break;
			}
		}
	}

	if (duiscc.fmetdu) mdgcler(hc1, hc2, vc1, vc2);
}

void dgscur(
	float *hs,
	float *vs
)
/*
 * Set cursor to position (hs,vs)
 */
{
	XRaiseWindow(disp, XtWindow(toplevel[window_num]));
	XWarpPointer(disp, None, XtWindow(toplevel[window_num]), 0, 0, 0, 0,
		(int) *hs, pixdata[window_num].y - (int) *vs);
	xptrco = (int) *hs;
	yptrco = pixdata[window_num].y - (int) *vs;
}

void dggtcu(
	float   *hs,
	float   *vs,
	logical *raise
)
/*
 * Store current cursor position in (hs,vs). If raise is True, lift
 * the window to be inquired to the top of the stack.
 * This routine will replace the former dggcur and dgcurg procedures.
 */
{
	Window rootwin, childwin;
	int xrootco, yrootco, xwinco, ywinco;
	unsigned int state;

	if(*raise)
		XRaiseWindow(disp, XtWindow(toplevel[window_num]));
	XFlush(disp);
	(void) XQueryPointer(disp, XtWindow(toplevel[window_num]), &rootwin,
		&childwin, &xrootco, &yrootco, &xwinco, &ywinco, &state);
	if (debug_mask[(int)('p'-'a')]) {
		printf("\n%d %d %d %d %d %d %d", rootwin, childwin, xrootco, yrootco,
			xwinco, ywinco, state);
		fflush(stdout);
	}

	*hs = (float) xwinco;
	*vs = (float) (pixdata[window_num].y - ywinco);
}

void dggcur(
	float *hs,
	float *vs
)
/*
 * Remove ASAP.
 */
{
	logical Yes = True;
	dggtcu(hs, vs, &Yes);
}

void dgcurg(
	float *hr,
	float *vr
)
/*
 * Remove ASAP.
 */
{
	logical no = False;
	dggtcu(hr, vr, &no);
}

void dgclwk()
/*
 * Clear DALI window.
 */
{
	unsigned long temp_fg;
	int index1, index2;

	temp_fg = grcon_val.foreground;
	grcon_val.foreground = grcon_val.background;
	XChangeGC(disp, grcon, GCForeground, &grcon_val);
	XFillRectangle(disp, drawin, grcon, 0, 0, pixdata[window_num].x, pixdata[window_num].y);
	grcon_val.foreground = temp_fg;
	XChangeGC(disp, grcon, GCForeground, &grcon_val);
	duiscc.nersdu = 0;
	for (index1 = 0; index1 <= mpnwdu; index1++) {
		duiscc.idsgdu[index1] = 0;
		for (index2 = 0; index2 < 7; index2++) {
			if (duiscc.iocldu[index2 + index1 * 7] != -2)
				duiscc.iocldu[index2 + index1 * 7] = 0;
		}
	}
	astcm1.fbox = False;
}

void dgalgr
(
	int *iwind,
	int *lev,
	char *tlet,
	int idir,
	float h1,
	float v1,
	float h2,
	float v2
)
/*
 * Draw ALEPH character and the greek letters 'RHO,THETA,PHI'
 */
{
	static float ax[] = { 2.30, 5.95, 7.30, 8.00, 8.20,
		 4.00,14.60,15.05,16.28,16.45,
		16.31,15.45,12.68,13.19,14.55,
		16.00,16.10,13.00,12.53,10.47,
		10.92,12.00,11.58, 4.62, 4.35,
		 2.50, 2.30, 3.50, 2.10, 2.75};
	static float ay[] = { 1.30, 1.20, 2.00, 3.00, 4.00,
		 9.85, 2.00, 1.00, 3.00, 4.00,
		 6.00, 7.72,10.30,11.44,10.88,
		13.00,14.48,17.88,18.90,14.61,
		13.30,12.27,11.11,16.45,18.50,
		16.00,13.10,10.65, 7.00, 1.70};

	float ah, av, bh, bv;
	float xxh[30], yyv[30];
	int index;
	int temp;
	int x, y;
	unsigned char cout[4];
#ifdef vms
	FortranString cout_dsc;

	cout_dsc.dsc$w_length = 4;
	cout_dsc.dsc$a_pointer = cout;
#endif /* VMS characters */

	ah = h1;
	av = v1;
	bh = (h2 - ah) / hmax;
	bv = (v2 - av) / vmax;

	x = (int) h1 + dtxth;
	y = pixdata[window_num].y - ((int) v1 + dtxtv - (greekfontinfo->descent +
		greekfontinfo->ascent));
	cout[0] = ' ';
	cout[1] = '\0';
/*
 * This code draws the Aleph sign.
 */
	if (!strcmp(tlet, "ALEPH")) {
		dglevl(lev);
		for (index = 0; index < 30; index++) {
			xxh[index] = ah + bh * ax[index];
			yyv[index] = av + bv * ay[index];
		}
		if (idir != 0) {
			dgrotg(30, xxh, yyv, h1, v1);
		}
		temp = 30;
		dgarea(&temp, xxh, yyv);
	} else

/*
 * This code draws the phi character.
 */
	if (!strcmp(tlet, "PHI  ")) {
		cout[0] = 'f';
		if (idir == 0) {
			grcon_val.font = greekfont;
			XChangeGC(disp, grcon, GCFont, &grcon_val);
			XDrawString(disp, drawin, grcon, x, y, "f", 1);
			grcon_val.font = mainfont;
			XChangeGC(disp, grcon, GCFont, &grcon_val);
		} else {
			temp = 1;
			drawtextvert(&h1, &v1, dtxth, dtxtv, "f", &temp, 1);
		}
	} else
/*
 * This code draws the theta character.
 */
	if (!strcmp(tlet, "THETA")) {
		cout[0] = 'q';
		if (idir == 0) {
			grcon_val.font = greekfont;
			XChangeGC(disp, grcon, GCFont, &grcon_val);
			XDrawString(disp, drawin, grcon, x, y, "q", 1);
			grcon_val.font = mainfont;
			XChangeGC(disp, grcon, GCFont, &grcon_val);
		} else {
			temp = 1;
			drawtextvert(&h1, &v1, dtxth, dtxtv, "q", &temp, 1);
		}
	} else
/*
 * This code draws the rho character.
 */
	if (!strcmp(tlet, "RHO  ")) {
		cout[0] = 'r';
		if (idir == 0) {
			grcon_val.font = greekfont;
			XChangeGC(disp, grcon, GCFont, &grcon_val);
			XDrawString(disp, drawin, grcon, x, y, "r", 1);
			grcon_val.font = mainfont;
			XChangeGC(disp, grcon, GCFont, &grcon_val);
		} else {
			temp = 1;
			drawtextvert(&h1, &v1, dtxth, dtxtv, "r", &temp, 1);
		}
	}
	if (duiscc.fmetdu) mdgalgr(iwind, lev, tlet, &idir, &h1, &v1, &h2, &v2,
#ifdef vms
		&cout_dsc, &greekfontinfo->ascent);
#else
		cout, &greekfontinfo->ascent);
#endif /* VMS characters */
}

void dgrar(
	float *h1,
	float *v1,
	float *h2,
	float *v2
)
{
	float h[4], v[4];
	int npt = 4;

	h[0] = *h1;
	h[1] = *h2;
	h[2] = *h2;
	h[3] = *h1;
	v[0] = *v1;
	v[1] = *v1;
	v[2] = *v2;
	v[3] = *v2;
	dgarea(&npt, h, v);
}

void dgrotg(
	int np,
	float x[],
	float y[],
	float h,
	float v
)
/*
 * Rotate polyline (x[],y[]) 90 degrees around point (h,v)
 */
{
	float pi, rv[2], z;
	float sintheta, costheta, sinphi, cosphi;
	int index;
	float xx, yy;

	pi = 3.14159;
	rv[0] = 0.0;
	rv[1] = -90.0;
	z = 1.0;

	sintheta = (float) sin((double) (rv[0] * pi / 180.0));
	costheta = (float) cos((double) (rv[0] * pi / 180.0));
	sinphi   = (float) sin((double) (rv[1] * pi / 180.0));
	cosphi   = (float) cos((double) (rv[1] * pi / 180.0));

	for (index = 0; index < np; index++) {
		xx = x[index] - h;
		yy = y[index] - v;
		x[index] = (xx * cosphi + yy * sinphi * costheta +
			z * sinphi * sintheta) + h;
		y[index] = (-xx * sinphi + yy * cosphi * costheta +
			z * cosphi * sintheta) + v;
	}
}

void dgstop()
{
/*
 * Do close down functions if any.
 */
#if !defined(vms)
	if (tty_reset() == 0) {
		if (debug_mask[(int)('d'-'a')])
		printf("\nResetting terminal.\n");
	}
#else
	printf("\n");
#endif /* VMS */
}

void dgstr1(
	int *iare,
	int *nocl
)
/*
 * This routine is called when a new picture is started.
 */
{
	char text[20];
	int  len;
	int five = 5;
#ifdef vms
	FortranString ftext;
#endif /* vms */

/*
 * Make sure any previous rubberband etc. is cleared.
 */
	astcm1.fbox = False;
/*
 * Switch to watch pointer (in most cases).
 */
	if(!dtvccc.fblwdt) dgswpt(&five);
/*
 * Insert a comment in the metafile when starting a new window.
 */
	if (duiscc.fmetdu) {
		strcpy(text,"Drawing window ");
		strncat(text,dopr2t.taredo[*iare],2);
		text[17] = '\0';
		len = strlen(text);
#ifdef vms
		ftext.dsc$a_pointer = (unsigned char *)text;
		ftext.dsc$w_length = len;
		dgcome(&ftext);
#else
		dgcome(text,len);
#endif /* vms */
	}
}

void setXcursor(
	char data[],
	int fg,
	int bg,
	int hsx,
	int hsy
)
/*
 * Change the cursor shape.
 */
{
	Pixmap bitsmap, maskmap;
	XColor fgcol, bgcol;
	Cursor previous_cursor;

	if (cursormodified)
		previous_cursor = cursor;
	bitsmap = XCreatePixmapFromBitmapData(disp, XtWindow(area[window_num]),
		data, 16, 16, 1, 0, 1);
	maskmap = XCreatePixmapFromBitmapData(disp, XtWindow(area[window_num]),
		&data[32], 16, 16, 1, 0, 1);
	fgcol.pixel = fg;
	bgcol.pixel = bg;
	XQueryColor(disp, cmap, &fgcol);
	XQueryColor(disp, cmap, &bgcol);
	cursor = XCreatePixmapCursor(disp, bitsmap, maskmap,
		&fgcol, &bgcol, hsx, hsy);
	XDefineCursor(disp, XtWindow(area[window_num]), cursor);
	if (cursormodified)
		XFreeCursor(disp, previous_cursor);
	XFlush(disp);
	cursormodified = True;
	XFreePixmap(disp, bitsmap);
	XFreePixmap(disp, maskmap);
}

void dgdfcu()
/*
 * Define cursors for DALI window. To be called from parameter setting
 * routines.
 */
{
	static char curale_bits[2][32] = {
		{
			0x00,0x00,0x08,0x08,0x0c,0x0c,0x1c,0x0e,
			0x3c,0x1e,0x7c,0x3e,0xf8,0x3c,0xf8,0x17,
			0xc8,0x07,0x9c,0x0f,0x3c,0x1e,0x7c,0x3c,
			0x7c,0x38,0x78,0x30,0x3c,0x10,0x00,0x00
		},{
			0x00,0x00,0x08,0x08,0x0c,0x0c,0x1c,0x0e,
			0x3c,0x1e,0x7c,0x3e,0xf8,0x3c,0xf8,0x17,
			0xc8,0x07,0x9c,0x0f,0x3c,0x1e,0x7c,0x3c,
			0x7c,0x38,0x78,0x30,0x3c,0x10,0x00,0x00
		}
	};
	static char curale_mask[2][32] = {
		{
			0x08,0x08,0x1c,0x1c,0x0e,0x1e,0x3e,0x1f,
			0x7e,0x3f,0xfe,0x7f,0xfc,0x7f,0xfc,0x3f,
			0xfc,0x0f,0xbe,0x1f,0x7e,0x3f,0xfe,0x7e,
			0xfe,0x7c,0xfc,0x78,0x7e,0x38,0x3e,0x10
		},{
			0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
			0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
			0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
			0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
		}
	};

	static char currb_bits[4][32] = {
		{
			0x00,0x00,0xc0,0x01,0xc0,0x01,0xc0,0x01,
			0xc0,0x01,0xc0,0x01,0xc0,0x01,0xff,0x7f,
			0xff,0x7f,0xff,0x7f,0xc0,0x01,0xc0,0x01,
			0xc0,0x01,0xc0,0x01,0xc0,0x01,0xc0,0x01
		},{
			0x00,0x00,0x80,0x00,0x80,0x00,0x80,0x00,
			0x80,0x00,0x80,0x00,0x80,0x00,0x80,0x00,
			0xff,0x7f,0x80,0x00,0x80,0x00,0x80,0x00,
			0x80,0x00,0x80,0x00,0x80,0x00,0x80,0x00
		},{
			0x00,0x00,0x80,0x00,0x80,0x00,0x80,0x00,
			0x80,0x00,0x80,0x00,0x80,0x00,0x00,0x00,
			0x3f,0x7e,0x00,0x00,0x80,0x00,0x80,0x00,
			0x80,0x00,0x80,0x00,0x80,0x00,0x80,0x00
		},{
			0x00,0x00,0x80,0x00,0x80,0x00,0x80,0x00,
			0x80,0x00,0x80,0x00,0x80,0x00,0x00,0x00,
			0x3f,0x7e,0x00,0x00,0x80,0x00,0x80,0x00,
			0x80,0x00,0x80,0x00,0x80,0x00,0x80,0x00
		}
	};
	static char currb_mask[4][32] = {
		{
			0x00,0x00,0x80,0x00,0x80,0x00,0x80,0x00,
			0x80,0x00,0x80,0x00,0x80,0x00,0x80,0x00,
			0xff,0x7f,0x80,0x00,0x80,0x00,0x80,0x00,
			0x80,0x00,0x80,0x00,0x80,0x00,0x80,0x00
		},{
			0x00,0x00,0x80,0x00,0x80,0x00,0x80,0x00,
			0x80,0x00,0x80,0x00,0x80,0x00,0x80,0x00,
			0xff,0x7f,0x80,0x00,0x80,0x00,0x80,0x00,
			0x80,0x00,0x80,0x00,0x80,0x00,0x80,0x00
		},{
			0x00,0x00,0x80,0x00,0x80,0x00,0x80,0x00,
			0x80,0x00,0x80,0x00,0x80,0x00,0x00,0x00,
			0x3f,0x7e,0x00,0x00,0x80,0x00,0x80,0x00,
			0x80,0x00,0x80,0x00,0x80,0x00,0x80,0x00
		},{
			0x00,0x00,0xc0,0x01,0xc0,0x01,0xc0,0x01,
			0xc0,0x01,0xc0,0x01,0xc0,0x01,0x3f,0x7e,
			0x3f,0x7e,0x3f,0x7e,0xc0,0x01,0xc0,0x01,
			0xc0,0x01,0xc0,0x01,0xc0,0x01,0xc0,0x01
		}
	};

	static char pntzer_bits[32] = {
		0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
		0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
		0x80,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
		0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
	};
	static char pntzer_mask[32] = {
		0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
		0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
		0x80,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
		0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
	};

	static char curpic_bits[2][32] = {
		{
			0xff,0x7f,0x81,0x40,0x81,0x40,0x81,0x40,
			0x81,0x40,0x01,0x40,0x01,0x40,0x1f,0x7c,
			0x01,0x40,0x01,0x40,0x81,0x40,0x81,0x40,
			0x81,0x40,0x81,0x40,0xff,0x7f,0x00,0x00
		},{
			0xff,0x7f,0x81,0x40,0x81,0x40,0x81,0x40,
			0x81,0x40,0x01,0x40,0x01,0x40,0x1f,0x7c,
			0x01,0x40,0x01,0x40,0x81,0x40,0x81,0x40,
			0x81,0x40,0x81,0x40,0xff,0x7f,0x00,0x00
		}
	};
	static char curpic_mask[2][32] = {
		{
			0xff,0x7f,0xff,0x7f,0x83,0x60,0x83,0x60,
			0x83,0x60,0x03,0x60,0x03,0x60,0x1f,0x7c,
			0x03,0x60,0x03,0x60,0x83,0x60,0x83,0x60,
			0x83,0x60,0xff,0x7f,0xff,0x7f,0x00,0x00
		},{
			0xff,0x7f,0x81,0x40,0x81,0x40,0x81,0x40,
			0x81,0x40,0x01,0x40,0x01,0x40,0x1f,0x7c,
			0x01,0x40,0x01,0x40,0x81,0x40,0x81,0x40,
			0x81,0x40,0x81,0x40,0xff,0x7f,0x00,0x00
		}
	};

	static char curpnt_bits[2][32] = {
		{
			0x00,0x00,0x03,0x60,0x07,0x70,0x0e,0x38,
			0x1c,0x1c,0x38,0x0e,0x70,0x07,0x20,0x02,
			0x00,0x00,0x20,0x02,0x70,0x07,0x38,0x0e,
			0x1c,0x1c,0x0e,0x38,0x07,0x70,0x03,0x60
		},{
			0x00,0x00,0x01,0x40,0x02,0x20,0x04,0x10,
			0x08,0x08,0x10,0x04,0x20,0x02,0x00,0x00,
			0x00,0x00,0x00,0x00,0x20,0x02,0x10,0x04,
			0x08,0x08,0x04,0x10,0x02,0x20,0x01,0x40
		}
	};
	static char curpnt_mask[2][32] = {
		{
			0x00,0x00,0x02,0x20,0x05,0x50,0x0a,0x28,
			0x14,0x14,0x28,0x0a,0x50,0x05,0x20,0x02,
			0x00,0x00,0x20,0x02,0x50,0x05,0x28,0x0a,
			0x14,0x14,0x0a,0x28,0x05,0x50,0x02,0x20
		},{
			0x00,0x00,0x01,0x40,0x02,0x20,0x04,0x10,
			0x08,0x08,0x10,0x04,0x20,0x02,0x00,0x00,
			0x00,0x00,0x00,0x00,0x20,0x02,0x10,0x04,
			0x08,0x08,0x04,0x10,0x02,0x20,0x01,0x40
		}
	};

	static char wtcpnt_bits[8][32] = {
		{
			0xe0,0x03,0xe0,0x03,0xf0,0x07,0x88,0x08,
			0x84,0x18,0x82,0x24,0x82,0x22,0x82,0x61,
			0x82,0x60,0x02,0x20,0x02,0x20,0x04,0x10,
			0x08,0x08,0xf0,0x07,0xe0,0x03,0xe0,0x03
		},{
			0xe0,0x03,0xe0,0x03,0xf0,0x07,0x88,0x08,
			0x84,0x10,0x82,0x20,0x82,0x20,0x82,0x60,
			0x82,0x6f,0x02,0x20,0x02,0x20,0x04,0x10,
			0x08,0x08,0xf0,0x07,0xe0,0x03,0xe0,0x03
		},{
			0xe0,0x03,0xe0,0x03,0xf0,0x07,0x88,0x08,
			0x84,0x10,0x82,0x20,0x82,0x20,0x82,0x60,
			0x82,0x60,0x02,0x21,0x02,0x22,0x04,0x14,
			0x08,0x08,0xf0,0x07,0xe0,0x03,0xe0,0x03
		},{
			0xe0,0x03,0xe0,0x03,0xf0,0x07,0x88,0x08,
			0x84,0x10,0x82,0x20,0x82,0x20,0x82,0x60,
			0x82,0x60,0x82,0x20,0x82,0x20,0x84,0x10,
			0x08,0x08,0xf0,0x07,0xe0,0x03,0xe0,0x03
		},{
			0xe0,0x03,0xe0,0x03,0xf0,0x07,0x88,0x08,
			0x84,0x10,0x82,0x20,0x82,0x20,0x82,0x60,
			0x82,0x60,0x42,0x20,0x22,0x20,0x14,0x10,
			0x08,0x08,0xf0,0x07,0xe0,0x03,0xe0,0x03
		},{
			0xe0,0x03,0xe0,0x03,0xf0,0x07,0x88,0x08,
			0x84,0x10,0x82,0x20,0x82,0x20,0x82,0x60,
			0xfa,0x60,0x02,0x20,0x02,0x20,0x04,0x10,
			0x08,0x08,0xf0,0x07,0xe0,0x03,0xe0,0x03
		},{
			0xe0,0x03,0xe0,0x03,0xf0,0x07,0x88,0x08,
			0x8c,0x10,0x92,0x20,0xa2,0x20,0xc2,0x60,
			0x82,0x60,0x02,0x20,0x02,0x20,0x04,0x10,
			0x08,0x08,0xf0,0x07,0xe0,0x03,0xe0,0x03
		},{
			0xe0,0x03,0xe0,0x03,0xf0,0x07,0x08,0x08,
			0x84,0x10,0x82,0x20,0x82,0x20,0x82,0x60,
			0x82,0x60,0x02,0x20,0x02,0x20,0x04,0x10,
			0x08,0x08,0xf0,0x07,0xe0,0x03,0xe0,0x03
		}
	};
	static char wtcpnt_mask[32] = {
		0xf0,0x07,0xf0,0x07,0xf8,0x0f,0xfc,0x1f,
		0xfe,0x3f,0xff,0x7f,0xff,0x7f,0xff,0x7f,
		0xff,0x7f,0xff,0x7f,0xff,0x7f,0xfe,0x3f,
		0xfc,0x1f,0xf8,0x0f,0xf0,0x07,0xf0,0x07
	};

	static char wtcpnt_mono_bits[32] = {
		0xf8,0x0f,0xf8,0x0f,0xf8,0x0f,0x0c,0x18,
		0x06,0x30,0x02,0xe0,0x03,0xe0,0x01,0xe0,
		0x01,0xe0,0x03,0xe0,0x02,0xe0,0x06,0x30,
		0x0c,0x18,0xf8,0x0f,0xf8,0x0f,0xf8,0x0f
	};

	int hspalh = 7;
	int hspalv = 8;
	static int hsprbh[4] = {7, 7, 7, 7};
	static int hsprbv[4] = {8, 8, 8, 8};
	int hsppih = 8;
	int hsppiv = 7;
	int hsppoh = 7;
	int hsppov = 8;
	int hspwth = 7;
	int hspwtv = 8;

	int icudu, ival, jval;
	int foldfl, index;
	time_t timedate;
	char* timedatestr;
	int hour, minute;
	int tmp1, tmp2;

	if (dgdfcu_first_call) {
		XFlush(disp);
		for (index = 0; index < ((2 * mxcudx) - 1); index++)
			dxpntr.fupodx[index] = 0;
	}

/*
 * Aleph cursor.
 */
	icudu = (int) (dusdac.dacudu + 0.01);
	ival = icudu % 10;
	foldfl = dxpntr.fupodx[0];
	if (ival == 1 || ival == 2) {
		dxpntr.fupodx[0] = 1;
		for (index = 0; index < 32; index++) {
			dxpntr.poindx[0][0][index] =
				curale_bits[ival - 1][index];
			dxpntr.poindx[0][0][index+32] =
				curale_mask[ival - 1][index];
		}
		dxpntr.hsphdx[0] = hspalh;
		dxpntr.hspvdx[0] = hspalv;
	} else {
		dxpntr.fupodx[0] = 0;
	}
	dxpntr.fupodx[mxcudx] = 0;
	if (foldfl != dxpntr.fupodx[0]) {
		tmp1 = 1;
		tmp2 = 1;
		dgsppt(&tmp1, &tmp2);
	}
/*
 * Rubberband cursor. Default is a plain cross.
 */
	ival = (icudu % 100) / 10;
	if (ival >= 0 && ival <= 7) {
		jval = (ival % 4) + 1;
	} else {
		if (ival == 9) {
			jval = 0;
		} else {
			jval = 1;
		}
	}
	if (jval > 0) {
		dxpntr.fupodx[1] = 1;
		dxpntr.fupodx[mxcudx + 1] = 1;
		for (index = 0; index < 32; index++) {
			dxpntr.poindx[1][0][index]      = currb_bits[jval - 1][index];
			dxpntr.poindx[1][0][index + 32] = currb_mask[jval - 1][index];
			dxpntr.poindx[1][1][index]      = currb_bits[jval - 1][index];
			dxpntr.poindx[1][1][index + 32] = currb_mask[jval - 1][index];
		}
		dxpntr.hsphdx[1] = hsprbh[jval - 1];
		dxpntr.hspvdx[1] = hsprbv[jval - 1];
	} else {
		dxpntr.fupodx[1] = 0;
		dxpntr.fupodx[1 + mxcudx] = 0;
	}
	if (ival >= 4 && ival <= 7) {
		for (index = 0; index < 32; index++) {
			dxpntr.poindx[1][1][index] = pntzer_bits[index];
			dxpntr.poindx[1][1][index + 32] = pntzer_mask[index];
		}
	}
	if (duiscc.indvcm == 2) {
		for (index = 0; index < 32; index++) {
			dxpntr.poindx[1][0][index + 32] = dxpntr.poindx[1][0][index];
			dxpntr.poindx[1][1][index + 32] = dxpntr.poindx[1][1][index];
		}
	}
/*
 * Pick cursor.
 */
	ival = (icudu % 1000) / 100;
	if (ival > 0) {
		dxpntr.fupodx[2] = 0;
		dxpntr.fupodx[2 + mxcudx] = 0;
	} else {
		dxpntr.fupodx[2] = 1;
		dxpntr.fupodx[2 + mxcudx] = 1;
		for (index = 0; index < 32; index++) {
			dxpntr.poindx[2][0][index]      = curpic_bits[0][index];
			dxpntr.poindx[2][0][index + 32] = curpic_mask[0][index];
			dxpntr.poindx[2][1][index]      = curpic_bits[1][index];
			dxpntr.poindx[2][1][index + 32] = curpic_mask[1][index];
		}
		if (duiscc.indvcm == 2) {
			for (index = 0; index < 32; index++) {
				dxpntr.poindx[2][0][index + 32] = dxpntr.poindx[2][0][index];
				dxpntr.poindx[2][1][index + 32] = dxpntr.poindx[2][1][index];
			}
		}
		dxpntr.hsphdx[2] = hsppih;
		dxpntr.hspvdx[2] = hsppiv;
	}
/*
 * Point cursor.
 */
	ival = (icudu % 10000) / 1000;
	if (ival > 0) {
		dxpntr.fupodx[3] = 0;
		dxpntr.fupodx[3 + mxcudx] = 0;
	} else {
		dxpntr.fupodx[3] = 1;
		dxpntr.fupodx[3 + mxcudx] = 1;
		for (index = 0; index < 32; index++) {
			dxpntr.poindx[3][0][index] = curpnt_bits[0][index];
			dxpntr.poindx[3][0][index + 32] = curpnt_mask[0][index];
			dxpntr.poindx[3][1][index] = curpnt_bits[1][index];
			dxpntr.poindx[3][1][index + 32] = curpnt_mask[1][index];
		}
		if (duiscc.indvcm == 2) {
			for (index = 0; index < 32; index++) {
				dxpntr.poindx[3][0][index + 32] = dxpntr.poindx[3][0][index];
				dxpntr.poindx[3][1][index + 32] = dxpntr.poindx[3][1][index];
			}
		}
		dxpntr.hsphdx[3] = hsppoh;
		dxpntr.hspvdx[3] = hsppov;
	}
/*
 * Timer cursor.
 */
	dxpntr.fupodx[4] = 1;
	dxpntr.fupodx[4 + mxcudx] = 1;
	timedate = time(NULL);
	timedatestr = asctime(localtime(&timedate));
	hour = ((int) timedatestr[11]  - 48) * 10 + (int) timedatestr[12] - 48;
	minute  = ((int) timedatestr[14]  - 48) * 10 +
		(int) timedatestr[15] - 48;
	hour = ((hour % 12) * 2 + minute / 30 + 1) / 3;
	if (hour < 1)
		hour = hour + 8;
	for (index = 0; index < 32; index++) {
		dxpntr.poindx[4][0][index] = wtcpnt_bits[hour - 1][index];
		dxpntr.poindx[4][0][index + 32] = wtcpnt_mask[index];
	}
	if ((duiscc.indvcm == 2) || isGray) {
		for (index = 0; index < 32; index++) {
			dxpntr.poindx[4][0][index] |= wtcpnt_mono_bits[index];
			dxpntr.poindx[4][0][index + 32] = dxpntr.poindx[4][0][index];
		}
	}
	dxpntr.hsphdx[4] = hspwth;
	dxpntr.hspvdx[4] = hspwtv;

	if (dgdfcu_first_call) {
		dgdfcu_first_call = False;
		if (dxpntr.fupodx[0]) {
			int bg = isColor ? ddefco.ltabdd[8] : ddefco.ltabdd[2];

			setXcursor(dxpntr.poindx[0][0],
				ddefco.ltabdd[12], bg,
				dxpntr.hsphdx[0], dxpntr.hspvdx[0]);
		}
	}
}

/*
 * New functions associated with dali_qio etc.
 */

void dgclev(
	int *clrflg
)
{
	if (*clrflg == 1) {
		if (keybufsize == 0) {
			evflag[(*clrflg) - 1] = 0;
		}
	} else {
		evflag[(*clrflg) - 1] = 0;
	}
}

void dgstev(
	int *setflg
)
{
	evflag[(*setflg) - 1] = 1;
}

void dgtflg(
	int *flagnum,
	short xiosb[4],
	FortranString *text
)
{
	int index, chridx;
	int fklen;

	for (index = 0; index < 7; index++) {
		if (evflag[index]) {
			*flagnum = index + 1;
			break;
		}
	}
	switch (*flagnum) {
	case 1:
		xiosb[0] = 1;
		xiosb[1] = 0;
		xiosb[2] = 0;
		xiosb[3] = 0;
		for (chridx = 0; chridx < reqnumchars; chridx++) {
			if ((keybuffer[0] <= 31 || keybuffer[0] == 127) &&
				keybuffer[0] != 0x1b) {
				xiosb[2] = keybuffer[0] | 0xff00;
				xiosb[3] = 1;
				for (index = 1; index <= keybufsize; index++) {
					keybuffer[index - 1] = keybuffer[index];
				}
				keybufsize -= 1;
				break;
			}
			if (keybuffer[0] != 0x1b) {
#ifdef vms
				text->dsc$a_pointer[xiosb[1]] = keybuffer[0];
#else
				text[xiosb[1]] = keybuffer[0];
#endif /* VMS characters */
				xiosb[1] += 1;
				for (index = 1; index <= keybufsize; index++) {
					keybuffer[index - 1] = keybuffer[index];
				}
				keybufsize -= 1;
			} else {
				int ksize = sizeof(keybuffer);
				xiosb[2] = 0x1b;
#ifdef vms
				text->dsc$a_pointer[xiosb[1]] = keybuffer[0];
#else
				text[xiosb[1]] = keybuffer[0];
#endif /* VMS characters */
				fklen = 1;
				while ((keybuffer[fklen] != 0x1b) && (keybuffer[fklen] != 0) &&
					(fklen < ksize)) {
#ifdef vms
					text->dsc$a_pointer[fklen + xiosb[1]] = keybuffer[fklen];
#else
					text[fklen + xiosb[1]] = keybuffer[fklen];
#endif /* VMS characters */
					fklen++;
				}
				xiosb[3] = (short) fklen;
				fklen++;
				for (index = fklen; index < keybufsize; index++) {
					keybuffer[index - fklen] = keybuffer[index];
				}
				keybufsize = keybufsize - fklen;
				if (keybufsize < 0) keybufsize = 0;
				break;
			}
		}
		reqnumchars = 0;
		break;
	case 2:
		break;
	default:
		break;
	}
}

void dgtlnx_lc()
/*
 * Swap lowercase/uppercase conversion flag.
 */
{
	convert_to_upper = !convert_to_upper;
}

void dgtlnx(
	int *maxlen,
	short xiosb[4],
#ifndef __unix__
	FortranString *text
#else
	FortranString *text,
	int l_text
#endif /* Unix */
)
/*
 * Read a string. Convert to uppercase if convert_to_upper is True
 * We also use this routine's waitloop to trace pointer movements
 * to be interrupted by keyboard input.
 */
{
	int index1, index2;
	logical finished;

	dgtlnx_on = True;
	finished = False;
	xiosb[1] = 0;
	index1= 0;
	fflush(stdout);
	while (index1 < *maxlen && !finished) {
		reqnumchars = *maxlen - index1;
		while(!(evflag[0]|evflag[5])) {
			XtAppProcessEvent(Dali_context,XtIMAll);
		}
/*
 * Return if event was a pointer movement we are tracing.
 */
		if (evflag[5]) {
			dgtlnx_on = False;
			return;
		}
		xiosb[0] = 1;
		for(index2 = 0; index2 < keybufsize && index1 < *maxlen;
			 index2++) {
			if (keybuffer[index2] <= 31 || keybuffer[index2] == 0x1b) {
				xiosb[2] = 0xff00 | keybuffer[index2];
				xiosb[3] = 1;
				if ((keybuffer[index2] == 0x1b) && (index2 == 0))
#ifdef VMS
					text->dsc$a_pointer[0] = keybuffer[1];
#else
					text[0] = keybuffer[1];
#endif /* VMS characters */
				finished = True;
				break;
			}
			if ((keybuffer[index2] == 127) || (keybuffer[index2] == 8)) {
				index1 -= 1;
				evflag[0] = 0;
				keybufsize = 0;
				xiosb[1] -= 1;
				if (index1 < 0 || xiosb[1] < 0) {
					index1 = 0;
					xiosb[1] = 0;
				} else {
					printf("%s", delete_string);
					fflush(stdout);
				}
				break;
			}
#ifdef vms
			text->dsc$a_pointer[index1] = keybuffer[index2];
#else
			text[index1] = keybuffer[index2];
#endif /* VMS characters */
			xiosb[1]++;
			index1++;
		}
	}
	evflag[0] = 0;
	keybufsize = 0;
	reqnumchars = 0;
	dgtlnx_on = False;
	fflush(stdout);
}

void dgchkx()
/*
 * Called to take care of pending events like exposures, focus change etc.
 */
{
	logical debugd;
	int index, iwin;
	XtInputMask appmask=0;

	for (iwin=0; iwin<MAX_WINDOWS; iwin++) {
		if (area[iwin] == 0) continue;
		XCopyArea(disp, winpix[iwin], XtWindow(area[iwin]), grcon, 0, 0,
			pixdata[iwin].x, pixdata[iwin].y, 0, 0);
	}
	debugd = debug_mask[(int)('d'-'a')];
	if (winpix_copied == 0) winpix_copied = 1;
/*
 * Also copy a gc255 box if any.
 */
	if (astcm1.fbox) {
		for (index = 0; index < 4; index++) {
			XDrawLine(disp, XtWindow(area[window_num]), gc255,
				(int) astcm1.pxx[index],
				pixdata[window_num].y - (int) astcm1.pyy[index],
				(int) astcm1.pxx[index + 1],
				pixdata[window_num].y - (int) astcm1.pyy[index + 1]);
		}
	}
	if (debugd) XFlush(disp);
/*
 * Here a difference between X11R5 and X11R6 (Digital UNIX 3.2 and 4.0 resp.)
 * showed up in May, 1997.
 */
	while(appmask=XtAppPending(Dali_context)) {
		if (debugd) printf("\nappmask: %x\n", appmask);
		XtAppProcessEvent(Dali_context,XtIMAll);
	}
	if (debugd) printf("\nappmask: %x\n", appmask);
}

void dgwtlp(
	int *numchars
)
/*
 * Copy memory pixmap to display.
 */
{
	logical debugd;
	int index, iwin;

	reqnumchars = *numchars;
	for (iwin=0; iwin<MAX_WINDOWS; iwin++) {
		if (area[iwin] == 0) continue;
		XCopyArea(disp, winpix[iwin], XtWindow(area[iwin]), grcon, 0, 0,
			pixdata[iwin].x, pixdata[iwin].y, 0, 0);
	}
	debugd = debug_mask[(int)('d'-'a')];
	if (winpix_copied == 0) winpix_copied = 1;
/*
 * Also copy a gc255 box if any.
 */
	if (astcm1.fbox) {
		for (index = 0; index < 4; index++) {
			XDrawLine(disp, XtWindow(area[window_num]), gc255,
				(int) astcm1.pxx[index],
				pixdata[window_num].y - (int) astcm1.pyy[index],
				(int) astcm1.pxx[index + 1],
				pixdata[window_num].y - (int) astcm1.pyy[index + 1]);
		}
	}
	if (debugd) XFlush(disp);
	fflush(stdout);
	while(!(evflag[0]||evflag[1]||evflag[2]||evflag[3]||evflag[4])) {
		XtAppProcessEvent(Dali_context,XtIMAll);
	}
}

XtEventHandler displaywinpix(
	Widget w,
	Pixmapsize *data,
	XEvent *junk
)
{
	int iwin;

	for (iwin=0; iwin<MAX_WINDOWS; iwin++) {
		if (area[iwin] == 0) continue;
		XCopyArea(disp, winpix[iwin], XtWindow(area[iwin]), grcon, 0, 0,
			data->x, data->y, 0, 0);
	}
	if (winpix_copied == 0) winpix_copied = 1;
	return 0;
}

XtEventHandler keyinput(
	Widget w,
	caddr_t junk,
	XKeyEvent *event
)
/*
 * Keyboard input handler.
 */
{
	KeySym key;
	int index, index1;
	unsigned char keystring[80];
	static char beep[] = {0x07, 0x00};
	int keystringlen;
	char *funckey;

#ifdef TEST
printf("\nKey event received: keycode=%d,\n", event->keycode);
#endif /* TEST */
	if (event->type == KeyPress) {
		keystringlen = XLookupString(event, (char *) keystring, 80, &key,
			&compstat);
		if (keystringlen > 0) {
			if (IsKeypadKey(key)) {
				keybuffer[keybufsize] = 0x1b;
				keybufsize++;
				if (keystring[0] >= '0' && keystring[0] <= '9'){
					keybuffer[keybufsize] = 'K';
					keybufsize++;
					keybuffer[keybufsize] = 'P';
					keybufsize++;
					keybuffer[keybufsize] = keystring[0];
					keybufsize++;
					keybuffer[keybufsize] = 0x1b;
					keybufsize++;
				} else {
					if (keystring[0] == 0x0d) {
						strcpy((char *) &keybuffer[keybufsize], "Enter");
						keybufsize += 5;
					}
					if (keystring[0] == '.') {
						strcpy((char *) &keybuffer[keybufsize], "Period");
						keybufsize += 6;
					}
					if (keystring[0] == '-') {
						strcpy((char *) &keybuffer[keybufsize], "Minus");
						keybufsize += 5;
					}
					if (keystring[0] == ',') {
						strcpy((char *) &keybuffer[keybufsize], "Comma");
						keybufsize += 5;
					}
					keybuffer[keybufsize] = 0x1b;
					keybufsize++;
				}
				evflag[0] = 1;
				return 0;
			}
			for (index = 0; index < keystringlen; index++) {
/*
 * Convert to upper depending on logical convert_to_upper
 */
				if ((((keystring[index] >= 97) && (keystring[index] <= 122)) ||
					((keystring[index] >= 225) && (keystring[index] <= 250))) &&
					convert_to_upper) {
							keystring[index] -= 32;
				}
				switch((int) keystring[index]) {
				case 3: case 5: case 9: case 12: case 18: case 31:
					index1 = (int) keystring[index];
					dgooba(index1);
					fflush(stdout);
					break;
				case 13: case 26: case 27:
/* CR, Ctrl-Z, ESC */
					keybuffer[keybufsize] = keystring[index]; keybufsize++;
					keybuffer[keybufsize] = '\0';
					evflag[0] = 1;
					break;
				case 8: case 127:
/* BS, DEL */
					if (reqnumchars > 1) {
						keybuffer[keybufsize] = '\0';
						if (keybufsize == 0) break;
						keybufsize--;
						printf("%s", delete_string);
						fflush(stdout);
					} else {
						keybuffer[keybufsize] = keystring[index]; keybufsize++;
						keybuffer[keybufsize] = '\0';
						evflag[0] = 1;
					}
					break;
				default:
					if (dmacrc.fntadm) {
/*
 * If type-ahead is disabled, beep on normal characters and return.
 */
						printf("%s", beep);
						fflush(stdout);
						break;
					}
					if (keystring[index] < 31) {
						printf ("\n^%c is not an accepted control character            |",
							keystring[index] + 64);
						fflush(stdout);
						break;
					}
					for (index1 = 0; index1 < keystringlen; index1++) {
						keybuffer[keybufsize] = keystring[index1];
						if (keybuffer[keybufsize] < 31)
							evflag[0] = 1;
						if (keybuffer[keybufsize] > 31) {
							keybuffer[keybufsize+1] = '\0';
							printf("%s", &keybuffer[keybufsize]);
							fflush(stdout);
						}
						keybufsize++;
					}
					if (keybufsize >= reqnumchars)
						evflag[0] = 1;
					break;
				}
			}
		} else {
			funckey = XKeysymToString(key);
			if (funckey == NULL) return 0;
			if (!(strcmp(funckey, "Control_L") &&
				strcmp(funckey, "Alt_L") &&
				strcmp(funckey, "Shift_L") &&
				strcmp(funckey, "Caps_Lock")))
				return 0;
			keystringlen = strlen(funckey);
			if (keystringlen == 0) {
				printf("\n%s\n", "Keyboard map error");
				fflush(stdout);
			}
/*
 * If a function key is typed when input goes to dgtlnx, beep and ignore it.
 */
			if (dgtlnx_on && !Allow_func_keys) {
				printf("%s", beep);
				fflush(stdout);
				return 0;
			}
			keybuffer[keybufsize] = 0x1b;
			keybufsize++;
			for (index = 0; index < keystringlen; index++) {
				keybuffer[keybufsize] = funckey[index];
				keybufsize++;
			}
			keybuffer[keybufsize] = 0x1b;
			keybufsize++;
			evflag[0] = 1;
		}
	}
}

void drawtextvert(
	float *htxt,
	float *vtxt,
	int   xoffset,
	int   yoffset,
	char  *text,
	int   *n,
	int   fontflag /* 0 = mainfont, 1 = greekfont */
)
{
	int direction, ascent, descent;
	XCharStruct overall;
	XImage *ximage;
	int x, y;
	Pixmap tempix;
	int xco, yco;
	GC monogc;
	XGCValues gcv;
	XFontStruct *fontinfo;
	Font font;
	logical debugg;
	register unsigned char *byt_x, *byt_y;
	int ihex, index, ip;
	static int pix_mask[] = {1,2,4,8,16,32,64,128};
	int *workdw, nworkdw;
#ifndef where_it_not_for_OSF
	XTextItem text_item;
#endif
	if (fontflag == 0) {
		font = mainfont;
		fontinfo = mainfontinfo;
	} else {
		font = greekfont;
		fontinfo = greekfontinfo;
	}
	debugg = debug_mask[(int)('g'-'a')];
	XTextExtents(fontinfo, text, *n, &direction, &ascent, &descent, &overall);

	tempix = XCreatePixmap(disp, XtWindow(area[window_num]), overall.width,
			fontinfo->ascent + fontinfo->descent, 1);
	gcv.font = font;
	gcv.function = GXcopy;
	gcv.foreground = None;
	gcv.background = None;
	monogc = XCreateGC(disp, tempix,
		GCBackground|GCForeground|GCFunction|GCFont, &gcv);
	XFillRectangle(disp, tempix, monogc, 0, 0, overall.width,
		fontinfo->ascent + fontinfo->descent);

	gcv.foreground = 1;
	XChangeGC(disp, monogc, GCForeground, &gcv);
#ifdef where_it_not_for_OSF
	XDrawImageString(disp, tempix, monogc, 0, fontinfo->ascent, text, *n);
#else
	text_item.chars  = text;
	text_item.nchars = *n;
	text_item.delta  = 0;
	text_item.font   = None;
	XDrawText(disp, tempix, monogc, 0, fontinfo->ascent, &text_item, 1);
#endif /* Bug on OSF-1 server ? */

	ximage = XGetImage(disp, tempix, 0, 0, overall.width, ascent + descent,
		AllPlanes, ZPixmap);
	if (debugg) {
		byt_y = (unsigned char *)ximage->data;
		for (y = 0; y < fontinfo->ascent + fontinfo->descent; y++) {
			printf("\n y: %d\n", y);
			ip = 0;
			byt_x = byt_y;
			byt_y += ximage->bytes_per_line;
			for (x = 0; x < ximage->bytes_per_line; x++) {
				ihex = (unsigned int) *(byt_x++);
				printf(" %2x", ihex);
				ip += 3;
				if (ip > 78) {
					printf("\n");
					ip = 0;
				}
			}
		}
	}

	if (debugg) goto another_server;
	if (ximage->byte_order != LSBFirst) goto another_server;
	if (ximage->bitmap_bit_order != LSBFirst) goto another_server;
	if (ximage->bitmap_pad != 32) goto another_server;
/*
 * DEC-like server.
 */
	nworkdw = ximage->bytes_per_line * 8;
	workdw  = (int *) calloc(nworkdw, sizeof(int));
	byt_y = (unsigned char *)ximage->data;
	for (y = 0; y < fontinfo->ascent + fontinfo->descent; y++) {
		byt_x = byt_y;
		byt_y += ximage->bytes_per_line;
		ip = 0;
		for (x = 0; x < ximage->bytes_per_line; x++) {
			ihex = (unsigned int) *(byt_x++);
			for (index = 0; index < 8; index++) {
				workdw[ip + index] = ihex & pix_mask[index];
			}
			ip += 8;
		}
		if (ip > nworkdw) {
			printf("\nBeware: Temporary working space allocated too small in drawtextvert\n");
			fflush(stdout);
		}
		xco = (int) *htxt + y + xoffset;
		yco = pixdata[window_num].y - ((int) *vtxt + yoffset);
		for (x = 0; x < overall.width; x++) {  /* Loop in y-direction */
			if (workdw[x] != 0)
				XDrawPoint(disp, drawin, grcon, xco, yco);
			yco--;
		}
	}
	free(workdw);

	XFreePixmap(disp, tempix);
	(void) XDestroyImage(ximage);
	return;

another_server:
	for (y = 0; y < fontinfo->ascent + fontinfo->descent; y++) {
		for (x = 0; x < overall.width; x++) {
			if (XGetPixel(ximage, x, y) == 1) {
				xco = (int) *htxt + y + xoffset;
				yco = pixdata[window_num].y - ((int) *vtxt + x + yoffset);
				XDrawPoint(disp, drawin, grcon, xco, yco);
			}
		}
	}

	XFreePixmap(disp, tempix);
	(void) XDestroyImage(ximage);
}

void dgifoc(
	int *winkind,
	int *winno
)
/*
 * Set input focus to winkind,winno
 * winkind = 0 for DALI window, 1 for terminal window and 2 for help window.
 */
{
	int winnext;

	Focus_Time = CurrentTime;
	switch (*winkind) {
		case 1:
/*
 * Terminal window.
 */
#ifdef VMS
		if (!debug_mask[(int)('i'-'a')]) {
				SYS$CANCEL(QIO_chan);
				QIO_Status = SYS$CLREF(QIO_event_flag);
			}
#endif /* VMS */
			XSetInputFocus(disp, terminal_window, RevertToNone, Focus_Time);
/*			XLowerWindow(disp, XtWindow(toplevel[window_num])); */
			XRaiseWindow(disp, terminal_window);
			XFlush(disp);
			break;
		case 2:
/*
 * Help window.
 */
			XSetInputFocus(disp, XtWindow(tophelp), RevertToNone, Focus_Time);
			XRaiseWindow(disp, XtWindow(tophelp));
			XFlush(disp);
			break;

		case 0:
		default:
/*
 * A DALI window.
 */
			winnext = (*winno >=0) ? *winno : window_num;
			XSetInputFocus(disp, XtWindow(toplevel[winnext]), RevertToNone,
				Focus_Time);
			XRaiseWindow(disp, XtWindow(toplevel[winnext]));
			XFlush(disp);
#ifdef VMS
			if (!debug_mask[(int)('i'-'a')])
				Init_QIO_Read();
#endif /* VMS */
			break;
		}
	dgchkx();
}

void dgbtit()
{
/*
 * Set window title.
 */
	static char title[] = "ALEPH-XDALI  dd mmm yyyy  version A1    X11/XUIT            ";
	static int first_time = 0;
	int index;
	time_t timedate;
	char* timedatestr;
	static int timeidx[11] = { 8, 9, 10, 4, 5, 6, 7, 20, 21, 22, 23};

	if (first_time == 0) {
		timedate = time((time_t *) NULL);
		timedatestr = asctime(localtime(&timedate));
		for (index = 0; index < 11; index++)
			title[index+13] = timedatestr[timeidx[index]];
		first_time = 1;
	}
	title[34] = dcftvt.tfildc[5];
	title[35] = dcftvt.tfildc[6];
	if (duiscc.fpswdu) {
		title[58] = 'P';
		title[59] = 'S';
	} else {
		title[58] = ' ';
		title[59] = ' ';
	}
	for (index=sizeof(title)-1; index>0; index--) {
		if(title[index] == '\0') continue;
		if(title[index] != ' ') break;
		title[index] = '\0';
	}
	XStoreName(disp, XtWindow(toplevel[0]), title);
}

#ifdef VMS
void Init_QIO_Read()
{
/*
 * Clear the event flag and issue a QIO read call.
 */
	struct itm {
		short int bl;
		short int code;
		int ba;
		int rla;
	} itmlst[2];

	QIO_Status = SYS$CLREF(QIO_event_flag);

	itmlst[0].bl   = 0;
	itmlst[0].code = TRM$_ESCTRMOVR;
	itmlst[0].ba   = QIO_BUFSIZE - 1;
	itmlst[0].rla  = 0;

	itmlst[1].bl   = 0;
	itmlst[1].code = TRM$_MODIFIERS;
	itmlst[1].ba   = TRM$M_TM_ESCAPE | TRM$M_TM_NOECHO | TRM$M_TM_TRMNOECHO |
		TRM$M_TM_NOEDIT | TRM$M_TM_NOFILTR;
	itmlst[1].rla  = 0;

	QIO_Status = SYS$QIO(QIO_event_flag, QIO_chan, IO$_READVBLK | IO$M_EXTEND,
		QIO_iosb, 0, 0, QIO_buf, QIO_BUFSIZE, 0, 0, &itmlst, 24);
}

static void get_nonX_input(
	char *cd,
	int *s,
	XtInputId *id
)
/* In VMS s is the event flag */
{
/*
 * Handler for stdin input.
 */
	static char beep[] = {0x07, 0x07, 0x07, 0x00};
	unsigned char c1;
	unsigned short il, index, ic1;

/*
 * The request was cancelled by SYS$CANCEL. Do nothing.
 */
	if (QIO_iosb[0] == SS$_ABORT)
		return;
/*
 * CTRL-Y, try to exit completely.
 */
	if (QIO_iosb[0] == SS$_CONTROLY) exit(0);
/*
 * Make checks similar to what is done in DALI_QIO
 */
	if (QIO_iosb[0] == SS$_CONTROLC) goto Cont;
	if (QIO_iosb[0] == SS$_BADESCAPE) {
		printf("%s", beep);
		goto Cont;
	}
	if (QIO_iosb[0] != SS$_NORMAL) goto Cont;

	if ((QIO_iosb[1] == 0) && ((QIO_iosb[3] % 256) == 1)) {
		ic1 = QIO_iosb[2] - 0xff00;
		switch(ic1) {
			case 3: case 5: case 8: case 9: case 12: case 18: case 31:
/* Control characters handled as OOBs */
				dgooba(ic1);
				break;
			case 13: case 26: case 27:
/* CR, Ctrl-Z, ESC */
				keybuffer[keybufsize] = ic1; keybufsize++;
				keybuffer[keybufsize] = '\0';
				evflag[0] = 1;
				break;
			case 127:
/* DEL */
				if (reqnumchars > 1) {
					keybuffer[keybufsize] = '\0';
					if (keybufsize == 0) break;
					keybufsize--;
					printf("%s", delete_string);
					fflush(stdout);
				} else {
					keybuffer[keybufsize] = ic1; keybufsize++;
					keybuffer[keybufsize] = '\0';
					evflag[0] = 1;
				}
				break;
			default:
				printf ("\n^%c is not an accepted control character          ..:",
					ic1 + 64);
				break;
		}
		goto Cont;
	}

	if ((int) QIO_buf[0] == 27) {
/*
 * If a function key is typed when input goes to dgtlnx, beep and ignore it.
 */
		if (dgtlnx_on && !Allow_func_keys) {
			printf("%s", beep);
			fflush(stdout);
			goto Cont;
			}
		il = QIO_iosb[3] % 256;
		for (index = 0; index < il; index++) {
			keybuffer[keybufsize] = QIO_buf[index]; keybufsize++;
		}
		keybuffer[keybufsize] = 27;
		keybuffer[keybufsize + 1] = '\0';
		keybufsize++;
		evflag[0] = 1;
		goto Cont;
	}

	if (QIO_iosb[1] >= 1) {
		if (dmacrc.fntadm) {
/*
 * If type-ahead is disabled, beep on normal characters and return.
 */
			printf("%s", beep);
			fflush(stdout);
			goto Cont;
		}
		il = QIO_iosb[1];
		for (index = 0; index < il; index++) {
			ic1 = (int) QIO_buf[index];
			if ((ic1 > 31 && ic1 < 127) || (ic1 > 159 && ic1 < 255)) {
				if (convert_to_upper) {
					c1 = _toupper(QIO_buf[index]);
				} else {
					c1 = QIO_buf[index];
				}
			} else {
				if (ic1 < 32) {
					printf ("\n^%c is not an accepted control character          ..:",
						ic1 + 64);
				} else {
					printf ("\n^%c+128 is not an accepted control character          ..:",
						ic1 + 64);
				}
				goto Cont;
			}
			keybuffer[keybufsize] = c1;
			keybuffer[keybufsize + 1] = '\0';
			printf("%s", &keybuffer[keybufsize]);
			keybufsize++;
		}
		if (keybufsize >= reqnumchars)
			evflag[0] = 1;
	} else {
		printf("%s\nNon interpretable input.", beep);
		printf("\n                                                   :");
	}
Cont:
	Init_QIO_Read();
}
#else
static int tty_changed = 0;
/* #ifndef __hpux */

void
tty_end()
{
/*
 * Just a dummy to satisfy atexit() typing.
 */
	(void) tty_reset();
}
#ifdef __PREVIOUS
/* BSN type terminal I/O */
static struct sgttyb tty_mode; /* For saving tty mode */

int
tty_set()
{
/*
 * Put the terminal in CBREAK mode with ECHO off.
 */
	int fd = fileno(stdin);
	struct sgttyb    temp_mode;

	if (debug_mask[(int)('i'-'a')]) return 0;
	if (ioctl(fd, TIOCGETP, (char *) &temp_mode) < 0)
		return(-1);
	tty_mode = temp_mode;

	temp_mode.sg_flags |= CBREAK;        /* Set CBREAK mode */
	temp_mode.sg_flags &= ~(ECHO|XTABS); /* Turn echo off, do not expand tabs */
	if (ioctl(fd, TIOCSETP, (char *) &temp_mode) < 0)
		return(-1);

	tty_changed = 1;
	return(0);
}

int
tty_reset()
{
/*
 * Reset terminal to initial state.
 */
	int fd = fileno(stdin);

/* printf("\ntty_reset called, tty_changed=%d \n", tty_changed); */
	if (tty_changed == 0) return(0);
	if (ioctl(fd, TIOCSETP, (char *) &tty_mode) < 0)
		return(-1);

	tty_changed = 0;
	return(0);
}
#else /* termios */
static struct termios tty_mode; /* For saving tty mode */

int
tty_set()
{
/*
 * Put the terminal in canonical mode with ECHO off.
 */
	int fd = fileno(stdin);
	struct termios temp_mode;

	if (debug_mask[(int)('i'-'a')]) return(0);
	if (tcgetattr(fd, &temp_mode) < 0)
		return(-1);
	tty_mode = temp_mode;

	temp_mode.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL);
	temp_mode.c_oflag |=  (TAB3|OPOST|ONLCR);
	temp_mode.c_oflag &= ~(OCRNL|ONOCR|ONLRET);
#if defined(__hpux) || defined(__sgi)
	temp_mode.c_cc[VMIN]  = 4; /* Certainly return after 4 characters */
	temp_mode.c_cc[VTIME] = 3; /* Get all characters in 0.3 sec  */
	temp_mode.c_cc[VSUSP] = _POSIX_VDISABLE; /* Try to disable Ctrl-Z */
#else
	temp_mode.c_cc[VMIN]  = 1; /* Return after each character */
	temp_mode.c_cc[VTIME] = 2; /* Get all characters in 0.2 sec */
#endif
	if (tcsetattr(fd, TCSADRAIN, &temp_mode) < 0)
		return(-1);

	tty_changed = 1;
	return(0);
}

int
tty_reset()
{
/*
 * Reset terminal to initial state.
 */
	int fd = fileno(stdin);

/* printf("\ntty_reset called, tty_changed=%d \n", tty_changed); */
	if (tty_changed == 0) return(0);
	if (tcsetattr(fd, TCSADRAIN, &tty_mode) < 0)
		return(-1);

	tty_changed = 0;
	return(0);
}
#endif /* termios */

static void get_nonX_input(
	caddr_t cd,
	int *fid,
	XtInputId *id
)
{
/*
 * Handler for stdin input, UNIX version.
 */
#define BUFSIZE 80
	char buf[BUFSIZE];
	int nbytes;
	static char beep[] = {0x07, 0x07, 0x07, 0x00};
	unsigned char c1;
	unsigned short il, index, ic1;

	if((nbytes = read(fileno(stdin), buf, BUFSIZE)) == -1) {
		perror("get_nonX_input");
		return;
	}
/* printf("\nIn get_nonX_input nbytes: %d\n", nbytes); fflush(stdout); */
/*
 * CTRL-Y or CTRL-C, try to exit completely.
 */
	if (nbytes == 1 && ((int) buf[0] == 3 || (int) buf[0] == 25))
		exit(0);
/*
 * Make checks similar to what is done in DALI_QIO
 */
	if ((nbytes == 1) && (((unsigned int) buf[0] < 32) || ((unsigned int) buf[0] == 127))) {
		ic1 = buf[0];
		switch(ic1) {
			case 3: case 5: case 9: case 12: case 18: case 31:
/* Control characters handled as OOBs */
				dgooba(ic1);
				fflush(stdout);
				break;
			case 10: case 13: case 26: case 27:
/* CR (and LF), Ctrl-Z, ESC */
				if (ic1 == 10) ic1 = 13;
 				keybuffer[keybufsize] = ic1; keybufsize++;
 				keybuffer[keybufsize] = '\0';
				evflag[0] = 1;
				break;
			case 8: case 127:
/* BS, DEL */
				if (reqnumchars > 1) {
					keybuffer[keybufsize] = '\0';
					if (keybufsize == 0) break;
					keybufsize--;
					printf("%s", delete_string);
					fflush(stdout);
				} else {
					keybuffer[keybufsize] = ic1; keybufsize++;
					keybuffer[keybufsize] = '\0';
					evflag[0] = 1;
				}
				break;
			default:
				printf ("\n^%c is not an accepted control character          ..:",
				    ic1 + 64);
				fflush(stdout);
				break;
		}
		goto Cont;
	}

	if ((int) buf[0] == 27) {
/*
 * If a function key is typed when input goes to dgtlnx, beep and ignore it.
 */
		if (dgtlnx_on && !Allow_func_keys) {
			printf("%s", beep);
			fflush(stdout);
			goto Cont;
		}
		for (index = 0; index < nbytes; index++) {
			keybuffer[keybufsize] = buf[index]; keybufsize++;
		}
		keybuffer[keybufsize] = 27;
		keybuffer[keybufsize + 1] = '\0';
		keybufsize++;
		evflag[0] = 1;
		goto Cont;
	}

	if (nbytes >= 1) {
		if (dmacrc.fntadm) {
/*
 * If type-ahead is disabled, beep on normal characters and return.
 */
			printf("%s", beep);
			fflush(stdout);
			goto Cont;
		}
		for (index = 0; index < nbytes; index++) {
			ic1 = (unsigned int) buf[index];
			if ((ic1 > 31 && ic1 < 127) || (ic1 > 159 && ic1 < 255)) {
				if (convert_to_upper) {
					c1 = _toupper(buf[index]);
				} else {
					c1 = buf[index];
				}
			}
			keybuffer[keybufsize] = c1;
			keybuffer[keybufsize + 1] = '\0';
			printf("%s", &keybuffer[keybufsize]);
			fflush(stdout);
			keybufsize++;
		}
		if (keybufsize >= reqnumchars)
			evflag[0] = 1;
	} else {
		printf("%s\nNon interpretable input.", beep);
		printf("\n                                                   :");
		fflush(stdout);
	}
Cont:
	return;
}

#include <sys/times.h>
static struct tms tbuf0, tbuf1;
static struct timeval tp0, tp1;

void
dgtim0()
/*
 * Reset timer.
 */
{
	clock_t time0;
	int status;
	struct timezone tzp;

	time0 = times(&tbuf0);
	status = gettimeofday(&tp0, &tzp);
}

void
dgtim1()
/*
 * Show elapsed time.
 */
{
	clock_t time1;
	float CPUt, CPUu, CPUs;
	int status;
	struct timezone tzp;
	long TIME_UNIT = sysconf(_SC_CLK_TCK);

	time1 = times(&tbuf1);
	status = gettimeofday(&tp1, &tzp);
	CPUu = (float)((tbuf1.tms_utime + tbuf1.tms_cutime) -
		(tbuf0.tms_utime + tbuf0.tms_cutime)) / (float) TIME_UNIT;
	CPUs = (float)((tbuf1.tms_stime + tbuf1.tms_cstime) -
		(tbuf0.tms_stime + tbuf0.tms_cstime)) / (float) TIME_UNIT;
	CPUt = CPUs + CPUu;
	printf("\nTiming information. Clock: %.3f CPU: %.3f (%.3fu + %.3fs)",
		(float)(tp1.tv_sec-tp0.tv_sec) + (float)(tp1.tv_usec-tp0.tv_usec)*0.000001,
		CPUt, CPUu, CPUs);
	fflush(stdout);
}
#endif /* VMS */
#ifdef __linux
/*
 * A few interface routines for Linux. All the 'big' UNIXes have several
 * VMS Fortran routines in their libraries, but not Linux. Note that the
 * following emulations are not well tested yet. (BSN, 25-Nov-1995).
 */
void
sleep_(int *Itime)
{
	unsigned int secs;

	secs = *Itime;

	(void) sleep(secs);
}

float
ran_(int *iran)
{
/*
 * Emulate ran. This ran does not return a seed and therefore not be reset.
 */
	float ran4;

	ran4 = (float) drand48();
	return ran4;
}

void
date_(char *buf)
{
	time_t tnow = time (NULL);
	char *ts = ctime (&tnow);

	strcpy(buf, "dd-mmm-yy");
	strncpy(buf,     &ts[8],  2);
	strncpy(&buf[3], &ts[4],  3);
	strncpy(&buf[7], &ts[22], 2);
}

void
time_(char *buf)
{
	time_t tnow = time (NULL);
	char *ts = ctime (&tnow);

	strncpy(buf, &ts[11], 8);
}


void
idate_(
       int *i,
       int *j,
       int *k
)
{
  time_t tnow = time (NULL);
  struct tm *ts = localtime(&tnow);

  *i = ts->tm_mon + 1;
  *j = ts->tm_mday;
  *k = ts->tm_year;
}

float secnds_(float *tim0)
{
    clock_t time1;
    float CPUt, CPUu, CPUs;
    long TIME_UNIT = sysconf(_SC_CLK_TCK);
    struct tms tbuf1;

    time1 = times(&tbuf1);
    CPUu = (float)(tbuf1.tms_utime + tbuf1.tms_cutime)/(float) TIME_UNIT;
    CPUs = (float)(tbuf1.tms_stime + tbuf1.tms_cstime)/(float) TIME_UNIT;
    CPUt = CPUs + CPUu;
    return (CPUt - *tim0);
}
#endif /* Linux */
