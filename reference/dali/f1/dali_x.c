/*
 * This module contain routines ported from DALI_UIS.FOR to X and C
 * by Mark Parsons.
 * Later changes add additions by Bjorn S. Nilsson.
 * Code modifications for VMS on AXP from Jan. 1993.
 * Ultrix port was started in Feb. 1993.
 * OSF/1 port started in Jan. 1994
 * This version is now also for ATLANTIS. BSN, 5-May-1997
 * After 8-Oct-1997 the file can also be compiled with a C++ compiler.
 *
 * IY version from 13-Jan-1998
 * Help text is now positioned at the top of the window 14-June-1998
 */

#if defined(__hpux) && !defined(__unix__)
#define _HPUX_SOURCE
#define unix
#define __unix__
#endif /* HP */
#if defined(__unix) && !defined(__unix__)
#define __unix__
#endif /* For SGI */
#if defined(_AIX) && !defined(__unix__)
#define __unix__
#endif /* For AIX */
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#ifdef __VMS
#include <decw$include/Intrinsic.h>
#include <decw$include/StringDefs.h>
#include <decw$include/Shell.h>
#include <starlet.h>
#else
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#endif /* VMS */

#include <math.h>
#include "dali_defs.h"
#include "commons.h"
#include "ps_abbrevs.h"

/*
 * Global X variables and structures.
 */
Widget tophelp;

extern logical debug_mask[30];
extern Display *disp;
#define MAX_WINDOWS 5
extern Pixmap iconpix, winpix[MAX_WINDOWS];
extern Arg wargs[10];
extern Widget toplevel[MAX_WINDOWS], area[MAX_WINDOWS];
extern int window_num;
extern Drawable drawin;
extern GC grcon, grconclip, gc253, gc255;
extern XGCValues grcon_val, grconclip_val, gc253_val, gc255_val;
extern Colormap cmap;
extern Font font[2][2], mainfont, helpfont, greekfont, boldhelpfont,
  italhelpfont;
extern XComposeStatus compstat;
extern XtAppContext Dali_context;
extern Cursor cursor;
extern int evflag[8];
extern int winpix_copied;
extern char fontname_n[6][80], fontname_s[6][80];
extern Pixmapsize pixdata[MAX_WINDOWS];
extern logical Allow_func_keys;
extern logical isGray;
extern logical isColor;
extern int fgindex;
extern logical fiy;
/*
 * Static variables holding info private to DALI_X.
 */

#define XY_HLP_OFFSET	2
#define MAX_HLP_LINES	62
#define MAX_HLP_WIDTH   52
#ifdef __VMS
/*
 * Windows are positioned differently for mwm and fvwm.
 */
#define X_EXTRA_OFFSET 0
#define Y_EXTRA_OFFSET 0
#else
#define X_EXTRA_OFFSET 6
#define Y_EXTRA_OFFSET 112
#endif
/* Some of these are the contents of common    */
/* block HELPTX1 which is not used externally. */
static char ttx[MAX_HLP_LINES][MAX_HLP_WIDTH];
static char ttxa[MAX_HLP_LINES][MAX_HLP_WIDTH];
static int tcol[MAX_HLP_LINES];
static logical tbf[MAX_HLP_LINES];
static int tf[MAX_HLP_LINES];
static char t8l[MAX_HLP_LINES][9];
static int lhlength; /* Expected length of input help string */
static int pcs;      /* Position of command substring */
static XFontStruct *fontinfo;
static Widget helparea;
static GC gchelp[6][2];
static int evpar;
static int height, width;
static Cursor helpcursor;
static Pixmap help_icon_pix;
static int ltxt, bottom_line;
static logical ficond;
static int nat0, nat[MAX_HLP_LINES], line;
static int charheight, charwidth;
static int ymark, mark;
static int retx, rety;
static int param10, param11, param12;
static int popflag;
static logical lpast;
static int evpnt, evpex, evbut;
static int xybut[4];
static int help_lines = MAX_HLP_LINES;
static Font font_n[6], font_s[6];
static XFontStruct *fontinfo_n[6] = {NULL, NULL, NULL, NULL, NULL, NULL};
static XFontStruct *fontinfo_s[6] = {NULL, NULL, NULL, NULL, NULL, NULL};
static float font_scale[6];
static logical fix_cursor = False;
static Pixmap fix_cursor_pix;
static int fix_cursor_x, fix_cursor_y;
static logical fover = False;
static logical fnewhelp = False;
static int LastButton = -1, hlpline = -1;
#ifdef __VMS
static int TimeStat;
/* int sys$setimr(), sys$cantim(); */
static int TimeOutId = 0;
static int TimeOutDT[2] = {-10000000, -1};
#endif /* VMS, timeouts */

/*
 * Function prototypes, only those really required are prototyped.
 */

XtEventHandler redrawhelp(Widget w, caddr_t junk1, XEvent *event);
static logical IsPtrInDaliWin(int *x, int *y, unsigned int *button);
static logical IsPtrInHelpWin(int *x, int *y, unsigned int *button);
XtEventHandler dgpast(Widget w, int *evprm, XEvent *event);
XtEventHandler dgbast(Widget w, int *evprm, XEvent *event);
CXX void dgiast(int *ncase, int *ncurs);
CXX void dgeast(int *ncase, int *ncurs);
CXX XtEventHandler keyinput(Widget w, caddr_t junk, XEvent *cdata);
CXX void ucopy(float hco[], int ihco[], int *n);
CXX void dgchkx(), dgclev(int *clrflg), dgdraw(int *n, float *h, float *v);
CXX void dgetst(FortranString *ichstr, int *lchstr, int *nchset, int *icntrl
#ifdef __VMS
	);
#else
	, int l_ichstr);
#endif /* Unix */
CXX void dglevl(int *l), dgstev(int *setflg);
CXX void dgtlnx(int *maxlen, short *xiosb,
#ifdef __VMS
	FortranString *text);
#else
	FortranString *text,
	int l_text);
#endif /* Unix */
CXX void dqcl0(float *hlo, float *vlo, float *hhg, float *vhg, float *dcl);
CXX void dqclp(float *h1, float *v1, float *h2, float *v2, logical *fdpr);
CXX void dqhlp(FortranString tinp[]);
CXX void dqzrb(float *h0, float *v0, float *h2, float *v2,
	float *ph, float *pv, logical *fstop);
void mdgarea(int *n, float h[], float v[]);
void mdglevl(int *lev);
void mdgtxtm(int *level, int *itsiz, FortranString *text, float *sangl,
	float *hret, float *vret, float *height, int *nitems, logical text_s[],
#ifdef __VMS
	FortranString text_chars[],
#else
	FortranString *text_chars[],
#endif
	int text_nchars[]);
void setXcursor(char *data, int fg, int bg, int hsx, int hsy);

/*
 * Data for icon image.
 */
#define help_icon_width 50
#define help_icon_height 50

static unsigned char help_icon_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x3e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff,
   0x00, 0xe0, 0x00, 0x02, 0x00, 0xc0, 0xff, 0x01, 0xc0, 0x01, 0x01, 0x00,
   0xe0, 0x80, 0x03, 0x80, 0x83, 0x00, 0x00, 0x70, 0x00, 0x07, 0x00, 0x47,
   0x00, 0x00, 0x70, 0x00, 0x0e, 0x00, 0x2e, 0x00, 0x00, 0x38, 0x00, 0x0e,
   0x00, 0x14, 0x00, 0x00, 0x38, 0x00, 0x0e, 0x00, 0x3a, 0x00, 0x00, 0x38,
   0x00, 0x0e, 0x00, 0x71, 0x00, 0x00, 0x00, 0x00, 0x0e, 0x80, 0xe0, 0x00,
   0x00, 0x00, 0x00, 0x0e, 0x40, 0xc0, 0x01, 0x00, 0x00, 0x00, 0x07, 0x20,
   0x80, 0x03, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc0,
   0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x00, 0x00, 0x1f, 0x00, 0x00,
   0x00, 0x70, 0x00, 0x00, 0x21, 0x00, 0x00, 0x00, 0x38, 0x00, 0x00, 0x41,
   0x00, 0x00, 0x00, 0x1c, 0x00, 0x00, 0x41, 0x00, 0x00, 0x00, 0x1c, 0x00,
   0x00, 0x41, 0x00, 0x00, 0x00, 0x1c, 0x00, 0x00, 0x21, 0x00, 0x00, 0x00,
   0x1c, 0x00, 0x00, 0x1f, 0x00, 0x00, 0x00, 0x1c, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x1c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x14, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x14, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x22, 0x00, 0x00,
   0x00, 0x1c, 0x00, 0x00, 0x3e, 0x00, 0x00, 0x00, 0x3e, 0x00, 0x00, 0x41,
   0x00, 0x00, 0x00, 0x1c, 0x00, 0x00, 0x41, 0x00, 0x00, 0x00, 0x08, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x82, 0xfc, 0x09,
   0xf0, 0x01, 0x01, 0x00, 0x82, 0x04, 0x08, 0x10, 0x02, 0x7f, 0x00, 0x82,
   0x04, 0x08, 0x10, 0x02, 0x00, 0x00, 0x82, 0x04, 0x08, 0x10, 0x02, 0x00,
   0x00, 0x82, 0x04, 0x08, 0x10, 0x02, 0x7f, 0x00, 0xfe, 0x7c, 0x08, 0xf0,
   0x01, 0x08, 0x00, 0x82, 0x04, 0x08, 0x10, 0x00, 0x08, 0x00, 0x82, 0x04,
   0x08, 0x10, 0x00, 0x08, 0x00, 0x82, 0x04, 0x08, 0x10, 0x00, 0x08, 0x00,
   0x82, 0x04, 0x08, 0x10, 0x00, 0x08, 0x00, 0x82, 0xfc, 0xf9, 0x13, 0x00,
   0x7f, 0x00};

/*
 * Start of functions.
 */

CXX void dghlp(
	FortranString *tcom,
	FortranString *t,
	int *l,
#ifdef __VMS
	int *inv
#else
	int *inv,
	int l_tcom,
	int l_t
#endif
)
/*
 * Perform various operations on the HELP window.
 *
 * ltxt is the number of lines on the present page
 * mark is the (Fortran-like) index of a line to be high-lighted, 1...ltxt
 * ymark is the corresponding y position in X coordinates. The origin is
 * in the upper left corner of the help window.
 * fnewhelp is set True each time dghlp is entered with TX action and set
 * False after a DO action.
 */
{
	logical debugd;
	int index, screen_height;
	XGCValues gval;
	XColor fgcol, bgcol;
	Pixmap bitsmap, maskmap;
	int xco, yco;
	logical status;
	int fgind, i, itf;

	static unsigned char beep[] = {0x07, 0x07, 0x07, 0x00};
	static unsigned char help_bits[] = {
		0x00, 0x00, 0x00, 0x00, 0xc0, 0x03, 0xc0, 0x03,
		0xc0, 0x03, 0xc0, 0x03, 0xc0, 0x03, 0xc0, 0x03,
		0xf8, 0x1f, 0xf8, 0x1f, 0xf0, 0x0f, 0xe0, 0x07,
		0xc0, 0x03, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00
	};
	static unsigned char help_mask[] = {
		0x00, 0x00, 0xe0, 0x07, 0xe0, 0x07, 0xe0, 0x07,
		0xe0, 0x07, 0xe0, 0x07, 0xe0, 0x07, 0xfc, 0x3f,
		0xfc, 0x3f, 0xfc, 0x3f, 0xf8, 0x1f, 0xf0, 0x0f,
		0xe0, 0x07, 0xc0, 0x03, 0x80, 0x01, 0x00, 0x00
	};
	int hspotx = 8;
	int hspoty = 13;
	char tcomin[4], tin[MAX_HLP_WIDTH];

	debugd = debug_mask[(int)('d'-'a')];
#ifdef __VMS
	strncpy(tcomin, (char *) tcom->dsc$a_pointer, tcom->dsc$w_length);
	tcomin[tcom->dsc$w_length] = '\0';
	i = (t->dsc$w_length > MAX_HLP_WIDTH-1) ? MAX_HLP_WIDTH-1 : t->dsc$w_length-1;
	strncpy(tin, (char *) t->dsc$a_pointer, i);
	tin[i] = '\0';
#else
	strncpy(tcomin, (char *) tcom, l_tcom);
	i = (l_t > MAX_HLP_WIDTH-1) ? MAX_HLP_WIDTH-1 : l_t;
	strncpy(tin, (char *) t, i);
	tin[i] = '\0';
#endif /* VMS characters */

	if (!(strncmp(tcomin, "TX", 2) && strncmp(tcomin, "TI", 2))) {
		fnewhelp = True;
		if (duiscc.fnrzdu && ltxt == 0) {
			ltxt += 1;
#ifndef BOTTOM_HELP
			bottom_line = ltxt;
#endif
			nat[ltxt - 1] = 1;
			strcpy(t8l[ltxt - 1], "********");
			strncpy(ttx[ltxt - 1], " ", 1);
			strncpy(ttxa[ltxt - 1], " ", 1);
		}
		if ((tin[3] == 'c') || (tin[3] == 'C')) {
/*
 * A line with extra attributes. The text will be displayed in boldface.
 * The 3 characters starting in pos. pcs define the colour number.
 */
			if (tin[3] == 'C') tbf[ltxt - 1] = True;
			strncpy(ttxa[ltxt - 1], &tin[4], *l - 4);
/*			ttxa[ltxt - 1][0] = ttxa[ltxt - 1][1] = ttxa[ltxt - 1][2] = ' '; */
			i = pcs - 4;
/*			ttxa[ltxt - 1][0] = ttxa[ltxt - 1][1] = ttxa[ltxt - 1][2] = ' '; */
			ttxa[ltxt - 1][i] = ttxa[ltxt - 1][i+1] = ' ';
			sscanf(&tin[pcs], "%2d", &tcol[ltxt - 1]);
			for (i=0; i<(int)strlen(ttxa[ltxt - 1]); i++) {
				if (ttxa[ltxt - 1][i] != ' ') ttx[ltxt - 1][i] = ' ';
			}
			return;
		}
		if (ltxt < MAX_HLP_LINES) {
			ltxt += 1;
#ifndef BOTTOM_HELP
			bottom_line = ltxt;
#endif
			tcol[ltxt - 1] = -1;
			tbf[ltxt - 1] = False;
			if (*inv < 0) {
				nat[ltxt - 1] = 1;
			} else {
				if (*inv == 0) {
					nat[ltxt - 1] = nat0;
				} else {
					nat[ltxt - 1] = nat0;
					line = ltxt;
				}
			}
			if (*l<6) {
				strcpy(ttx[ltxt - 1], " ");
				strcpy(t8l[ltxt - 1], "********");
				fover = False;
				return;
			}
			strncpy(ttx[ltxt - 1], &tin[4], *l - 4);
			if (!strncmp(&tin[3], ".", 1)) {
				strncpy(t8l[ltxt - 1], &tin[pcs], 8);
				if (!strncmp(ttx[ltxt - 1], "?", 1))
					strncpy(ttx[ltxt - 1], "  ", 2);
			} else {
				if (!strncmp(&tin[3], "+", 1)) {
					strncpy(t8l[ltxt - 1], &tin[pcs], 2);
/*					strncpy(&ttx[ltxt - 1][pcs-4], "=>", 2); */
					strncpy(&ttx[ltxt - 1][pcs-4], " ", 2);
				} else {
					strcpy(t8l[ltxt - 1], "********");
				}
			}
			fover = False;
/*
 * Choose font (regular or italic)
 */
			tf[ltxt - 1] = 0;
			if(!strncmp(tcomin, "TI", 2)) tf[ltxt - 1] = 1;
		} else {
			fover = True;
		}
		return;
	}
	if (!strncmp(tcomin, "IN", 2)) {
/*
 * The 3 and 4th parameters are now the width of the input text
 * and the position of the command substring.
 */
		lhlength = *l;
/*
 * Temp. I think Hans calculated this wrong by one.
		pcs = *inv;
		pcs = *inv - 1;
 */
		pcs = *inv - 1;
		for (index = 0; index < MAX_HLP_LINES; index++) {
			strcpy(ttx[index],
			"                                                  ");
			strcpy(t8l[index], "        ");
			nat[index] = 0;
		}
		param10 = 10;
		param11 = 11;
		param12 = 12;
		nat0 = 3;
		popflag = -1;
		duiscc.fnrzdu = True;
		fontinfo = XQueryFont(disp, helpfont);
		charheight = fontinfo->max_bounds.ascent +
			fontinfo->max_bounds.descent;
		charwidth = fontinfo->max_bounds.width;
		help_lines = MAX_HLP_LINES;
		height = help_lines * charheight + XY_HLP_OFFSET;
		screen_height = XHeightOfScreen(XtScreen(toplevel[0]));
/* make it max. screen_height - two rows for top decorations */
		if (height > screen_height - 2 * charheight) {
			help_lines =
				(screen_height - 2 * charheight - XY_HLP_OFFSET) / charheight;
			height = help_lines * charheight + XY_HLP_OFFSET;
		}
		bottom_line = help_lines;
/*
 * Create the pixmap that is displayed when the help window is iconified.
 */
		help_icon_pix = XCreateBitmapFromData(disp, XDefaultRootWindow(disp),
			(char *)help_icon_bits, help_icon_width, help_icon_height);
/*		width = 44 * charwidth + XY_HLP_OFFSET; */
		width = (lhlength - 4) * charwidth + XY_HLP_OFFSET;
		index = 0;
		xco = XWidthOfScreen(XtScreen(toplevel[0])) - width - 4;
		yco = screen_height - height - 4;
		XtSetArg(wargs[index], XtNinput, True); index++;
		XtSetArg(wargs[index], XtNiconPixmap, help_icon_pix); index++;
		XtSetArg(wargs[index], XtNx, xco - X_EXTRA_OFFSET); index++;
		XtSetArg(wargs[index], XtNy, yco - Y_EXTRA_OFFSET); index++;
#ifdef ATLAS
		tophelp = XtCreatePopupShell("ATLANTIS help",
#else
		tophelp = XtCreatePopupShell("DALIhelp",
#endif
			topLevelShellWidgetClass, toplevel[0], wargs, index);
		fgind = duiscc.indvcm>16 ? ddefco.ltabdd[1+ddefcn.numcdd] : 8;
		index = 0;
		XtSetArg(wargs[index], XtNbackground,
			(unsigned long) ddefco.ltabdd[1]); index++;
		XtSetArg(wargs[index], XtNforeground, fgind); index++;
		XtSetArg(wargs[index], XtNheight, height); index++;
		XtSetArg(wargs[index], XtNwidth, width); index++;
		helparea = XtCreateManagedWidget("helparea", widgetClass,
			tophelp, wargs, index);
		XtAddEventHandler(tophelp, KeyPressMask, False,
			(XtEventHandler)keyinput, NULL);
		XtRealizeWidget(tophelp);
		if (duiscc.indvcm != 2)
			XSetWindowColormap(disp, XtWindow(tophelp), cmap);
/*
 * Create gc's using different fonts etc. [][0] use regular fonts
 * [][1] use italics.
 */
		gval.font = helpfont;
		gval.foreground = fgind;
		gval.background = (unsigned long) ddefco.ltabdd[1];
		gval.function = GXcopy;
		gchelp[0][0] = XCreateGC(disp, XtWindow(helparea),
			GCForeground|GCBackground|GCFunction|GCFont, &gval);
		gchelp[2][0] = XCreateGC(disp, XtWindow(helparea),
			GCForeground|GCBackground|GCFunction|GCFont, &gval);
		gchelp[3][0] = XCreateGC(disp, XtWindow(helparea),
			GCForeground|GCBackground|GCFunction|GCFont, &gval);
		gval.font = italhelpfont;
		gchelp[0][1] = XCreateGC(disp, XtWindow(helparea),
			GCForeground|GCBackground|GCFunction|GCFont, &gval);
		gchelp[2][1] = XCreateGC(disp, XtWindow(helparea),
			GCForeground|GCBackground|GCFunction|GCFont, &gval);
		gchelp[3][1] = XCreateGC(disp, XtWindow(helparea),
			GCForeground|GCBackground|GCFunction|GCFont, &gval);
		gval.font = boldhelpfont;
		gchelp[4][0] = XCreateGC(disp, XtWindow(helparea),
			GCForeground|GCBackground|GCFunction|GCFont, &gval);
		gval.font = italhelpfont;
		gchelp[4][1] = XCreateGC(disp, XtWindow(helparea),
			GCForeground|GCBackground|GCFunction|GCFont, &gval);
		gval.font = helpfont;
		gval.foreground = (unsigned long) ddefco.ltabdd[1];
		gval.background = fgind;
		gval.function = GXcopy;
		gchelp[1][0] = XCreateGC(disp, XtWindow(helparea),
			GCForeground|GCBackground|GCFunction|GCFont, &gval);
		gval.font = italhelpfont;
		gval.foreground = (unsigned long) ddefco.ltabdd[1];
		gval.background = fgind;
		gval.function = GXcopy;
		gchelp[1][1] = XCreateGC(disp, XtWindow(helparea),
			GCForeground|GCBackground|GCFunction|GCFont, &gval);
		gval.font = boldhelpfont;
		gchelp[5][0] = XCreateGC(disp, XtWindow(helparea),
			GCForeground|GCBackground|GCFunction|GCFont, &gval);
		gval.font = italhelpfont;
		gchelp[5][1] = XCreateGC(disp, XtWindow(helparea),
			GCForeground|GCBackground|GCFunction|GCFont, &gval);
		if (duiscc.indvcm != 2) {
			nat0 = 3;
		} else {
			nat0 = 0;
		}
		evpar = 1;
		XtAddEventHandler(helparea, EnterWindowMask|PointerMotionMask,
			False, (XtEventHandler)dgpast, &param10);
		XtAddEventHandler(helparea, LeaveWindowMask, False,
			(XtEventHandler)dgpast, &param11);
		XtAddEventHandler(helparea, ButtonPressMask, False,
			(XtEventHandler)dgbast, &param12);
		bitsmap = XCreatePixmapFromBitmapData(disp, XtWindow(helparea),
			(char *)help_bits, 16, 16, 1, 0, 1);
		maskmap = XCreatePixmapFromBitmapData(disp, XtWindow(helparea),
			(char *)help_mask, 16, 16, 1, 0, 1);
		fgcol.pixel = ddefco.ltabdd[12];
		bgcol.pixel = isColor ? ddefco.ltabdd[8] : ddefco.ltabdd[2];
		XQueryColor(disp, cmap, &fgcol);
		XQueryColor(disp, cmap, &bgcol);
		helpcursor = XCreatePixmapCursor(disp, bitsmap, maskmap,
			&fgcol, &bgcol, hspotx, hspoty);
		XDefineCursor(disp, XtWindow(helparea), helpcursor);
		XFreePixmap(disp, bitsmap);
		XFreePixmap(disp, maskmap);
		mark = 0;
		ymark = 0;
		ltxt = 0;
		ficond = False;
		XtAddEventHandler(helparea, ExposureMask, False,
			(XtEventHandler)redrawhelp, NULL);
		return;
	}
	if (!strncmp(tcomin, "DO", 2)) {
		hlpline = -1;
		if (ltxt <= 0)
			return;
		if (fover) {
			printf("\nMenu too long. Please inform H. Drevermann.        :");
			printf("%s", beep); fflush(stdout);
		}
		evpar = 0;
		if(debug_mask[(int)('h'-'a')])
			XRaiseWindow(disp, XtWindow(tophelp));
		xco = XY_HLP_OFFSET;
		yco = (bottom_line - ltxt + 1) * charheight - XY_HLP_OFFSET;
#ifdef OLD
		ficond = False;
#endif
		for (index = 1; index <= ltxt; index++) {
			itf = tf[index - 1];
			if (index != line) {
				if (nat[index - 1] == 1) {
					XDrawImageString(disp, XtWindow(helparea),
						gchelp[1][0], 0, yco, " ", 1);
					XDrawImageString(disp, XtWindow(helparea),
						gchelp[1][itf], xco, yco,
						ttx[index - 1], strlen(ttx[index - 1]));
					if (tcol[index - 1] >= 0)
						XDrawString(disp, XtWindow(helparea), gchelp[5][itf],
							xco, yco, ttxa[index - 1], strlen(ttxa[index - 1]));
				} else {
					XDrawString(disp, XtWindow(helparea),
						gchelp[nat[index - 1]][itf], xco, yco,
						ttx[index - 1], strlen(ttx[index - 1]));
					if (tcol[index - 1] >= 0) {
						gval.font = tbf[index - 1] ? boldhelpfont : helpfont;
						gval.foreground = (unsigned long) ddefco.ltabdd[tcol[index-1]];
						XChangeGC(disp, gchelp[4][itf], GCForeground|GCFont, &gval);
						XDrawString(disp, XtWindow(helparea), gchelp[4][itf],
							xco, yco, ttxa[index - 1], strlen(ttxa[index - 1]));
					}
				}
			} else {
				XDrawImageString(disp, XtWindow(helparea),
					gchelp[1][0], 0, yco, " ", 1);
				XDrawImageString(disp, XtWindow(helparea),
					gchelp[1][itf], xco, yco,
					ttx[index - 1], strlen(ttx[index - 1]));
				if (tcol[index - 1] >= 0) {
					XDrawString(disp, XtWindow(helparea), gchelp[5][itf],
						xco, yco, ttxa[index - 1], strlen(ttxa[index - 1]));
				}
				mark = index;
				ymark = yco;
				status = IsPtrInHelpWin(NULL, NULL, NULL);
				if (status && fnewhelp) {
					XWarpPointer(disp, None, XtWindow(helparea), 0, 0, 0, 0,
						22 * charwidth, (int)(yco - 0.5 * charheight));
				}
			}
			yco += charheight;
		}
		evpar = 1;
		if (line < 1 || line > ltxt) {
			mark = 0;
			ymark = 0;
			status = IsPtrInHelpWin(&retx, &rety, NULL);
			if (status) {
				int temp;

				index = ((charheight * bottom_line - rety) / charheight) + 1;
				temp = ltxt - index + 1;
				if (temp > 0 && temp <= ltxt) {
					if (strncmp(t8l[temp - 1], "*", 1)) {
						xco = XY_HLP_OFFSET;
						yco = (bottom_line - index + 1) * charheight - XY_HLP_OFFSET;
						mark = temp;
						ymark = yco;
						itf = tf[mark - 1];
						XDrawImageString(disp, XtWindow(helparea),
							gchelp[1][0], 0, yco, " ", 1);
						XDrawImageString(disp, XtWindow(helparea),
							gchelp[1][itf], xco, yco,
							ttx[mark - 1], strlen(ttx[mark - 1]));
						if (tcol[mark - 1] >= 0)
							XDrawString(disp, XtWindow(helparea), gchelp[5][itf],
								xco, yco, ttxa[mark - 1], strlen(ttxa[mark - 1]));
					}
				}
			}
		}
		fnewhelp = False;
		if (debugd) XFlush(disp);
		return;
	}
	if (!strncmp(tcomin, "ER", 2)) {
		ltxt = 0;
		mark = 0;
		ymark = 0;
		line = 0;
		for (index = 0; index < MAX_HLP_LINES; index++) {
			strcpy(ttx[index],
			"                                                  ");
			strcpy(ttxa[index],
			"                                                  ");
			strcpy(t8l[index], "        ");
			nat[index] = 0;
		}
		XClearWindow(disp, XtWindow(helparea));
		if (debugd) XFlush(disp);
		return;
	}
	if ((!strncmp(tcomin, "OF", 2)) && (!ficond)) {
		evpar = 0;
		ficond = True;
		XClearWindow(disp, XtWindow(helparea));
		XtPopdown(tophelp);
		evpar = 2;
		mark = -1;
		if (debugd) XFlush(disp);
		return;
	}
	if (!strncmp(tcomin, "ON", 2)) {
		evpar = 0;
#ifdef Old
		XtPopup(tophelp, XtGrabNone);
#else
		if(ficond) {
			ficond = False;
			XtPopup(tophelp, XtGrabNone);
		}
#endif
		XClearWindow(disp, XtWindow(helparea));
		mark = 0;
		return;
	}
	if (!strncmp(tcomin, "LI", 2)) {
		ltxt = ltxt - 1;
		strncpy(ttx[ltxt - 1],
			"*****************************************", lhlength-7);
		return;
	}
}

CXX void dghlp_line(
	int *line
)
/*
 * Return index of last help line clicked at.
 */
{
	*line = hlpline;
}

CXX void dghlev(
	int *fg,
	int *bg
)
/*
 * Change foreground and background text colors in helpwindow.
 */
{
	XGCValues gval;
	int tmp = 0, index, i;
	unsigned long fgi, bgi;
#ifdef __VMS
	static unsigned char doc[] = "DO";
	static unsigned char blc[] = " ";
	static FortranString do_d = {sizeof(doc)-1,0,0,doc};
	static FortranString bl_d = {sizeof(blc)-1,0,0,blc};
#else
	static FortranString do_d[] = "DO";
	static FortranString bl_d[] = " ";
#endif

	for (i = 0; i < 2; i++) {
		gval.foreground = fgi = (unsigned long) ddefco.ltabdd[*fg];
		gval.background = bgi = (unsigned long) ddefco.ltabdd[*bg];
		XChangeGC(disp, gchelp[0][i], GCForeground|GCBackground, &gval);
		XChangeGC(disp, gchelp[2][i], GCForeground|GCBackground, &gval);
		XChangeGC(disp, gchelp[3][i], GCForeground|GCBackground, &gval);
		XChangeGC(disp, gchelp[4][i], GCForeground|GCBackground, &gval);
		gval.foreground = bgi;
		gval.background = fgi;
		XChangeGC(disp, gchelp[1][i], GCBackground|GCBackground, &gval);
		XChangeGC(disp, gchelp[5][i], GCBackground|GCBackground, &gval);
	}

	index = 0;
	XtSetArg(wargs[index], XtNbackground, bgi); index++;
	XtSetArg(wargs[index], XtNforeground, fgi); index++;
	XtSetValues(helparea, wargs, index);
	XClearWindow(disp, XtWindow(helparea));
#ifdef __VMS
	dghlp(&do_d, &bl_d, &tmp, &tmp);
#else
	dghlp(do_d, bl_d, &tmp, &tmp, 2, 1);
#endif
}

CXX void dghmrk()
{
/*
 * We want the high-lighting to go away while the command invoked
 * from the Help menu is executing. This routine then ensures, that high-
 * lighting is switched on again if appropriate.
 */
	logical status, debugd;
	int index, temp;
	int xco, yco, itf;
	XGCValues gval;

	debugd = debug_mask[(int)('d'-'a')];
	status = IsPtrInHelpWin(&retx, &rety, NULL);
	if (!status) return;
	index = ((charheight * bottom_line - rety) / charheight) + 1;
	temp = ltxt - index + 1;
	if (temp > 0 && temp <= ltxt) {
		if (mark != temp) {
			if (mark != 0) {
				xco = XY_HLP_OFFSET;
				yco = ymark;
				XDrawImageString(disp, XtWindow(helparea),
					gchelp[2][0], 0, yco, " ", 1);
				itf = tf[mark - 1];
				XDrawImageString(disp, XtWindow(helparea),
					gchelp[2][itf], xco, yco,
					ttx[mark - 1], strlen(ttx[mark - 1]));
				if (tcol[mark - 1] >= 0) {
					gval.font = tbf[mark - 1] ? boldhelpfont : helpfont;
					gval.foreground = (unsigned long) ddefco.ltabdd[tcol[mark-1]];
					XChangeGC(disp, gchelp[4][itf], GCForeground|GCFont, &gval);
					XDrawString(disp, XtWindow(helparea), gchelp[4][itf],
						xco, yco, ttxa[mark - 1], strlen(ttxa[mark - 1]));
				}
			}
			if (debugd) XFlush(disp);
			if (strncmp(t8l[temp - 1], "*", 1)) {
				xco = XY_HLP_OFFSET;
				yco = (bottom_line - index + 1) *charheight - XY_HLP_OFFSET;
				mark = temp;
				ymark = yco;
				XDrawImageString(disp, 	XtWindow(helparea),
					gchelp[1][0], 0, yco,
					 " ", 1);
				itf = tf[mark - 1];
				XDrawImageString(disp, 	XtWindow(helparea),
					gchelp[1][itf], xco, yco,
					ttx[mark - 1], strlen(ttx[mark - 1]));
				if (tcol[mark - 1] >= 0)
					XDrawString(disp, XtWindow(helparea), gchelp[5][itf],
						xco, yco, ttxa[mark - 1], strlen(ttxa[mark - 1]));
			} else {
				ymark = 0;
				mark = 0;
			}
		}
	}
	if (debugd) XFlush(disp);
	return;
}

CXX void dgpop(
	FortranString *t
)
{
#ifdef __VMS
	if (!strcmp((char *) t->dsc$a_pointer, "POP")) {
#else
	if (!strcmp((char *) t, "POP")) {
#endif /* VMS characters */
		XRaiseWindow(disp, XtWindow(toplevel[window_num]));
		popflag = -1;
		return;
	}
#ifdef __VMS
	if (!strcmp((char *) t->dsc$a_pointer, "HPOP")) {
#else
	if (!strcmp((char *) t, "HPOP")) {
#endif /* VMS characters */
		XRaiseWindow(disp, XtWindow(tophelp));
		return;
	}
#ifdef __VMS
	if (!strcmp((char *) t->dsc$a_pointer, "PUSH")) {
#else
	if (!strcmp((char *) t, "PUSH")) {
#endif /* VMS characters */
		XLowerWindow(disp, XtWindow(toplevel[window_num]));
		popflag = 1;
		return;
	}
#ifdef __VMS
	if (!strcmp((char *) t->dsc$a_pointer, "FLIP")) {
#else
	if (!strcmp((char *) t, "FLIP")) {
#endif /* VMS characters */
		if (popflag == 1) {
			XRaiseWindow(disp, XtWindow(toplevel[window_num]));
		} else {
			XLowerWindow(disp, XtWindow(toplevel[window_num]));
		}
		popflag = -popflag;
		return;
	}
}

CXX void dgzoom(
	int *iact,
	int *iwin,
	float px[6],
	float py[6]
)
/*
 * Perform various box operations like rubberband, frames.
 */
{
	static float phw[50], pvw[50];
	float dw;
	static float dwi = 1.0;
	float x1, x2, y1, y2;
	logical fin, debugd;
	static logical frame = False;
	static logical frmac = False;
	int tmp1, tmp2, nw, index, lchstr;
#ifdef __VMS
	FortranString ichstr;
#else
	FortranString *ichstr;
	int l_ichstr;
#endif /* VMS characters */

	debugd = debug_mask[(int)('d'-'a')];
	switch (*iact) {

	case 0:
/*
 * Erase box.
 */
		if (!astcm1.fbox) return;
		if (*iwin > 0 && *iwin <= mnuwdw) {
			x1 = dgrdec.hmindg[dopr1c.iaredo];
			y1 = dgrdec.vmindg[dopr1c.iaredo];
			x2 = dgrdec.hhghdg[dopr1c.iaredo];
			y2 = dgrdec.vhghdg[dopr1c.iaredo];
			dqcl0(&x1, &y1, &x2, &y2, &dwi);
			for (index = 0; index < 4; index++) {
				x1 = astcm1.pxx[index];
				y1 = astcm1.pyy[index];
				x2 = astcm1.pxx[index + 1];
				y2 = astcm1.pyy[index + 1];
				dqclp(&x1, &y1, &x2, &y2, &fin);
				if (fin) goto l2;
			}
			return;
		}
l2:
		for (index = 0; index < 4; index++) {
			XDrawLine(disp, XtWindow(area[window_num]), gc255,
				(int) astcm1.pxx[index],
				fiy ? (int) astcm1.pyy[index] :
					(pixdata[window_num].y - (int) astcm1.pyy[index]),
				(int) astcm1.pxx[index + 1],
				fiy ? (int)astcm1.pyy[index + 1] :
					(pixdata[window_num].y - (int)astcm1.pyy[index + 1]));
		}
		astcm1.fbox = False;
		XFlush(disp);
		return;

	case 1:
/*
 * Make a rubberband. It's moving with M2 down and stopped when M2 is
 * released. Any other action should cancel the request.
 */
		astcm1.lowx = -99999.0;
		astcm1.lowy = -99999.0;
		tmp1 = 2;
		tmp2 = 1;
		dgiast(&tmp1, &tmp2);
#ifdef __VMS
		ichstr.dsc$a_pointer = (unsigned char *) calloc(4, sizeof(char));
		ichstr.dsc$w_length = 2;
		ichstr.dsc$b_dtype = 14;
		ichstr.dsc$b_class = 1;
		tmp1 = 1;
		tmp2 = 1;
		dgetst(&ichstr, &lchstr, &tmp1, &tmp2);
		free(ichstr.dsc$a_pointer);
#else
		ichstr = (FortranString *) calloc(4, sizeof(char));
		ichstr[0] = 32;
		ichstr[1] = 0;
		l_ichstr = 2;
		tmp1 = 1;
		tmp2 = 1;
		dgetst(ichstr, &lchstr, &tmp1, &tmp2, l_ichstr);
		free(ichstr);
#endif /* VMS characters */
		tmp1 = 1;
		tmp2 = 1;
		dgeast(&tmp1, &tmp2);
		if (debugd) {
			printf("\n dgzoom after dgeast");
			printf("\n astcm1.pxx: %f %f %f %f %f", astcm1.pxx[0],
				astcm1.pxx[1],astcm1.pxx[2],astcm1.pxx[3],astcm1.pxx[4]);
			printf("\n astcm1.pyy: %f %f %f %f %f\n", astcm1.pyy[0],
				astcm1.pyy[1],astcm1.pyy[2],astcm1.pyy[3],astcm1.pyy[4]);
		}
#ifdef __osf__
#ifndef OSF_has_no_bugs
/*
 * The next statement seems to cause some kind of buffer flush making
 * astcm1.pxx and pyy getting their right values locally. Without this
 * we will always return with astcm1.butsta = -1
 * OSF/1 1.3A 28-Jan-1994
 */
 fflush(stdout);
#endif /* OSF cc bug */
#endif /* OSF/1 */
		if (astcm1.butsta == 3 && astcm1.fbox) {
			if ((fabs(astcm1.pxx[0] - astcm1.pxx[1]) +
				fabs(astcm1.pyy[0] - astcm1.pyy[1])) < 2.0) {
				astcm1.butsta = -1;
				return;
			}
			if ((fabs(astcm1.pxx[2] - astcm1.pxx[1]) +
				fabs(astcm1.pyy[2] - astcm1.pyy[1])) < 2.0) {
				astcm1.butsta = -1;
				return;
			}
			for (index = 0; index < 5; index++) {
				px[index] = astcm1.pxx[index];
				py[index] = astcm1.pyy[index];
			}
			astcm1.butsta = 0;
		} else {
			astcm1.butsta = -1;
		}
		if (debugd) XFlush(disp);
		return;

	case 2:
		if (!astcm1.fbox) return;
/*
 * If (Box On Screen) erase old box and draw box with given parameters.
 */

	case 3:
/*
 * Erase old box and draw box with given parameters.
 */
		if (astcm1.fbox) {
			for (index = 0; index < 4; index++) {
				XDrawLine(disp, XtWindow(area[window_num]), gc255,
					(int) astcm1.pxx[index],
					fiy ? (int) astcm1.pyy[index] :
						(pixdata[window_num].y - (int) astcm1.pyy[index]),
					(int) astcm1.pxx[index + 1],
					fiy ? (int)astcm1.pyy[index + 1] :
						(pixdata[window_num].y - (int)astcm1.pyy[index + 1]));
			}
		}
		for (index = 0; index < 4; index++) {
			XDrawLine(disp, XtWindow(area[window_num]), gc255,
				(int) px[index],
				fiy ? (int) py[index] :
					(pixdata[window_num].y - (int) py[index]),
				(int) px[index + 1],
				fiy ? (int) py[index + 1] :
					(pixdata[window_num].y - (int) py[index + 1]));
		}
		astcm1.fbox = True;
		for (index = 0; index < 5; index++) {
			astcm1.pxx[index] = px[index];
			astcm1.pyy[index] = py[index];
		}
		XFlush(disp);
		return;

	case 4:
/*
 * Draw old box.
 */
		if (!astcm1.fbox) {
			for (index = 0; index < 4; index++) {
				XDrawLine(disp, XtWindow(area[window_num]), gc255,
					(int) astcm1.pxx[index],
					fiy ? (int) astcm1.pyy[index] :
						(pixdata[window_num].y - (int) astcm1.pyy[index]),
					(int) astcm1.pxx[index + 1],
					fiy ? (int)astcm1.pyy[index + 1] :
						(pixdata[window_num].y - (int)astcm1.pyy[index + 1]));
			}
			astcm1.fbox = True;
		}
		XFlush(disp);
		return;

	case 5:
/*
 * Draw old box fix.
 */
		if (astcm1.fbox) {
			for (index = 0; index < 4; index++) {
				XDrawLine(disp, XtWindow(area[window_num]), gc255,
					(int) astcm1.pxx[index],
					fiy ? (int) astcm1.pyy[index] :
						(pixdata[window_num].y - (int) astcm1.pyy[index]),
					(int) astcm1.pxx[index + 1],
					fiy ? (int)astcm1.pyy[index + 1]:
						(pixdata[window_num].y - (int)astcm1.pyy[index + 1]));
			}
			astcm1.fbox = False;
		}
		tmp1 = 8;
		dglevl(&tmp1);
		tmp1 = 5;
		dgdraw(&tmp1, astcm1.pxx, astcm1.pyy);
		if (debugd) XFlush(disp);
		return;

	case 6:
/*
 * Clear and redraw bounding frame.
 */
		nw = (int) dusdac.frnldu;
		if (*iwin == 99) {
			if (!frame && frmac) {
				for (index = 0; index < (5 * nw - 1); index++) {
					XDrawLine(disp, drawin, gc255,
						(int) phw[index],
						fiy ? (int) pvw[index] :
							(pixdata[window_num].y - (int) pvw[index]),
						(int) phw[index + 1],
						fiy ? (int) pvw[index + 1] :
							(pixdata[window_num].y - (int) pvw[index + 1]));
				}
				frame = True;
			}
			return;
		}
		if (frame) {
			for (index = 0; index < (5 * nw - 1); index++) {
				XDrawLine(disp, drawin, gc255,
					(int) phw[index],
					fiy ? (int) pvw[index] :
						(pixdata[window_num].y - (int) pvw[index]),
					(int) phw[index + 1],
					fiy ? (int) pvw[index + 1] :
						(pixdata[window_num].y - (int) pvw[index + 1]));
			}
			XFlush(disp);
		}
		if (*iwin < 0 || *iwin > mnuwdw || dusdac.frnldu < 1.0) {
			frame = False;
			frmac = False;
			return;
		}
		dw = dwi;
		for (index = 0; index < (5 * nw - 1); index+=5) {
			phw[index]     = dgrdec.hmindg[*iwin] + dw;
			phw[index + 1] = dgrdec.hhghdg[*iwin] - dw;
			phw[index + 2] = phw[index + 1];
			phw[index + 3] = phw[index];
			phw[index + 4] = phw[index];
			pvw[index]     = dgrdec.vmindg[*iwin] + dw;
			pvw[index + 1] = pvw[index];
			pvw[index + 2] = dgrdec.vhghdg[*iwin] - dw;
			pvw[index + 3] = pvw[index + 2];
			pvw[index + 4] = pvw[index];
			dw = dw + dwi;
		}
		if (!dmacrc.fmacdm) {
			for (index = 0; index < (5 * nw - 1); index++) {
				XDrawLine(disp, drawin, gc255,
					(int) phw[index],
					fiy ? (int) pvw[index] :
						(pixdata[window_num].y - (int) pvw[index]),
					(int) phw[index + 1],
					fiy ? (int) pvw[index + 1] :
						(pixdata[window_num].y - (int) pvw[index + 1]));
			}
			frame = True;
			XFlush(disp);
		}
		frmac = True;
		return;
	}
}

CXX void dgiast(
	int *ncase,
	int *ncurs
)
/*
 * Change cursor and do other initialization for RB, PO and PI operations.
 */
{
	dxpntr.nxcudx = *ncase;
	XRaiseWindow(disp, XtWindow(toplevel[window_num]));
	if (dxpntr.fupodx[(*ncase - 1) + (*ncurs - 1) * mxcudx]) {
		int bg = isColor ? ddefco.ltabdd[8] : ddefco.ltabdd[2];

		setXcursor(dxpntr.poindx[*ncase - 1][*ncurs - 1],
			ddefco.ltabdd[12], bg,
			dxpntr.hsphdx[*ncase - 1], dxpntr.hspvdx[*ncase - 1]);
	} else {
		XUndefineCursor(disp, XtWindow(area[window_num]));
	}
	evpnt = (*ncase - 2) * 3 + 1;
	evpex = (*ncase - 2) * 3 + 2;
	evbut = (*ncase - 2) * 3 + 3;
	if (*ncase == 2) {
		XtAddEventHandler(area[window_num], EnterWindowMask|PointerMotionMask,
			False, (XtEventHandler)dgpast, &evpnt);
		XtAddEventHandler(area[window_num], LeaveWindowMask, False,
			(XtEventHandler)dgpast, &evpex);
		lpast = True;
	} else {
		lpast = False;
	}
	XtAddEventHandler(area[window_num], ButtonPressMask|ButtonReleaseMask,
		False, (XtEventHandler)dgbast, &evbut);
	astcm1.butsta = 1;
}

CXX void dgeast(
	int *ncase,
	int *ncurs
)
/*
 * Reset pointers and other post operation for RB, PI and PO operations.
 */
{
	dxpntr.nxcudx = *ncase;
	if (lpast) {
		XtRemoveEventHandler(area[window_num], EnterWindowMask|PointerMotionMask,
			False, (XtEventHandler)dgpast, &evpnt);
		XtRemoveEventHandler(area[window_num], LeaveWindowMask,
			False, (XtEventHandler)dgpast, &evpex);
		lpast = False;
	}
	XtRemoveEventHandler(area[window_num], ButtonPressMask|ButtonReleaseMask,
		False, (XtEventHandler)dgbast, &evbut);
	XRaiseWindow(disp, XtWindow(toplevel[window_num]));
	if (dxpntr.fupodx[(*ncase - 1) + (*ncurs - 1) * mxcudx]) {
		int bg = isColor ? ddefco.ltabdd[8] : ddefco.ltabdd[2];

		setXcursor(dxpntr.poindx[*ncase - 1][*ncurs - 1],
			ddefco.ltabdd[12], bg,
			dxpntr.hsphdx[*ncase - 1], dxpntr.hspvdx[*ncase - 1]);
	} else {
		XUndefineCursor(disp, XtWindow(area[window_num]));
	}
}

CXX void dgmbon(
	float *xmin,
	float *ymin,
	float *xmax,
	float *ymax
)
/*
 * Init mouse button call back to be used for top bar window.
 * xy defines the sensitive area, xy[0]<x<xy[1], xy[2]<y<xy[3].
 * This is sensitive to button down events only.
 * This routine could later be exanded for more button functions.
 */
{
	xybut[0] = (int)*xmin;
	xybut[1] = pixdata[0].y - (int)*ymax;
	xybut[2] = (int)*xmax;
	xybut[3] = pixdata[0].y - (int)*ymin;
	evbut = 2005;
	XtAddEventHandler(area[0], ButtonPressMask,
		False, (XtEventHandler)dgbast, &evbut);
}

CXX void dgmbof(
	float *xmin,
	float *ymin,
	float *xmax,
	float *ymax
)
/*
 * Remove mouse button call back to be used for top bar window.
 */
{
	evbut = 2005;
	XtRemoveEventHandler(area[0], ButtonPressMask,
		False, (XtEventHandler)dgbast, &evbut);
}

CXX void dgsppt(
	int *ncase,
	int *ncurs
)
/*
 * Change pointer pattern. Normally it is red on white. For case 4 it is white.
 */
{
	int fg = ddefco.ltabdd[12];

	if (*ncase == 4) fg = ddefco.ltabdd[8];
	dxpntr.nxcudx = *ncase;
	if (dxpntr.fupodx[(*ncase - 1) + (*ncurs - 1) * mxcudx]) {
		int bg = isColor ? ddefco.ltabdd[8] : ddefco.ltabdd[2];

		setXcursor(dxpntr.poindx[*ncase - 1][*ncurs - 1], fg, bg,
			dxpntr.hsphdx[*ncase - 1], dxpntr.hspvdx[*ncase - 1]);
	} else {
		XUndefineCursor(disp, XtWindow(area[window_num]));
	}
}

#ifdef __VMS
static void time_out_proc()
{
	dgchkx();
	if (TimeOutId != 0) {
		TimeOutId++;
		TimeStat = sys$setimr(0, TimeOutDT, time_out_proc, TimeOutId, 0);
	}
}
#endif /* VMS timeouts */

CXX void dgswpt(
	int *ncase
)
/*
 * Swap pointer pattern.
 */
{
	static int nswap = 0;
	int ncurs;
	int nc;

#ifndef Keep_our_lovely_Watch
	if ((*ncase == -5) || (*ncase == 5)) return;
#endif
#ifdef __VMS
/*
 * Introduce/remove a timed event that can update the screen.
 */
	if (*ncase == -5) {
		if (TimeOutId != 0) {
			TimeStat = sys$cantim(TimeOutId, 0);
		}
		TimeOutId = 0;
	} else if (*ncase == 5) {
		if (TimeOutId != 0) {
			TimeStat = sys$cantim(TimeOutId, 0);
		}
		TimeOutId = 1993;
		TimeStat = sys$setimr(0, TimeOutDT, time_out_proc, TimeOutId, 0);
	}
#endif /* VMS, timeouts */
	if (nswap == *ncase) return;
	nswap = *ncase;
	if (*ncase > 0) {
		nc = *ncase;
	} else {
		nc = dxpntr.nxcudx;
	}
	ncurs = 1;
	if (dxpntr.fupodx[(nc - 1) + (ncurs - 1) * mxcudx]) {
		int bg = isColor ? ddefco.ltabdd[8] : ddefco.ltabdd[2];

		setXcursor(dxpntr.poindx[nc - 1][ncurs - 1],
			ddefco.ltabdd[12], bg,
			dxpntr.hsphdx[nc - 1], dxpntr.hspvdx[nc - 1]);
	} else {
		XUndefineCursor(disp, XtWindow(area[window_num]));
	}
}

XtEventHandler dgpast(
	Widget w,
	int *evprm,
	XEvent *event
)
/*
 * Handle certain pointer movement events.
 */
{
	int retxx, retyy;
	float fretx, frety;
	logical status, fstop;
	unsigned int button;
	int index, temp;
	int xco, yco, itf;
	logical debugd;
	static char beep[] = {0x07, 0x07, 0x07, 0x00};
	XGCValues gval;

	debugd = debug_mask[(int)('d'-'a')];

	if (*evprm == 1) {
/*
 * Make a rubberband if M2 is down and pointer in DALI window.
 */
		if (astcm1.butsta != 2) return 0;
		status = IsPtrInDaliWin(&retxx, &retyy, &button);
		if (!status) return 0;
		if (button&Button2Mask != Button2Mask) return 0;
		if (astcm1.fbox) {
			for (index = 0; index < 4; index++) {
				XDrawLine(disp, XtWindow(area[window_num]), gc255,
					(int) astcm1.pxx[index],
					fiy ? (int) astcm1.pyy[index] :
						(pixdata[window_num].y - (int) astcm1.pyy[index]),
					(int) astcm1.pxx[index + 1],
					fiy ? (int) astcm1.pyy[index + 1] :
						(pixdata[window_num].y - (int) astcm1.pyy[index + 1]));
			}
			astcm1.fbox = False;
		}
		if (debugd) XFlush(disp);
		fstop = False;
		retyy = fiy ? retyy : (pixdata[window_num].y - retyy);
		fretx = (float) retxx;
		frety = (float) retyy;
		dqzrb(&astcm1.lowx, &astcm1.lowy, &fretx, &frety,
			astcm1.pxx, astcm1.pyy, &fstop);
		if (fstop) {
			astcm1.fbox = False;
			return 0;
		}
		for (index = 0; index < 4; index++) {
			XDrawLine(disp, XtWindow(area[window_num]), gc255,
				(int) astcm1.pxx[index],
				fiy ? (int) astcm1.pyy[index] :
					(pixdata[window_num].y - (int) astcm1.pyy[index]),
				(int) astcm1.pxx[index + 1],
				fiy ? (int) astcm1.pyy[index + 1] :
					(pixdata[window_num].y - (int) astcm1.pyy[index + 1]));
		}
		astcm1.fbox = True;
		if (debugd) XFlush(disp);
		return 0;
	}

	if (*evprm == 2) {
/*
 * Pointer is moved out of the DALI window. Beep and cancel.
 */
		if (astcm1.butsta != 2) return 0;
		printf("%s", beep); fflush(stdout);
		if (astcm1.fbox) {
			for (index = 0; index < 4; index++) {
				XDrawLine(disp, XtWindow(area[window_num]), gc255,
					(int) astcm1.pxx[index],
					fiy ? (int) astcm1.pyy[index] :
						(pixdata[window_num].y - (int) astcm1.pyy[index]),
					(int) astcm1.pxx[index + 1],
					fiy ? (int) astcm1.pyy[index + 1] :
						(pixdata[window_num].y - (int) astcm1.pyy[index + 1]));
			}
			astcm1.fbox = False;
		}
		index = 4;
		dgstev(&index);
		return 0;
	}

	if (*evprm == 20) {
/*
 * Moving markers or text in DALI window.
 */
		index = 6;
		dgstev(&index);
		return 0;
	}

	if (evpar == 0 || !(*evprm == 10 || *evprm == 11)) return 0;

	if (evpar == 1) {
/*
 * Pointer in HELP window.
 */
		status = IsPtrInHelpWin(&retx, &rety, NULL);
		if (!status) {
			if (mark != 0) {
				xco = XY_HLP_OFFSET;
				yco = ymark;
				XDrawImageString(disp, XtWindow(helparea),
					gchelp[2][0], 0, yco, " ", 1);
				itf = tf[mark - 1];
				XDrawImageString(disp, XtWindow(helparea),
					gchelp[2][itf], xco, yco,
					ttx[mark - 1], strlen(ttx[mark - 1]));
				if (tcol[mark - 1] >= 0) {
					gval.font = tbf[mark - 1] ? boldhelpfont : helpfont;
					gval.foreground = (unsigned long) ddefco.ltabdd[tcol[mark-1]];
					XChangeGC(disp, gchelp[4][itf], GCForeground|GCFont, &gval);
					XDrawString(disp, XtWindow(helparea), gchelp[4][itf],
						xco, yco, ttxa[mark - 1], strlen(ttxa[mark - 1]));
				}
			}
			ymark = 0;
			mark = 0;
			if (debugd) XFlush(disp);
			return 0;
		}
		index = ((charheight * bottom_line - rety) / charheight) + 1;
		temp = ltxt - index + 1;
		hlpline = temp;
		if (temp > 0 && temp <= ltxt) {
			if (mark != temp) {
				if (mark != 0) {
					xco = XY_HLP_OFFSET;
					yco = ymark;
					XDrawImageString(disp, XtWindow(helparea),
						gchelp[2][0], 0, yco, " ", 1);
					itf = tf[mark - 1];
					XDrawImageString(disp, XtWindow(helparea),
						gchelp[2][itf], xco, yco,
						ttx[mark - 1], strlen(ttx[mark - 1]));
					if (tcol[mark - 1] >= 0) {
						gval.font = tbf[mark - 1] ? boldhelpfont : helpfont;
						gval.foreground = (unsigned long) ddefco.ltabdd[tcol[mark-1]];
						XChangeGC(disp, gchelp[4][itf], GCForeground|GCFont, &gval);
						XDrawString(disp, XtWindow(helparea), gchelp[4][itf],
							xco, yco, ttxa[mark - 1], strlen(ttxa[mark - 1]));
					}
				}
				if (debugd) XFlush(disp);
				if (strncmp(t8l[temp - 1], "*", 1)) {
					xco = XY_HLP_OFFSET;
					yco = (bottom_line - index + 1) *charheight - XY_HLP_OFFSET;
					mark = temp;
					ymark = yco;
					XDrawImageString(disp, 	XtWindow(helparea),
						gchelp[1][0], 0, yco, " ", 1);
					itf = tf[mark - 1];
					XDrawImageString(disp, 	XtWindow(helparea),
						gchelp[1][itf], xco, yco,
						ttx[mark - 1], strlen(ttx[mark - 1]));
					if (tcol[mark - 1] >= 0)
						XDrawString(disp, XtWindow(helparea), gchelp[5][itf],
							xco, yco, ttxa[mark - 1], strlen(ttxa[mark - 1]));
				} else {
					ymark = 0;
					mark = 0;
				}
			}
		} else {
			if (mark != 0) {
					xco = XY_HLP_OFFSET;
					yco = ymark;
					XDrawImageString(disp, XtWindow(helparea),
						gchelp[2][0], 0, yco,
						" ", 1);
					itf = tf[mark - 1];
					XDrawImageString(disp, XtWindow(helparea),
						gchelp[2][itf], xco, yco,
						ttx[mark - 1], strlen(ttx[mark - 1]));
					if (tcol[mark - 1] >= 0) {
						gval.font = tbf[mark - 1] ? boldhelpfont : helpfont;
						gval.foreground = (unsigned long) ddefco.ltabdd[tcol[mark-1]];
						XChangeGC(disp, gchelp[4][itf], GCForeground|GCFont, &gval);
						XDrawString(disp, XtWindow(helparea), gchelp[4][itf],
							xco, yco, ttxa[mark - 1], strlen(ttxa[mark - 1]));
					}
					ymark = 0;
					mark = 0;
			}
		}
		if (debugd) XFlush(disp);
		return 0;
	}

	if (evpar == 2) {
		if (mark < 0) {
			status = IsPtrInHelpWin(&retx, &rety, NULL);
			if (!status) {
				XDrawString(disp, XtWindow(helparea),
					gchelp[2][0], 0, 57 * charheight,
				"Point here and click to switch HELP menu ON", 43);
			} else {
				XDrawString(disp, XtWindow(helparea),
					gchelp[1][0], 0, 57 * charheight,
				"Point here and click to switch HELP menu ON", 43);
			}
		}
	}
	return 0;
}

XtEventHandler dgbast(
	Widget w,
	int *evprm,
	XEvent *event
)
/*
 * Handling of button press and release.
 */
{
	int retxx, retyy;
	float fretx, frety;
	logical status, debugd, fstop;
	int temp, index, yco, xco, itf;
	unsigned int button;
	XButtonEvent *butevent;
	XGCValues gval;

	debugd = debug_mask[(int)('d'-'a')];
	if (*evprm == 3) {

/*
 * A mouse click in the DALI display window with RB on.
 *
 * Note: I use the pointer position and button pressed values included in the
 * event rather than those provided by IsPtr... because they are up to date
 * and the other ones may not be (eg. quick click and move would give wrong
 * values for x,y and possibly the button).
 */

		status = IsPtrInDaliWin(&retxx, &retyy, &button);
		if (!status) return 0;
		butevent = (XButtonEvent *) event;
		retxx = butevent->x;
		retyy = butevent->y;
		retyy = fiy ? retyy : (pixdata[window_num].y - retyy);
		if (butevent->button == Button1) {
			XRaiseWindow(disp, XtWindow(toplevel[window_num]));
			return 0;
		}
		if (butevent->button == Button2) {
			if (astcm1.butsta < 1) return 0;
			if (butevent->type == ButtonPress) {
/*
 * M2 down in DALI display.
 */
				if (astcm1.fbox) {
					for (index = 0; index < 4; index++) {
						XDrawLine(disp, XtWindow(area[window_num]),	gc255,
							(int) astcm1.pxx[index],
							fiy ? (int) astcm1.pyy[index] :
								(pixdata[window_num].y - (int) astcm1.pyy[index]),
							(int) astcm1.pxx[index + 1],
							fiy ? (int) astcm1.pyy[index + 1] :
								(pixdata[window_num].y - (int) astcm1.pyy[index + 1]));
					}
					astcm1.fbox = False;
				}
				if (debugd) XFlush(disp);
				astcm1.lowx = (float) retxx;
				astcm1.lowy = (float) retyy;
				for (index = 0; index < 5; index++) {
					astcm1.pxx[index] = (float) retxx;
					astcm1.pyy[index] = (float) retyy;
				}
				fstop = False;
				fretx = (float) retxx;
				frety = (float) retyy;
				dqzrb(&(astcm1.lowx), &(astcm1.lowy),
					&fretx, &frety, astcm1.pxx,
					astcm1.pyy, &fstop);
				if (fstop) {
					index = 4;
					dgstev(&index);
					return 0;
				}
				astcm1.butsta = 2;
				if (dxpntr.fupodx[1 + mxcudx]) {
					int bg = isColor ? ddefco.ltabdd[8] : ddefco.ltabdd[2];

					setXcursor(dxpntr.poindx[1][1],
					ddefco.ltabdd[12], bg,
					dxpntr.hsphdx[1], dxpntr.hspvdx[1]);
				} else {
					XUndefineCursor(disp, XtWindow(area[window_num]));
				}
			} else {
/*
 * M2 release, still in DALI display.
 */
				if (astcm1.fbox) {
					for (index = 0; index < 4; index++) {
						XDrawLine(disp, XtWindow(area[window_num]),	gc255,
							(int) astcm1.pxx[index],
							fiy ? (int) astcm1.pyy[index] :
								(pixdata[window_num].y - (int) astcm1.pyy[index]),
							(int) astcm1.pxx[index + 1],
							fiy ? (int) astcm1.pyy[index + 1] :
								(pixdata[window_num].y - (int) astcm1.pyy[index + 1]));
					}
					astcm1.fbox = False;
				}
				if (debugd) XFlush(disp);
				fstop = False;
				fretx = (float) retxx;
				frety = (float) retyy;
				dqzrb(&(astcm1.lowx), &(astcm1.lowy), &fretx,
					&frety, astcm1.pxx, astcm1.pyy, &fstop);
				if (fstop) {
					astcm1.fbox = False;
					return 0;
				}
				for (index = 0; index < 4; index++) {
					XDrawLine(disp, XtWindow(area[window_num]), gc255,
						(int) astcm1.pxx[index],
						fiy ? (int) astcm1.pyy[index] :
							(pixdata[window_num].y - (int) astcm1.pyy[index]),
						(int) astcm1.pxx[index + 1],
						fiy ? (int) astcm1.pyy[index + 1] :
							(pixdata[window_num].y - (int) astcm1.pyy[index + 1]));
				}
				if (debugd) XFlush(disp);
				astcm1.fbox = True;
				astcm1.butsta = 3;
				index = 4;
				dgstev(&index);
			}
			return 0;
		}
		return 0;
	}

	if ((*evprm == 6) || (*evprm == 9)) {
/*
 * Buttonpress in DALI display with PI or PO on.
 */
		status = IsPtrInDaliWin(&retxx, &retyy, &button);
		if (!status) return 0;
		butevent = (XButtonEvent *) event;
		retxx = butevent->x;
		retyy = butevent->y;
		retyy = fiy ? retyy : (pixdata[window_num].y - retyy);
		if (butevent->button == Button1) {
/*
 * Button 1. Just pop up DALI window.
 */
			XRaiseWindow(disp, XtWindow(toplevel[window_num]));
		} else if (butevent->button == Button2) {
			if (butevent->type == ButtonPress) {
/*
 * Button 2 down click. Store pointer position.
 */
				astcm1.hpopos = retxx;
				astcm1.vpopos = retyy;
				astcm1.butsta = 2;
			} else {
/*
 * Button 2 is released. Set event flag.
 */
				astcm1.butsta = 3;
				index = 4;
				dgstev(&index);
			}
		}
		return 0;
	}

	if (*evprm == 20) {
/*
 * Buttonpress when moving text or markers should trigger like a pointer move.
 */
		index = 6;
		dgstev(&index);
		return 0;
	}
	if (*evprm == 2005) {
/*
 * Buttonpress in main window. Check if it is within the area defined
 * as sensitive.
 */
		butevent = (XButtonEvent *) event;
		retxx = butevent->x;
		retyy = butevent->y;
		if ((retxx < xybut[0]) || (retxx > xybut[2]) ||
			(retyy < xybut[1]) || (retyy > xybut[3])) return 0;
/* printf("\n xy (%d,%d) was good for top bar button events\n",retxx,retyy); */
/* fflush(stdout); */
		evflag[7] = 1;
		return 0;
		}
	if (evpar != 2) {
/*
 * Button actions in help window. Only M1 down events are interesting.
 * From 20-Jun-1996 we permit all buttons. Save button number in LastButton
 */
		if (cmflag.ton == 'N' || ltxt == 0) return 0;
		butevent = (XButtonEvent *) event;
		if (butevent->type == ButtonRelease) return 0;
#ifdef Old
		if ((butevent->button != Button1) || mark == 0) return 0;
#else
		if (mark == 0) return 0;
		LastButton = butevent->button;
#endif
		if (mark > 0) {
			xco = XY_HLP_OFFSET;
			yco = ymark;
			XDrawImageString(disp, XtWindow(helparea), gchelp[0][0],
				0, yco, " ", 1);
			itf = tf[mark - 1];
			XDrawImageString(disp, XtWindow(helparea), gchelp[0][itf],
				xco, yco, ttx[mark - 1], strlen(ttx[mark - 1]));
			if (tcol[mark - 1] >= 0) {
				gval.font = tbf[mark - 1] ? boldhelpfont : helpfont;
				gval.foreground = (unsigned long) ddefco.ltabdd[tcol[mark-1]];
				XChangeGC(disp, gchelp[4][itf], GCForeground|GCFont, &gval);
				XDrawString(disp, XtWindow(helparea), gchelp[4][itf],
					xco, yco, ttxa[mark - 1], strlen(ttxa[mark - 1]));
			}
		}
		mark = 0;
		ymark = 0;
		index = butevent->y;
		index = ((charheight * bottom_line - index) / charheight) + 1;
		temp = ltxt - index;
		if (temp <= 0 || temp > ltxt) return 0;
		strncpy(cmflag.commnd, t8l[temp], 8);
		index = 2;
		dgstev(&index);
		if (debugd) XFlush(disp);
		return 0;
	}
	return 0;
}

CXX void dglbut(
	int *nbut
)
/*
 * Return number of last button pressed in help window, if any.
 */
{
	*nbut = LastButton;
}

CXX void dgsbut(
	int *nbut
)
/*
 * Reset the variable holding the number of last button pressed.
 */
{
	LastButton = *nbut;
}

CXX void dguxcu(
	int *ncase,
	int *ncurs
)
/*
 * Remove a previously fixed cursor pattern.
 */
{
	if (!fix_cursor) return;
	XCopyArea(disp, fix_cursor_pix, drawin, gc255, 0, 0, 15, 15,
		fix_cursor_x, fix_cursor_y);
	XFreePixmap(disp, fix_cursor_pix);
	fix_cursor = False;
}

CXX void dgfxcu(
	int *ncase,
	int *ncurs
)
/*
 * Write the cursor pattern for case NCASE at the present cursor position.
 */
{
	int win_x, win_y;
	unsigned int keys_buttons;

	if (fix_cursor) dguxcu(ncase, ncurs);

	if (!IsPtrInDaliWin(&win_x, &win_y, &keys_buttons)) return;
	fix_cursor_pix = XCreatePixmapFromBitmapData(disp, drawin,
		dxpntr.poindx[*ncase-1][*ncurs-1], 16, 16,
		fgindex, 0,
		XDefaultDepthOfScreen(XtScreen(area[0])));
	fix_cursor_x = win_x - dxpntr.hsphdx[*ncase-1];
	fix_cursor_y = win_y - dxpntr.hspvdx[*ncase-1];
	XCopyArea(disp, fix_cursor_pix, drawin, gc255, 0, 0, 15, 15,
		fix_cursor_x, fix_cursor_y);
	fix_cursor = True;
}

void dgtxt_com(
	int *level,
	int *itsiz,
	FortranString *text,
	float *sangl,
	float *hret,
	float *vret,
#ifndef __unix__
	int fix_nofix
#else
	int fix_nofix,
	int *l_text
#endif /* unix */
)
/*
 * Main DALI text routine.
 * For fix_nofix=0, use text as the pointer. Start at (hret,vret). End
 * coordinates are returned in (hret,vret). Coordinates refer to the
 * leftmost point of the baseline.
 * For fix_nofix=1, draw text at (hret,vret) with colour specified by level.
 * Text characteristica are size (itsiz) and slanting angle (sangl).
 *
 * Bjorn S. Nilsson, 5-Nov-1991.
 */
{
	logical debugf, debugg;
	register unsigned char *byt_x, *byt_y;
	char *textptr, *cp, *cpf, tempc[101];
	char escaped_chars[20];
	static unsigned char f_char;
#ifdef __VMS
	static unsigned char hlp[] = "<<";
	static unsigned char pop[] = "POP";
	static FortranString f_char_d = {sizeof(f_char),0,0,&f_char};
	static FortranString hlp_d = {sizeof(hlp)-1,0,0,hlp};
	static FortranString pop_d = {sizeof(pop)-1,0,0,pop};
	short *l_text;
#else
	int l_f_char = sizeof(f_char);
	static FortranString hlp_d[] = "<<";
	static FortranString pop_d[] = "POP";
#endif /* VMS characters */
	double radarg;
	float scos, ssin;
	int num_fonts, index, index1, nitems, nchars, direction, ascent, descent;
	int x_hot, y_hot, x_rot, y_rot, win_x, win_y, win_xp, win_yp;
	int add_x, add_y;
	int see_text, disp_on, ichar_esc, ihex;
	int ip, ichar, xsize, ysize, xco, yco, quadrant, maxlen = 1;
	static int pix_mask[] = {1,2,4,8,16,32,64,128};
	short iosb[4];
	unsigned int keys_buttons;
	logical decoded, ptr_in_window;
	Cursor zero_cursor;
	GC textgc;
	Pixmap text_pix, zero_pix;
	XCharStruct overall;
	XColor fgcol, bgcol;
	XImage *ximage;
	int x, y, y1;
	XGCValues textgc_val;
	XTextItem text_items[20];
	logical text_s[20];
	int text_nchars[20];
	int *workdw, nworkdw;
#ifdef __VMS
	FortranString text_chars[20];
#else
	FortranString *text_chars[20];
#endif /* VMS */

#include "symbol_font.h"
#include "aleph_symbol.h"
	XPoint *poly_points;
	float *outl_x, *outl_y;

	char **fontlist_n, **fontlist_s;
	static char pntzer_bits[] = {
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	};
	int hsp0h=7, hsp0v=7;
	int evpnt = 20, evbut = 20;
	static char back_space[] = {0x08, 0x00};

	debugf = debug_mask[(int)('f'-'a')];
	debugg = debug_mask[(int)('g'-'a')];
/*
 * Determine text length, load fonts.
 */
#ifdef __VMS
	textptr = (char *) text->dsc$a_pointer;
	l_text = (short *)&(text->dsc$w_length);
#else
	textptr = (char *) text;
#endif /* VMS characters */
	cp = NULL;
	nchars = 0;
	decoded = False;
	for (index = 0; index < *l_text; index++) {
		if (textptr[index] != ' ') nchars = index + 1;
		if (textptr[index] == '\\') cp = &textptr[index];
	}
	if (nchars <= 0) {
		printf("\nEmpty textstring, no action taken."); fflush(stdout);
		return;
	}
	if ((*itsiz > 6) || (*itsiz < 1)) {
		printf("\nFonts of size %d are not defined", *itsiz);
		fflush(stdout);
		return;
	}
/*
 * Check if the symbol is to be generated from outlines. This applies
 * only to a large Aleph so far.
 */
	if (strncmp(textptr, "\\ALEPH", 6) == 0) {
		radarg = (double) ((*sangl) * 3.14159 / 180.);
		scos = cos(radarg);
		ssin = sin(radarg);
		xsize = (int)((float)*itsiz * (fabs(hamax * scos) + fabs(vamax * ssin)));
		ysize = (int)((float)*itsiz * (fabs(vamax * scos) + fabs(hamax * ssin)));
		quadrant = ((int)(*sangl/90.)) % 4;
		switch (quadrant) {
			case 0:
			default:
				x_rot = (int)((float)(*itsiz) * fabs(vamax * ssin));
				y_rot = ysize;
				break;
			case 1:
				x_rot = xsize;
				y_rot = ysize - (int)((float)(*itsiz) * fabs(vamax * scos));
				break;
			case 2:
				x_rot = (int)((float)(*itsiz) * fabs(hamax * scos));
				y_rot = 0;
				break;
			case 3:
				x_rot = 0;
				y_rot = ysize - (int)((float)(*itsiz) * fabs(hamax * ssin));
				break;
		}
		x_hot = xsize/2;
		y_hot = ysize/2;
		text_pix = XCreatePixmap(disp, XtWindow(area[window_num]), xsize, ysize,
			XDefaultDepthOfScreen(XtScreen(area[window_num])));
		textgc_val.function = GXcopy;
		textgc_val.foreground = None;
		textgc_val.background = None;
		textgc = XCreateGC(disp, text_pix,
			GCBackground|GCForeground|GCFunction, &textgc_val);
		XFillRectangle(disp, text_pix, textgc, 0, 0, xsize, ysize);
		textgc_val.foreground = fgindex;
		textgc_val.background = None;
		XChangeGC(disp, textgc, GCForeground|GCBackground, &textgc_val);
		poly_points = (XPoint *) calloc(sizeof(XPoint), n_aleph_points);
		for (x = 0; x < n_aleph_points; x++) {
			poly_points[x].x = (short)(x_rot + (*itsiz) *
				(scos * aleph_points[x].x - ssin * aleph_points[x].y));
			poly_points[x].y = (short)(y_rot - (*itsiz) *
				(ssin * aleph_points[x].x + scos * aleph_points[x].y));
		}
		if (fix_nofix == 1) goto fixit_outline;
		XFillPolygon(disp, text_pix, textgc, poly_points, n_aleph_points,
			Complex, CoordModeOrigin);
		free (poly_points);
		goto after_rotation;
/*
 * End special Aleph character code.
 */
	}
	if (fontinfo_n[*itsiz-1] != NULL) goto font_n_loaded;
	fontlist_n = XListFonts(disp, fontname_n[*itsiz-1], 1, &num_fonts);
	if (fontlist_n == NULL) {
		printf("\nNo such font %s could be loaded.\n", fontname_n[*itsiz-1]);
		printf("If you are using a high-resolution (>75 dpi) display on a Unix machine, you\n");
		printf("may have to load the 75 dpi fonts. On Digital UNIX this can be done by\n");
		printf("  %% xset fp+ /usr/lib/X11/fonts/decwin/75dpi/\n");
		printf("For other systems, check with your systems manager.");
		fflush(stdout);
		return;
	}
	fontinfo_n[*itsiz-1] = XLoadQueryFont(disp, *fontlist_n);
	if (fontinfo_n[*itsiz-1] == NULL) {
		printf("\nUnsuccesful load of font %s.", *fontlist_n);
		fflush(stdout);
		return;
	} else {
		font_n[*itsiz-1] = fontinfo_n[*itsiz-1]->fid;
		if (debugf) {
			printf("\nFont #%d chosen as %s.", *itsiz, *fontlist_n);
			fflush(stdout);
		}
		font_scale[*itsiz-1] = (float) (-1);
		cpf = (char *)strstr(*fontlist_n, "--");
		if (cpf != NULL) {
			cpf++; cpf++;
			strncpy(tempc, cpf, 2);
			if ((tempc[0] < '0') || (tempc[0] > '9')) goto after_fscale;
			if ((tempc[1] < '0') || (tempc[1] > '9')) tempc[1] = '\0';
			tempc[2] = '\0';
			sscanf(tempc, "%d", &index);
			font_scale[*itsiz-1] = (float) index;
		}
	}
after_fscale:
	XFreeFontNames(fontlist_n);

font_n_loaded:
	if (cp == NULL) {
		nitems = 1;
		text_items[0].chars = textptr;
		text_items[0].nchars = nchars;
		text_items[0].delta = 0;
		text_items[0].font = None;
		text_s[0] = False;
		text_nchars[0] = nchars;
#ifdef __VMS
		text_chars[0].dsc$w_length = nchars;
		text_chars[0].dsc$a_pointer = (unsigned char *) textptr;
#else
		text_chars[0] = (FortranString *) textptr;
#endif /* VMS characters */
		goto text_scanned;
	}

	if (fontinfo_s[*itsiz-1] != NULL) goto font_s_loaded;
	fontlist_s = XListFonts(disp, fontname_s[*itsiz-1], 1, &num_fonts);
	if (fontlist_s == NULL) {
		printf("\nNo such font %s could be loaded.", fontname_s[*itsiz-1]);
		fflush(stdout);
		return;
	}
	fontinfo_s[*itsiz-1] = XLoadQueryFont(disp, *fontlist_s);
	if (fontinfo_s[*itsiz-1] == NULL) {
		printf("\nUnsuccesful load of font %s.", *fontlist_s);
		fflush(stdout);
		return;
	} else {
		font_s[*itsiz-1] = fontinfo_s[*itsiz-1]->fid;
		if (debugf) {
			printf("\nFont #%d chosen as %s.", *itsiz, *fontlist_s);
			fflush(stdout);
		}
	}
	XFreeFontNames(fontlist_s);

font_s_loaded:
/*
 * Decode text into normal and symbol font components.
 */
	index = 0; cp = textptr; nitems = 0; ichar = 0; ichar_esc = 0;
	decoded = True;
	do {
/*
 * Last character if normal font.
 */
		if (index == nchars - 1) {
			ichar++;
			index++;
			text_items[nitems].chars = cp;
			text_items[nitems].nchars = ichar;
			text_items[nitems].delta = 0;
			text_items[nitems].font = font_n[*itsiz-1];
			text_s[nitems] = False;
			text_nchars[nitems] = ichar;
#ifdef __VMS
			text_chars[nitems].dsc$w_length = ichar;
			text_chars[nitems].dsc$a_pointer = (unsigned char *) cp;
#else
			text_chars[nitems] = (FortranString *) cp;
#endif /* VMS characters */
			nitems++;
			break;
		}
/*
 * Normal character, continue.
 */
		if (textptr[index] != '\\') {
			ichar++;
			index++;
			continue;
		}
/*
 * An escape (backspace) was encountered. First output any buffered text.
 */
		if (ichar != 0) {
			text_items[nitems].chars = cp;
			text_items[nitems].nchars = ichar;
			text_items[nitems].delta = 0;
			text_items[nitems].font = font_n[*itsiz-1];
			text_s[nitems] = False;
			text_nchars[nitems] = ichar;
#ifdef __VMS
			text_chars[nitems].dsc$w_length = ichar;
			text_chars[nitems].dsc$a_pointer = (unsigned char *) cp;
#else
			text_chars[nitems] = (FortranString *) cp;
#endif /* VMS characters */
			nitems++;
			ichar = 0;
		}
/*
 * Find the escaped character's value and output.
 */
		index++;
		cp = &textptr[index];
		index1 = 0;
		for (; index < nchars ; index++) {
			if (textptr[index] == '\\') {
				index--;
				break;
			}
			if (textptr[index] == ' ') break;
			index1++;
		}
		index++;
		if (index1 == 1) {
/*
 * An one-letter abbreviation. Use the same character value in the symbol font.
 */
			ichar = (int) *cp;
		} else {
			strncpy(tempc, cp, index1);
			tempc[index1] = '\0';
/*
 * A keyword of index1 characters has been found. Try to find it in our table.
 */
			ichar = -1;
			for (index1 = 0; index1 < 255; index1++) {
				if (strcmp(tempc, ADOBE_Codes[index1]) == 0) {
					ichar = index1;
					break;
				}
			}
			if (ichar == (-1)) {
				printf("\nSymbol \\%s is not known to the text routine.",tempc);
				decoded = False;
				continue;
			}
		}
		escaped_chars[ichar_esc] = ichar;
		text_items[nitems].chars = &escaped_chars[ichar_esc];
		text_items[nitems].nchars = 1;
		text_items[nitems].delta = 0;
		text_items[nitems].font = font_s[*itsiz-1];
		text_s[nitems] = True;
		text_nchars[nitems] = 1;
#ifdef __VMS
		text_chars[nitems].dsc$w_length = 1;
		text_chars[nitems].dsc$a_pointer =
			(unsigned char *) &escaped_chars[ichar_esc];
#else
		text_chars[nitems] = (FortranString *) &escaped_chars[ichar_esc];
#endif /* VMS characters */
		nitems++;
		ichar_esc++;
		ichar = 0;
		cp = &textptr[index];
	} while (index < nchars);

	fflush(stdout);
	if (!decoded) return;
text_scanned:
/*
 * Create GC and also a pixmap from the text if moving.
 */
	XTextExtents(fontinfo_n[*itsiz-1], textptr, nchars, &direction,
		&ascent, &descent, &overall);
	if (ascent < overall.ascent) ascent = overall.ascent;
	if (descent < overall.descent) descent = overall.descent;
	x_hot = 0;
	y_hot = ascent;

	if ((int) *sangl != 0) goto rotation;
	if (fiy) goto rotation;
	if (fix_nofix == 1) goto fixit;
	text_pix = XCreatePixmap(disp, XtWindow(area[window_num]), overall.width,
			ascent + descent, XDefaultDepthOfScreen(XtScreen(area[0])));
	textgc_val.font = font_n[*itsiz-1];
	textgc_val.function = GXcopy;
	textgc_val.foreground = None;
	textgc_val.background = None;
	textgc = XCreateGC(disp, text_pix,
		GCBackground|GCForeground|GCFunction|GCFont, &textgc_val);

	XFillRectangle(disp, text_pix, textgc, 0, 0, overall.width,
		ascent + descent);
	textgc_val.foreground = fgindex;
	textgc_val.background = None;
	XChangeGC(disp, textgc, GCForeground|GCBackground, &textgc_val);
	XDrawText(disp, text_pix, textgc, 0, ascent, text_items, nitems);
	xsize = overall.width;
	ysize = ascent + descent;

	goto after_rotation;

rotation:
	text_pix = XCreatePixmap(disp, XtWindow(area[window_num]), overall.width,
			ascent + descent, 1);
	textgc_val.font = font_n[*itsiz-1];
	textgc_val.function = GXcopy;
	textgc_val.foreground = None;
	textgc_val.background = None;
	textgc = XCreateGC(disp, text_pix,
		GCBackground|GCForeground|GCFunction|GCFont, &textgc_val);
	XFillRectangle(disp, text_pix, textgc, 0, 0, overall.width,
		ascent + descent);
	textgc_val.foreground = 1;
	textgc_val.background = None;
	XChangeGC(disp, textgc, GCForeground|GCBackground, &textgc_val);
	XDrawText(disp, text_pix, textgc, 0, ascent, text_items, nitems);
/*
 * This is a rotated text. We must extract the pixels from text_pix.
 */
	quadrant = ((int)(*sangl/90.)) % 4;
	radarg = (double) ((*sangl) * 3.14159 / 180.);
	scos = cos(radarg);
	ssin = sin(radarg);
	xsize =	(int)(fabs(overall.width * scos) + fabs((ascent + descent) * ssin));
	ysize = (int)(fabs((ascent + descent) * scos) + fabs(overall.width * ssin));
	switch (quadrant) {
		case 0:
		default:
			x_hot = (int)fabs(ascent * ssin);
			y_hot = (int)(ysize - fabs(descent * scos));
			x_rot = (int)fabs((ascent + descent) * ssin);
			y_rot = ysize;
			break;
		case 1:
			x_hot = (int)(xsize - fabs(descent * ssin));
			y_hot = (int)(ysize - fabs(ascent * scos));
			x_rot = xsize;
			y_rot = (int)(ysize - fabs((ascent + descent) * scos));
			break;
		case 2:
			x_hot = (int)(fabs(overall.width * scos) + fabs(descent * ssin));
			y_hot = (int)fabs(descent * scos);
			x_rot = (int)fabs(overall.width * scos);
			y_rot = 0;
			break;
		case 3:
			x_hot = (int)fabs(descent * ssin);
			y_hot = (int)fabs(ascent * scos);
			x_rot = 0;
			y_rot = (int)(ysize - fabs(overall.width * ssin));
			break;
	}
	ximage = XGetImage(disp, text_pix, 0, 0, overall.width, ascent + descent,
		AllPlanes, ZPixmap);
	XFreePixmap(disp, text_pix);
	if (fix_nofix == 1) goto fixit_rotated;
	text_pix = XCreatePixmap(disp, XtWindow(area[window_num]), xsize, ysize,
		XDefaultDepthOfScreen(XtScreen(area[0])));
	textgc_val.function = GXcopy;
	textgc_val.foreground = None;
	textgc_val.background = None;
	textgc = XCreateGC(disp, text_pix,
		GCBackground|GCForeground|GCFunction, &textgc_val);

	XFillRectangle(disp, text_pix, textgc, 0, 0, xsize, ysize);
	textgc_val.foreground = fgindex;
	textgc_val.background = None;
	XChangeGC(disp, textgc, GCForeground|GCBackground, &textgc_val);

	if (debugg) goto another_server;
	if (ximage->byte_order != LSBFirst) goto another_server;
	if (ximage->bitmap_bit_order != LSBFirst) goto another_server;
	if (ximage->bitmap_pad != 32) goto another_server;
/*
 * DEC-like server.
 */
	byt_y = (unsigned char *)ximage->data;
	ip = 0;
	nworkdw = (ascent + descent) * ximage->bytes_per_line * 8;
	workdw  = (int *) calloc(nworkdw, sizeof(int));
	for (y = 0; y < ascent + descent; y++) {
		byt_x = byt_y;
		byt_y += ximage->bytes_per_line;
		for (x = 0; x < ximage->bytes_per_line; x++) {
			ihex = (unsigned int) *(byt_x++);
			for (index = 0; index < 8; index++) {
				workdw[ip + index] = ihex & pix_mask[index];
			}
			ip += 8;
		}
	}
	if (ip > nworkdw) {
		printf("\nBeware: Temporary working space allocated too small in dgtxt\n");
		fflush(stdout);
	}

	ip = 0;
	for (y = 0; y < ascent + descent; y++) {
		for (x = 0; x < overall.width; x++) {
			if (workdw[ip] != 0) {
				xco = (int)(x_rot + (x * scos - (ascent + descent - y) * ssin));
				yco = (int)(y_rot - (x * ssin + (ascent + descent - y) * scos));
				XDrawPoint(disp, text_pix, textgc, xco, fiy ? (ysize-yco):yco);
			}
			ip++;
		}
		for (x = overall.width; x < 8 * ximage->bytes_per_line; x++)
			ip++;
	}
	free(workdw);

	(void) XDestroyImage(ximage);
	goto after_rotation;

another_server:
/*
 * The general case.
 */
	for (y = 0; y < ascent + descent; y++) {
		for (x = 0; x < overall.width; x++) {
			if (XGetPixel(ximage, x, y) == 1) {
				xco = (int)(x_rot + (x * scos - (ascent + descent - y) * ssin));
				yco = (int)(y_rot - (x * ssin + (ascent + descent - y) * scos));
				XDrawPoint(disp, text_pix, textgc, xco, fiy ? (ysize-yco):yco);
			}
		}
	}

	(void) XDestroyImage(ximage);
	goto after_rotation;
/*
 * All cases again.
 */
after_rotation:
	XFreeGC(disp, textgc);
#ifdef Not_now
	if (cursor == 0) {
		printf("\nProgramming error, variable cursor is undefined.");
		fflush(stdout);
		return;
	}
#endif /* Seems to be a mistake to check */
/*
 * Make an one-dot "invisible" cursor.
 */
	fgcol.pixel = ddefco.ltabdd[12];
	bgcol.pixel = ddefco.ltabdd[8];
	XQueryColor(disp, cmap, &fgcol);
	XQueryColor(disp, cmap, &bgcol);
	zero_pix = XCreatePixmapFromBitmapData(disp, XtWindow(area[window_num]),
		pntzer_bits, 16, 16, 1, 0, 1);
	zero_cursor = XCreatePixmapCursor(disp, zero_pix, zero_pix,
		&fgcol, &bgcol, hsp0h, hsp0v);

	XRaiseWindow(disp, XtWindow(toplevel[window_num]));
	if (((int) *hret > 0) && ((int) *vret > 0))
		XWarpPointer(disp, None, XtWindow(toplevel[window_num]), 0, 0, 0, 0,
		(int) *hret, fiy ? (int) *vret : (pixdata[window_num].y - (int) *vret));
	XDefineCursor(disp, XtWindow(area[window_num]), zero_cursor);
	if (IsPtrInDaliWin(&win_x, &win_y, &keys_buttons)) {
		XCopyArea(disp, text_pix, XtWindow(area[window_num]), gc255, 0, 0,
			xsize, ysize, win_x - x_hot, win_y - y_hot);
		disp_on = 1;
	} else {
		disp_on = 0;
	}
	XFreePixmap(disp, zero_pix);
	XtAddEventHandler(area[window_num], EnterWindowMask|LeaveWindowMask|PointerMotionMask,
		False, (XtEventHandler)dgpast, &evpnt);
/*
 * A button event handler, although dummy, must be registered to override
 * possible button defaults.
 */
	XtAddEventHandler(area[window_num], ButtonPressMask|ButtonReleaseMask,
		False, (XtEventHandler)dgbast, &evbut);
	index = 6;
	dgclev(&index);
	see_text = 1;
	winpix_copied = 0;
	Allow_func_keys = True;

	for (;;) {
		XFlush(disp);
/*
 * Wait here until something is typed on the keyboard or the pointer is moved.
 * An expose event should also bring us here,
 */
#ifdef __VMS
		dgtlnx(&maxlen, iosb, &f_char_d);
#else
		dgtlnx(&maxlen, iosb, &f_char, l_f_char);
#endif /* VMS characters */
/*
 * First check if the screen has been updated from backing store.
 */
		if ((winpix_copied == 1) && (disp_on == 1)) {
			XCopyArea(disp, text_pix, XtWindow(area[window_num]), gc255,
				0, 0, xsize, ysize, win_x - x_hot, win_y - y_hot);
		}
		winpix_copied = 0;
		if (evflag[5] != 0) {
			if (see_text != 0) {
/*
 * Track the moving text.
 */
				ptr_in_window = IsPtrInDaliWin(&win_xp, &win_yp, &keys_buttons);
				if (disp_on == 1) {
					XCopyArea(disp, text_pix, XtWindow(area[window_num]), gc255,
						0, 0, xsize, ysize, win_x - x_hot, win_y - y_hot);
					disp_on = 0;
				}
				if (ptr_in_window) {
					win_x = win_xp;
					win_y = win_yp;
					XCopyArea(disp, text_pix, XtWindow(area[window_num]), gc255,
						0, 0, xsize, ysize, win_x - x_hot, win_y - y_hot);
					disp_on = 1;
				}
			}
			index = 6;
			dgclev(&index);
			continue;
		}

		if (iosb[1] == 0) {
/*
 * String starts with a control character. Finish on Return, handle
 * escape sequences and continue on other control characters.
 */
			if (iosb[2] == (short) 0xff0d) break;
			if (iosb[2] == (short) 0xff1b) {
/*
 * A function key was pressed. React on arrow keys only.
 */
				if (see_text == 0) break;
				switch (f_char) {
					case 'U': add_x =  0; add_y = -1; break;
					case 'D': add_x =  0; add_y =  1; break;
					case 'L': add_x = -1; add_y =  0; break;
					case 'R': add_x =  1; add_y =  0; break;
					default:  add_x =  0; add_y =  0; break;
				}
				if (disp_on == 1) {
					XCopyArea(disp, text_pix, XtWindow(area[window_num]), gc255,
						0, 0, xsize, ysize, win_x - x_hot, win_y - y_hot);
					disp_on = 0;
				}
				win_x = win_x + add_x;
				win_y = win_y + add_y;
				XCopyArea(disp, text_pix, XtWindow(area[window_num]), gc255,
					0, 0, xsize, ysize, win_x - x_hot, win_y - y_hot);
				XWarpPointer(disp, None, XtWindow(area[window_num]), 0, 0, 0, 0,
					win_x, win_y);
				disp_on = 1;
			}
			continue;
		} else {
			if (iosb[1] != 1) continue;
			switch (f_char) {
				case ' ':
/*
 * Toggle display of text.
 */
					if (see_text == 1) {
						if (disp_on == 1) {
							XCopyArea(disp, text_pix,
								XtWindow(area[window_num]), gc255, 0, 0,
								xsize, ysize, win_x - x_hot, win_y - y_hot);
							disp_on = 0;
						}
						see_text = 0;
					} else {
						if (IsPtrInDaliWin(&win_x, &win_y, &keys_buttons)) {
							XCopyArea(disp, text_pix,
								XtWindow(area[window_num]), gc255, 0, 0,
								xsize, ysize, win_x - x_hot, win_y - y_hot);
							disp_on = 1;
						} else {
							disp_on = 0;
						}
						see_text = 1;
					}
					printf("%s", back_space); fflush(stdout);
					continue;
				case '<':
				case '>':
/*
 * Toggle HELP window.
 */
#ifdef __VMS
					dqhlp(&hlp_d);
#else
					dqhlp(hlp_d);
#endif /* VMS characters */
					continue;
				case '^':
/*
 * Pop DALI window.
 */
#ifdef __VMS
					dgpop(&pop_d);
#else
					dgpop(pop_d);
#endif /* VMS characters */
					continue;
				default:
					printf("%s", back_space); fflush(stdout);
					continue;
			}
		}
	}

	Allow_func_keys = False;
	XtRemoveEventHandler(area[window_num],
		EnterWindowMask|LeaveWindowMask|PointerMotionMask,
		False, (XtEventHandler)dgpast, &evpnt);
	XtRemoveEventHandler(area[window_num], ButtonPressMask|ButtonReleaseMask,
		False, (XtEventHandler)dgbast, &evbut);
	index = 6;
	dgclev(&index);
	winpix_copied = -1;
	if (disp_on == 1) {
		XCopyArea(disp, text_pix, XtWindow(area[window_num]), gc255,
			0, 0, xsize, ysize, win_x - x_hot, win_y - y_hot);
		disp_on = 0;
	}
	XRaiseWindow(disp, XtWindow(toplevel[window_num]));
	if (IsPtrInDaliWin(&win_x, &win_y, &keys_buttons)) {
		*hret = (float) win_x;
		*vret = (float) (fiy ? win_y : (pixdata[window_num].y - win_y));
	}

	XDefineCursor(disp, XtWindow(area[window_num]), cursor);
	XFreeCursor(disp, zero_cursor);
	XFreePixmap(disp, text_pix);
	XFlush(disp);
	return;

fixit:
/*
 * Here we draw the text permanently.
 */
	textgc_val.foreground = ddefco.ltabdd[*level];
	textgc_val.background = 0;
	textgc_val.function = GXcopy;
	textgc_val.font = font_n[*itsiz-1];
	textgc = XCreateGC(disp, drawin,
		GCBackground|GCForeground|GCFunction|GCFont, &textgc_val);
	XDrawText(disp, drawin, textgc,
		(int) *hret - x_hot, fiy ? ((int)*vret + y_hot - ascent) :
			(pixdata[window_num].y - ((int)*vret + y_hot - ascent)),
		text_items, nitems);

	XFreeGC(disp, textgc);
	goto PS_output;
fixit_rotated:
	textgc_val.foreground = ddefco.ltabdd[*level];
	textgc_val.background = 0;
	textgc_val.function = GXcopy;
	textgc = XCreateGC(disp, drawin,
		GCBackground|GCForeground|GCFunction, &textgc_val);
	ip = 0;
	for (y = 0; y < ascent + descent; y++) {
		for (x = 0; x < overall.width; x++) {
			if (XGetPixel(ximage, x, y) == 1) {
				xco = (int)(x_rot + (x * scos - (ascent + descent - y) * ssin)+
					(int)*hret - x_hot);
				y1 = (int)(y_rot - (x * ssin + (ascent + descent - y) * scos)-
					(int)*vret - y_hot);
				yco = fiy ? -y1 : (pixdata[window_num].y + y1);
				XDrawPoint(disp, drawin, textgc, xco, yco);
			}
			ip++;
		}
		for (x = overall.width; x < 8 * ximage->bytes_per_line; x++)
			ip++;
	}
	XFreeGC(disp, textgc);
	(void) XDestroyImage(ximage);
PS_output:
	if (duiscc.fmetdu) {
		float height;

		height = font_scale[*itsiz-1];
		if (height < 0.0)
			height = ascent + descent;
		mdgtxtm(level, itsiz, text, sangl, hret, vret, &height, &nitems,
			text_s, text_chars, text_nchars);
	}
	return;
fixit_outline:
/*
 * Fix a symbol generated by outlines.
 */
	XFreeGC(disp, textgc);
	textgc_val.foreground = ddefco.ltabdd[*level];
	textgc_val.background = 0;
	textgc_val.function = GXcopy;
	textgc = XCreateGC(disp, drawin,
		GCBackground|GCForeground|GCFunction, &textgc_val);
	for (x = 0; x < n_aleph_points; x++) {
		poly_points[x].x += (int) *hret - x_hot;
		poly_points[x].y += fiy ? ((int)*vret + y_hot) :
			(pixdata[window_num].y - ((int)*vret + y_hot));
	}
	XFillPolygon(disp, drawin, textgc, poly_points, n_aleph_points,
		Complex, CoordModeOrigin);
	free (poly_points);
	XFreeGC(disp, textgc);
	if (duiscc.fmetdu) {
		outl_x = (float *) calloc(sizeof(float), n_aleph_points);
		outl_y = (float *) calloc(sizeof(float), n_aleph_points);
		for (x = 0; x < n_aleph_points; x++) {
			outl_x[x] = *hret - x_hot + x_rot + (*itsiz) *
				(scos * aleph_points[x].x - ssin * aleph_points[x].y);
			outl_y[x] = *vret + (ysize - y_hot) - y_rot + (*itsiz) *
				(ssin * aleph_points[x].x + scos * aleph_points[x].y);
		}
		mdglevl(level);
		mdgarea(&n_aleph_points, outl_x, outl_y);
		free(outl_x);
		free(outl_y);
	}
	return;
}

CXX void dgtxtm(
	int *itsiz,
	FortranString *text,
	float *sangl,
	float *hret,
#ifndef __unix__
	float *vret
#else
	float *vret,
	int *l_text
#endif /* unix */
)
/*
 * "Moving" interface to text routine dgtxt_com.
 */
{
	int fix_nofix = 0;
	int *level = 0;

#ifdef __unix__
	dgtxt_com(level, itsiz, text, sangl, hret, vret, fix_nofix, l_text);
#else
	dgtxt_com(level, itsiz, text, sangl, hret, vret, fix_nofix);
#endif /* unix */
}

CXX void dgtxtf(
	int *level,
	int *itsiz,
	FortranString *text,
	float *sangl,
	float *hret,
#ifndef __unix__
	float *vret
#else
	float *vret,
	int *l_text
#endif /* unix */
)
/*
 * "Fixing" interface to text routine dgtxt_com.
 */
{
	int fix_nofix = 1;

#ifdef __unix__
	dgtxt_com(level, itsiz, text, sangl, hret, vret, fix_nofix, l_text);
#else
	dgtxt_com(level, itsiz, text, sangl, hret, vret, fix_nofix);
#endif /* unix */
}

CXX void dglinm(
	int *ihmv,
	int *ivmv,
	float hco[],
	float vco[],
	int ihco[],
	int ivco[],
	int *nw,
	void (*dfu)(int *, int*, float *, float*, int [][2], int *, int *, FortranString *
#ifdef __VMS
		),
#else
		, int),
#endif /* VMS */
	logical *fint,
	int nds[][2],
	int *nns
)
/*
 * Display a set of linesegments.
 * (ihmv,ivmv) is the start and end position of the pointer.
 * The float pair (hco,vco) (or integer (ihco,ivco) depending on fint) are
 * the coordinates for a set of nns line segments. nds[0][*] contains the
 * first index of each segment, abs(nds[1][*]) is the number of points in the
 * segment. If this number is positive simple lines are drawn, if negative
 * a filled area is drawn. Space for these variables must be allocated by
 * the calling routine.
 *
 * The actual values are returned by calling
 *    dfu(ihmv, ivmv, hco, vco, nds, nns, fbut, ftext)
 * which is given the position (ihmv,ivmv) and button state fbut (sorry for
 * the order). fbut[0], fbut[1] or fbut[2] is 0 if M1, M2, M3 resp. is down,
 * otherwise 1. If there was keyboard input fbut[3] will be 0 and ftext
 * will contain the ASCII character of the key pressed.
 *  The so called routine can also return a termination status in fbut[4].
 * I.e., if dfu returns a non-zero fbut[4] dglinm should exit.
 *
 * The routine will always finish on a Return and toggle line display if
 * the spacebar is pressed. Default action for "<", ">" and "^" keys.
 *
 * X-version by Björn S. Nilsson on 13-Apr-1992.
 */
{
	logical debugl;
#ifdef __VMS
	static unsigned char hlp[] = "<<", pop[] = "POP", f_char;
	static FortranString hlp_d = {sizeof(hlp)-1,0,0,hlp},
		pop_d = {sizeof(pop)-1,0,0,pop},
		f_char_d = {sizeof(f_char),0,0,&f_char};
#else
	static FortranString hlp_d[] = "<<", pop_d[] = "POP", f_char;
	int l_f_char = sizeof(f_char);
#endif /* VMS characters */
	int win_x, win_y, see_lines, disp_on, maxlen = 1, ihprv, ivprv;
	int add_x, add_y;
	int index, i, i1, i2, i3, iq, nfirst, allocated_points;
	int fbut[5];
	short iosb[4];
	unsigned int keys_buttons;
	int prev_buttons;
	Cursor zero_cursor;
	Pixmap zero_pix;
	XColor fgcol, bgcol;
	XPoint *points;
	logical ptr_in_window;
	static char pntzer_bits[] = {
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	};
	int hsp0h=7, hsp0v=7;
	int evpnt = 20, evbut = 20;
	static char back_space[] = {0x08, 0x00};
	XEvent dummy_event;

	debugl = debug_mask[(int)('l'-'a')];
/*
 * Make an one-dot "invisible" cursor.
 */
	if (!debugl) pntzer_bits[12] = 0x00;
	fgcol.pixel = ddefco.ltabdd[12];
	bgcol.pixel = ddefco.ltabdd[8];
	XQueryColor(disp, cmap, &fgcol);
	XQueryColor(disp, cmap, &bgcol);
	zero_pix = XCreatePixmapFromBitmapData(disp, XtWindow(area[window_num]),
		pntzer_bits, 16, 16, 1, 0, 1);
	zero_cursor = XCreatePixmapCursor(disp, zero_pix, zero_pix,
		&fgcol, &bgcol, hsp0h, hsp0v);

	XRaiseWindow(disp, XtWindow(toplevel[window_num]));
	if ((*ihmv > 0) && (*ivmv > 0))
		XWarpPointer(disp, None, XtWindow(toplevel[window_num]), 0, 0, 0, 0,
		*ihmv, fiy ? *ivmv : (pixdata[window_num].y - *ivmv));

	XDefineCursor(disp, XtWindow(area[window_num]), zero_cursor);
	XFreePixmap(disp, zero_pix);
	XtAddEventHandler(area[window_num], EnterWindowMask|LeaveWindowMask|PointerMotionMask,
		False, (XtEventHandler)dgpast, &evpnt);
	XtAddEventHandler(area[window_num], ButtonPressMask|ButtonReleaseMask,
		False, (XtEventHandler)dgbast, &evbut);
	ihco[0] = -1;
	ivco[0] = -1;
	ihprv = *ihmv;
	ivprv = *ivmv;
	prev_buttons = -1;
	index = 6;
	dgclev(&index);
	see_lines = 1;
	winpix_copied = 0;
	Allow_func_keys = True;
	disp_on = 0;
	nfirst = 1;
	allocated_points = 0;
	if (debugl) printf("\n");

/*
 * Here is the loop.
 */
	for (;;) {
		XFlush(disp);
/*
 * Wait here until something is typed on the keyboard or the pointer is moved.
 * An expose event should also bring us here.
 */
		if (nfirst == 0)
#ifdef __VMS
			dgtlnx(&maxlen, iosb, &f_char_d);
#else
			dgtlnx(&maxlen, iosb, &f_char, l_f_char);
#endif /* VMS characters */
/*
 * First check if the screen has been updated from backing store.
 */
		if ((winpix_copied == 1) && (disp_on == 1)) {
			for (i1 = 0; i1 < *nns; i1++) {
				i2 = nds[i1][0] - 1;
				if (nds[i1][1] > 0) {
					XDrawLines(disp, XtWindow(area[window_num]), gc255,
						&points[i2], nds[i1][1], CoordModeOrigin);
				} else {
					XFillPolygon(disp, XtWindow(area[window_num]), gc255,
						&points[i2], -nds[i1][1], Complex, CoordModeOrigin);
				}
			}
		}
		winpix_copied = 0;
		if ((evflag[5] != 0) || (nfirst == 1)) {
			nfirst = 0;
			iq = QLength(disp);
			if (debugl) printf("\rX qlength:%3d ", iq);
			while (XCheckTypedEvent(disp, MotionNotify, &dummy_event)) {
				continue;
			}
			if (see_lines != 0) {
/*
 * Track the moving lines.
 */
				keys_buttons = 1;
				ptr_in_window = IsPtrInDaliWin(&win_x, &win_y, &keys_buttons);
				if ((win_x == *ihmv) &&
					(win_y == fiy ? *ivmv : (pixdata[window_num].y - *ivmv)) &&
					((int)keys_buttons == prev_buttons)) goto after_move;
				if (disp_on == 1) {
					for (i1 = 0; i1 < *nns; i1++) {
						i2 = nds[i1][0] - 1;
						if (nds[i1][1] > 0) {
							XDrawLines(disp, XtWindow(area[window_num]), gc255,
								&points[i2], nds[i1][1], CoordModeOrigin);
						} else {
							XFillPolygon(disp, XtWindow(area[window_num]),
								gc255, &points[i2], -nds[i1][1], Complex,
								CoordModeOrigin);
						}
					}
					disp_on = 0;
				}
				if (ptr_in_window) {
					*ihmv = win_x;
					*ivmv = fiy ? win_y : (pixdata[window_num].y - win_y);
					ihprv = *ihmv;
					ivprv = *ivmv;
					fbut[0]=(keys_buttons & Button1Mask)==Button1Mask ? 0 : 1;
					fbut[1]=(keys_buttons & Button2Mask)==Button2Mask ? 0 : 1;
					fbut[2]=(keys_buttons & Button3Mask)==Button3Mask ? 0 : 1;
					fbut[3] = 1;
					fbut[4] = 0;
#ifdef __VMS
					(*dfu)(ihmv, ivmv, hco, vco, nds, nns, fbut, &f_char_d);
#else
					(*dfu)(ihmv, ivmv, hco, vco, nds, nns, fbut, &f_char,
						l_f_char);
#endif /* VMS characters */
					if (fbut[4] != 0) {
						iosb[1] = 0;
						iosb[2] = (short) 0xff0d;
/* The next statement is redundant. What did I want to do ? */
						break;
					}
					if ((*ihmv != ihprv) || (*ivmv != ivprv)) {
						if ((*ihmv > 0) && (*ivmv > 0)) {
							XWarpPointer(disp, None,
								XtWindow(toplevel[window_num]),
								0, 0, 0, 0, *ihmv, fiy ? *ivmv:
								(pixdata[window_num].y - *ivmv));
						}
					}
					if (*nns <= 0) break;
					i3 = nds[*nns-1][0] + abs(nds[*nns-1][1]) - 1;
					if (*fint) {
						ucopy(hco, ihco, &i3);
						ucopy(vco, ivco, &i3);
					} else {
						for (i = 0; i < i3; i++) {
							ihco[i] = (int) hco[i];
							ivco[i] = (int) vco[i];
						}
					}
					if (i3 > allocated_points) {
						if (allocated_points !=0) XtFree((char *)points);
						points = (XPoint *) XtCalloc(i3, sizeof(XPoint));
						if (debugl) {
							printf("\nNew points array allocated with size %d", i3);
							printf("\nPrevious size: %d\n", allocated_points);
						}
						allocated_points = i3;
					}
					for (i = 0; i < i3; i++) {
						points[i].x = (short) ihco[i];
						points[i].y = (short) (fiy ? ivco[i] :
							(pixdata[window_num].y - ivco[i]));
					}
					for (i1 = 0; i1 < *nns; i1++) {
						i2 = nds[i1][0] - 1;
						if (nds[i1][1] > 0) {
							XDrawLines(disp, XtWindow(area[window_num]), gc255,
								&points[i2], nds[i1][1], CoordModeOrigin);
						} else {
							XFillPolygon(disp, XtWindow(area[window_num]),
								gc255, &points[i2], -nds[i1][1], Complex,
								CoordModeOrigin);
						}
					}
					disp_on = 1;
				}
			}
after_move:
			index = 6;
			dgclev(&index);
			continue;
		}
		nfirst = 0;

		if (iosb[1] == 0) {
/*
 * The input string starts with a control character. Finish on Return,
 * handle escape sequences and continue on other control characters.
 */
			if (iosb[2] == (short) 0xff0d) break;
			if (iosb[2] == (short) 0xff1b) {
/*
 * A function key was pressed. React on arrow keys only.
 */
				if (see_lines == 0) break;
				switch (f_char) {
					case 'U': add_x =  0; add_y = -1; break;
					case 'D': add_x =  0; add_y =  1; break;
					case 'L': add_x = -1; add_y =  0; break;
					case 'R': add_x =  1; add_y =  0; break;
					default:  add_x =  0; add_y =  0; break;
				}
				win_x = win_x + add_x;
				win_y = win_y + add_y;
				XWarpPointer(disp, None, XtWindow(area[window_num]), 0, 0, 0, 0,
					win_x, win_y);
			}
			continue;
		} else {
			if (iosb[1] != 1) continue;
			switch (f_char) {
				case ' ':
/*
 * Toggle display of moving lines.
 */
					if (see_lines == 1) {
						if (disp_on == 1) {
							for (i1 = 0; i1 < *nns; i1++) {
								i2 = nds[i1][0] - 1;
								if (nds[i1][1] > 0) {
									XDrawLines(disp, XtWindow(area[window_num]),
										gc255, &points[i2], nds[i1][1],
										CoordModeOrigin);
								} else {
									XFillPolygon(disp,
										XtWindow(area[window_num]), gc255,
										&points[i2], -nds[i1][1], Complex,
										CoordModeOrigin);
								}
							}
							disp_on = 0;
						}
						see_lines = 0;
					} else {
						if (IsPtrInDaliWin(&win_x, &win_y, &keys_buttons)) {
							for (i1 = 0; i1 < *nns; i1++) {
								i2 = nds[i1][0] - 1;
								if (nds[i1][1] > 0) {
									XDrawLines(disp, XtWindow(area[window_num]),
										gc255, &points[i2], nds[i1][1],
										CoordModeOrigin);
								} else {
									XFillPolygon(disp,
										XtWindow(area[window_num]), gc255,
										&points[i2], -nds[i1][1], Complex,
										CoordModeOrigin);
								}
							}
							disp_on = 1;
						} else {
							disp_on = 0;
						}
						see_lines = 1;
					}
					printf("%s", back_space); fflush(stdout);
					continue;
				case '<':
				case '>':
/*
 * Toggle HELP window.
 */
#ifdef __VMS
					dqhlp(&hlp_d);
#else
					dqhlp(hlp_d);
#endif /* VMS characters */
					continue;
				case '^':
/*
 * Pop DALI window.
 */
#ifdef __VMS
					dgpop(&pop_d);
#else
					dgpop(pop_d);
#endif /* VMS characters */
					continue;
				default:
					fbut[0] = fbut[1] = fbut[2] = 1;
					fbut[3] = 0;
					ptr_in_window = IsPtrInDaliWin(&win_x, &win_y, &keys_buttons);
					*ihmv = win_x;
					*ivmv = fiy ? win_y : (pixdata[window_num].y - win_y);
/*
 * Note. In the next call hco, vco, nds and nns are arguments. They should
 * not be changed by the called routine.
 */
#ifdef __VMS
					(*dfu)(ihmv, ivmv, hco, vco, nds, nns, fbut, &f_char_d);
#else
					(*dfu)(ihmv, ivmv, hco, vco, nds, nns, fbut, &f_char, l_f_char);
#endif /* VMS characters */
					printf("%s", back_space); fflush(stdout);
					if (fbut[4] != 0) {
						iosb[1] = 0;
						iosb[2] = (short) 0xff0d;
					}
					if (iosb[2] == (short) 0xff0d) goto out;
					continue;
			}
		}
	}

out:
	Allow_func_keys = False;
	XtRemoveEventHandler(area[window_num],
		EnterWindowMask|LeaveWindowMask|PointerMotionMask,
		False, (XtEventHandler)dgpast, &evpnt);
	XtRemoveEventHandler(area[window_num], ButtonPressMask|ButtonReleaseMask,
		False, (XtEventHandler)dgbast, &evbut);
	index = 6;
	dgclev(&index);
	winpix_copied = -1;
	if (disp_on == 1) {
		for (i1 = 0; i1 < *nns; i1++) {
			i2 = nds[i1][0] - 1;
			if (nds[i1][1] > 0) {
				XDrawLines(disp, XtWindow(area[window_num]), gc255,
					&points[i2], nds[i1][1], CoordModeOrigin);
			} else {
				XFillPolygon(disp, XtWindow(area[window_num]), gc255,
					&points[i2], -nds[i1][1], Complex, CoordModeOrigin);
			}
		}
		disp_on = 0;
	}
	XRaiseWindow(disp, XtWindow(toplevel[window_num]));
	if (IsPtrInDaliWin(&win_x, &win_y, &keys_buttons)) {
		*ihmv = win_x;
		*ivmv = fiy ? win_y : (pixdata[window_num].y - win_y);
	}

	XDefineCursor(disp, XtWindow(area[window_num]), cursor);
	XFreeCursor(disp, zero_cursor);
	if (allocated_points !=0) XtFree((char *)points);
	XFlush(disp);
	return;

}

CXX void dglinf(
	float hco[],
	float vco[],
	int ihco[],
	int ivco[],
	int nds[][2],
	int *nns,
	int *level
)
/*
 * Fix the lines moved by dglinm.
 */
{
	int i, i1, i2, i3;
	GC lingc;
	XGCValues lingc_val;
	XPoint *points;

	if (*nns <= 0) return;
	i3 = nds[*nns-1][0] + abs(nds[*nns-1][1]) - 1;
	points = (XPoint *) XtCalloc(i3, sizeof(XPoint));
	for (i = 0; i < i3; i++) {
		points[i].x = (short) ihco[i];
		points[i].y = (short) (fiy ? ivco[i] :
			(pixdata[window_num].y - ivco[i]));
	}
	lingc_val.foreground = ddefco.ltabdd[*level];
	lingc_val.background = 0;
	lingc_val.function = GXcopy;
	lingc = XCreateGC(disp, drawin, GCBackground|GCForeground|GCFunction,
		&lingc_val);
	for (i1 = 0; i1 < *nns; i1++) {
		i2 = nds[i1][0] - 1;
		if (nds[i1][1] > 0) {
			XDrawLines(disp, drawin, lingc,
				&points[i2], nds[i1][1], CoordModeOrigin);
		} else {
			XFillPolygon(disp, drawin, lingc,
				&points[i2], -nds[i1][1], Complex, CoordModeOrigin);
		}
	}
	XtFree((char *)points);
	XFreeGC(disp, lingc);
}

/*
 * New routines private to dali_x.c
 */

CXX logical dgpntr(
	int *x,
	int *y,
	logical bstate[]
)
/*
 * Report position of pointer and the state of the pointer buttons.
 * The bstate[0] is true if button 1 is pressed etc.
 */
{
	int retxx, retyy;
	unsigned int button;

	bstate[0] = bstate[1] = bstate[2] = False;
	if (!IsPtrInDaliWin(&retxx, &retyy, &button)) return False;
	*x = retxx;
	*y = fiy ? retyy : (pixdata[window_num].y - retyy);
	bstate[0] = (button & Button1Mask) == Button1Mask;
	bstate[1] = (button & Button2Mask) == Button2Mask;
	bstate[2] = (button & Button3Mask) == Button3Mask;

	return True;
}

XtEventHandler redrawhelp(
	Widget w,
	caddr_t junk1,
	XEvent *event
)
{
	int tmp = 0;
#ifdef __VMS
	static unsigned char doc[] = "DO";
	static unsigned char blc[] = " ";
	static FortranString do_d = {sizeof(doc)-1,0,0,doc};
	static FortranString bl_d = {sizeof(blc)-1,0,0,blc};
#else
	static FortranString do_d[] = "DO";
	static FortranString bl_d[] = " ";
#endif

	if (event->xexpose.window == XtWindow(helparea)) {
		XClearWindow(disp, XtWindow(helparea));
#ifdef __VMS
		dghlp(&do_d, &bl_d, &tmp, &tmp);
#else
		dghlp(do_d, bl_d, &tmp, &tmp, 2, 1);
#endif
	} else {
		printf("%s/n", "Something wrong with XEvent ...");
	}
	return 0;
}

static logical IsPtrInHelpWin(
	int *x,
	int *y,
	unsigned int *button
)
{
	Window root, child;
	int root_x, root_y, win_x, win_y;
	unsigned int state;

	(void) XQueryPointer(disp, XtWindow(tophelp), &root, &child,
		&root_x, &root_y, &win_x, &win_y, &state);
	if (x != NULL)
		*x = win_x;
	if (y != NULL)
		*y = win_y;
	if (button != NULL)
		*button = state;
	if (child == XtWindow(helparea)) {
		return(True);
	} else {
		return(False);
	}
}

static logical IsPtrInDaliWin(
	int *x,
	int *y,
	unsigned int *button
)
{
	Window root, child;
	int root_x, root_y, win_x, win_y;
	unsigned int state;

	(void) XQueryPointer(disp, XtWindow(toplevel[window_num]), &root, &child,
		&root_x, &root_y, &win_x, &win_y, &state);
	if (x != NULL)
		*x = win_x;
	if (y != NULL)
		*y = win_y;
	if (button != NULL)
		*button = state;
	if (child == XtWindow(area[window_num])) {
		return(True);
	} else {
		return(False);
	}
}

#if !defined(__VMS) && !defined(__hpux)
#include <sys/ioctl.h>

/*
 * Get size of the output screen. Stolen from less.
 */
CXX void scrsize(
	int *p_height,
	int *p_width
)
{
	register char *s;
	struct winsize w;

	if ((ioctl(2, TIOCGWINSZ, &w) == 0) && (w.ws_row > 0))
		*p_height = w.ws_row;
	else
	if ((s = getenv("LINES")) != NULL)
		*p_height = atoi(s);
	else
		*p_height = 24;

	if ((ioctl(2, TIOCGWINSZ, &w) == 0) && (w.ws_col > 0))
		*p_width = w.ws_col;
	else
	if ((s = getenv("COLUMNS")) != NULL)
		*p_width = atoi(s);
	else
		*p_width = 80;
}
#endif /* Not VMS or HP */
