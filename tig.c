#ifndef	VERSION
#define VERSION	"tig-0.3"
#endif

#ifndef DEBUG
#define NDEBUG
#endif

#include <assert.h>
#include <errno.h>
#include <ctype.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#include <curses.h>

#define ABS(x)		((x) >= 0  ? (x) : -(x))
#define MIN(x, y)	((x) < (y) ? (x) :  (y))

#define ARRAY_SIZE(x)	(sizeof(x) / sizeof(x[0]))
#define STRING_SIZE(x)	(sizeof(x) - 1)

#define SIZEOF_REF	256	/* Size of symbolic or SHA1 ID. */
#define SIZEOF_CMD	1024	/* Size of command buffer. */
#define SIZEOF_REVGRAPH	19	/* Size of revision ancestry graphics. */

#define COLOR_DEFAULT	(-1) /* This color name can be used to refer to the default term colors. */

#define DATE_FORMAT	"%Y-%m-%d %H:%M" /* The format and size of the date column in the main view. */
#define DATE_COLS	STRING_SIZE("2006-04-29 14:21 ")

#define AUTHOR_COLS	20
#define NUMBER_INTERVAL	1 /* The default interval between line numbers. */
#define TABSIZE		8

#define	SCALE_SPLIT_VIEW(height)	((height) * 2 / 3)

#define TIG_LS_REMOTE "git ls-remote . 2>/dev/null"
#define TIG_DIFF_CMD "git show --patch-with-stat --find-copies-harder -B -C %s"
#define TIG_LOG_CMD "git log --cc --stat -n100 %s"
#define TIG_MAIN_CMD "git log --topo-order --stat --pretty=raw %s"
#define TIG_HELP_CMD	"" /* XXX: Needs to be defined to the empty string. */

#define KEY_TAB		'\t' /* Some ascii-shorthands fitted into the ncurses namespace. */
#define KEY_RETURN	'\r'
#define KEY_ESC		27

struct ref {
	char *name;		/* Ref name; tag or head names are shortened. */
	char id[41];		/* Commit SHA1 ID */
	unsigned int tag:1;	/* Is it a tag? */
	unsigned int next:1;	/* For ref lists: are there more refs? */
};

struct int_map {
	const char *name;
	int namelen;
	int value;
};

static inline void string_ncopy(char *dst, const char *src, int dstlen)
{
	strncpy(dst, src, dstlen - 1);
	dst[dstlen - 1] = 0;
}

#define string_copy(dst, src) string_ncopy(dst, src, sizeof(dst))

static char* chomp_string(char *name)
{
	int namelen;

	while (isspace(*name))
		name++;

	namelen = strlen(name) - 1;
	while (namelen > 0 && isspace(name[namelen]))
		name[namelen--] = 0;

	return name;
}

static bool string_nformat(char *buf, size_t bufsize, int *bufpos, const char *fmt, ...)
{
	va_list args;
	int pos = bufpos ? *bufpos : 0;

	va_start(args, fmt);
	pos += vsnprintf(buf + pos, bufsize - pos, fmt, args);
	va_end(args);

	if (bufpos)
		*bufpos = pos;

	return pos >= bufsize ? FALSE : TRUE;
}

#define string_format(buf, fmt, args...) string_nformat(buf, sizeof(buf), NULL, fmt, args)
#define string_format_from(buf, from, fmt, args...) string_nformat(buf, sizeof(buf), from, fmt, args)

#define REQ_INFO \
	REQ_GROUP("View switching") \
	REQ_(VIEW_MAIN,		"Show main view"), \
	REQ_(VIEW_DIFF,		"Show diff view"), \
	REQ_(VIEW_LOG,		"Show log view"), \
	REQ_(VIEW_HELP,		"Show help page"), \
	\
	REQ_GROUP("View manipulation") \
	REQ_(ENTER,		"Enter current line and scroll"), \
	REQ_(NEXT,		"Move to next"), \
	REQ_(PREVIOUS,		"Move to previous"), \
	REQ_(VIEW_NEXT,		"Move focus to next view"), \
	REQ_(VIEW_CLOSE,	"Close the current view"), \
	\
	REQ_GROUP("Cursor navigation") \
	REQ_(MOVE_UP,		"Move cursor one line up"), \
	REQ_(MOVE_DOWN,		"Move cursor one line down"), \
	REQ_(MOVE_PAGE_DOWN,	"Move cursor one page down"), \
	REQ_(MOVE_PAGE_UP,	"Move cursor one page up"), \
	REQ_(MOVE_FIRST_LINE,	"Move cursor to first line"), \
	REQ_(MOVE_LAST_LINE,	"Move cursor to last line"), \
	\
	REQ_GROUP("Scrolling") \
	REQ_(SCROLL_LINE_UP,	"Scroll one line up"), \
	REQ_(SCROLL_LINE_DOWN,	"Scroll one line down"), \
	REQ_(SCROLL_PAGE_UP,	"Scroll one page up"), \
	REQ_(SCROLL_PAGE_DOWN,	"Scroll one page down"), \
	\
	REQ_GROUP("Misc") \
	REQ_(PROMPT,		"Bring up the prompt"), \
	REQ_(SCREEN_UPDATE,	"Update the screen"), \
	REQ_(SCREEN_REDRAW,	"Redraw the screen"), \
	REQ_(SCREEN_RESIZE,	"Resize the screen"), \
	REQ_(SHOW_VERSION,	"Show version information"), \
	REQ_(STOP_LOADING,	"Stop all loading views"), \
	REQ_(TOGGLE_LINENO,	"Toggle line numbers"), \
	REQ_(TOGGLE_REV_GRAPH,	"Toggle revision graph visualization"),

enum request {
#define REQ_GROUP(help)
#define REQ_(req, help) REQ_##req

	REQ_OFFSET = KEY_MAX + 1,
	REQ_INFO

#undef	REQ_GROUP
#undef	REQ_
};

struct request_info {
	enum request request;
	char *help;
};

static struct request_info req_info[] = {
#define REQ_GROUP(help)	{ 0, (help) },
#define REQ_(req, help)	{ REQ_##req, (help) }
	REQ_INFO
#undef	REQ_GROUP
#undef	REQ_
};

static bool opt_line_number	= FALSE;
static bool opt_rev_graph	= TRUE;
static int opt_num_interval	= NUMBER_INTERVAL;
static int opt_tab_size		= TABSIZE;
static enum request opt_request = REQ_VIEW_MAIN;
static char opt_cmd[SIZEOF_CMD]	= "";
static char opt_encoding[20]	= "";
static bool opt_utf8		= TRUE;
static FILE *opt_pipe		= NULL;

#define LINE_INFO \
LINE(DIFF_HEADER,  "diff --git ",	COLOR_YELLOW,	COLOR_DEFAULT,	0), \
LINE(DIFF_CHUNK,   "@@",		COLOR_MAGENTA,	COLOR_DEFAULT,	0), \
LINE(DIFF_ADD,	   "+",			COLOR_GREEN,	COLOR_DEFAULT,	0), \
LINE(DIFF_DEL,	   "-",			COLOR_RED,	COLOR_DEFAULT,	0), \
LINE(DIFF_INDEX,	"index ",	  COLOR_BLUE,	COLOR_DEFAULT,	0), \
LINE(DIFF_OLDMODE,	"old file mode ", COLOR_YELLOW,	COLOR_DEFAULT,	0), \
LINE(DIFF_NEWMODE,	"new file mode ", COLOR_YELLOW,	COLOR_DEFAULT,	0), \
LINE(DIFF_COPY_FROM,	"copy from",	  COLOR_YELLOW,	COLOR_DEFAULT,	0), \
LINE(DIFF_COPY_TO,	"copy to",	  COLOR_YELLOW,	COLOR_DEFAULT,	0), \
LINE(DIFF_RENAME_FROM,	"rename from",	  COLOR_YELLOW,	COLOR_DEFAULT,	0), \
LINE(DIFF_RENAME_TO,	"rename to",	  COLOR_YELLOW,	COLOR_DEFAULT,	0), \
LINE(DIFF_SIMILARITY,   "similarity ",	  COLOR_YELLOW,	COLOR_DEFAULT,	0), \
LINE(DIFF_DISSIMILARITY,"dissimilarity ", COLOR_YELLOW,	COLOR_DEFAULT,	0), \
LINE(DIFF_TREE,		"diff-tree ",	  COLOR_BLUE,	COLOR_DEFAULT,	0), \
LINE(PP_AUTHOR,	   "Author: ",		COLOR_CYAN,	COLOR_DEFAULT,	0), \
LINE(PP_COMMIT,	   "Commit: ",		COLOR_MAGENTA,	COLOR_DEFAULT,	0), \
LINE(PP_MERGE,	   "Merge: ",		COLOR_BLUE,	COLOR_DEFAULT,	0), \
LINE(PP_DATE,	   "Date:   ",		COLOR_YELLOW,	COLOR_DEFAULT,	0), \
LINE(PP_ADATE,	   "AuthorDate: ",	COLOR_YELLOW,	COLOR_DEFAULT,	0), \
LINE(PP_CDATE,	   "CommitDate: ",	COLOR_YELLOW,	COLOR_DEFAULT,	0), \
LINE(PP_REFS,	   "Refs: ",		COLOR_RED,	COLOR_DEFAULT,	0), \
LINE(COMMIT,	   "commit ",		COLOR_GREEN,	COLOR_DEFAULT,	0), \
LINE(PARENT,	   "parent ",		COLOR_BLUE,	COLOR_DEFAULT,	0), \
LINE(TREE,	   "tree ",		COLOR_BLUE,	COLOR_DEFAULT,	0), \
LINE(AUTHOR,	   "author ",		COLOR_CYAN,	COLOR_DEFAULT,	0), \
LINE(COMMITTER,	   "committer ",	COLOR_MAGENTA,	COLOR_DEFAULT,	0), \
LINE(SIGNOFF,	   "    Signed-off-by", COLOR_YELLOW,	COLOR_DEFAULT,	0), \
LINE(DEFAULT,	   "",			COLOR_DEFAULT,	COLOR_DEFAULT,	A_NORMAL), \
LINE(CURSOR,	   "",			COLOR_WHITE,	COLOR_GREEN,	A_BOLD), \
LINE(STATUS,	   "",			COLOR_GREEN,	COLOR_DEFAULT,	0), \
LINE(TITLE_BLUR,   "",			COLOR_WHITE,	COLOR_BLUE,	0), \
LINE(TITLE_FOCUS,  "",			COLOR_WHITE,	COLOR_BLUE,	A_BOLD), \
LINE(MAIN_DATE,    "",			COLOR_BLUE,	COLOR_DEFAULT,	0), \
LINE(MAIN_AUTHOR,  "",			COLOR_GREEN,	COLOR_DEFAULT,	0), \
LINE(MAIN_COMMIT,  "",			COLOR_DEFAULT,	COLOR_DEFAULT,	0), \
LINE(MAIN_DELIM,   "",			COLOR_MAGENTA,	COLOR_DEFAULT,	0), \
LINE(MAIN_TAG,     "",			COLOR_MAGENTA,	COLOR_DEFAULT,	A_BOLD), \
LINE(MAIN_REF,     "",			COLOR_CYAN,	COLOR_DEFAULT,	A_BOLD), \

enum line_type {
#define LINE(type, line, fg, bg, attr) \
	LINE_##type
	LINE_INFO
#undef	LINE
};

struct line_info {
	const char *name;	/* Option name. */
	int namelen;		/* Size of option name. */
	const char *line;	/* The start of line to match. */
	int linelen;		/* Size of string to match. */
	int fg, bg, attr;	/* Color and text attributes for the lines. */
};

static struct line_info line_info[] = {
#define LINE(type, line, fg, bg, attr) \
	{ #type, STRING_SIZE(#type), (line), STRING_SIZE(line), (fg), (bg), (attr) }
	LINE_INFO
#undef	LINE
};

static enum line_type get_line_type(char *line)
{
	int linelen = strlen(line);
	enum line_type type;

	for (type = 0; type < ARRAY_SIZE(line_info); type++)
		/* Case insensitive search matches Signed-off-by lines better. */
		if (linelen >= line_info[type].linelen &&
		    !strncasecmp(line_info[type].line, line, line_info[type].linelen))
			return type;

	return LINE_DEFAULT;
}

static inline int get_line_attr(enum line_type type)
{
	assert(type < ARRAY_SIZE(line_info));
	return COLOR_PAIR(type) | line_info[type].attr;
}

static void init_colors(void)
{
	int default_bg = COLOR_BLACK;
	int default_fg = COLOR_WHITE;
	enum line_type type;

	start_color();

	if (use_default_colors() != ERR) {
		default_bg = -1;
		default_fg = -1;
	}

	for (type = 0; type < ARRAY_SIZE(line_info); type++) {
		struct line_info *info = &line_info[type];
		int bg = info->bg == COLOR_DEFAULT ? default_bg : info->bg;
		int fg = info->fg == COLOR_DEFAULT ? default_fg : info->fg;

		init_pair(type, fg, bg);
	}
}

struct line {
	enum line_type type;
	void *data;		/* User data */
};

static struct view *display[2]; /* The display array of active views and the index of the current view. */
static unsigned int current_view;

#define foreach_view(view, i) for (i = 0; i < ARRAY_SIZE(display) && (view = display[i]); i++)
#define displayed_views()     (display[1] != NULL ? 2 : 1)

static char ref_commit[SIZEOF_REF]	= "HEAD";
static char ref_head[SIZEOF_REF]	= "HEAD";

struct view {
	const char *name;	/* View name */
	const char *cmd_fmt;	/* Default command line format */
	const char *cmd_env;	/* Command line set via environment */
	const char *id;		/* Points to either of ref_{head,commit} */

	struct view_ops *ops;	/* View operations */

	char cmd[SIZEOF_CMD];	/* Command buffer */
	char ref[SIZEOF_REF];	/* Hovered commit reference */
	char vid[SIZEOF_REF];	/* View ID. Set to id member when updating. */

	int height, width;	/* The width and height of the main window */
	WINDOW *win;		/* The main window */
	WINDOW *title;		/* The title window living below the main window */

	unsigned long offset;	/* Offset of the window top */
	unsigned long lineno;	/* Current line number */

	/* If non-NULL, points to the view that opened this view. If this view
	 * is closed tig will switch back to the parent view. */
	struct view *parent;

	unsigned long lines;	/* Total number of lines */
	struct line *line;	/* Line index */
	unsigned long line_size;/* Total number of allocated lines */
	unsigned int digits;	/* Number of digits in the lines member. */

	FILE *pipe;
	time_t start_time;
};

struct view_ops {
	const char *type;/* What type of content being displayed. Used in the title bar. */
	bool (*draw)(struct view *view, struct line *line, unsigned int lineno);/* Draw one line; lineno must be<view->height. */
	bool (*read)(struct view *view, char *data);/* Read one line; updates view->line. */
	bool (*enter)(struct view *view, struct line *line);/* Depending on view, change display based on current line. */
};

static struct view_ops pager_ops;
static struct view_ops main_ops;

#define VIEW_STR(name, cmd, env, ref, ops) { name, cmd, #env, ref, ops }
#define VIEW_(id, name, ops, ref) VIEW_STR(name, TIG_##id##_CMD,  TIG_##id##_CMD, ref, ops)

static bool draw_view_line(struct view *view, unsigned int lineno)
{
	if (view->offset + lineno >= view->lines)
		return FALSE;

	return view->ops->draw(view, &view->line[view->offset + lineno], lineno);
}

static void redraw_view_from(struct view *view, int lineno)
{
	assert(0 <= lineno && lineno < view->height);

	for (; lineno < view->height; lineno++) {
		if (!draw_view_line(view, lineno))
			break;
	}

	redrawwin(view->win);
	wrefresh(view->win);
}

static void redraw_view(struct view *view)
{
	wclear(view->win);
	redraw_view_from(view, 0);
}

static void update_view_title(struct view *view)
{
	if (view == display[current_view])
		wbkgdset(view->title, get_line_attr(LINE_TITLE_FOCUS));
	else
		wbkgdset(view->title, get_line_attr(LINE_TITLE_BLUR));

	werase(view->title);
	wmove(view->title, 0, 0);

	if (*view->ref)
		wprintw(view->title, "[%s] %s", view->name, view->ref);
	else
		wprintw(view->title, "[%s]", view->name);

	if (view->lines || view->pipe) {
		unsigned int view_lines = view->offset + view->height;
		unsigned int lines = view->lines
				   ? MIN(view_lines, view->lines) * 100 / view->lines
				   : 0;

		wprintw(view->title, " - %s %d of %d (%d%%)",
			view->ops->type,
			view->lineno + 1,
			view->lines,
			lines);
	}

	if (view->pipe) {
		time_t secs = time(NULL) - view->start_time;

		if (secs > 2) /* Three git seconds are a long time ... */
			wprintw(view->title, " %lds", secs);
	}

	wmove(view->title, 0, view->width - 1);
	wrefresh(view->title);
}

static void resize_display(void)
{
	int offset, i;
	struct view *base = display[0];
	struct view *view = display[1] ? display[1] : display[0];

	getmaxyx(stdscr, base->height, base->width); /* Setup window dimensions */

	base->height -= 1; /* Make room for the status window. */

	if (view != base) {
		view->width   = base->width; /* Horizontal split. */
		view->height  = SCALE_SPLIT_VIEW(base->height);
		base->height -= view->height;

		view->height -= 1; /* Make room for the title bar. */
	}

	base->height -= 1;/* Make room for the title bar. */

	offset = 0;

	foreach_view (view, i) {
		if (!view->win) {
			view->win = newwin(view->height, 0, offset, 0);

			scrollok(view->win, TRUE);

			view->title = newwin(1, 0, offset + view->height, 0);

		} else {
			wresize(view->win, view->height, view->width);
			mvwin(view->win,   offset, 0);
			mvwin(view->title, offset + view->height, 0);
		}

		offset += view->height + 1;
	}
}

static void redraw_display(void)
{
	struct view *view;
	int i;

	foreach_view (view, i) {
		redraw_view(view);
		update_view_title(view);
	}
}

static void update_display_cursor(void)
{
	struct view *view = display[current_view];

	if (view->lines) {
		wmove(view->win, view->lineno - view->offset, view->width - 1);
		wrefresh(view->win);
	}
}

static bool cursed = FALSE;

static WINDOW *status_win;

static void report(const char *msg, ...)
{
	static bool empty = TRUE;
	struct view *view = display[current_view];

	if (!empty || *msg) {
		va_list args;

		va_start(args, msg);

		werase(status_win);
		wmove(status_win, 0, 0);
		if (*msg) {
			vwprintw(status_win, msg, args);
			empty = FALSE;
		} else {
			empty = TRUE;
		}
		wrefresh(status_win);

		va_end(args);
	}

	update_view_title(view);
	update_display_cursor();
}

static void do_scroll_view(struct view *view, int lines, bool redraw)
{
	view->offset += lines; /* The rendering expects the new offset. */

	assert(0 <= view->offset && view->offset < view->lines);
	assert(lines);

	if (view->height < ABS(lines)) { /* Redraw the whole screen if scrolling is pointless. */
		redraw_view(view);

	} else {
		int line = lines > 0 ? view->height - lines : 0;
		int end = line + ABS(lines);

		wscrl(view->win, lines);

		for (; line < end; line++) {
			if (!draw_view_line(view, line))
				break;
		}
	}

	if (view->lineno < view->offset) { /* Move current line into the view. */
		view->lineno = view->offset;
		draw_view_line(view, 0);

	} else if (view->lineno >= view->offset + view->height) {
		if (view->lineno == view->offset + view->height) {
			wmove(view->win, view->height, 0); /* Clear hidden line so it doesn't show if the view is scrolled up. */
			wclrtoeol(view->win);
		}
		view->lineno = view->offset + view->height - 1;
		draw_view_line(view, view->lineno - view->offset);
	}

	assert(view->offset <= view->lineno && view->lineno < view->lines);

	if (!redraw)
		return;

	redrawwin(view->win);
	wrefresh(view->win);
	report("");
}

static void scroll_view(struct view *view, enum request request)
{
	int lines = 1;

	switch (request) {
	case REQ_SCROLL_PAGE_DOWN:
		lines = view->height;
	case REQ_SCROLL_LINE_DOWN:
		if (view->offset + lines > view->lines)
			lines = view->lines - view->offset;

		if (lines == 0 || view->offset + view->height >= view->lines) {
			report("Cannot scroll beyond the last line");
			return;
		}
		break;

	case REQ_SCROLL_PAGE_UP:
		lines = view->height;
	case REQ_SCROLL_LINE_UP:
		if (lines > view->offset)
			lines = view->offset;

		if (lines == 0) {
			report("Cannot scroll beyond the first line");
			return;
		}

		lines = -lines;
		break;
	}

	do_scroll_view(view, lines, TRUE);
}

static void move_view(struct view *view, enum request request, bool redraw)
{
	int steps;

	switch (request) {
	case REQ_MOVE_FIRST_LINE:
		steps = -view->lineno;
		break;

	case REQ_MOVE_LAST_LINE:
		steps = view->lines - view->lineno - 1;
		break;

	case REQ_MOVE_PAGE_UP:
		steps = view->height > view->lineno
		      ? -view->lineno : -view->height;
		break;

	case REQ_MOVE_PAGE_DOWN:
		steps = view->lineno + view->height >= view->lines
		      ? view->lines - view->lineno - 1 : view->height;
		break;

	case REQ_MOVE_UP:
		steps = -1;
		break;

	case REQ_MOVE_DOWN:
		steps = 1;
		break;
	}

	if (steps <= 0 && view->lineno == 0) {
		report("Cannot move beyond the first line");
		return;

	} else if (steps >= 0 && view->lineno + 1 >= view->lines) {
		report("Cannot move beyond the last line");
		return;
	}

	view->lineno += steps; /* Move the current line */
	assert(0 <= view->lineno && view->lineno < view->lines);

	if (ABS(steps) < view->height) {/* Repaint the old "current" line if we be scrolling */
		int prev_lineno = view->lineno - steps - view->offset;

		wmove(view->win, prev_lineno, 0);
		wclrtoeol(view->win);
		draw_view_line(view,  prev_lineno);
	}

	if (view->lineno < view->offset ||
	    view->lineno >= view->offset + view->height) { /* Check whether the view needs to be scrolled */
		if (steps < 0 && -steps > view->offset) {
			steps = -view->offset;
		} else if (steps > 0) {
			if (view->lineno == view->lines - 1 &&
			    view->lines > view->height) {
				steps = view->lines - view->offset - 1;
				if (steps >= view->height)
					steps -= view->height - 1;
			}
		}

		do_scroll_view(view, steps, redraw);
		return;
	}

	draw_view_line(view, view->lineno - view->offset);/* Draw the current line */

	if (!redraw)
		return;

	redrawwin(view->win);
	wrefresh(view->win);
	report("");
}

static void set_nonblocking_input(bool loading) /* Controls when nodelay should be in effect when polling user input. */
{
	static unsigned int loading_views;

	if ((loading == FALSE && loading_views-- == 1) ||
	    (loading == TRUE  && loading_views++ == 0))
		nodelay(status_win, loading);
}

static void end_update(struct view *view)
{
	if (!view->pipe)
		return;
	set_nonblocking_input(FALSE);
	if (view->pipe == stdin)
		fclose(view->pipe);
	else
		pclose(view->pipe);
	view->pipe = NULL;
}

static bool begin_update(struct view *view)
{
	const char *id = view->id;

	if (view->pipe)
		end_update(view);

	if (opt_cmd[0]) {
		string_copy(view->cmd, opt_cmd);
		opt_cmd[0] = 0;
		view->ref[0] = 0;
	} else {
		const char *format = view->cmd_env ? view->cmd_env : view->cmd_fmt;

		if (!string_format(view->cmd, format, id, id, id, id, id))
			return FALSE;
	}

	if (opt_pipe) { /* Special case for the pager view. */
		view->pipe = opt_pipe;
		opt_pipe = NULL;
	} else {
		view->pipe = popen(view->cmd, "r");
	}

	if (!view->pipe)
		return FALSE;

	set_nonblocking_input(TRUE);

	view->offset = 0;
	view->lines  = 0;
	view->lineno = 0;
	string_copy(view->vid, id);

	if (view->line) {
		int i;

		for (i = 0; i < view->lines; i++)
			if (view->line[i].data)
				free(view->line[i].data);

		free(view->line);
		view->line = NULL;
	}

	view->start_time = time(NULL);

	return TRUE;
}

static struct line* realloc_lines(struct view *view, size_t line_size)
{
	struct line *tmp = realloc(view->line, sizeof(*view->line) * line_size);

	if (!tmp)
		return NULL;

	view->line = tmp;
	view->line_size = line_size;
	return view->line;
}

static bool update_view(struct view *view)
{
	char buffer[BUFSIZ];
	char *line;
	unsigned long lines = view->height;
	int redraw_from = -1;

	if (!view->pipe)
		return TRUE;

	if (view->offset + view->height >= view->lines) /* Only redraw if lines are visible. */
		redraw_from = view->lines - view->offset;

	if (!realloc_lines(view, view->lines + lines))
		goto alloc_error;

	while ((line = fgets(buffer, sizeof(buffer), view->pipe))) {
		int linelen = strlen(line);

		if (linelen)
			line[linelen - 1] = 0;

		if (!view->ops->read(view, line))
			goto alloc_error;

		if (lines-- == 1)
			break;
	}

	{
		int digits;

		lines = view->lines;
		for (digits = 0; lines; digits++)
			lines /= 10;

		if (digits != view->digits) { /* Keep the displayed view in sync with line number scaling. */
			view->digits = digits;
			redraw_from = 0;
		}
	}

	if (redraw_from >= 0) {
		/* If this is an incremental update, redraw the previous line
		 * since for commits some members could have changed when loading the main view. */
		if (redraw_from > 0)
			redraw_from--;

		redraw_view_from(view, redraw_from);/* Incrementally draw avoids flickering. */
	}

	/* Update the title _after_ the redraw so that if the redraw picks up a
	 * commit reference in view->ref it'll be available here. */
	update_view_title(view);

	if (ferror(view->pipe)) {
		report("Failed to read: %s", strerror(errno));
		goto end;

	} else if (feof(view->pipe)) {
		report("");
		goto end;
	}

	return TRUE;

alloc_error:
	report("Allocation failure");

end:
	end_update(view);
	return FALSE;
}

enum open_flags {
	OPEN_DEFAULT = 0,	/* Use default view switching. */
	OPEN_SPLIT = 1,		/* Split current view. */
	OPEN_BACKGROUNDED = 2,	/* Backgrounded. */
	OPEN_RELOAD = 4,	/* Reload view even if it is the current. */
};

static struct ref *refs;
static size_t refs_size;

static struct ref ***id_refs; /* Id <-> ref store */
static size_t id_refs_size;

static struct ref** get_refs(char *id)
{
	struct ref ***tmp_id_refs;
	struct ref **ref_list = NULL;
	size_t ref_list_size = 0;
	size_t i;

	for (i = 0; i < id_refs_size; i++)
		if (!strcmp(id, id_refs[i][0]->id))
			return id_refs[i];

	tmp_id_refs = realloc(id_refs, (id_refs_size + 1) * sizeof(*id_refs));
	if (!tmp_id_refs)
		return NULL;

	id_refs = tmp_id_refs;

	for (i = 0; i < refs_size; i++) {
		struct ref **tmp;

		if (strcmp(id, refs[i].id))
			continue;

		tmp = realloc(ref_list, (ref_list_size + 1) * sizeof(*ref_list));
		if (!tmp) {
			if (ref_list)
				free(ref_list);
			return NULL;
		}

		ref_list = tmp;
		if (ref_list_size > 0)
			ref_list[ref_list_size - 1]->next = 1;
		ref_list[ref_list_size] = &refs[i];

		ref_list[ref_list_size]->next = 0;
		ref_list_size++;
	}

	if (ref_list)
		id_refs[id_refs_size++] = ref_list;

	return ref_list;
}

static void add_pager_refs(struct view *view, struct line *line)
{
	char buf[1024];
	char *data = line->data;
	struct ref **refs;
	int bufpos = 0, refpos = 0;
	const char *sep = "Refs: ";

	assert(line->type == LINE_COMMIT);

	refs = get_refs(data + STRING_SIZE("commit "));
	if (!refs)
		return;

	do {
		struct ref *ref = refs[refpos];
		char *fmt = ref->tag ? "%s[%s]" : "%s%s";

		if (!string_format_from(buf, &bufpos, fmt, sep, ref->name))
			return;
		sep = ", ";
	} while (refs[refpos++]->next);

	if (!realloc_lines(view, view->line_size + 1))
		return;

	line = &view->line[view->lines];
	line->data = strdup(buf);
	if (!line->data)
		return;

	line->type = LINE_PP_REFS;
	view->lines++;
}

static struct view views[] = {
	VIEW_(MAIN,  "main",  &main_ops,  ref_head),
	VIEW_(DIFF,  "diff",  &pager_ops, ref_commit),
	VIEW_(LOG,   "log",   &pager_ops, ref_head),
	VIEW_(HELP,  "help",  &pager_ops, "static"),
};

#define VIEW(req) (&views[(req) - REQ_OFFSET - 1])
static bool pager_read(struct view *view, char *data)
{
	struct line *line = &view->line[view->lines];

	line->data = strdup(data);
	if (!line->data)
		return FALSE;

	line->type = get_line_type(line->data);
	view->lines++;

	if (line->type == LINE_COMMIT &&
	    (view == VIEW(REQ_VIEW_DIFF) ||
	     view == VIEW(REQ_VIEW_LOG)))
		add_pager_refs(view, line);

	return TRUE;
}

struct keymap {
	int alias;
	int request;
};

static struct keymap keymap[] = {
	/* View switching */
	{ 'm',		REQ_VIEW_MAIN },
	{ 'd',		REQ_VIEW_DIFF },
	{ 'l',		REQ_VIEW_LOG },
	{ 'h',		REQ_VIEW_HELP },
	{ '?',		REQ_VIEW_HELP },

	/* View manipulation */
	{ 'q',		REQ_VIEW_CLOSE },
	{ KEY_TAB,	REQ_VIEW_NEXT },
	{ KEY_RETURN,	REQ_ENTER },
	{ KEY_UP,	REQ_PREVIOUS },
	{ KEY_DOWN,	REQ_NEXT },

	/* Cursor navigation */
	{ 'k',		REQ_MOVE_UP },
	{ 'j',		REQ_MOVE_DOWN },
	{ KEY_HOME,	REQ_MOVE_FIRST_LINE },
	{ KEY_END,	REQ_MOVE_LAST_LINE },
	{ KEY_NPAGE,	REQ_MOVE_PAGE_DOWN },
	{ ' ',		REQ_MOVE_PAGE_DOWN },
	{ KEY_PPAGE,	REQ_MOVE_PAGE_UP },
	{ 'b',		REQ_MOVE_PAGE_UP },
	{ '-',		REQ_MOVE_PAGE_UP },

	/* Scrolling */
	{ KEY_IC,	REQ_SCROLL_LINE_UP },
	{ KEY_DC,	REQ_SCROLL_LINE_DOWN },
	{ 'w',		REQ_SCROLL_PAGE_UP },
	{ 's',		REQ_SCROLL_PAGE_DOWN },

	/* Misc */
	{ 'z',		REQ_STOP_LOADING },
	{ 'v',		REQ_SHOW_VERSION },
	{ 'r',		REQ_SCREEN_REDRAW },
	{ 'n',		REQ_TOGGLE_LINENO },
	{ 'g',		REQ_TOGGLE_REV_GRAPH},
	{ ':',		REQ_PROMPT },

	/* wgetch() with nodelay() enabled returns ERR when there's no input. */
	{ ERR,		REQ_SCREEN_UPDATE },

	/* Use the ncurses SIGWINCH handler. */
	{ KEY_RESIZE,	REQ_SCREEN_RESIZE },
};

struct key {
	char *name;
	int value;
};

static struct key key_table[] = {
	{ "Enter",	KEY_RETURN },
	{ "Space",	' ' },
	{ "Backspace",	KEY_BACKSPACE },
	{ "Tab",	KEY_TAB },
	{ "Escape",	KEY_ESC },
	{ "Left",	KEY_LEFT },
	{ "Right",	KEY_RIGHT },
	{ "Up",		KEY_UP },
	{ "Down",	KEY_DOWN },
	{ "Insert",	KEY_IC },
	{ "Delete",	KEY_DC },
	{ "Home",	KEY_HOME },
	{ "End",	KEY_END },
	{ "PageUp",	KEY_PPAGE },
	{ "PageDown",	KEY_NPAGE },
	{ "F1",		KEY_F(1) },
	{ "F2",		KEY_F(2) },
	{ "F3",		KEY_F(3) },
	{ "F4",		KEY_F(4) },
	{ "F5",		KEY_F(5) },
	{ "F6",		KEY_F(6) },
	{ "F7",		KEY_F(7) },
	{ "F8",		KEY_F(8) },
	{ "F9",		KEY_F(9) },
	{ "F10",	KEY_F(10) },
	{ "F11",	KEY_F(11) },
	{ "F12",	KEY_F(12) },
};

static char* get_key(enum request request)
{
	static char buf[BUFSIZ];
	static char key_char[] = "'X'";
	int pos = 0;
	char *sep = "    ";
	int i;

	buf[pos] = 0;

	for (i = 0; i < ARRAY_SIZE(keymap); i++) {
		char *seq = NULL;
		int key;

		if (keymap[i].request != request)
			continue;

		for (key = 0; key < ARRAY_SIZE(key_table); key++)
			if (key_table[key].value == keymap[i].alias)
				seq = key_table[key].name;

		if (seq == NULL &&
		    keymap[i].alias < 127 &&
		    isprint(keymap[i].alias)) {
			key_char[1] = (char) keymap[i].alias;
			seq = key_char;
		}

		if (!seq)
			seq = "'?'";

		if (!string_format_from(buf, &pos, "%s%s", sep, seq))
			return "Too many keybindings!";
		sep = ", ";
	}

	return buf;
}

static void load_help_page(void)
{
	char buf[BUFSIZ];
	struct view *view = VIEW(REQ_VIEW_HELP);
	int lines = ARRAY_SIZE(req_info) + 2;
	int i;

	if (view->lines > 0)
		return;

	for (i = 0; i < ARRAY_SIZE(req_info); i++)
		if (!req_info[i].request)
			lines++;

	view->line = calloc(lines, sizeof(*view->line));
	if (!view->line) {
		report("Allocation failure");
		return;
	}

	pager_read(view, "Quick reference for tig keybindings:");

	for (i = 0; i < ARRAY_SIZE(req_info); i++) {
		char *key;

		if (!req_info[i].request) {
			pager_read(view, "");
			pager_read(view, req_info[i].help);
			continue;
		}

		key = get_key(req_info[i].request);
		if (!string_format(buf, "%-25s %s", key, req_info[i].help))
			continue;

		pager_read(view, buf);
	}
}

static void open_view(struct view *prev, enum request request, enum open_flags flags)
{
	bool backgrounded = !!(flags & OPEN_BACKGROUNDED);
	bool split = !!(flags & OPEN_SPLIT);
	bool reload = !!(flags & OPEN_RELOAD);
	struct view *view = VIEW(request);
	int nviews = displayed_views();
	struct view *base_view = display[0];

	if (view == prev && nviews == 1 && !reload) {
		report("Already in %s view", view->name);
		return;
	}

	if (view == VIEW(REQ_VIEW_HELP)) {
		load_help_page();

	} else if ((reload || strcmp(view->vid, view->id)) &&
		   !begin_update(view)) {
		report("Failed to load %s view", view->name);
		return;
	}

	if (split) {
		display[1] = view;
		if (!backgrounded)
			current_view = 1;
	} else {
		memset(display, 0, sizeof(display));/* Maximize the current view. */
		current_view = 0;
		display[current_view] = view;
	}

	if (nviews != displayed_views() || (nviews == 1 && base_view != display[0]))
		resize_display();

	if (split && prev->lineno - prev->offset >= prev->height) {
		int lines = prev->lineno - prev->offset - prev->height + 1; /* Take the title line into account. */

		do_scroll_view(prev, lines, TRUE);
	}

	if (prev && view != prev) {
		if (split && !backgrounded) {
			update_view_title(prev);/* "Blur" the previous view. */
		}

		view->parent = prev;
	}

	if (view->pipe && view->lines == 0) {
		wclear(view->win); /* Clear the old view and let the incremental updating refill the screen. */
		report("");
	} else {
		redraw_view(view);
		report("");
	}

	if (backgrounded) /* If the view is backgrounded the above calls to report() won't redraw the view title. */
		update_view_title(view);
}


static int view_driver(struct view *view, enum request request)
{
	int i;

	switch (request) {
	case REQ_MOVE_UP:
	case REQ_MOVE_DOWN:
	case REQ_MOVE_PAGE_UP:
	case REQ_MOVE_PAGE_DOWN:
	case REQ_MOVE_FIRST_LINE:
	case REQ_MOVE_LAST_LINE:
		move_view(view, request, TRUE);
		break;

	case REQ_SCROLL_LINE_DOWN:
	case REQ_SCROLL_LINE_UP:
	case REQ_SCROLL_PAGE_DOWN:
	case REQ_SCROLL_PAGE_UP:
		scroll_view(view, request);
		break;

	case REQ_VIEW_MAIN:
	case REQ_VIEW_DIFF:
	case REQ_VIEW_LOG:
	case REQ_VIEW_HELP:
		open_view(view, request, OPEN_DEFAULT);
		break;

	case REQ_NEXT:
	case REQ_PREVIOUS:
		request = request == REQ_NEXT ? REQ_MOVE_DOWN : REQ_MOVE_UP;

		if (view == VIEW(REQ_VIEW_DIFF) &&
		    view->parent == VIEW(REQ_VIEW_MAIN)) {
			bool redraw = display[1] == view;

			view = view->parent;
			move_view(view, request, redraw);
			if (redraw)
				update_view_title(view);
		} else {
			move_view(view, request, TRUE);
			break;
		}
		/* Fall-through */

	case REQ_ENTER:
		if (!view->lines) {
			report("Nothing to enter");
			break;
		}
		return view->ops->enter(view, &view->line[view->lineno]);

	case REQ_VIEW_NEXT:
	{
		int nviews = displayed_views();
		int next_view = (current_view + 1) % nviews;

		if (next_view == current_view) {
			report("Only one view is displayed");
			break;
		}

		current_view = next_view;

		update_view_title(view); /* Blur out the title of the previous view. */
		report("");
		break;
	}
	case REQ_TOGGLE_LINENO:
		opt_line_number = !opt_line_number;
		redraw_display();
		break;

	case REQ_TOGGLE_REV_GRAPH:
		opt_rev_graph = !opt_rev_graph;
		redraw_display();
		break;

	case REQ_PROMPT:
		open_view(view, opt_request, OPEN_RELOAD); /* Always reload^Wrerun commands from the prompt. */
		break;

	case REQ_STOP_LOADING:
		for (i = 0; i < ARRAY_SIZE(views); i++) {
			view = &views[i];
			if (view->pipe)
				report("Stopped loading the %s view", view->name),
			end_update(view);
		}
		break;

	case REQ_SHOW_VERSION:
		report("%s (built %s)", VERSION, __DATE__);
		return TRUE;

	case REQ_SCREEN_RESIZE:
		resize_display();
		/* Fall-through */
	case REQ_SCREEN_REDRAW:
		redraw_display();
		break;

	case REQ_SCREEN_UPDATE:
		doupdate();
		return TRUE;

	case REQ_VIEW_CLOSE:
		/* XXX: Mark closed views by letting view->parent point to the
		 * view itself. Parents to closed view should never be
		 * followed. */
		if (view->parent &&
		    view->parent->parent != view->parent) {
			memset(display, 0, sizeof(display));
			current_view = 0;
			display[current_view] = view->parent;
			view->parent = view;
			resize_display();
			redraw_display();
			break;
		}
		return FALSE;

	default:
		report("Unknown key, press 'h' for help"); /* An unknown key will show most commonly used commands. */
		return TRUE;
	}

	return TRUE;
}

static bool pager_draw(struct view *view, struct line *line, unsigned int lineno)
{
	char *text = line->data;
	enum line_type type = line->type;
	int textlen = strlen(text);
	int attr;

	wmove(view->win, lineno, 0);

	if (view->offset + lineno == view->lineno) {
		if (type == LINE_COMMIT) {
			string_copy(view->ref, text + 7);
			string_copy(ref_commit, view->ref);
		}

		type = LINE_CURSOR;
		wchgat(view->win, -1, 0, type, NULL);
	}

	attr = get_line_attr(type);
	wattrset(view->win, attr);

	if (opt_line_number || opt_tab_size < TABSIZE) {
		static char spaces[] = "                    ";
		int col_offset = 0, col = 0;

		if (opt_line_number) {
			unsigned long real_lineno = view->offset + lineno + 1;

			if (real_lineno == 1 ||
			    (real_lineno % opt_num_interval) == 0) {
				wprintw(view->win, "%.*d", view->digits, real_lineno);

			} else {
				waddnstr(view->win, spaces,
					 MIN(view->digits, STRING_SIZE(spaces)));
			}
			waddstr(view->win, ": ");
			col_offset = view->digits + 2;
		}

		while (text && col_offset + col < view->width) {
			int cols_max = view->width - col_offset - col;
			char *pos = text;
			int cols;

			if (*text == '\t') {
				text++;
				assert(sizeof(spaces) > TABSIZE);
				pos = spaces;
				cols = opt_tab_size - (col % opt_tab_size);

			} else {
				text = strchr(text, '\t');
				cols = line ? text - pos : strlen(pos);
			}

			waddnstr(view->win, pos, MIN(cols, cols_max));
			col += cols;
		}

	} else {
		int col = 0, pos = 0;

		for (; pos < textlen && col < view->width; pos++, col++)
			if (text[pos] == '\t')
				col += TABSIZE - (col % TABSIZE) - 1;

		waddnstr(view->win, text, pos);
	}

	return TRUE;
}

static bool pager_enter(struct view *view, struct line *line)
{
	int split = 0;

	if (line->type == LINE_COMMIT &&
	   (view == VIEW(REQ_VIEW_LOG))) {
		open_view(view, REQ_VIEW_DIFF, OPEN_SPLIT);
		split = 1;
	}

	/* Always scroll the view even if it was split. That way
	 * you can use Enter to scroll through the log view and split open each commit diff. */
	scroll_view(view, REQ_SCROLL_LINE_DOWN);

	/* FIXME: A minor workaround. Scrolling the view will call report("")
	 * but if we are scrolling a non-current view this won't properly update the view title. */
	if (split)
		update_view_title(view);

	return TRUE;
}

static struct view_ops pager_ops = {
	"line",
	pager_draw,
	pager_read,
	pager_enter,
};

struct commit {
	char id[41];			/* SHA1 ID. */
	char title[75];			/* First line of the commit message. */
	char author[75];		/* Author of the commit. */
	struct tm time;			/* Date from the author ident. */
	struct ref **refs;		/* Repository references. */
	chtype graph[SIZEOF_REVGRAPH];	/* Ancestry chain graphics. */
	size_t graph_size;		/* The width of the graph array. */
};

/* Number of bytes used for encoding a UTF-8 character indexed by first byte. Illegal bytes are set one. */
static const unsigned char utf8_bytes[256] = {
	1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
	2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,
	3,3,3,3,3,3,3,3, 3,3,3,3,3,3,3,3, 4,4,4,4,4,4,4,4, 5,5,5,5,6,6,1,1,
};

static inline int unicode_width(unsigned long c)
{
	if (c >= 0x1100 &&
	   (c <= 0x115f				/* Hangul Jamo */
	    || c == 0x2329
	    || c == 0x232a
	    || (c >= 0x2e80  && c <= 0xa4cf && c != 0x303f)
						/* CJK ... Yi */
	    || (c >= 0xac00  && c <= 0xd7a3)	/* Hangul Syllables */
	    || (c >= 0xf900  && c <= 0xfaff)	/* CJK Compatibility Ideographs */
	    || (c >= 0xfe30  && c <= 0xfe6f)	/* CJK Compatibility Forms */
	    || (c >= 0xff00  && c <= 0xff60)	/* Fullwidth Forms */
	    || (c >= 0xffe0  && c <= 0xffe6)
	    || (c >= 0x20000 && c <= 0x2fffd)
	    || (c >= 0x30000 && c <= 0x3fffd)))
		return 2;

	return 1;
}

static inline unsigned long utf8_to_unicode(const char *string, size_t length)
{
	unsigned long unicode;

	switch (length) {
	case 1:
		unicode  =   string[0];
		break;
	case 2:
		unicode  =  (string[0] & 0x1f) << 6;
		unicode +=  (string[1] & 0x3f);
		break;
	case 3:
		unicode  =  (string[0] & 0x0f) << 12;
		unicode += ((string[1] & 0x3f) << 6);
		unicode +=  (string[2] & 0x3f);
		break;
	case 4:
		unicode  =  (string[0] & 0x0f) << 18;
		unicode += ((string[1] & 0x3f) << 12);
		unicode += ((string[2] & 0x3f) << 6);
		unicode +=  (string[3] & 0x3f);
		break;
	case 5:
		unicode  =  (string[0] & 0x0f) << 24;
		unicode += ((string[1] & 0x3f) << 18);
		unicode += ((string[2] & 0x3f) << 12);
		unicode += ((string[3] & 0x3f) << 6);
		unicode +=  (string[4] & 0x3f);
		break;
	case 6:
		unicode  =  (string[0] & 0x01) << 30;
		unicode += ((string[1] & 0x3f) << 24);
		unicode += ((string[2] & 0x3f) << 18);
		unicode += ((string[3] & 0x3f) << 12);
		unicode += ((string[4] & 0x3f) << 6);
		unicode +=  (string[5] & 0x3f);
		break;
	}

	return unicode > 0xffff ? 0 : unicode;
}

/* Calculates how much of string can be shown within the given maximum width
 * and sets trimmed parameter to non-zero value if all of string could not be shown.
 *
 * Additionally, adds to coloffset how many many columns to move to align with
 * the expected position. Takes into account how multi-byte and double-width
 * characters will effect the cursor position.
 *
 * Returns the number of bytes to output from string to satisfy max_width. */
static size_t utf8_length(const char *string, size_t max_width, int *coloffset, int *trimmed)
{
	const char *start = string;
	const char *end = strchr(string, '\0');
	size_t mbwidth = 0;
	size_t width = 0;

	*trimmed = 0;

	while (string < end) {
		int c = *(unsigned char *) string;
		unsigned char bytes = utf8_bytes[c];
		size_t ucwidth;
		unsigned long unicode;

		if (string + bytes > end)
			break;

		/* Change representation to figure out whether
		 * it is a single- or double-width character. */

		unicode = utf8_to_unicode(string, bytes);
		/* FIXME: Graceful handling of invalid unicode character. */
		if (!unicode)
			break;

		ucwidth = unicode_width(unicode);
		width  += ucwidth;
		if (width > max_width) {
			*trimmed = 1;
			break;
		}

		/* The column offset collects the differences between the
		 * number of bytes encoding a character and the number of
		 * columns will be used for rendering said character.
		 *
		 * So if some character A is encoded in 2 bytes, but will be
		 * represented on the screen using only 1 byte this will and up
		 * adding 1 to the multi-byte column offset.
		 *
		 * Assumes that no double-width character can be encoding in
		 * less than two bytes. */
		if (bytes > ucwidth)
			mbwidth += bytes - ucwidth;

		string  += bytes;
	}

	*coloffset += mbwidth;

	return string - start;
}

static bool main_draw(struct view *view, struct line *line, unsigned int lineno)
{
	char buf[DATE_COLS + 1];
	struct commit *commit = line->data;
	enum line_type type;
	int col = 0;
	size_t timelen;
	size_t authorlen;
	int trimmed = 1;

	if (!*commit->author)
		return FALSE;

	wmove(view->win, lineno, col);

	if (view->offset + lineno == view->lineno) {
		string_copy(view->ref, commit->id);
		string_copy(ref_commit, view->ref);
		type = LINE_CURSOR;
		wattrset(view->win, get_line_attr(type));
		wchgat(view->win, -1, 0, type, NULL);

	} else {
		type = LINE_MAIN_COMMIT;
		wattrset(view->win, get_line_attr(LINE_MAIN_DATE));
	}

	timelen = strftime(buf, sizeof(buf), DATE_FORMAT, &commit->time);
	waddnstr(view->win, buf, timelen);
	waddstr(view->win, " ");

	col += DATE_COLS;
	wmove(view->win, lineno, col);
	if (type != LINE_CURSOR)
		wattrset(view->win, get_line_attr(LINE_MAIN_AUTHOR));

	if (opt_utf8) {
		authorlen = utf8_length(commit->author, AUTHOR_COLS - 2, &col, &trimmed);
	} else {
		authorlen = strlen(commit->author);
		if (authorlen > AUTHOR_COLS - 2) {
			authorlen = AUTHOR_COLS - 2;
			trimmed = 1;
		}
	}

	if (trimmed) {
		waddnstr(view->win, commit->author, authorlen);
		if (type != LINE_CURSOR)
			wattrset(view->win, get_line_attr(LINE_MAIN_DELIM));
		waddch(view->win, '~');
	} else {
		waddstr(view->win, commit->author);
	}

	col += AUTHOR_COLS;
	if (type != LINE_CURSOR)
		wattrset(view->win, A_NORMAL);

	if (opt_rev_graph && commit->graph_size) {
		size_t i;

		wmove(view->win, lineno, col);
		/* Using waddch() instead of waddnstr() ensures that they'll be rendered correctly for the cursor line. */
		for (i = 0; i < commit->graph_size; i++)
			waddch(view->win, commit->graph[i]);

		col += commit->graph_size + 1;
	}

	wmove(view->win, lineno, col);

	if (commit->refs) {
		size_t i = 0;

		do {
			if (type == LINE_CURSOR)
				;
			else if (commit->refs[i]->tag)
				wattrset(view->win, get_line_attr(LINE_MAIN_TAG));
			else
				wattrset(view->win, get_line_attr(LINE_MAIN_REF));
			waddstr(view->win, "[");
			waddstr(view->win, commit->refs[i]->name);
			waddstr(view->win, "]");
			if (type != LINE_CURSOR)
				wattrset(view->win, A_NORMAL);
			waddstr(view->win, " ");
			col += strlen(commit->refs[i]->name) + STRING_SIZE("[] ");
		} while (commit->refs[i++]->next);
	}

	if (type != LINE_CURSOR)
		wattrset(view->win, get_line_attr(type));

	{
		int titlelen = strlen(commit->title);

		if (col + titlelen > view->width)
			titlelen = view->width - col;

		waddnstr(view->win, commit->title, titlelen);
	}

	return TRUE;
}

/* Reads git log --pretty=raw output and parses it into the commit struct. */
static bool main_read(struct view *view, char *line)
{
	enum line_type type = get_line_type(line);
	struct commit *commit = view->lines
			      ? view->line[view->lines - 1].data : NULL;

	switch (type) {
	case LINE_COMMIT:
		commit = calloc(1, sizeof(struct commit));
		if (!commit)
			return FALSE;

		line += STRING_SIZE("commit ");

		view->line[view->lines++].data = commit;
		string_copy(commit->id, line);
		commit->refs = get_refs(commit->id);
		commit->graph[commit->graph_size++] = ACS_LTEE;
		break;

	case LINE_AUTHOR:
	{
		char *ident = line + STRING_SIZE("author ");
		char *end = strchr(ident, '<');

		if (!commit)
			break;

		if (end) {
			for (; end > ident && isspace(end[-1]); end--) ;
			*end = 0;
		}

		string_copy(commit->author, ident);

		if (end) { /* Parse epoch and timezone */
			char *secs = strchr(end + 1, '>');
			char *zone;
			time_t time;

			if (!secs || secs[1] != ' ')
				break;

			secs += 2;
			time = (time_t) atol(secs);
			zone = strchr(secs, ' ');
			if (zone && strlen(zone) == STRING_SIZE(" +0700")) {
				long tz;

				zone++;
				tz  = ('0' - zone[1]) * 60 * 60 * 10;
				tz += ('0' - zone[2]) * 60 * 60;
				tz += ('0' - zone[3]) * 60;
				tz += ('0' - zone[4]) * 60;

				if (zone[0] == '-')
					tz = -tz;

				time -= tz;
			}
			gmtime_r(&time, &commit->time);
		}
		break;
	}
	default:
		if (!commit)
			break;

		if (commit->title[0]) /* Fill in the commit title if it has not already been set. */
			break;

		/* Require titles to start with a non-space character at the offset used by git log. */
		/* FIXME: More gracefull handling of titles; append "..." to shortened titles, etc. */
		if (strncmp(line, "    ", 4) || isspace(line[4]))
			break;

		string_copy(commit->title, line + 4);
	}

	return TRUE;
}

static bool main_enter(struct view *view, struct line *line)
{
	enum open_flags flags = display[0] == view ? OPEN_SPLIT : OPEN_DEFAULT;

	open_view(view, REQ_VIEW_DIFF, flags);
	return TRUE;
}

static struct view_ops main_ops = {
	"commit",
	main_draw,
	main_read,
	main_enter,
};

static enum request get_request(int key)
{
	int i;

	for (i = 0; i < ARRAY_SIZE(keymap); i++)
		if (keymap[i].alias == key)
			return keymap[i].request;

	return (enum request) key;
}

static void init_display(void)
{
	int x, y;

	if (isatty(STDIN_FILENO)) { /* Initialize the curses library */
		cursed = !!initscr();
	} else {
		FILE *io = fopen("/dev/tty", "r+"); /* Leave stdin and stdout alone when acting as a pager. */

		cursed = !!newterm(NULL, io, io);
	}

	nonl();         /* Tell curses not to do NL->CR/NL on output */
	cbreak();       /* Take input chars one at a time, no wait for \n */
	noecho();       /* Don't echo input */
	leaveok(stdscr, TRUE);

	if (has_colors())
		init_colors();

	getmaxyx(stdscr, y, x);
	status_win = newwin(1, 0, y - 1, 0);

	keypad(status_win, TRUE); /* Enable keyboard mapping */
	wbkgdset(status_win, get_line_attr(LINE_STATUS));
}


#define __NORETURN __attribute__((__noreturn__))

int main(int argc, char *argv[])
{
	struct view *view;
	enum request request;
	size_t i;

	for (i = 0; i < ARRAY_SIZE(views) && (view = &views[i]); i++)
		view->cmd_env = getenv(view->cmd_env);

	request = opt_request;

	init_display();

	while (view_driver(display[current_view], request)) {
		int key;
		int i;

		foreach_view (view, i)
			update_view(view);

		key = wgetch(status_win);/* Refresh, accept single keystroke of input */
		request = get_request(key);
	}

	if (cursed)
		endwin();

	return 0;
}

