/*** includes ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE



#include <fcntl.h>
#include <unistd.h>
#include <termios.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <stdarg.h>

/*** defines ***/

#define  CTRL_KEY(k) ((k) & 0x1f)
#define KILO_VERSION "0.0.1"
#define KILO_TAB_STOP 8
#define KILO_QUIT_TIMES 3

// internal representation of all our important keys
// keyboard keys for special operations are converted to this enum
// and the enum will expand as more special keys are added
enum editorKey {
	BACKSPACE = 127,
	ARROW_LEFT = 1000,
	ARROW_RIGHT,
	ARROW_UP,
	ARROW_DOWN,
	
	//makes the cursor go to the top/bottom of the screen
	PAGE_UP,
	PAGE_DOWN,

	// makes the cursor go to the start/end of a line
	HOME_KEY,
	END_KEY,

	// you know what this does
	DEL_KEY
};

// an enum used to highlight different types of characters
enum editorHighlight {
	HL_NORMAL = 0,
	HL_COMMENT,
	HL_MLCOMMENT,
	HL_KEYWORD1,
	HL_KEYWORD2,
	HL_STRING,
	HL_NUMBER,
	HL_MATCH
};

// flags for enabling and disabling certain highlights
# define HL_HIGHLIGHT_NUMBERS (1<<0)
# define HL_HIGHLIGHT_STRINGS (1<<1)

/*** data ***/

// a structure for enabling syntax
// at the start of the program, the filename is analyzed against
// a list below and the global struct is updated
// with the relevant information for the filetype
struct editorSyntax {
	// the filetype that is displayed to the user
	char* filetype;
	// the file endings and names that match the given filetype ".c",".cpp",".rs", etc.
	char** filematch;
	// the characters that signify the start of a single line comment
	char* singleline_comment_start;
	// all special keywords for the filetype "break","if","for", etc.
	char** keywords;
	// start and end for multiline comments
	char* multiline_comment_start;
	char* multiline_comment_end;
	// flags for what highlighting is enabled and disabled
	int flags;
}; 

//this represents a row of text
//
typedef struct erow {
	// index of the erow in the file
	int idx;

	// this stores the raw characters... 
	int size;
	char *chars;
	
	// ... and this stores the rendered version
	// tabs and special characters will look different here
	int rsize;
	char* render;

	// this array is the same size as the render arraw
	// and essentially adds metadata to the values
	// in render saying how they should be highlighted
	unsigned char* hl;

	// say if this row is parat of a multiline comment
	int hl_open_comment;
} erow;

struct editorConfig {
	// represents where the cursor is supposed to be located
	int cx;
	int cy;

	// for rendering tabs and special characters,
	// the x coordinate needs to be translated from cx to rx
	int rx;

	int rowoff;
	int coloff;

	// the size of the terminal window. 
	// used to constrain the cursor and 
	// keep all printed characters on the screen
	int screenrows;
	int screencols;

	// this array of rows has all the text in the file
	int numrows;
	erow* row;

	// the filename is stored here for the status bar
	char* filename;

	// for the status message and when it happened
	char statusmsg[80];
	time_t statusmsg_time;
	
	// this is the original state of the terminal before the program is started
	// it's used reset the terminal back to normal at the end of the program
	struct termios orig_termios;

	// a flag to show that the file has been modified
	// this is used when exiting to prompt the user if they want to 
	// exit with changes
	int dirty;

	// the syntax configuration for this filetype 
	struct editorSyntax *syntax;
};

struct editorConfig E;

/*** filetypes ***/

// specific extensions for the c/c++ filetypes
char *C_HL_extensions[] = { ".c",".h",".cpp",NULL};
char* C_HL_keywords[] = {
	"switch", "if", "while", "for", "break", "continue", "return", "else", "struct", "union", "typedef", "static", "enum", "class", "case", "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|", "void|", NULL
};

// "Highlight Database" this stores the configurations for all supported filetypes.
// cann be added to to include more filetypes
struct editorSyntax HLDB[] = {
	{
		"c",
		C_HL_extensions,
		"//",
		C_HL_keywords,
		"/*",
		"*/",
		HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
	},
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/*** prototypes ***/

void editorSetStatusMessage(const char* fmt, ...);
void editorRefreshScreen();
char* editorPrompt(char *prompt, void (*callback)(char*, int));


/*** terminal ***/

void die( const char *s) {

	// clear the scren and set the cursor to the top 
	write(STDOUT_FILENO,"\x1b[2J",4);
	write(STDOUT_FILENO,"\x1b[H",3);

	perror(s);
	exit(1);
}

void disableRawMode() {
	if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
		die("tcsetattr");
}

void enableRawMode() {
	// get the current attributes from the terminal
	if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetattr");
	atexit(disableRawMode);
	
	// create a copy of the current struct and modify it with the following flags
	//  they do a lot of individual things
	//  the end result is that there are no special
	//  actions that will result from keyboard combinations
	//  we basically disable CTRL + C and CTRL + V 
	//  as well as a bunch of other things
	struct termios raw = E.orig_termios;
	raw.c_iflag &= ~(BRKINT | IXON | ICRNL| ISTRIP| INPCK);
	raw.c_oflag &= ~(OPOST);
	raw.c_cflag |= (CS8);
	raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
	raw.c_cc[VMIN] = 0;
	raw.c_cc[VTIME] = 1;

	// take the struct for the attributes
	// and overwrite the current attributes 
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

int editorReadKey() {
	int nread;
	char c;
	while ((nread = read(STDIN_FILENO,&c,1)) != 1) {
		if (nread == -1 && errno != EAGAIN) die("read");
	}

	// if the character read is an escape sequesnce, do some thing different
	if (c == '\x1b') {
		// use this buffer to read in special charafters
		char seq[3];
		if(read(STDIN_FILENO,&seq[0],1) != 1) return '\x1b';
		if(read(STDIN_FILENO,&seq[1],1) != 1) return '\x1b';

		// if the sequence starts  with '[' go here
		if (seq[0] == '[') {
			if (seq[1] >= '0' && seq[1] <='9') {
				// read in a third character and see if it is a '~'
				// if so, then we look at the numbers and return
				// a value accordingly
				if (read(STDIN_FILENO,&seq[2],1) != 1) return '\x1b';
				if (seq[2] == '~') {
					switch(seq[1]) {
						case '1': return HOME_KEY;
						case '3': return DEL_KEY;
						case '4': return END_KEY;
						case '5': return PAGE_UP;
						case '6': return PAGE_DOWN;
						case '7': return HOME_KEY;
						case '8': return END_KEY;
					}
				}
			} else {

				// \xb1 + [ + A is the up arrow's character sequence
				// similar for down arrow, left and right arrow.
				// this switch converts the character sequence to the correct internal
				// representation (enum)
				switch (seq[1]) {
					case 'A': return ARROW_UP;
					case 'B': return ARROW_DOWN;
					case 'C': return ARROW_RIGHT;
					case 'D': return ARROW_LEFT;
					case 'H': return HOME_KEY;
					case 'F': return END_KEY;
				}
			}
		// if the escape sequence doesnt start with '[' then see if it starts with 'O'
		} else if (seq[0] == 'O') {
			// if it does, then see what letter follows the 'O'
			switch(seq[1]) {
				case 'H': return HOME_KEY;
				case 'F': return END_KEY;
			}
		}
		
		// if none of our cases match, then just return the escape sequence character
		return '\x1b';
		
	} else {
		return c;
	}
}

int getCursorPosition( int* rows, int* cols) {
	char buf[32];
	unsigned int i = 0;
	// this 'n' command returns an escape sequence
	// that shows the current position of the cursor
	// ex: <esc>[24;25R
	if(write(STDOUT_FILENO, "\x1b[6n",4) != 4) return -1;
	
	// here, we read the result of the above write operation
	// into a buffer. 'R' is the end of the escape sequence so we break after that 
	while (i < sizeof(buf) - 1) {
		if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
		if (buf[i] == 'R') break;
		i++;
	}

	// to make the buffer a valid string, we have to replace the final character
	// ('R' currently) with the null character
	buf[i] = '\0';

	// final check to make sure that an escape sequence was read
	if(buf[0] != '\x1b' || buf[1] != '[') return -1;

	// sscanf reads in the first parameter as a string
	// and stores the values that match the second string
	// in the following variables
	// kinda like a primitive regex
	if(sscanf(&buf[2],"%d;%d",rows,cols) != 2) return -1;

	return 0;
}

int getWindowSize(int* rows, int* cols) {
	struct winsize wz;

	// first line tries to get the window size the 'easy' way
	// through the ioctl() function. the TIOCGWINSZ operatoin
	// will store the window size in the winsize data structure
	if(ioctl(STDOUT_FILENO,TIOCGWINSZ,&wz) == -1 || wz.ws_col == 0) {
		// if the easy way failes, then we yse the command \x1b[<num>C and \x1b[<num>B
		// to position the cursor at the end of the program window
		// we then call getCursorPosition() to find the window size
		if(write(STDOUT_FILENO,"\x1b[999C\x1b[999B",12) != 12) return -1;
		return getCursorPosition(rows,cols);
	} else {
		*rows = wz.ws_row;
		*cols = wz.ws_col;
	}
	return 0;
	
}

/*** syntax highlight ***/

int is_separator(int c) {
	return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];",c) != NULL;
}

// update the syntax array to match the size of the render array
// initialise it all to normal, and set values in render to the correct
// syntax highlight
void editorUpdateSyntax(erow* row) {
	// update the size to match the size of the render array
	row->hl = realloc(row->hl, row->rsize);
	// set all values to start at normal
	memset(row->hl,HL_NORMAL,row->rsize);

	// if there is no filetype syntax, then we do no highlighting
	if (E.syntax == NULL) return;

	// store quick names for the different comments
	char* scs = E.syntax->singleline_comment_start;
	char* mcs = E.syntax->multiline_comment_start;
	char* mce = E.syntax->multiline_comment_end;

	// store the length of the comments and 
	// at the same time see if htey are not null/emtpy
	int scs_len = scs ? strlen(scs) : 0;
	int mcs_len = mcs ? strlen(mcs) : 0;
	int mce_len = mce ? strlen(mce) : 0;

	// store a shorthand for all the keywords 
	char** keywords = E.syntax->keywords;

	// store if the previous character was a separator
	int prev_sep = 1;
	// store if we are traversing through a string
	int in_string = 0;
	// store if we are traversing through a multiline comment
	int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);

	int i = 0;
	while (i < row->rsize) {
		char c = row->render[i];
		// store the previous highlight value
		unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;
		
		// first, see if there is a single line comment
		// if we are in a string or multiline comment though
		// we skip over this 
		if (scs_len && !in_string && !in_comment) {
			if(!strncmp(&row->render[i],scs, scs_len)) {
				// if we are at the start of a single line comment
				// then we want to write the rest of the row as a comment
				// and can break out of the loop to the end of the function
				memset(&row->hl[i],HL_COMMENT, row->rsize - i);
				break;
			}
		}
		
		// if we're not in a string, see if we are in a multiline comment
		if (mcs_len && mce_len && !in_string) {
			if(in_comment) {
				// if we are already in a multiline comment,
				// set the current character and see if 
				// we are at the end of the comment
				row->hl[i] = HL_MLCOMMENT;
				if(!strncmp(&row->render[i],mce,mce_len)) {
					memset(&row->hl[i], HL_MLCOMMENT,mce_len);
					i += mce_len;
					in_comment = 0;
					prev_sep = 1;
					continue;
				} else {
					i++;
					continue;
				}
				// if we are not alredy in a comment, see if we 
				// are at the start of a comment, set the HL array if so
			} else if (!strncmp(&row->render[i], mcs, mcs_len)) {
				memset(&row->hl[i],HL_MLCOMMENT,mcs_len);
				in_comment = 1;
				i += mcs_len;
				continue;
			}
		}

		// if the string flag is selected, we now check if we are in a string
		if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
			// if we are already in a string, then set the current character
			// to be a string
			if(in_string) {
				row->hl[i] = HL_STRING;
				// if something is escaped, highlight it and skip
				// over it
				if (c == '\\' && i + 1 < row->rsize) {
					row->hl[i+1] = HL_STRING;
					i = i + 2;
					continue;
				}

				// if the current char matches the start of the string
				// (stored in in_string) then we end the string 
				if (c == in_string) in_string = 0;
				prev_sep = 1;
				i++;
				continue;
				
			} else {
				// strings with double and single quotes
				// currently supported
				if(c == '"' || c == '\'') {
					// save which char started the string 
					in_string = c;
					row->hl[i] = HL_STRING;
					i++;
					continue;
				}

			}
		}

		// if numbers are enabled, then we check if it's a number
		if(E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
			// need to check if the character is a digit, is alone or only with other numbers
			// and if there is a decimal
			if (((isdigit(c)) && (prev_sep || prev_hl == HL_NUMBER)) || ((c == '.') && (prev_hl == HL_NUMBER))) {
				row->hl[i] = HL_NUMBER;
				i++;
				prev_sep = 0;
				continue;
			}
		}

		// now, we check for keywords
		if(prev_sep) {
			int j;
			for (j = 0; keywords[j]; j++) {
				// see if the keyword ends in a pipe
				// that means its a keyword2 to be highlighted
				int klen = strlen(keywords[j]);
				int kw2 = keywords[j][klen - 1] == '|';;
				if (kw2) klen--;

				// if the strings match and ther is not a separater after, then we can set the 
				// highlight array to be the appropriate keyword
				if (!strncmp(&row->render[i], keywords[j],klen) && is_separator(row->render[i + klen])) {
					memset(&row->hl[i],kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
					i += klen;
				       	break;
				}
			}

			// if we found a keyword, then set the prev_sep to 0 and go to next iteration
			if (keywords[j] != NULL) {
				prev_sep = 0;
				continue;
			}
		}
		prev_sep = is_separator(c);

		i++;
	}
	
	// if the status of a multiline comment changed
	// then we need to recurse through the subsequent rows before continuing
	// the base case is when the hl_open_comment status matches the in_comment status
	// for a row after it is processed.
	int changed = row->hl_open_comment != in_comment;
	row->hl_open_comment = in_comment;
	if(changed && row->idx + 1 < E.numrows) {
		editorUpdateSyntax(&E.row[row->idx + 1]);
	}
}

// given a syntax enum value, return
// the correct escape sequence color code for it 
int editorSyntaxtoColor(int hl) {
	switch(hl) {
		case HL_MLCOMMENT:
		case HL_COMMENT: return 36;
		case HL_KEYWORD1: return 33;
		case HL_KEYWORD2: return 32;
		case HL_NUMBER: return 31;
		case HL_STRING: return 35;
		case HL_MATCH: return 34;
		default: return 37;
	}
}

// here we select the correct syntax highlighing from the highlight databse (HLDB)
void editorSelectSyntaxHighlight() {
	E.syntax = NULL;
	// if no filename, then no syntax
	if (E.filename == NULL) return;

	// get a pointer to the substring with the extension
	char* ext = strrchr(E.filename, '.');

	unsigned int j;
	for (j = 0; j < HLDB_ENTRIES; j++) {
		struct editorSyntax *s = &HLDB[j];
		unsigned int i = 0;
		while (s->filematch[i]) {
				// loop through the db and see if the extention (ext) of the current file matches
				// the extensions of an entry in the db....
				int is_ext = (s->filematch[i][0] == '.');
				if((is_ext && ext && !strcmp(ext,s->filematch[i])) || (!is_ext && strstr(E.filename,s->filematch[i]))) {
				// if os, then we set the syntax
				E.syntax = s;

				// if this function is called when the file is saved
				// then we need to update all rows with the new syntax
				int filerow;
				for (filerow = 0; filerow < E.numrows; filerow++) {
					editorUpdateSyntax(&E.row[filerow]);
				}
				return;
			}
			i++;
		}
	}
}

/*** row operations ***/

// conver the cx coordinate to a rendered rx coordinate
// this is use for tabs and special characters in the future
int editorRowCxToRx(erow *row, int cx) {
	int rx = 0;
	int j;

	// for tabs, move the cursor forward to the next tab stop (rx % KILO_TAB_STOP)
	for (j = 0; j < cx; j++) {
		if (row->chars[j] == '\t')
			rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
		rx++;
	}
	return rx;
}

// convert a render index to a coordinate index
// used in searching I think
int editorRowRxToCx(erow* row, int rx) {
	int cur_rx = 0;
	int cx;
	for (cx = 0; cx < row->size; cx++) {
		if (row->chars[cx] == '\t')
			cur_rx += (KILO_TAB_STOP - 1) - (cur_rx & KILO_TAB_STOP);
		cur_rx++;

		if (cur_rx > rx)
			return cx;
	}

	return cx;
}

// update the render and highligh arrays to match the raw character array of a row
void editorUpdateRow(erow *row) {
	int tabs = 0;
	int j;

	// count the tabs
	for (j = 0; j < row->size; j++) {
		if (row->chars[j] == '\t') tabs++;
	}

	//allocate memory for the chars + tabs + null char
	free(row->render);
	row->render = malloc(row->size + tabs*(KILO_TAB_STOP - 1) + 1);

	int idx = 0;
	for(j = 0; j < row->size; j++) {
		// if a tab, then add spaces until the next tab stop at idx %KILO_TAB_STOP
		if (row->chars[j] == '\t') {
			row->render[idx++] = ' ';
			while (idx % KILO_TAB_STOP != 0) row->render[idx++] = ' ';
		} else {
			//add the character to the render array
			row->render[idx++] = row->chars[j];
		}
	}

	// add null char at end to make it print friendly
	row->render[idx] = '\0';
	row->rsize=idx;

	// at the very end, update the syntax array to match
	editorUpdateSyntax(row);
}

// used to insert a row at and part of the E.row array
void editorInsertRow(int at, char *s, size_t len) {
	if (at < 0 || at > E.numrows) return;

	// increase the size of the array and move all rows after 'at' down one
	E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
	memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));

	// since we added a row, all subsequent rows need to have their indexes incremented
	int j;
	for ( j = at + 1; j < E.numrows; j++) E.row[at].idx++;	


	// store the current index of the row
	E.row[at].idx = at;
		
	// initialize the new row
	// and copy the input string into it 
	E.row[at].size = len;
	E.row[at].chars = malloc(len + 1);
	memcpy(E.row[at].chars, s, len);
	E.row[at].chars[len] = '\0';

	// rendering stuff wil be empty first 
	E.row[at].rsize =0;
	E.row[at].render = NULL;
	E.row[at].hl = NULL;

	// initialize the row to have no multiline comment on it
	E.row[at].hl_open_comment = 0;

	// this line 'renders' the row
	editorUpdateRow(&E.row[at]);
	

	E.numrows++;
	E.dirty++;
}

// all pointers in the erow struct need to be freed
// this prevents memory leaks
void editorFreeRow(erow *row) {
	free(row->chars);
	free(row->render);
	free(row->hl);
}

// delete a row at a given index
void editorDelRow(int at) {
	if (at < 0 || at > E.numrows) return;
	editorFreeRow(&E.row[at]);
	memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
	
	// if the row is deleted, then we need to subtract the index from
	// all subsequent rows
	for (int j = at; j < E.numrows -1; j++) E.row[j].idx--;

	E.numrows--;
	E.dirty++;
}

// inserts a character within a row
void editorRowInsertChar(erow *row, int at, int c) {
	if (at < 0 || at > row->size) at = row->size;
	
	// make the row bigger for the character
	row->chars = realloc(row->chars, row->size + 2);
	// move all characters after it down one
	memmove(&row->chars[at + 1], &row->chars[at],row->size - at + 1);
	// increase the size
	row->size++;
	// set the character
	row->chars[at] = c;
	// update the render and higlight arrays
	editorUpdateRow(row);
	// tell the program that the file has been modified
	E.dirty++;
}

// add a whole string to the end of a row
void editorRowAppendString(erow *row, char* s, size_t len) {
	// make the row big enough for the new string
	row->chars = realloc(row->chars, row->size + len + 1);
	// add the string to the end of the array
	memcpy(&row->chars[row->size], s, len);
	// increase the length in the struct
	row->size+= len;
	// add the null character
	row->chars[row->size] = '\0';
	// update the render and highlight row
	editorUpdateRow(row);
	// tell the program that the row has been modified
	E.dirty++;
}

// remove a character from the row
void editorRowDelChar(erow *row, int at ) {
	if (at < 0 || at > row->size) return;
	// move all caracters after at back one
	// (overwrites the character to be deleted)
	memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
	// decrement the size in the struct
	row->size--;
	// update the render and highlight structs accordingly
	editorUpdateRow(row);
	// tell the program that the file has been modified
	E.dirty++;
}



/*** editor operations ***/

// takes in the character, and the cursor coordinates
// and passes the information down to editorRowInsertchar to be deleted
void editorInsertChar(int c) {
	if (E.cy == E.numrows) {
		editorInsertRow(E.numrows,"",0);
	}

	editorRowInsertChar(&E.row[E.cy],E.cx,c);
	E.cx++;
}

// inserts a new line where the current cursor is
// handles the case where the cursor is in the middle of the line
void editorInsertNewline() {
	if (E.cx == 0) {
		editorInsertRow(E.cy,"",0);
	} else {
		// if the cursor is in the middle of the line
		erow *row = &E.row[E.cy];
		// pass in the string for the new line, starting from the location of the x cursor
		editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
		row = &E.row[E.cy];
		// clip the current row to only be as big as E.cx
		row->size = E.cx;
		// set a new null character for the row
		row->chars[row->size] = '\0';
		// update the render and highlight arrays accordingly
		editorUpdateRow(row);
	}
	E.cy++;
	E.cx = 0;
}

// takes in the cursor position and deletes the character before it 
void editorDelChar() {
	if (E.cy == E.numrows) return;
	if (E.cx == 0 && E.cy == 0) return;

	erow *row = &E.row[E.cy];

	if (E.cx > 0) {
		editorRowDelChar(row, E.cx - 1);
		E.cx--;
	} else {
		E.cx = E.row[E.cy - 1].size;
		editorRowAppendString(&E.row[E.cy -1], row->chars, row->size);
		editorDelRow(E.cy);
		E.cy--;
	}
}

/*** file i/o ***/

// convers the rows in the E.row struct to a raw string
// in prep to send it to a file
char* editorRowstoString(int *buflen) {
	int totlen = 0;
	int j;
	
	// calculate how long the rows will be 
	// the +1 is to include the new line character	
	for (j = 0; j < E.numrows; j++)
		totlen += E.row[j].size + 1;

	*buflen = totlen;

	// allocate the buffer
	char *buf = malloc(totlen);
	// create a second pointer to traverse the buffer
	// the first pointer is to be returned
	char *p = buf;

	// loop through all rows
	for (j = 0; j < E.numrows; j++) {
		// copy the memory into the buffer
		memcpy(p,E.row[j].chars,E.row[j].size);
		// move the pointer to the end of the string
		p+= E.row[j].size;
		// add the new line character and move the pointer past it
		*p = '\n';
		p++;
	}
	return buf;
}

// open a file if on ewas input to the editor
void editorOpen(char* filename) {
	//store the name of the file globally
	free(E.filename);
	E.filename = strdup(filename);

	editorSelectSyntaxHighlight();

	// some c functions to open the file
	FILE *fp = fopen(filename, "r");
	if (!fp) die("fopen");

	char *line = NULL;
	size_t linecap = 0;
	ssize_t linelen;
	
	// here we loop through every row in the file to add it to the E.rows array
	while ((linelen = getline(&line, &linecap, fp)) != -1) {
		// get rid of the \r and \n at the end of the line
		while(linelen > 0 && (line[linelen - 1] == '\r' || line[linelen - 1] == '\n')) linelen--;

		//add the line from the file to the global E.row array
		editorInsertRow(E.numrows, line, linelen);
	}
	free(line);
	fclose(fp);

	E.dirty = 0;
}

// saves the file to memory
void editorSave() {
	// if the filename is null
	// that means the editor was opened without a file and 
	// so we need to ask for a name and save the file
	if (E.filename == NULL) {
		E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
		if (E.filename == NULL) {
			// editorPrompt returns NULL when the user presses ESC. so we just 
			// tell them that that's what we did
			editorSetStatusMessage("Save Aborted");
			return;
		}
		editorSelectSyntaxHighlight();
	}

	int len;

	// get all the characters from the global struct
	char* buf = editorRowstoString(&len);

	// open th efile
	int fd = open(E.filename, O_RDWR | O_CREAT, 0644);

	// if opening failes, then get outta here
	if (fd == -1) {
		free(buf);
		return;
	}

	// set the file length to the length of the buffer
	// if that fails, get outta here
	if(ftruncate(fd,len) == -1) {
		close(fd);
		free(buf);
		return;
	}
	
	// put all content in the file
	// let the user know that we wrote everything to disk
	// reset the dirty flag
	if(write(fd, buf, len) == len) {
		close(fd);
		free(buf);
		E.dirty = 0;
		editorSetStatusMessage("%d bytes written to disk",len);
		return;
	}

	// if we get here, then something failed when saving
	// let the user know that 
	close(fd);
	free(buf);
	editorSetStatusMessage("Can't save! I/O error: %s",strerror(errno));
	
}

/*** find ***/

// calback used for incremental search
void editorFindCallback(char* query, int key) {
	// holds the location of the last match and the direction we are headed in
	static int last_match = -1;
	static int direction = 1;
	
	// holds the original state 
	// of the highlight array
	// so we can revert back to it when we exit out
	static int saved_hl_line;
	static char* saved_hl = NULL;

	// if we have a previous saved state, then lets reset back to it's original
	if (saved_hl) {
		memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
		saved_hl = NULL;
	}

	// checks for enter and arrow keys to determine what direction we are going
	if (key == '\r' || key == '\x1b') {
		last_match = -1;
		direction = 1;
		return;
	} else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
		direction = 1;
	} else if (key == ARROW_LEFT || key == ARROW_UP) {
		direction = -1;
		
	} else {
		// if we just have a new key, start
		// the search from the beginning of the file again
		last_match = -1;
		direction = 1;
	}

	if (last_match == -1) direction = 1;
	int current = last_match;
	int i;
	// loop through all rows
	for (i = 0; i < E.numrows; i++) {
		// go the the next row
		current += direction;
		// loop around if we are at the beginning/end of the file
		if (current == -1) current = E.numrows - 1;
		else if (current == E.numrows) current = 0;

		// grab the row, test if it has the string from our query	
		erow* row = &E.row[current];
		char* match = strstr(row->render, query);
		if (match) {
			// if so, then move the cursor there 
			last_match = current;
			E.cy = current;
			E.cx = editorRowRxToCx(row, match - row->render);
			// move the offset to the end of the file
			// when we re-render, it will move the offset back up to show where 
			// the cursor is 
			E.rowoff = E.numrows;
			
			// save the raw highlights of the row so we can
			// reset them in the future
			saved_hl_line = current;
			saved_hl = malloc(row->rsize);
			memcpy(saved_hl,row->hl, row->rsize);

			// set all the matching characters to be highlighted
			memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
			break;
		}
	}

}

// function usd to find a string
// prompts the user for input
// and calls the editorFindCallback on each keypress
// to search the file for the user's input
void editorFind() {
	// save the current state of the cursor to reset if the user cancels
	int saved_cx = E.cx;
	int saved_cy = E.cy;
	int saved_coloff = E.coloff;
	int saved_rowoff = E.rowoff;

	// prompt the user
	char* query = editorPrompt("Search: %s (ESC/Arrows/Enter)", editorFindCallback);

	if (query) {
		// query only returns with stuff if successfully used
		free(query);
	} else {
		// if query is null, then user cancelled.
		// reset their state back to what it was 
		E.cx = saved_cx;
		E.cy = saved_cy;
		E.coloff = saved_coloff;
		E.rowoff = saved_rowoff;
	}
}

/*** append buffer ***/
// this is a dynamic string data structure we created to store the information that is displayed

struct abuf {
	char *b;
	int len;
};

// constructor function
#define ABUF_INIT {NULL,0}

void abAppend(struct abuf* ab, const char *s, int len) {
	// realloc takes a current pointer and a new length
	// and tries to allocate new memory that is the size of the 
	// new length. in our case, we want the new length
	// to be the current length (ab->len) and the length of the 
	// string passed in
	char *new = realloc(ab->b, ab->len + len);

	// realloc can return null if it's not successful. catch this
	if (new == NULL) return;

	// copy the new string (s) into the buffer after the old string 
	// replace the old data structure with the new one
	memcpy(&new[ab->len],s,len);
	ab->b = new;
	ab->len += len;
}

void abFree(struct abuf *ab) {
	free(ab->b);
}

/*** output ***/

//here, we implement scrolling, 
// E.rowoff determines where the top left corner is printed.
// It is an offset from the start of the file
// E.coloff + E.rx = where the cursor's x coordinate is 
// E.rowoff + E.cy = where the cursor's y coordinate is 
void editorScroll() {
	// first we must convert the Cx value to it's Rx value
	// this operation is what makes tabs and special characters print differently
	E.rx = 0;
	if (E.cy < E.numrows) {
		E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
	}

	// if the cursor goes above the screen...
	if (E.cy < E.rowoff) {
		E.rowoff = E.cy;
	}
	
	// if the cursor goes below the screen...
	if (E.cy > E.rowoff + E.screenrows) {
		E.rowoff = E.cy - E.screenrows + 1;
	}

	// if the cursor goes to the right of the screen...
	if(E.rx >= E.coloff +  E.screencols) {
		E.coloff = E.rx - E.screencols + 1;
	}

	// if the cursor goes to the left of the screen
	if (E.rx < E.coloff) {
		E.coloff = E.rx;
	}
}

void editorDrawRows(struct abuf *ab) {
	int y = 0;
	for(y = 0; y < E.screenrows; y++) {
		// here we convert the raw cooridnate(y) to the viewable coordinate for scrolling
		int filerow = y + E.rowoff;
		if (filerow >= E.numrows) {
			// print a little welcome message a third of the way down
			if (E.numrows == 0 && y == (E.screenrows / 3)) {
				char welcome[80];

				// snprint works similar to the sscanf. takes in a string with a parameter (%s, %d, etc.)
				// and replaces it with the values to follow
				// this new stirng is then sent to the character buffer used in the first parameter
				int welcomelen = snprintf(welcome,sizeof(welcome), "Kilo editor -- version %s", KILO_VERSION);
			
				// prevent the string from going off the screen
				if(welcomelen > E.screencols) welcomelen = E.screencols;

				// add padding to center the screen
				// (E.screencols - welcomelen) gives the extra
				// space leftover and we divide it by 2
				// to get the space to the left
				int padding = (E.screencols - welcomelen) / 2;
			
				// every line must start with a tilde
				if(padding) {
					abAppend(ab,"~",1);
					padding--;
				}

				// and here we add the padding
				while(padding--) abAppend(ab," ",1);
			        abAppend(ab,welcome,welcomelen);
			} else { // every line must start with a tilde abAppend(ab,"~",1);
				abAppend(ab,"~",1);
			}
		} else {
			int len = E.row[filerow].rsize - E.coloff; // we subtrace E.coloff to make sure we can scroll left and right
			if (len < 0) len = 0;
			if (len > E.screencols) len = E.screencols;
			// grab the render array
			char* c = &E.row[filerow].render[E.coloff];
			// grab the highlight array
			unsigned char* hl = &E.row[filerow].hl[E.coloff];
			int current_color = -1;
			
			// loop through every character in the render/highlight array
			// (they are the same size)
			int j;
			for (j = 0; j < len; j++) {
				if(iscntrl(c[j])) {
					char sym = (c[j] < 26) ? '@' + c[j]: '?';
					abAppend(ab, "\x1b[7m",4);
					abAppend(ab,&sym,1);
					abAppend(ab,"\x1b[m",3);

					if (current_color != -1) {
						char buf[16];
						int clen = snprintf(buf, sizeof(buf),"\x1b[%dm",current_color);
						abAppend(ab,buf,clen);
					}

				} else if(hl[j] == HL_NORMAL) {
					if (current_color != -1) {
						// if the highlight is normal and it wasn't
						// normal before, then we reset the color code
						current_color = -1;
						abAppend(ab,"\x1b[39m",5);
					}
					// append the character to the buffer to be printed
					abAppend(ab,&c[j],1);
				} else {
					// if the highlight wasn't normal,
					// then we get the color from the enum
					int color = editorSyntaxtoColor(hl[j]);

					// if the color is different, then we add the new color excape sequence
					if (color != current_color) {
						current_color = color;
						char buf[16];
						int clen = snprintf(buf, sizeof(buf), "\x1b[%dm",color);
						abAppend(ab,buf,clen);
					}
					// append the character to the buffer to be printed
					abAppend(ab,&c[j],1);
				}
			}

			// reset the color at the very end just in case
			abAppend(ab,"\x1b[39m",5);

		}


		// the K command clears the contesnts of a line. it takes in the following parameters
		// 0: clear everything to the right
		// 1: clear everything to the left
		// 2: clear the whole line
		// it's the line version of the J command
		// the default value is 0 so we use that here to clear the rest of the line
		// past the tilde
		abAppend(ab,"\x1b[K",3);

		abAppend(ab,"\r\n",2);
	}
}

// here we draw a status bar at the bottom of the text editor
// this status bar starts by showing the filename, but will be edited in the future
void editorDrawStatusBar(struct abuf *ab) {
	abAppend(ab,"\x1b[7m",4);// here we set the text to print inverted
	char status[80];
	char rstatus[80];

	// copy the file name into the status buffer made above
	// the %.20s only copies 20 characters of the file 
	int len = snprintf(status, sizeof(status), "%.20s - %d lines %5s",
			E.filename ? E.filename : "[No File]",
			E.numrows,
			E.dirty ? "(modified)":"");

	// this copies the current location of the cursor into the 
	// rstatus buffer
	int rlen = snprintf(rstatus, sizeof(rstatus),"%s %d/%d",E.syntax? E.syntax->filetype: "no ft", E.cy,E.numrows);

	if (len > E.screencols) len = E.screencols;
	abAppend(ab,status,len);

	// this section is to right align the current line # 
	// it prints out spaces until there is only enough room
	// left on the line for the rstatus buffer
	while(len < E.screencols) {
		if (E.screencols - len == rlen) {
			abAppend(ab, rstatus,rlen);
			break;
		} else {		
			abAppend(ab," ",1);
			len++;
		}
	}

	abAppend(ab,"\x1b[m",3);
	abAppend(ab,"\r\n",2);
}

// Append the status message into the append buffer
// starts by inverting the colors with the \x1b[K code
// it only prints the status message if it's under 5 seconds old 
void editorDrawMessageBar(struct abuf *ab) {
	abAppend(ab, "\x1b[K",3);
	int msglen = strlen(E.statusmsg);
	if (msglen > E.screencols) msglen = E.screencols;
	if (msglen && time(NULL) - E.statusmsg_time < 5) 
		abAppend(ab, E.statusmsg, msglen);
}

void editorRefreshScreen() {
	editorScroll();
	// we refresh the screen but writting a whole buffer once
	struct abuf ab = ABUF_INIT; 
	
	// the 25l command hides the cursor
	abAppend(&ab,"\x1b[?25l",6);

	// the J command clears the screen with the following options
	// 0: clear everything to the right of the cursor
	// 1: clear everything to the left of the cursor
	// 2: clear the whole screen
	// we did this previously to clear the old values,
	// but opted for the K command to clear things one line at a time instead
	//abAppend(&ab,"\x1b[2J",4);

	// the H command move sthe cursor to a specified location
	// we specify the location with numbers semicolon separated
	// ex: <esc>[23;30H
	// default is 0;0 so we use that to send the cursor back to the start
	// of the window
	abAppend(&ab,"\x1b[H",3);

	// append all text to the row buffer and the welcome message
	editorDrawRows(&ab);
	editorDrawStatusBar(&ab);
	editorDrawMessageBar(&ab);

	// after adding all text, we move the cursor to it's appropriate position
	// this is located in the global config data structure E
	// and we use snprintf to format the escape sequence and put it in the buf 
	// variable
	char buf[32];
	snprintf(buf,sizeof(buf),"\x1b[%d;%dH",(E.cy - E.rowoff) + 1, (E.rx - E.coloff) + 1);
	abAppend(&ab,buf,strlen(buf));

	// 25h shows the cursor. used to bring it back from hiding
	abAppend(&ab,"\x1b[?25h",6);

	// finally, we write the whole buffer to the screen and free it
	write(STDOUT_FILENO,ab.b,ab.len);
	abFree(&ab);
}

// helper function to set the global status message
// uses a specific header file 
// that makes it easy to take in 
// any number of arguments
void editorSetStatusMessage(const char* fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	vsnprintf(E.statusmsg, sizeof(E.statusmsg),fmt,ap);
	va_end(ap);
	E.statusmsg_time = time(NULL);
}

/*** input ***/

// used to prompt the user for a string
// takes in a callback that can be called everytime the user types in a character
// the callback takes in the current string and the last character the user typed
char* editorPrompt(char* prompt, void (*callback)(char*, int)) {
	// create a buffer to store the user's input in 
	size_t bufsize = 128;
	char* buf = malloc(bufsize);

	size_t buflen = 0;
	buf[0] = '\0';

	while(1) {
		// put the prompt in the status message
		editorSetStatusMessage(prompt, buf);
		editorRefreshScreen();

		// read the users input
		// this makes sure that the user's input won't be interpreted as normal keystrokes
		int c = editorReadKey();
		if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
			// remove a character if backspace
			if (buflen != 0) buf[--buflen] = '\0';
		} else if(c == '\x1b') {
			// if we press escape, reset the status message and call
			// the callback one more time
			editorSetStatusMessage("");
			if (callback) callback(buf,c);
			free(buf);
			return NULL;
		} else if(c == '\r') {
			// if we press enter, then return the user's input 
			// and call the callback one mor etime
			if(buflen !=0) {
				editorSetStatusMessage("");
				if (callback) callback(buf,c);
				return buf;
			}
		} else if (!iscntrl(c) && c < 128) {
			// if a regular character,
			// then add to the buffer
			// if the buffer is full, then make it bigger
			if (buflen == bufsize - 1) {
				bufsize *=2;
				buf = realloc(buf, bufsize);
			}
			buf[buflen++] = c;
			buf[buflen] = '\0';
		}

		// call the callback for each character input
		if (callback) callback(buf,c);
	}
}

void editorMoveCursor(int key) {

	erow* row = (E.cy > E.numrows) ? NULL: &E.row[E.cy];

	// this function is only used for moving the cursor with the arrow keys
	// it looks for the ARROW_<DIR> enum defined in editorKey
	// there is also a check to make sure the cursor is not on the edge of the screen
	// to ensure it does not fly off the screen 
	switch (key) {
		case ARROW_LEFT:
			if (E.cx != 0) {
				E.cx--;
			} else if (E.cy > 0) {
				E.cy--;
				E.cx = E.row[E.cy].size;
			}
			break;
		case ARROW_RIGHT:
			if (row && E.cx < row->size) {
				E.cx++;
			} else if (row && E.cx == row->size) {
				E.cy ++;
				E.cx = 0;
			}
			break;
		case ARROW_UP:
			if (E.cy != 0) {
				E.cy--;
			}
			break;
		case ARROW_DOWN:
			if (E.cy < E.numrows) {
				E.cy++;
			}
			break;
	}

	row = (E.cy > E.numrows) ? NULL: &E.row[E.cy];
	int rowlen = row ? row->size: 0;
	if (E.cx > rowlen) {
		E.cx = rowlen;
	}
}

// main function for processing keys
void editorProcessKeyPress() {
	
	static int quit_times = KILO_QUIT_TIMES;

	// grabs the key from the terminal
	int c = editorReadKey();

	switch(c) {
		case '\r':
			editorInsertNewline();
			break;
		// CTRL + q is used to quit out the program
		case CTRL_KEY('q'):
			if (E.dirty && quit_times > 0) {
				editorSetStatusMessage("WARNING!!! File has unsaved changes."
						"Press Ctrl+q %d more times to quit",quit_times);
				quit_times--;
				return;
			}

			write(STDOUT_FILENO,"\x1b[2J",4);
			write(STDOUT_FILENO,"\x1b[H",3);
			exit(0);
			break;

		// now we can save on ctrl + s!
		case CTRL_KEY('s'):
			editorSave();
			break;

		// set key to top, bottom, start, end of screen accordingly
		case HOME_KEY:
			E.cx = 0;
			break;
		case END_KEY:
			if (E.cy < E.numrows) 
				E.cx = E.row[E.cy].size;
			break;
		
		// find stuf
		case CTRL_KEY('f'):
			editorFind();
			break;

		// for when we want to get rid of characters
		case BACKSPACE:
		case CTRL_KEY('h'):
		case DEL_KEY:
			// delete key is basically moving right and backspacing
			// we we just do that instead of making a near-identical
			// delete function
			if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
			editorDelChar();
			break;
		case PAGE_UP:
		case PAGE_DOWN:
			{
				if (c == PAGE_UP) {
					E.cy = E.rowoff;
				} else if (c == PAGE_DOWN) {
					E.cy = E.rowoff + E.screenrows - 1;
					if (E.cy > E.numrows) E.cy = E.numrows;
				}

				int times = E.screenrows;
				while(times--)
					editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
			}
			break;
		
		// if the key is one of the arrow keys, then we move the cursor
		case ARROW_UP:
		case ARROW_DOWN:
		case ARROW_LEFT:
		case ARROW_RIGHT:
			editorMoveCursor(c);
			break;

		case CTRL_KEY('l'):
		case '\x1b':
			break;

		// cheeky line I wrote, will be replaced eventually
		default:
			editorInsertChar(c);
			break;
	}

	quit_times = KILO_QUIT_TIMES;
}

/*** init ***/

void initEditor() {
	E.cx= 0;
	E.cy= 0;
	E.rx= 0;
	E.numrows= 0;
	E.row = NULL;
	E.rowoff= 0;
	E.coloff= 0;
	E.filename = NULL;
	E.statusmsg[0] = '\0';
	E.statusmsg_time = 0;
	E.dirty = 0;
	E.syntax = NULL;

	// find the size of the window and store it in the global data state
	if(getWindowSize(&E.screenrows,&E.screencols) == -1) die("getWindowSize");
	E.screenrows -= 2;
}

int main(int argc, char* argv[]) {
	enableRawMode();
	initEditor();
	if (argc >= 2) {
		editorOpen(argv[1]);
	}

	editorSetStatusMessage("HELP: Ctrl-S = Save | Ctrl-Q = quit | Ctrl-F = find");

	while(1) {
		// print the new screen
		editorRefreshScreen();

		// interpret the next key pressed
		editorProcessKeyPress();
	}

	return 0;
}

