#include <stdlib.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/ioctl.h>
#include <unistd.h>
#include <errno.h>
#endif


// Get terminal size
// @param[out] columns  Pointer to store terminal width
// @param[out] lines    Pointer to store terminal height
// @param[out] stat     Pointer to store error code
//                      (0: Success, otherwise: Error, Windows: GetLastError(), Unix: ENOTTY/errno)
void get_terminal_size(int *columns, int *lines, int *stat)
{
    /* Initialize outputs to error state */
    *columns = -1;
    *lines = -1;
    *stat = 0;

#ifdef _WIN32
    /* Windows implementation using Console API */
    HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hConsole == INVALID_HANDLE_VALUE)
    {
        *stat = (int)GetLastError(); // Return Windows system error code
        return;
    }

    CONSOLE_SCREEN_BUFFER_INFO csbi;
    if (!GetConsoleScreenBufferInfo(hConsole, &csbi))
    {
        *stat = (int)GetLastError(); // Failed to get console info
        return;
    }

    /* Calculate visible window dimensions */
    *columns = csbi.srWindow.Right - csbi.srWindow.Left + 1;
    *lines = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;

#else
    /* Unix implementation using termios ioctl */
    if (!isatty(STDOUT_FILENO))
    {
        *stat = ENOTTY;
        return;
    }

    struct winsize w;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) == -1)
    {
        *stat = errno; // Return POSIX system error code
        return;
    }

    /* Directly use reported terminal dimensions */
    *columns = w.ws_col;
    *lines = w.ws_row;
#endif
}