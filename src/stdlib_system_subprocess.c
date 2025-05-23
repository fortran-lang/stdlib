#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#else
#define _POSIX_C_SOURCE 199309L
#include <sys/wait.h>
#include <sys/stat.h>  
#include <unistd.h>
#include <time.h>
#include <errno.h>
#include <signal.h>
#endif // _WIN32

// Typedefs
typedef void* stdlib_handle;
typedef int64_t stdlib_pid;


/////////////////////////////////////////////////////////////////////////////////////
// Windows-specific code
/////////////////////////////////////////////////////////////////////////////////////
#ifdef _WIN32

// On Windows systems: create a new process
void process_create_windows(const char* cmd, const char* stdin_stream,  
                    const char* stdin_file, const char* stdout_file, const char* stderr_file, 
                    stdlib_pid* pid) {
                                                  
    STARTUPINFO si;
    PROCESS_INFORMATION pi;
    HANDLE hStdout = NULL, hStderr = NULL;
    SECURITY_ATTRIBUTES sa = { sizeof(SECURITY_ATTRIBUTES), NULL, TRUE };
    FILE* stdin_fp = NULL;
    
    // Initialize null handle
    (*pid) = 0;

    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(STARTUPINFO);
    
    // If possible, we redirect stdout/stderr to file handles directly. 
    // This will override any cmd redirection settings (<>). For stdin

    // Write stdin_stream to stdin_file if provided
    if (stdin_stream && stdin_file) {
        stdin_fp = fopen(stdin_file, "w");
        if (!stdin_fp) {
            fprintf(stderr, "Failed to open stdin file for writing\n");
            return;
        }
        fputs(stdin_stream, stdin_fp);
        fclose(stdin_fp);
    }

    // Open stdout file if provided, otherwise use the null device
    if (stdout_file) {
        hStdout = CreateFile(stdout_file, GENERIC_WRITE, 0, &sa, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
        if (hStdout == INVALID_HANDLE_VALUE) {
            fprintf(stderr, "Failed to open stdout file\n");
            return;
        }
    } else {
        hStdout = CreateFile("NUL", GENERIC_WRITE, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
        if (hStdout == INVALID_HANDLE_VALUE) {
            fprintf(stderr, "Failed to open null device for stdout\n");
            return;
        }
    }
    si.hStdOutput = hStdout;
    si.dwFlags |= STARTF_USESTDHANDLES;

    // Open stderr file if provided, otherwise use the null device
    if (stderr_file) {
        hStderr = CreateFile(stderr_file, GENERIC_WRITE, 0, &sa, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
        if (hStderr == INVALID_HANDLE_VALUE) {
            fprintf(stderr, "Failed to open stderr file\n");
            return;
        }
    } else {
        hStderr = CreateFile("NUL", GENERIC_WRITE, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
        if (hStderr == INVALID_HANDLE_VALUE) {
            fprintf(stderr, "Failed to open null device for stderr\n");
            return;
        }
    }
    si.hStdError = hStderr;
    si.dwFlags |= STARTF_USESTDHANDLES;
    
    // Prepare the command line with redirected stdin
    char* full_cmd;
    size_t cmd_len = strlen(cmd);
    size_t stdin_len = stdin_file ? strlen(stdin_file) : 0;    
    size_t full_cmd_len = cmd_len + stdin_len + 5;
    full_cmd = (char*)malloc(full_cmd_len);
    if (!full_cmd) {
        fprintf(stderr, "Failed to allocate memory for full_cmd\n");
        return;
    }
    
    // Use full_cmd as needed (e.g., pass to CreateProcess)    
    if (stdin_file) {
        snprintf(full_cmd, full_cmd_len, "%s < %s", cmd, stdin_file);
    } else {
        snprintf(full_cmd, full_cmd_len, "%s", cmd);
    }

    // Create the process
    BOOL success = CreateProcess(
        NULL,               // Application name
        full_cmd,           // Command line
        NULL,               // Process security attributes
        NULL,               // Thread security attributes
        TRUE,               // Inherit handles
        0,                  // Creation flags
        NULL,               // Environment variables
        NULL,               // Current directory
        &si,                // STARTUPINFO
        &pi                 // PROCESS_INFORMATION
    );

    // Free the allocated memory
    free(full_cmd);    
    
    if (!success) {
        fprintf(stderr, "CreateProcess failed (%lu).\n", GetLastError());
        return;
    }

    // Close unneeded handles
    if (hStdout) CloseHandle(hStdout);
    if (hStderr) CloseHandle(hStderr);

    // Return the process handle for status queries
    CloseHandle(pi.hThread);  // Close the thread handle
    (*pid) = (stdlib_pid) pi.dwProcessId;
    
}

// Query process state on a Windows system
void process_query_status_windows(stdlib_pid pid, bool wait, bool* is_running, int* exit_code)
{
    int wait_code;
    HANDLE hProcess;
    DWORD dwExitCode,dwPid;
    
    dwPid = (DWORD) pid;    
    
    // Open the process with the appropriate access rights
    hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | SYNCHRONIZE, FALSE, dwPid);
    
    // Error opening the process, likely pid does not exist
    if (hProcess == NULL) {
        *is_running = false;
        *exit_code = -1; 
        return;
    }

    
    if (wait) {
        // Wait for the process to terminate
        wait_code = WaitForSingleObject(hProcess, INFINITE);
    } else {
        // Check if the process has terminated
        wait_code = WaitForSingleObject(hProcess, 0);
    }

    if (wait_code == WAIT_OBJECT_0) {
        // Process has exited, get the exit code
        *is_running = false;        
        if (GetExitCodeProcess(hProcess, &dwExitCode)) {            
            *exit_code = dwExitCode;
        } else {
            *exit_code = -1; // Error retrieving the exit code
        }
    } else if (wait_code == WAIT_TIMEOUT) {
        // Process is still running
        *is_running = true;
        *exit_code = 0;
    } else { // WAIT_FAILED
        // Error occurred
        *is_running = false;
        *exit_code = -1; // Error occurred in WaitForSingleObject
    }

    // Close the process handle
    CloseHandle(hProcess);
}

// Kill a process on Windows by sending a PROCESS_TERMINATE signal. 
// Return true if the operation succeeded, or false if it failed (process does not 
// exist anymore, or we may not have the rights to kill the process).
bool process_kill_windows(stdlib_pid pid) {
    HANDLE hProcess;
    DWORD dwPid;
    
    dwPid = (DWORD) pid;

    // Open the process with terminate rights
    hProcess = OpenProcess(PROCESS_TERMINATE, FALSE, dwPid);

    if (hProcess == NULL) {
        // Failed to open the process; return false
        return false;
    }

    // Attempt to terminate the process
    if (!TerminateProcess(hProcess, 1)) {
        // Failed to terminate the process
        CloseHandle(hProcess);
        return false;
    }

    // Successfully terminated the process
    CloseHandle(hProcess);
    return true;
}

// Check if input path is a directory
bool stdlib_is_directory_windows(const char *path) {
    DWORD attrs = GetFileAttributesA(path);
    return    (attrs != INVALID_FILE_ATTRIBUTES)  // Path exists
           && (attrs & FILE_ATTRIBUTE_DIRECTORY); // Path is a directory
}

#else // _WIN32

/////////////////////////////////////////////////////////////////////////////////////
// Unix-specific code
/////////////////////////////////////////////////////////////////////////////////////
void process_query_status_unix(stdlib_pid pid, bool wait, bool* is_running, int* exit_code)
{
    int status;    
    int wait_code;
    
    // Wait or return immediately if no status change
    int options = wait ? 0 : WNOHANG; 

    // Call waitpid to check the process state
    wait_code = waitpid(pid, &status, options);

    if (wait_code > 0) {
        // Process state was updated
        if (WIFEXITED(status)) {
            *is_running = false;
            
            // Get exit code
            *exit_code = WEXITSTATUS(status); 
        } else if (WIFSIGNALED(status)) {
            *is_running = false;
            
            // Use negative value to indicate termination by signal
            *exit_code = -WTERMSIG(status); 
        } else {
            // Process is still running: no valid exit code yet
            *is_running = true; 
            *exit_code = 0; 
        }
    } else if (wait_code == 0) {
        // No status change; process is still running
        *is_running = true;
        *exit_code = 0;
    } else {
        // Error occurred
        *is_running = false;
        *exit_code = -1; // Indicate an error
    }
}

// Kill a process by sending a SIGKILL signal. Return .true. if succeeded, or false if not. 
// Killing process may fail due to unexistent process, or not enough rights to kill.
bool process_kill_unix(stdlib_pid pid) {
    // Send the SIGKILL signal to the process
    if (kill(pid, SIGKILL) == 0) {
        // Successfully sent the signal
        return true;
    }

    // If `kill` fails, check if the process no longer exists
    if (errno == ESRCH) {
        // Process does not exist
        return true; // Already "terminated"
    }

    // Other errors occurred
    return false;
}


// On UNIX systems: just fork a new process. The command line will be executed from Fortran.
void process_create_posix(stdlib_pid* pid) 
{

    (*pid) = (stdlib_pid) fork();
}

// On UNIX systems: check if input path is a directory
bool stdlib_is_directory_posix(const char *path) {
    struct stat sb;
    return stat(path, &sb) == 0 && S_ISDIR(sb.st_mode);
}

#endif // _WIN32

/////////////////////////////////////////////////////////////////////////////////////
// Cross-platform interface
/////////////////////////////////////////////////////////////////////////////////////

// Cross-platform interface: query directory state
bool stdlib_is_directory(const char *path) {
    // Invalid input
    if (path == NULL || strlen(path) == 0) return false;  
#ifdef _WIN32
    return stdlib_is_directory_windows(path);
#else
    return stdlib_is_directory_posix(path);
#endif // _WIN32
}

// Create or fork process
void process_create(const char* cmd, const char* stdin_stream, const char* stdin_file, 
                    const char* stdout_file, const char* stderr_file, 
                    stdlib_pid* pid) {                                                  
#ifdef _WIN32
    process_create_windows(cmd, stdin_stream, stdin_file, stdout_file, stderr_file, pid);
#else
    process_create_posix(pid);
#endif // _WIN32
}

// Cross-platform interface: query process state
void process_query_status(stdlib_pid pid, bool wait, bool* is_running, int* exit_code)
{
#ifdef _WIN32
   process_query_status_windows(pid, wait, is_running, exit_code);
#else
   process_query_status_unix   (pid, wait, is_running, exit_code);
#endif // _WIN32
}

// Cross-platform interface: kill process by ID
bool process_kill(stdlib_pid pid)
{
#ifdef _WIN32
   return process_kill_windows(pid);
#else
   return process_kill_unix(pid);
#endif // _WIN32
}

// Cross-platform interface: sleep(seconds)
void process_wait(float seconds)
{
#ifdef _WIN32
   DWORD dwMilliseconds = (DWORD) (seconds * 1000);
   Sleep(dwMilliseconds);
#else
   int ierr;
   
   unsigned int ms = (unsigned int) (seconds * 1000);
   struct timespec ts_remaining =
   { 
     ms / 1000, 
     (ms % 1000) * 1000000L 
   };   
   
   do
   {
     struct timespec ts_sleep = ts_remaining;
     ierr = nanosleep(&ts_sleep, &ts_remaining);
   } 
   while ((EINTR == errno) && (-1 == ierr));
      
   if (ierr != 0){
     switch(errno){
       case EINTR:
         fprintf(stderr, "nanosleep() interrupted\n");
         break;
       case EINVAL:
         fprintf(stderr, "nanosleep() bad milliseconds value\n");
         exit(EINVAL);
       case EFAULT:
         fprintf(stderr, "nanosleep() problem copying information to user space\n");
         exit(EFAULT);
       case ENOSYS:
         fprintf(stderr, "nanosleep() not supported on this system\n");
         exit(ENOSYS);
       default:
         fprintf(stderr, "nanosleep() error\n");
         exit(1);
     }
   }   
   
#endif // _WIN32    
}

// Returns the cross-platform file path of the null device for the current operating system.
const char* process_null_device(size_t* len) 
{
#ifdef _WIN32    
        (*len) = strlen("NUL");
        return "NUL";
#else
        (*len) = strlen("/dev/null");
        return "/dev/null";
#endif
}

// Returns a boolean flag if macro _WIN32 is defined
bool process_is_windows()
{
#ifdef _WIN32
   return true;
#else
   return false;
#endif // _WIN32
}

