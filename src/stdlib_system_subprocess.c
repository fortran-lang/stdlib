#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/wait.h>
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

    // Open stdout file if provided
    if (stdout_file) {
        hStdout = CreateFile(stdout_file, GENERIC_WRITE, 0, &sa, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
        if (hStdout == INVALID_HANDLE_VALUE) {
            fprintf(stderr, "Failed to open stdout file\n");
            return;
        }
        si.hStdOutput = hStdout;
        si.dwFlags |= STARTF_USESTDHANDLES;
    }

    // Open stderr file if provided
    if (stderr_file) {
        hStderr = CreateFile(stderr_file, GENERIC_WRITE, 0, &sa, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
        if (hStderr == INVALID_HANDLE_VALUE) {
            fprintf(stderr, "Failed to open stderr file\n");
            return;
        }
        si.hStdError = hStderr;
        si.dwFlags |= STARTF_USESTDHANDLES;
    }

    // Prepare the command line with redirected stdin
    char full_cmd[4096];
    if (stdin_file) {
        snprintf(full_cmd, sizeof(full_cmd), "%s < %s", cmd, stdin_file);
    } else {
        snprintf(full_cmd, sizeof(full_cmd), "%s", cmd);
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

    if (!success) {
        fprintf(stderr, "CreateProcess failed (%lud).\n", GetLastError());
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
void process_query_status_windows(int pid, bool wait, bool* is_running, int* exit_code)
{
    int wait_code;
    HANDLE hProcess;
    DWORD dwExitCode;
    
    // Open the process with the appropriate access rights
    hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | SYNCHRONIZE, FALSE, pid);
    
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
bool process_kill_windows(int pid) {
    HANDLE hProcess;

    // Open the process with terminate rights
    hProcess = OpenProcess(PROCESS_TERMINATE, FALSE, pid);

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


#else // _WIN32

/////////////////////////////////////////////////////////////////////////////////////
// Unix-specific code
/////////////////////////////////////////////////////////////////////////////////////
void process_query_status_unix(int pid, bool wait, bool* is_running, int* exit_code)
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
bool process_kill_unix(int pid) {
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

#endif // _WIN32

/////////////////////////////////////////////////////////////////////////////////////
// Cross-platform interface
/////////////////////////////////////////////////////////////////////////////////////

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
void process_query_status(int pid, bool wait, bool* is_running, int* exit_code)
{
#ifdef _WIN32
   process_query_status_windows(pid, wait, is_running, exit_code);
#else
   process_query_status_unix   (pid, wait, is_running, exit_code);
#endif // _WIN32
}

// Cross-platform interface: kill process by ID
bool process_kill(int pid)
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
   DWORD dwMilliseconds = 1000*seconds;
   Sleep(dwMilliseconds);
#else
   int uSeconds = (int) 1.0e6*seconds;
   
   struct timespec t;

   t.tv_sec  = seconds;
   t.tv_nsec = seconds * 1000000;

   int ierr = nanosleep(&t, NULL);

   if (ierr != 0){
     switch(errno){
       case EINTR:
         fprintf(stderr, "nanosleep() interrupted\n");
         break;
       case EINVAL:
         fprintf(stderr, "nanosleep() bad milliseconds value\n");
         exit(EINVAL);
       case EFAULT:
         fprintf(stderr, "nanosleep() bad milliseconds value\n");
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
const char* process_null_device(int* len) 
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
bool process_has_win32()
{
#ifdef _WIN32
   return true;
#else
   return false;
#endif // _WIN32
}

