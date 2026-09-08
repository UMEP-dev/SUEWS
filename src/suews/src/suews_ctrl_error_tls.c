/*
 * Thread-local storage for the SUEWS fatal error state (GH#1736).
 *
 * module_ctrl_error_state (suews_ctrl_error.f95) used to keep the fatal
 * error flag, code and message as module-level SAVE variables, i.e. one
 * process-wide copy. The Rust bridge runs one grid per Rayon worker thread
 * inside a single address space, so grids saw, reset and inherited each
 * other's fatal errors. Keeping the store in C11 _Thread_local variables
 * gives every OS thread, and therefore every concurrent grid run, its own
 * copy without changing any Fortran call site beyond the accessor names.
 *
 * Fortran callers must not touch these variables directly; they go through
 * the BIND(C) accessors below, which module_ctrl_error_state wraps.
 */
#include <string.h>

#define SUEWS_TLS_ERROR_MESSAGE_LEN 512

static _Thread_local int suews_tls_error_flag_v = 0;
static _Thread_local int suews_tls_error_code_v = 0;
/* One extra byte keeps the buffer NUL-terminated at full length. */
static _Thread_local char suews_tls_error_message_v[SUEWS_TLS_ERROR_MESSAGE_LEN + 1] = {0};

void suews_tls_error_reset(void)
{
    suews_tls_error_flag_v = 0;
    suews_tls_error_code_v = 0;
    memset(suews_tls_error_message_v, 0, sizeof suews_tls_error_message_v);
}

/* message is a Fortran character buffer of length msg_len (no NUL). */
void suews_tls_error_set(int code, const char *message, int msg_len)
{
    size_t n = msg_len > 0 ? (size_t)msg_len : 0;
    if (n > SUEWS_TLS_ERROR_MESSAGE_LEN)
        n = SUEWS_TLS_ERROR_MESSAGE_LEN;
    suews_tls_error_flag_v = 1;
    suews_tls_error_code_v = code;
    memset(suews_tls_error_message_v, 0, sizeof suews_tls_error_message_v);
    if (n > 0 && message != NULL)
        memcpy(suews_tls_error_message_v, message, n);
}

int suews_tls_error_flag(void)
{
    return suews_tls_error_flag_v;
}

int suews_tls_error_code(void)
{
    return suews_tls_error_code_v;
}

/* Copy the stored message into a Fortran character buffer of length
 * buf_len, blank-padded so the caller can use it as CHARACTER(LEN=buf_len). */
void suews_tls_error_message(char *buffer, int buf_len)
{
    size_t n = buf_len > 0 ? (size_t)buf_len : 0;
    size_t stored = strlen(suews_tls_error_message_v);
    size_t ncopy = stored < n ? stored : n;
    if (n == 0 || buffer == NULL)
        return;
    memcpy(buffer, suews_tls_error_message_v, ncopy);
    if (ncopy < n)
        memset(buffer + ncopy, ' ', n - ncopy);
}
