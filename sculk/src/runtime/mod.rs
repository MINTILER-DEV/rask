//! Native host runtime shims for JIT-compiled Sculk programs.

#[cfg(feature = "cranelift-backend")]
use std::ffi::{c_char, c_void, CStr};

/// Symbol name for string printing.
pub const PRINT_CSTR_SYMBOL: &str = "__sculk_print_cstr";
/// Symbol name for integer printing.
pub const PRINT_I64_SYMBOL: &str = "__sculk_print_i64";
/// Symbol name for floating-point printing.
pub const PRINT_F64_SYMBOL: &str = "__sculk_print_f64";
/// Symbol name for `std.http.get`.
pub const HTTP_GET_SYMBOL: &str = "__sculk_http_get";
/// Symbol name for `std.http.post`.
pub const HTTP_POST_SYMBOL: &str = "__sculk_http_post";
/// Symbol name for `std.http.put`.
pub const HTTP_PUT_SYMBOL: &str = "__sculk_http_put";
/// Symbol name for `std.http.delete`.
pub const HTTP_DELETE_SYMBOL: &str = "__sculk_http_delete";
/// Symbol name for reading `response.status`.
pub const HTTP_RESPONSE_STATUS_SYMBOL: &str = "__sculk_http_response_status";

/// Returns all runtime symbols that must be linked for JIT execution.
#[cfg(feature = "cranelift-backend")]
pub fn host_symbols() -> [(&'static str, *const u8); 8] {
    [
        (PRINT_CSTR_SYMBOL, sculk_print_cstr as *const u8),
        (PRINT_I64_SYMBOL, sculk_print_i64 as *const u8),
        (PRINT_F64_SYMBOL, sculk_print_f64 as *const u8),
        (HTTP_GET_SYMBOL, sculk_http_get as *const u8),
        (HTTP_POST_SYMBOL, sculk_http_post as *const u8),
        (HTTP_PUT_SYMBOL, sculk_http_put as *const u8),
        (HTTP_DELETE_SYMBOL, sculk_http_delete as *const u8),
        (
            HTTP_RESPONSE_STATUS_SYMBOL,
            sculk_http_response_status as *const u8,
        ),
    ]
}

#[cfg(feature = "cranelift-backend")]
#[derive(Debug)]
struct HttpResponseHandle {
    status: i64,
}

/// Runtime print shim for nul-terminated UTF-8 strings.
#[cfg(feature = "cranelift-backend")]
extern "C" fn sculk_print_cstr(ptr: *const c_char) {
    if ptr.is_null() {
        println!();
        return;
    }

    // SAFETY: Generated code passes pointers to static nul-terminated strings.
    let text = unsafe { CStr::from_ptr(ptr) };
    println!("{}", text.to_string_lossy());
}

/// Runtime print shim for signed 64-bit integers.
#[cfg(feature = "cranelift-backend")]
extern "C" fn sculk_print_i64(value: i64) {
    println!("{}", value);
}

/// Runtime print shim for 64-bit floating-point values.
#[cfg(feature = "cranelift-backend")]
extern "C" fn sculk_print_f64(value: f64) {
    println!("{}", value);
}

/// Runtime shim for `std.http.get(url)`.
#[cfg(feature = "cranelift-backend")]
extern "C" fn sculk_http_get(url: *const c_char) -> *mut c_void {
    match http_request(reqwest::Method::GET, url, std::ptr::null()) {
        Ok(ptr) => ptr,
        Err(err) => {
            eprintln!("sculk runtime error: {}", err);
            std::ptr::null_mut()
        }
    }
}

/// Runtime shim for `std.http.post(url, body)`.
#[cfg(feature = "cranelift-backend")]
extern "C" fn sculk_http_post(url: *const c_char, body: *const c_char) -> *mut c_void {
    match http_request(reqwest::Method::POST, url, body) {
        Ok(ptr) => ptr,
        Err(err) => {
            eprintln!("sculk runtime error: {}", err);
            std::ptr::null_mut()
        }
    }
}

/// Runtime shim for `std.http.put(url, body)`.
#[cfg(feature = "cranelift-backend")]
extern "C" fn sculk_http_put(url: *const c_char, body: *const c_char) -> *mut c_void {
    match http_request(reqwest::Method::PUT, url, body) {
        Ok(ptr) => ptr,
        Err(err) => {
            eprintln!("sculk runtime error: {}", err);
            std::ptr::null_mut()
        }
    }
}

/// Runtime shim for `std.http.delete(url)`.
#[cfg(feature = "cranelift-backend")]
extern "C" fn sculk_http_delete(url: *const c_char) -> *mut c_void {
    match http_request(reqwest::Method::DELETE, url, std::ptr::null()) {
        Ok(ptr) => ptr,
        Err(err) => {
            eprintln!("sculk runtime error: {}", err);
            std::ptr::null_mut()
        }
    }
}

/// Runtime shim for reading `response.status` from an HTTP response handle.
#[cfg(feature = "cranelift-backend")]
extern "C" fn sculk_http_response_status(response: *mut c_void) -> i64 {
    if response.is_null() {
        return 0;
    }

    // SAFETY: Pointers come from `sculk_http_*` constructors in this module.
    let handle = unsafe { &*(response as *mut HttpResponseHandle) };
    handle.status
}

#[cfg(feature = "cranelift-backend")]
fn http_request(
    method: reqwest::Method,
    url_ptr: *const c_char,
    body_ptr: *const c_char,
) -> Result<*mut c_void, String> {
    let url = parse_c_string(url_ptr, "http url")?;
    let body = if body_ptr.is_null() {
        None
    } else {
        Some(parse_c_string(body_ptr, "http body")?)
    };

    let client = reqwest::blocking::Client::new();
    let mut request = client.request(method, &url);
    if let Some(body) = body {
        request = request.body(body);
    }

    let response = request
        .send()
        .map_err(|err| format!("http request failed for '{}': {}", url, err))?;
    let status = i64::from(response.status().as_u16());

    let handle = Box::new(HttpResponseHandle { status });
    Ok(Box::into_raw(handle) as *mut c_void)
}

#[cfg(feature = "cranelift-backend")]
fn parse_c_string(ptr: *const c_char, label: &str) -> Result<String, String> {
    if ptr.is_null() {
        return Err(format!("{} pointer was null", label));
    }

    // SAFETY: Callers pass valid nul-terminated C strings.
    let text = unsafe { CStr::from_ptr(ptr) }
        .to_str()
        .map_err(|err| format!("{} was not valid UTF-8: {}", label, err))?;
    Ok(text.to_string())
}
