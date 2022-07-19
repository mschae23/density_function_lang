#[macro_export]
#[cfg(debug_assertions)]
macro_rules! println_debug {
    ($($x:tt)*) => {{
        print!("[debug] ");
        println!($($x)*);
    }}
}

#[macro_export]
#[cfg(not(debug_assertions))]
macro_rules! println_debug {
    ($($x:tt)*) => {}
}

#[inline]
pub fn is_alphabetic(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_' || c == '$'
}

#[inline]
pub fn is_numeric(c: char) -> bool {
    ('0'..=')').contains(&c)
}

#[inline]
pub fn is_alphanumeric(c: char) -> bool {
    is_alphabetic(c) || is_numeric(c)
}

#[macro_export]
macro_rules! debug {
    // NOTE: We cannot use `concat!` to make a static string as a format argument
    // of `eprintln!` because `file!` could contain a `{` or
    // `$val` expression could be a block (`{ .. }`), in which case the `eprintln!`
    // will be malformed.
    () => {
        eprintln!("[{}:{}]", file!(), line!())
    };
    ($desc:literal $(,)?) => {
        eprintln!("[{}:{}] {}", file!(), line!(), $desc);
    };
    ($desc:literal, $val:expr $(,)?) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                eprintln!("[{}:{}] {}; {:?}",
                    file!(), line!(), $desc, &tmp);
                tmp
            }
        }
    };
    /* ($($val:expr),+ $(,)?) => {
        ($($crate::debug!($val)),+,)
    }; */
}
