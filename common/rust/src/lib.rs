#[macro_export]
macro_rules! notify {
    ($($arg:tt)*) => {{
        if let Ok(mut stream) = std::net::TcpStream::connect(("127.0.0.1", 9001)) {
            _ = write!(stream, $($arg)*);
        }
    }};
}
