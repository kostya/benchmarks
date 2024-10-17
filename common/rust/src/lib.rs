#[macro_export]
macro_rules! notify {
    ($($arg:tt)*) => {{
        use std::net::{Ipv4Addr, TcpStream};

        if let Ok(mut stream) = TcpStream::connect((Ipv4Addr::LOCALHOST, 9001)) {
            _ = write!(stream, $($arg)*);
        }
    }};
}
