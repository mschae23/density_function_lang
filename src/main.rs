fn main() {
    match density_function_lang::run() {
        Ok(_) => {}
        Err(_) => std::process::exit(1),
    }
}
