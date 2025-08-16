use std::backtrace;

pub struct CallerInfo {
    pub file: String,
    pub line: u32,
    pub column: u32,
    pub function: String,
}

pub fn get_immediate_caller() -> CallerInfo {
    let mut backtrace_json = format!("{}", backtrace::Backtrace::force_capture());

    let lines : Vec<String> = backtrace_json.lines().map(|s| s.to_string()).collect();
    let lines_count = lines.len();

    let mut c = 0;

    for i in (0..lines_count).step_by(2) {
        // let fn_name = String::from(lines[i].clone());
        let file_info = String::from(lines[i + 1].clone());

        // println!("Function:{}", fn_name);
        // println!("File Info:{}", file_info);

        if file_info.contains("/backtrace.rs:") && !file_info.starts_with("             at /rustc/") {
            c = i;
            break;
        }
    }

    c += 4;

    // let fn_name = String::from(lines[c].clone());
    // let file_info = String::from(lines[c + 1].clone());

    // println!("Function:{}", fn_name);
    // println!("File Info:{}", file_info);

    let mut function = lines[c].clone();
    function = function.trim_start_matches(char::is_numeric).strip_prefix(": ").unwrap_or_default().to_string();

    let mut file = lines[c + 1].clone();
    file = file.trim_start().strip_prefix("at ").unwrap_or_default().to_string();

    let flc : Vec<String> = file.split(":").map(|s| s.to_string()).collect();

    CallerInfo {
        file: flc[0].to_string(),
        line: flc.get(1).and_then(|s| s.trim().parse().ok()).unwrap_or(0),
        column: flc.get(2).and_then(|s| s.trim().parse().ok()).unwrap_or(0),
        function
    }
}