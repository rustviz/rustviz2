use actix_web::body::BoxBody;
use actix_web::http::StatusCode;
use actix_web::{get, post, web, App, HttpResponse, HttpServer, Responder, middleware::Logger, Result, error};
use actix_files::{Files, NamedFile};
use env_logger;
use log::info;
use std::{fs, str};
use std::process::Command;
use serde::{Deserialize, Serialize};


#[derive(Deserialize)]
struct SubmitCodePayload {
    code: String,
}


#[derive(Deserialize, Serialize)]
struct SubmitResponse {
  code_panel: String, 
  timeline_panel: String
}

async fn submit_code(payload: web::Json<SubmitCodePayload>) -> HttpResponse {
    let code = &payload.code;

    //Write the received code to ./test-crate/lib.rs
    match fs::write("./test-crate/src/lib.rs", code) {
        Ok(_) => (),
        Err(e) => {return HttpResponse::from_error(<std::io::Error as Into<error::Error>>::into(e));}
    }

    // Run the `cargo rv-plugin` command inside the `test-crate` directory
    let output = Command::new("cargo")
        .arg("rv-plugin")
        .current_dir("./test-crate")
        .output();
    match output {
        Ok(res) => {
            if !res.status.success() {
              // Capture stderr and return it in the error response
              let stderr = String::from_utf8_lossy(&res.stderr);
              return HttpResponse::BadRequest().body(format!("Error: {}", stderr));
            }
        }
        Err(E) => {
            return HttpResponse::from_error(<std::io::Error as Into<error::Error>>::into(E));
        }
    }

    // Copy the resulting SVG files to ex-assets/
    let c_bytes = fs::read("./test-crate/src/vis_code.svg").unwrap();
    let t_bytes = fs::read("./test-crate/src/vis_timeline.svg").unwrap();

    let c_string = str::from_utf8(&c_bytes).unwrap();
    let t_string = str::from_utf8(&t_bytes).unwrap();
    let body = serde_json::to_string(&SubmitResponse {code_panel: c_string.to_owned(), timeline_panel: t_string.to_owned()}).unwrap();
    HttpResponse::Ok().body(body.into_bytes())
    // match fs::copy("./test-crate/src/vis_code.svg", "./ex-assets/vis_code.svg") {
    //     Ok(_) => {},
    //     Err(E) => { return HttpResponse::from_error(<std::io::Error as Into<error::Error>>::into(E)); }
    // }
    // match fs::copy("./test-crate/src/vis_timeline.svg", "./ex-assets/vis_timeline.svg") {
    //     Ok(_) => {},
    //     Err(E) => { return HttpResponse::from_error(<std::io::Error as Into<error::Error>>::into(E)) }
    // }

    // // Send a success response back to the frontend
    // HttpResponse::Ok().body("Success")
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {

    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));
    HttpServer::new(|| {
        App::new()
        .wrap(Logger::default()) // Enable logger
        .service(Files::new("/static", "./frontend/build/static/").show_files_listing())
        .service(Files::new("/dist", "./frontend/dist/").show_files_listing())
        .service(Files::new("/ex-assets", "./ex-assets/").show_files_listing())
        .route("/submit-code", web::post().to(submit_code))
        .service(Files::new("/", "./frontend/build/").index_file("index.html"))
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}