use actix_web::{get, post, web, App, HttpResponse, HttpServer, Responder, middleware::Logger, Result, error};
use actix_files::{Files, NamedFile};
use env_logger;
use log::info;
use std::{fs, str};
use std::process::Command;
use serde::{Deserialize, Serialize};
use rustviz2::Rustviz;

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
    match Rustviz::new(code) {
      Ok(rv) => {
        let body = serde_json::to_string(&SubmitResponse {code_panel: rv.code_panel_string(), timeline_panel: rv.timeline_panel_string()}).unwrap();
        HttpResponse::Ok().body(body.into_bytes())
      }
      Err(e) => {
        HttpResponse::from_error(<Box<dyn std::error::Error> as Into<error::Error>>::into(e))
      }
    }
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