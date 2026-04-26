use actix_web::{web, App, HttpResponse, HttpServer, middleware::Logger, error};
use actix_files::Files;
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

    let bind_addr = std::env::var("RV_BIND").unwrap_or_else(|_| "127.0.0.1:8080".to_string());

    HttpServer::new(|| {
        App::new()
            .wrap(Logger::default())
            .service(Files::new("/ex-assets", "./ex-assets/"))
            .route("/submit-code", web::post().to(submit_code))
            // Vite emits the SPA + hashed assets/ subdir under frontend/dist/.
            .service(Files::new("/", "./frontend/dist/").index_file("index.html"))
    })
    .bind(bind_addr)?
    .run()
    .await
}