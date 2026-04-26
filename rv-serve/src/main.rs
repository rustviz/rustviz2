use actix_files::Files;
use actix_governor::{Governor, GovernorConfigBuilder};
use actix_web::{error, middleware::Logger, web, App, HttpResponse, HttpServer};
use rustviz2::Rustviz;
use serde::{Deserialize, Serialize};

/// Hard cap on the JSON body Actix will accept on /submit-code.
/// 16 KiB is comfortably above the largest hand-written teaching example
/// while leaving no headroom for an attacker to ship a hostile macro corpus.
const MAX_BODY_BYTES: usize = 16 * 1024;

/// Same cap, applied to the inner `code` string after JSON parsing. Belt and
/// suspenders — the body limit is the actual gate, but checking again here
/// gives us a clean error message instead of a silent truncation if the
/// frontend ever stops constraining input.
const MAX_CODE_BYTES: usize = 16 * 1024;

#[derive(Deserialize)]
struct SubmitCodePayload {
    code: String,
}

#[derive(Deserialize, Serialize)]
struct SubmitResponse {
    code_panel: String,
    timeline_panel: String,
}

async fn submit_code(payload: web::Json<SubmitCodePayload>) -> HttpResponse {
    let code = &payload.code;
    if code.len() > MAX_CODE_BYTES {
        return HttpResponse::PayloadTooLarge()
            .body(format!("code must be at most {} bytes", MAX_CODE_BYTES));
    }

    match Rustviz::new(code) {
        Ok(rv) => {
            let body = serde_json::to_string(&SubmitResponse {
                code_panel: rv.code_panel_string(),
                timeline_panel: rv.timeline_panel_string(),
            })
            .unwrap();
            HttpResponse::Ok().body(body.into_bytes())
        }
        Err(e) => HttpResponse::from_error(
            <Box<dyn std::error::Error> as Into<error::Error>>::into(e),
        ),
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let bind_addr = std::env::var("RV_BIND").unwrap_or_else(|_| "127.0.0.1:8080".to_string());

    // Per-IP token bucket. `seconds_per_replenish` controls the steady-state
    // rate (one token per N seconds); `burst_size` is the bucket depth.
    // Tunable via env so the deploy can adjust without a recompile.
    let seconds_per_replenish = std::env::var("RV_RATE_SECONDS_PER_REQUEST")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(2u64);
    let rate_burst = std::env::var("RV_RATE_BURST")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(5u32);
    let governor_conf = GovernorConfigBuilder::default()
        .per_second(seconds_per_replenish)
        .burst_size(rate_burst)
        .finish()
        .expect("governor config");

    HttpServer::new(move || {
        App::new()
            .wrap(Logger::default())
            .app_data(web::JsonConfig::default().limit(MAX_BODY_BYTES))
            .service(Files::new("/ex-assets", "./ex-assets/"))
            .service(
                web::resource("/submit-code")
                    .wrap(Governor::new(&governor_conf))
                    .route(web::post().to(submit_code)),
            )
            // Vite emits the SPA + hashed assets/ subdir under frontend/dist/.
            .service(Files::new("/", "./frontend/dist/").index_file("index.html"))
    })
    .bind(bind_addr)?
    .run()
    .await
}
