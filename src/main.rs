mod elm_dev;

use actix_web::middleware::Logger;
use actix_web::{get, post, web, App, HttpResponse, HttpServer, Responder};
use clap::Parser;
use std::fs;

#[get("/")]
async fn index() -> impl Responder {
    match fs::read("./public/index.html") {
        Ok(contents) => HttpResponse::Ok().body(contents),
        Err(err) => {
            HttpResponse::InternalServerError().body(format!("Error reading file: {}", err))
        }
    }
}

#[get("/elm.js")]
async fn elm_js() -> impl Responder {
    match fs::read("./public/elm.js") {
        Ok(contents) => HttpResponse::Ok().body(contents),
        Err(err) => {
            HttpResponse::InternalServerError().body(format!("Error reading file: {}", err))
        }
    }
}

/////////////////////////////////////////////////////////////////////
// Main
///////////////////////////////////////////////////////////////////////

#[derive(Debug, Parser, Clone)]
#[clap(
    author = "Chad Stearns",
    version = "0.1",
    about = "Commands for my well project"
)]
enum Cmd {
    Run,
}

#[actix_web::main]
async fn main() -> Result<(), String> {
    let cmd = Cmd::parse();

    match cmd {
        Cmd::Run => {
            tokio::spawn(async move {
                if let Err(err) = elm_dev::run().await {
                    println!("Error running Elm dev: {}", err);
                }
            });

            HttpServer::new(move || {
                App::new()
                    .wrap(Logger::default())
                    .service(index)
                    .service(elm_js)
            })
            .bind(("127.0.0.1", 4355))
            .map_err(|err| format!("Error binding server: {}", err))?
            .run()
            .await
            .map_err(|err| format!("Error starting server: {}", err))
        }
    }
}
