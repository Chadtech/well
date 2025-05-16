use notify::event::ModifyKind;
use notify::{EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std;

pub async fn run() -> Result<(), String> {
    let run_cmd = |from_cmd: fn(&mut std::process::Command) -> Result<(), String>| {
        from_cmd(std::process::Command::new("elm").args([
            "make",
            "./src/Main.elm",
            "--output=./public/elm.js",
        ]))
    };

    println!("Compiling Elm");
    run_cmd(|cmd| cmd.spawn().map(|_| ()).map_err(|err| err.to_string()))?;

    let (tx, rx) = std::sync::mpsc::channel();

    let mut watcher =
        RecommendedWatcher::new(tx, notify::Config::default()).map_err(|err| err.to_string())?;

    watcher
        .watch("./src".as_ref(), RecursiveMode::Recursive)
        .unwrap();

    for res in rx {
        match res {
            Ok(event) => {
                let mut any_elm = false;

                for path in event.paths {
                    let file_extension = path.extension().and_then(|ext| ext.to_str());
                    if let Some("elm") = file_extension {
                        if let EventKind::Modify(ModifyKind::Data(_)) = event.kind {
                            any_elm = true;
                        }
                    }
                }

                if any_elm {
                    run_cmd(|cmd| cmd.spawn().map(|_| ()).map_err(|err| err.to_string())).unwrap();
                }
            }
            Err(err) => {
                println!("Watch error: {}", err);
            }
        }
    }

    Ok(())
}
