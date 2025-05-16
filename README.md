# Chad's To Do App
# [You can see this app live by clicking here](https://chad-stearns-well-todo-app.netlify.app/)

## Running this locally
I believe you can just open the index.html file in your browser and it should work. The index.html file can be found at `/public/index.html`.

## Local Development
You will need Rust installed. The command to start the hot-reloading development webserver is
```bash
cargo run -- run
```

## Organization
The modules in this project have been documented with comments, so feel free to click around
and read the code.

## /src
`src/Main.elm` is the core module of this app.

`src/Assignee.elm`, `src/Task_.elm`, and `src/Status.elm`, are all modules to represent specific domain entities of the app. 

`src/AddTask.elm` is a sub-module for the Dialog box for adding a task. 

## /src/View
The directory `src/View` is for re-usable view components like buttons, dropdowns, dialogs, and text fields.

## /src/Ext
"Ext" is an abbreviation of "Extension", and all the modules in it are likely extensions of the core Elm libraries. For example, the
`src/Ext` directory includes `List.elm`, which contains functions for working with `Lists`. 

## /public/index.html
There is one bit of JavaScript code that is embedded directly in the index.html file. This javascript is for figuring out which status column a user's mouse events were over, so that the Elm app can know which status to assign to a task that has been dragged and dropped.