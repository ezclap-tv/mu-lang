```rust
//! name: who
//! desc: >
//!   Fetches the ID of the target. Target is optional
//!   and defaults to the user who executed the command.

import {
  bot.Context,
  api.Twitch.get_user
}

/*
enum GetUser {
  ByName(String),
  ById(String)
}

type User {
  id: String,
  name: String,
  created_at: Date,
}

fn get_user(v: GetUser) -> User? { /* */ }
*/

type Args {
  target: String?
}

fn execute(ctx: Context, args: Args) throws {
  match args.target {
    null => ctx.respond(f"Your id is {id}"),
    name => {
      id := get_user(.ByName(name)).try.id
      ctx.respond(f"That user's id is {id}")
    }
  }
}

export { Args, execute }
```
