extern crate iron;
extern crate router;

use iron::prelude::*;
use iron::status;
use router::Router;

fn main() {
    let mut router = Router::new();
    router.get("/hello", hello_world, "hello");
    router.get("/", index, "index");

    let _server = Iron::new(router).http("localhost:3000").unwrap();
    println!("On 3000");
}

fn hello_world(_: &mut Request) -> IronResult<Response> {
    Ok(Response::with((status::Ok, "Hello!!")))
}

fn index(_: &mut Request) -> IronResult<Response> {
    Ok(Response::with((status::Ok, "index!!")))
}
