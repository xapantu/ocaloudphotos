[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5
    open F
]

open Config
open Data
open Mimes
open User
open Photos
open Files
open Welcome

module Config = Config
module Data = Data(Config)


(* Apps can register to read, write, use a filetype, and ask for apps
 * that can do those things. *)
module Mimes = Mimes(Config)
module User = User(Data)

module Env = struct
	module Mimes = Mimes
	module Config = Config
	module Data = Data
end

module Filess = Files(Env)

module EnvBase = struct
	include Env
	module Files = Filess
end

module Photos = Photos(EnvBase)

module Welcome = Welcome(Env)

let main_service =
  Eliom_service.Http.service ~path:["main"] ~get_params:Eliom_parameter.unit ()

let () =
  Eliom_registration.Html5.register
    ~service:main_service
    (fun () () ->
       Lwt.return
         (Eliom_tools.F.html
            ~title:"reactivenodes"
            ~css:[["css"; "reactivenodes.css"]]
            (body [F.p [pcdata "ocaloud v1"];
                  ])
         ))

let () = Mimes.register_public "main" main_service
