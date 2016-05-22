module Mimes(Config:App_stub.CONFIG) = struct
  let services = Hashtbl.create 20

  let register_public (name:string) (service:App_stub.unit_service) = Hashtbl.add services "public" (name, service)

  let get_all_public_services () =
    Hashtbl.find_all services "public"

  let sidebar: (unit -> Html5_types.div Eliom_content.Html5.elt) list ref = ref []

  let register_sidebar name f =
    sidebar := f :: !sidebar

  let build_sidebar () =
    List.map ((|>) ()) !sidebar

end
