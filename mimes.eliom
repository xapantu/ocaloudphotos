module Mimes(Config:App_stub.CONFIG) = struct
	let services = Hashtbl.create 20

	let register_public (name:string) (service:App_stub.unit_service) = Hashtbl.add services "public" (name, service)

	let get_all_public_services () =
		Hashtbl.find_all services "public"
end
