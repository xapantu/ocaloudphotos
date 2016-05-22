type unit_service = (unit, unit, Eliom_service.get_service_kind, Eliom_service.attached,
	Eliom_service.service_kind, [ `WithoutSuffix ], unit, 
	unit, Eliom_service.registrable, [ `Appl ])
	Eliom_service.service 

type path_service =
	(string list, unit, Eliom_service.get_service_kind,
	Eliom_service.attached,
	 Eliom_service.service_kind, [ `WithSuffix ],
	[ `One of string list ] Eliom_parameter.param_name, 
	unit, Eliom_service.registrable,
	[ `Appl ])
	Eliom_service.service

