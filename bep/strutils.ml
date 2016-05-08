exception BadString

let hex_of_string_upper str =
	let l = String.length str in
	let buf = String.make (l/2) '0' in
	let a = Char.code '0' in
	let a9 = Char.code '9' in
	let aa = Char.code 'A' in
	let f = Char.code 'F' in
	for i = 0 to l/2 - 1 do
		let c = 
			let c = Char.code str.[2*i] in
			if c > a9 then
				c - aa + 10
			else
				c - a
		in
		let d =
			let c = Char.code str.[2*i+1] in
			if c > a9 then
				c - aa + 10
			else
				c - a
		in
		if c < 0 || c > 15 || d < 0 || d > 15 then
			raise BadString;
		buf.[i] <- Char.unsafe_chr (c*16 + d)
	done;
	buf
