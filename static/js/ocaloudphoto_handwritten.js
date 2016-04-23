(function () {
	var border = 30;
	function resizeall() {
		var l = document.getElementsByClassName("myimg");
		for(var i = 0; i < l.length; i++) {
			var img = l[i].children[0].children[0];
			var p = l[i].children[1];
			var max_height = window.innerHeight -p.offsetHeight -20;
			img.style["max-height"] = max_height + "px";
			img.style["max-width"] = window.innerWidth + "px";
			p.style["max-width"] = Math.min(window.innerWidth, img.offsetWidth) + "px"
			l[i].style["width"] = Math.min(window.innerWidth, img.offsetWidth) + "px"
		}
	}
	resizeall();
	window.addEventListener("resize", resizeall);
	window.addEventListener("load", resizeall);
	})
()
