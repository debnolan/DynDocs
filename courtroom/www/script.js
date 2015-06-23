var main = function() {
	
	"use strict";
	
	$("#whichCell").change(function() {
		switch($("#whichCell").val()) {
			case "cell11":
				$("#cellCount").prop("min", 0);
				$("#cellCount").prop("max", 74);
				$("#cellCount").prop("value", 40);
				break;
			case "cell21":
				$("#cellCount").prop("min", 0);
				$("#cellCount").prop("max", 74);
				$("#cellCount").prop("value", 34);
				break;
			case "cell12":
				$("#cellCount").prop("min", 183);
				$("#cellCount").prop("max", 257);
				$("#cellCount").prop("value", 217);
				break;
			case "cell22":
				$("#cellCount").prop("min", 1310);
				$("#cellCount").prop("max", 1384);
				$("#cellCount").prop("value", 1350);
				break;
		}
	});
		
	$("#cellCount").change(function() {
		var cellCount = parseInt($("#cellCount").val());
		switch($("#whichCell").val()) {
			case "cell11":
				document.getElementById("n11").innerHTML = cellCount;
				document.getElementById("n21").innerHTML = 74 - cellCount;
				document.getElementById("n12").innerHTML = 257 - cellCount;
				document.getElementById("n22").innerHTML = 1310 + cellCount;
				break;
			case "cell21":
				document.getElementById("n11").innerHTML = 74 - cellCount;
				document.getElementById("n21").innerHTML = cellCount;
				document.getElementById("n12").innerHTML = 183 + cellCount;
				document.getElementById("n22").innerHTML = 1384 - cellCount;
				break;
			case "cell12":
				document.getElementById("n11").innerHTML = 257 - cellCount;
				document.getElementById("n21").innerHTML = -183 + cellCount;
				document.getElementById("n12").innerHTML = cellCount;
				document.getElementById("n22").innerHTML = 1567 - cellCount;
				break;
			case "cell22":
				document.getElementById("n11").innerHTML = -1310 + cellCount;
				document.getElementById("n21").innerHTML = 1384 - cellCount;
				document.getElementById("n12").innerHTML = 1567 - cellCount;
				document.getElementById("n22").innerHTML = cellCount;
				break;
		}
	});
	
};

$(document).ready(main);