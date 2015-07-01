var main = function() {
	
	"use strict";

	var range = $("#cell-count"),
		value = $(".range-value");
    value.html(range.attr("value"));

    $("#whichCell").change(function() {
    	switch($("#whichCell").val()) {
		case "cell11":
			$("#cell-count").prop("min", 0);
			$("#cell-count").prop("max", 74);
			var cellCount = 40;
			break;
		case "cell21":
			$("#cell-count").prop("min", 0);
			$("#cell-count").prop("max", 74);
			var cellCount = 34;
			break;
		case "cell12":
			$("#cell-count").prop("min", 183);
			$("#cell-count").prop("max", 257);
			var cellCount = 217;
			break;
		case "cell22":
			$("#cell-count").prop("min", 1310);
			$("#cell-count").prop("max", 1384);
			var cellCount = 1350;
			break;
		}
		$("#cell-count").prop("value", cellCount);
		Shiny.onInputChange("cellCount", cellCount)
		value.html(cellCount);
		document.getElementById("n11").innerHTML = 40;
		document.getElementById("n21").innerHTML = 34;
		document.getElementById("n12").innerHTML = 217;
		document.getElementById("n22").innerHTML = 1350;
    });
	
	$("#cell-count").change(function() {
		var cellCount = parseInt($("#cell-count").val());
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
		Shiny.onInputChange("cellCount", cellCount);
		value.html(cellCount);
	});
	
	$("#cell-count").keypress(function() {
		if (event.which === 37) {
			var cellCount = parseInt($("#cell-count").val()) - 1;
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
			};
			Shiny.onInputChange("cellCount", cellCount);
			value.html(cellCount);
		} else if (event.which === 39) {
			var cellCount = parseInt($("#cell-count").val()) + 1;
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
			};
			Shiny.onInputChange("cellCount", cellCount);
			value.html(cellCount);
	});

};

$(document).ready(main);