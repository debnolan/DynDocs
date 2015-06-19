$(document).ready(function() {
	initializeAnimationFrames();

	$('#sim_animate').click(function() {
		if (!animation) {
			runAnimationFrames();
		} else {
			stopAnimationFrames();
		}
	});

	$('#sim_step').click(function() {
		stepAnimationFrames();
	});

	$('#sim_reset').click(function() {
		resetAnimationFrames()
	});
});

var animation = null;

function initializeAnimationFrames() {
	$('#sim_step_slider').ionRangeSlider({
		grid: false,
		min: 0,
		max: $('#sim_time_steps').val(),
		from: 0,
		step: 1,
		keyboard: true,
		keyboard_step: 100 / $('#sim_time_steps').val(),
	});
}

function setAnimationFrames() {
	$('#sim_step_slider').data('ionRangeSlider').update({
		max: $('#sim_time_steps').val(),
		keyboard_step: 100 / $('#sim_time_steps').val(),
	});
}

function runAnimationFrames() {
	animation = setInterval(stepAnimationFrames, 1000);
	$('#sim_animate').text('Stop');
}

function stopAnimationFrames() {
	clearInterval(animation);
	animation = null;
	$('#sim_animate').text('Animate');
}

function stepAnimationFrames() {
	$('#sim_step_slider').data('ionRangeSlider').update({
		from: $('#sim_step_slider').data('from') + 1,
	});
}

function resetAnimationFrames() {
	$('#sim_step_slider').data('ionRangeSlider').update({
		from: 0,
	});
}