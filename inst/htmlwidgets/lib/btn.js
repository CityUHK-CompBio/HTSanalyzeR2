$(function() {
	$('li.messages-menu').click(function (ev) {
		ev.stopPropagation();
		console.log("setting btn clicked")
		$('#settingBar').toggleClass('active');
	});
});
