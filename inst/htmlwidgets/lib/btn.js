$(function() {
	$('li.messages-menu').click(function (ev) {
		// console.log("setting btn clicked")
		ev.stopPropagation();
		$('#settingBar').toggleClass('active');
	});

	
	$(".tab-content .tab-pane").first().addClass("active");

	$('section.sidebar ul.sidebar-menu li').click(function(ev) {
		// console.log(ev.currentTarget);
		$(".tab-content .tab-pane.active").removeClass("active")
		var tabId = $("a", ev.currentTarget).attr("href");
		$(tabId).addClass("active");
		
		tabSwitched(tabId);
	});

});
