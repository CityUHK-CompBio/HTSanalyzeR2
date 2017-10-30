$(function() {
	$('li.messages-menu').click(function (ev) {
		// console.log("setting btn clicked")
		ev.stopPropagation();
		$('#settingBar').toggleClass('active');
	});

	var firstTab = $('section.sidebar ul.sidebar-menu li a').first().attr("href");
	$('li.messages-menu').css("display", firstTab == "#shiny-tab-table_tab" ? "none" : "block");
	
	$(".tab-content .tab-pane").first().addClass("active");

	$('section.sidebar ul.sidebar-menu li').click(function(ev) {
		// console.log(ev.currentTarget);
		$(".tab-content .tab-pane.active").removeClass("active")
		var tabId = $("a", ev.currentTarget).attr("href");
		$(tabId).addClass("active");

		$('li.messages-menu').css("display", tabId == "#shiny-tab-table_tab" ? "none" : "block");
		$('#settingBar').removeClass('active');

		tabSwitched(tabId);
	});

});
