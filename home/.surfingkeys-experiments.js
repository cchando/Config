

mapkey(':dH', '#14Delete history newer than 2 hours', function(e){
		var d=e.days*1e3*86400||0,
				h=e.hours*1e3*3600||0,
				now=(newDate).getTime();
		chrome.history.deleteRange({startTime:now-(d+h), endTime:0},
															 (function(){}
															 )
															)
})



mapkey(':dH', '#14Delete history newer than 2 hours', function() {
		RUNTIME('deleteHistoryNewerThan', {
				hours: 2
		});
});



function deleteHistoryNewerThan(e,t,n){
		var d=e.days*1e3*86400||0,
				h=e.hours*1e3*3600||0,
				now=(newDate).getTime();
		chrome.history.deleteRange( {startTime:now-d-h, endTime:0},
															 (function(){})
															)}



s.deleteHistoryNewerThan=function(e,t,n){var o=e.days||0, r=e.hours||0,; chrome.history.deleteRange({startTime:(new Date).getTime()-1e3*(86400*o+3600*r), endTime:(new Date).getTime()}, (function(){}))}


// from .config/google-chrome/'Profile 1'/Extensions/gfbliohnnapiefjpjlpjnehglfpaknnc/1.0.3_15/background.js
s.deleteHistoryOlderThan=function(e,t,n){var o=e.days||0,r=e.hours||0;chrome.history.deleteRange({startTime:0,endTime:(new Date).getTime()-1e3*(86400*o+3600*r)},(function(){}))}
