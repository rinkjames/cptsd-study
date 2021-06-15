$(function(){

	// Make section headers into toolbar CSS
	$('.header').addClass('toolbar');

	// Add extra border about main survey container for survey acknowledgement, etc.
	if (!$('#questiontable').length) $('#container').css('border','1px solid #ccc');

	// Remove ability to submit form via Enter button on keyboard
	$(':input').keypress(function(e) {
		if ((this.type == 'checkbox' || this.type == 'text') && e.which == 13) {
			return false;
		}
	});

	// Safari hack for large radios/checkboxes
	var chrome = navigator.userAgent.indexOf('Chrome') > -1;
	var safari = navigator.userAgent.indexOf("Safari") > -1;
	if (chrome && safari) safari = false;
    // If Safari and large text, increase padding of checkbox/radio (should be done with classes - not style)
    if (safari && $('#questiontable input[type="checkbox"]:first, #questiontable input[type="radio"]:first').length) {
        if ($('#questiontable input[type="checkbox"]:first, #questiontable input[type="radio"]:first').css('margin').substring(0,3) == '6px') {
            $('#questiontable input[type="checkbox"], #questiontable input[type="radio"]').css('margin', '8px');
        }
    }

	// Hack to un-truncate long select options: http://stackoverflow.com/a/19734474/1402028
	if (isIOS) $("#questiontable select").append('<optgroup label=""></optgroup>');

	// Enable action tags
	enableActionTags();

	// Enable auto-complete for drop-downs
	enableDropdownAutocomplete();

	// Hide or disable fields if using special annotation
	triggerActionTags();

	// Bubble pop-up for Return Code widget
	if ($('.bubbleInfo').length) {
		$('.bubbleInfo').each(function () {
			var distance = 10;
			var time = 250;
			var hideDelay = 500;
			var hideDelayTimer = null;
			var beingShown = false;
			var shown = false;
			var trigger = $('.trigger', this);
			if (!trigger.length) return;
			var info = $('.popup', this).css('opacity', 0);
			$([trigger.get(0), info.get(0)]).mouseover(function (e) {
				if (hideDelayTimer) clearTimeout(hideDelayTimer);
				if (beingShown || shown) {
					// don't trigger the animation again
					return;
				} else {
					// reset position of info box
					beingShown = true;
					info.css({
						top: 0,
						right: 0,
						width: 300,
						display: 'block'
					}).animate({
						top: '+=' + distance + 'px',
						opacity: 1
					}, time, 'swing', function() {
						beingShown = false;
						shown = true;
					});
				}
				return false;
			}).mouseout(function () {
				if (hideDelayTimer) clearTimeout(hideDelayTimer);
				hideDelayTimer = setTimeout(function () {
					hideDelayTimer = null;
					info.animate({
						top: '-=' + distance + 'px',
						opacity: 0
					}, time, 'swing', function () {
						shown = false;
						info.css('display', 'none');
					});

				}, hideDelay);

				return false;
			});
		});
	}

	// Set autocomplete for BioPortal ontology search for ALL fields on a page
	initAllWebServiceAutoSuggest();

	// Make sure dropdowns don't get too wide so that they create horizontal scrollbar
	shrinkWideDropDowns();

	// Improve onchance event of text/notes fields to trigger branching logic before leaving the field
	// (so that we don't skip over the next field, which becomes displayed)
	improveBranchingOnchange();
});

// Display the Survey Login dialog (login form)
function displaySurveyLoginDialog() {
	$('#survey_login_dialog').dialog({ bgiframe: true, modal: true, width: (isMobileDevice ? $(window).width() : 670), open:function(){fitDialog(this);},
		close:function(){ window.location.href=window.location.href; },
		title: '<img src="'+app_path_images+'lock_big.png" style="vertical-align:middle;margin-right:2px;"><span style="color:#A86700;font-size:18px;vertical-align:middle;">'+langSurveyLoginForm4+'</span>', buttons: [
		{ text: langSurveyLoginForm1, click: function () {
			// Make sure enough inputs were entered
			var numValuesEntered = 0;
			$('#survey_auth_form input').each(function(){
				var thisval = trim($(this).val());
				if (thisval != '') numValuesEntered++;
			});
			// If not enough values entered, give error message
			if (numValuesEntered < survey_auth_min_fields) {
				simpleDialog(langSurveyLoginForm2, langSurveyLoginForm3);
				return;
			}
			// Submit form
			$('#survey_auth_form').submit();
		} }] });
	// If there are no login fields displayed in the dialog, then remove the "Log In" button
	if ($('#survey_auth_form table.form_border tr').length == 0) {
		$('#survey_login_dialog').parent().find('div.ui-dialog-buttonpane').hide();
	}
	// Add extra style to the "Log In" button
	else {
		$('#survey_login_dialog').parent().find('div.ui-dialog-buttonpane button').css({'font-weight':'bold','color':'#444','font-size':'15px'});
	}
}

// Send confirmation message to respondent after they provide their email address
function sendConfirmationEmail(record, s) {
	showProgress(1,100);
	$.post(dirname(dirname(app_path_webroot))+"/surveys/index.php?s="+s+"&__passthru="+encodeURIComponent("Surveys/email_participant_confirmation.php"),{ record: record, email: $('#confirmation_email_address').val() },function(data){
		showProgress(0,0);
		if (data == '0') {
			alert(woops);
		} else {
			simpleDialog(data,null,null,350);
			$('#confirmation_email_sent').show();
		}
	});
}

// Because the survey logo can lag in loading after the page loads, it can dislocate the text-to-speech
// icons of the title and instructions, so reload those icons when logo loads
function reloadSpeakIconsForLogo() {
	// If not using text-to-speech, then do nothing
	if (typeof texttospeech_js_loaded == 'undefined') return;
	// First, remove icons already loaded
	$('#surveyinstructions img.spkrplay, #surveytitle img.spkrplay').remove();
	// Now re-add the icons
	addSpeakIconsToSurvey(true);
}

// Using button click, add "speak" icon to all viable elements on the survey page
function addSpeakIconsToSurveyViaBtnClick(enable) {
	if (enable == '1') {
		if (typeof texttospeech_js_loaded == 'undefined') {
			$.loadScript(app_path_webroot+'Resources/js/TextToSpeech.js');
		} else {
			addSpeakIconsToSurvey();
		}
		$('#enable_text-to-speech').hide();
		$('#disable_text-to-speech').show();
		setCookie('texttospeech','1',365);
	} else {
		if (typeof texttospeech_js_loaded == 'undefined') {
			$.loadScript(app_path_webroot+'Resources/js/TextToSpeech.js');
		}
		$('#enable_text-to-speech').show();
		$('#disable_text-to-speech').hide();
		setCookie('texttospeech','0',365);
		$('.spkrplay').remove();
	}
}

// Deal with random IE image sizing issues
function imgSizeIE(ob) {
	var w = $(ob).width();
	if (w > 0 && w != 75) $(ob).width(w);
}

// If user tries to close the page after modifying any values on the data entry form, then stop and prompt user if they really want to leave page
window.onbeforeunload = function() {
	// If form values have changed...
	if (dataEntryFormValuesChanged) {
		var separator = "#########################################\n";
		// Prompt user with confirmation
		return separator + langDlgSaveDataTitleCaps + "\n\n" + langDlgSaveDataMsg + "\n" + separator;
	}
}

