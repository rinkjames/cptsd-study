$(function(){
    enabledFloatingMatrixHeaders();
    // Maintain original value of dataEntryFormValuesChanged. Since we're just triggering piping here, no data is changed, but the jquery triggers will set dataEntryFormValuesChanged = true.
    var thisDataEntryFormValuesChanged = dataEntryFormValuesChanged;
    // If any fields are using @DEFAULT, @NOW, or @TODAY action tag, make sure piping of default value is performed on page load
    $('#questiontable tr.\\@DEFAULT, #questiontable tr.\\@NOW, #questiontable tr.\\@TODAY, #questiontable tr.\\@NOW-SERVER, #questiontable tr.\\@TODAY-SERVER, #questiontable tr.\\@NOW-UTC, #questiontable tr.\\@TODAY-UTC').each(function(i, row) {
        var fname = $(row).attr('sq_id');
        var field = $(row).find('[name="'+fname+'"]');
        if (field.val() != '') { // Only do anything if the field has a value
            // Trigger piping on page
            if ($(field).is('select')) {
                field.trigger('change');
            } else if ($('input[name="'+fname+'___radio"]').length) {
                $('input[name="'+fname+'___radio"][value="'+field.val()+'"]').trigger('click');
            } else {
                // Set @DEFAULT field to show that it changed value
                if ($('#'+fname+'-tr').hasClass('@DEFAULT') && $(this).attr('ignoreDefault') != '1') {
                    $('input[name="'+fname+'"][type="text"]').addClass('calcChanged');
                }
                // For all other fields, simply propagate the value manually to minimize branching/calculation triggering on page load
                $('.piping_receiver.piperec-'+event_id+'-'+fname+'-label').html(field.val());
            }
        }
    });
    // Now set dataEntryFormValuesChanged back to its original value
    dataEntryFormValuesChanged = thisDataEntryFormValuesChanged;
});
$(window).on('resize', function () {
    enabledFloatingMatrixHeaders();
});

// Is element visible in the browser viewport?
!function(t){var i=t(window);t.fn.visible=function(t,e,o){if(!(this.length<1)){var r=this.length>1?this.eq(0):this,n=r.get(0),f=i.width(),h=i.height(),o=o?o:"both",l=e===!0?n.offsetWidth*n.offsetHeight:!0;if("function"==typeof n.getBoundingClientRect){var g=n.getBoundingClientRect(),u=g.top>=0&&g.top<h,s=g.bottom>0&&g.bottom<=h,c=g.left>=0&&g.left<f,a=g.right>0&&g.right<=f,v=t?u||s:u&&s,b=t?c||a:c&&a;if("both"===o)return l&&v&&b;if("vertical"===o)return l&&v;if("horizontal"===o)return l&&b}else{var d=i.scrollTop(),p=d+h,w=i.scrollLeft(),m=w+f,y=r.offset(),z=y.top,B=z+r.height(),C=y.left,R=C+r.width(),j=t===!0?B:z,q=t===!0?z:B,H=t===!0?R:C,L=t===!0?C:R;if("both"===o)return!!l&&p>=q&&j>=d&&m>=L&&H>=w;if("vertical"===o)return!!l&&p>=q&&j>=d;if("horizontal"===o)return!!l&&m>=L&&H>=w}}}}(jQuery);

// Floating matrix headers
var matrices = [];
function enabledFloatingMatrixHeaders()
{
    var isSurvey = (page == 'surveys/index.php');
    if (!$('#questiontable').length) return;

    var form = $('#questiontable');
    var formPosLeft = form.position().left;
    var formWidth = form.width();
    var offset = isSurvey ? 0 : $('#west').width();
    var mtx_bgcolor = $('.labelmatrix ').css('background-color');

    // Destroy existing scroll-triggered function to reset it
    $(window).off("scroll", scrollHandler);

    // If no visible matrixes, then stop here
    if (!$('.headermatrix:visible').length) return;

    // Destroy all existing (in case this function has already been run)
    $('.floatMtxHdr').remove();

    // create floating headers
    var i = 0;
    $('.headermatrix:visible').each(function () {
        var header = $(this);
        var floatingHeader = $('<div></div>').append(header.clone());
        matrices[i++] = {
            "header": header,
            "floatingHeader": floatingHeader
        };
        floatingHeader
            .addClass('floatMtxHdr')
            .css({
                position: 'fixed',
                display: 'none',
                top: '-5px',
                left: (formPosLeft+offset)+'px',
                width: formWidth,
                'border': '1px solid #dddddd',
                'padding-bottom': '5px',
                'padding-left': formWidth - header.width(),
                'background-color': mtx_bgcolor
            });
        $('body').append(floatingHeader);
    });

    // decide when to show each floating header based on scroll
    $(window).scroll(scrollHandler);
}
var scrollHandler = function()
{
    var isSurvey = (page == 'surveys/index.php');
    var offsetTop = (!isSurvey && $('.rcproject-navbar:visible').length) ? $('.rcproject-navbar').outerHeight() : 0;
    var scrollTop = $(window).scrollTop();
    for (var i = 0; i < matrices.length; i++) {
        try {
            var header = matrices[i].header;
            var matrixGroup = header.attr('hdrmtxgrp');
            var inViewport = false;
            $('#questiontable tr.mtxfld[mtxgrp="'+matrixGroup+'"]').each(function(){
                if ($(this).visible(true)) {
                    inViewport = true;
                    return;
                }
            });
            var floatingHeader = matrices[i].floatingHeader;
            if (inViewport) {
                var headerTop = header.offset().top;
                var lastRow = $('#questiontable tr.mtxfld[mtxgrp=' + matrixGroup + ']:visible:last');
                var lastRowTop = lastRow.offset().top;
                if (scrollTop > headerTop && scrollTop <= lastRowTop) {
                    var top = 0;
                    if (scrollTop > (lastRowTop - floatingHeader.height())) top = -(scrollTop - (lastRowTop - floatingHeader.height()) + 2);		// + 2 to prevent floating header from overlapping last row
                    floatingHeader.css({
                        display: 'block',
                        top: offsetTop + top + 'px'
                    });
                } else {
                    floatingHeader.css({
                        display: 'none'
                    });
                }
            } else {
                floatingHeader.css({
                    display: 'none'
                });
            }
        } catch(e){}
    }
}

// Update checkboxes for piping
function updatePipingCheckboxes(ob) {
    var name = $(ob).attr('name').substring(8, $(ob).attr('name').length);
    var labelsChecked = new Array(), valsChecked = new Array(), i=0;
    var labelsUnchecked = new Array(), valsUnchecked = new Array(), j=0;
    var isMatrix = $(ob).parent().hasClass('choicematrix');
    var matrixName = isMatrix ? $(ob).parentsUntil('table').parent().parentsUntil('tr').parent().attr('mtxgrp') : "";
    // Get labels of all choices checked
    $('form#form input[name="__chkn__'+name+'"]').each(function(){
        var thisCode = $(this).attr('code');
        var thisLabel = isMatrix ? $('#matrixheader-'+matrixName+'-'+thisCode).text().trim() : $(this).parent().text().trim();
        var thisChecked = $(this).prop('checked');
        var thisCheckedText = thisChecked ? lang_checked : lang_unchecked;
        var thisCheckedVal = thisChecked ? '1' : '0';
        if (thisChecked) {
            labelsChecked[i] = thisLabel;
            valsChecked[i] = thisCode;
            i++;
        } else {
            labelsUnchecked[j] = thisLabel;
            valsUnchecked[j] = thisCode;
            j++;
        }
        // Set "checked"/"unchecked" for any using [checkbox(code)]
        $(piping_receiver_class_field_js+event_id+'-'+name+'-choice-'+thisCode+'-label').html(thisCheckedText);
        $(piping_receiver_class_field_js+event_id+'-'+name+'-choice-'+thisCode+'-value').html(thisCheckedVal);
    });
    // If value is a Missing Data Code
    if (missing_data_codes_check && $('#'+name+'_MDLabel:visible').length) {
        labelsChecked[0] = $('#'+name+'_MDLabel').attr('label');
        valsChecked[0] = $('#'+name+'_MDLabel').attr('code');
    }
    // Set value for all piping receivers on page
    if (labelsChecked.length == 0) labelsChecked[0] = missing_data_replacement_js;
    if (labelsUnchecked.length == 0) labelsUnchecked[0] = missing_data_replacement_js;
    if (valsChecked.length == 0) valsChecked[0] = missing_data_replacement_js;
    if (valsUnchecked.length == 0) valsUnchecked[0] = missing_data_replacement_js;
    $(piping_receiver_class_field_js+event_id+'-'+name+'-checked-checked-label').html(labelsChecked.join(', '));
    $(piping_receiver_class_field_js+event_id+'-'+name+'-checked-unchecked-label').html(labelsUnchecked.join(', '));
    $(piping_receiver_class_field_js+event_id+'-'+name+'-checked-checked-value').html(valsChecked.join(', '));
    $(piping_receiver_class_field_js+event_id+'-'+name+'-checked-unchecked-value').html(valsUnchecked.join(', '));
}

// Radio fields
function updatePipingRadiosDo(ob) {
    // Remove "___radio" from end of name
    var name = ob.attr('name').substring(0, ob.attr('name').length-8);
    var label = ob.parent().html();
    // Remove radio input from label
    if (ob.attr('label') != null) {
        label = ob.attr('label');
    } else {
        label = label.substring(label.indexOf('>')+2);
    }
    if (label.substring(0,7) == '<label ') {
        // In case the label is still inside a <label> tag, get the contents of the tag
        label = $(label).first().html();
    }
    var val = (ob.val() != '') ? ob.val() : missing_data_replacement_js;
    updatePipingRadiosDoValLabel(name,val,label);
}
function updatePipingRadiosDoValLabel(name,val,label) {
    // Set value for all piping receivers on page
    $(piping_receiver_class_field_js+event_id+'-'+name).html(label);
    $(piping_receiver_class_field_js+event_id+'-'+name+'-label').html(label);
    $(piping_receiver_class_field_js+event_id+'-'+name+'-value').html(val);
    // Update drop-down options separately via ajax
    try{ updatePipingDropdowns(name,val); } catch(e) { }

}
function updatePipingRadios(selector) {
    $(selector).click(function(){
        updatePipingRadiosDo($(this));
    });
}

// Drop-down fields
function updatePipingDropdownsPre(selector) {
    $(selector).change(function(){
        var name = $(this).attr('name');
        // Find selected option to get its label
        var label = ($(this).val() != '') ? $("form#form select[name='" + name + "'] option:selected").text() : missing_data_replacement_js;
        var val = ($(this).val() != '') ? $(this).val() : missing_data_replacement_js;
        if (missing_data_codes_check && in_array($(this).val(), missing_data_codes)) {
            // Remove parentheses from label
            var posLastParen = label.lastIndexOf('(');
            if (posLastParen > 0) {
                label = label.substr(0, posLastParen).trim();
            }
        }
        // Set value for all piping receivers on page
        $(piping_receiver_class_field_js+event_id+'-'+name).html(label);
        $(piping_receiver_class_field_js+event_id+'-'+name+'-label').html(label);
        $(piping_receiver_class_field_js+event_id+'-'+name+'-value').html(val);
        // Update drop-down options separately via ajax
        updatePipingDropdowns(name,val);
    });
}

// Text fields
function updatePipingTextFields(selector) {
    $(selector).blur(function(){
        if ($(this).hasClass('autosug-search')) {
            var idname = $(this).prop('id').split('-');
            var name = idname[0];
            var val = $('#'+$(this).prop('id')+'-span').val();
        } else {
            var name = $(this).attr('name');
            var val = $(this).val();
        }
        val = (val != '') ? nl2br(filter_tags(val)) : missing_data_replacement_js;
        // Set value for all piping receivers on page
        $(piping_receiver_class_field_js+event_id+'-'+name).html(val);
        $(piping_receiver_class_field_js+event_id+'-'+name+'-label').html(val);
        // Update drop-down options separately via ajax
        updatePipingDropdowns(name,val);
    });
}

// Evaluate branching logic and show/hide table row based upon its evaluation
function evalLogic(this_field, byPassEraseFieldPrompt, logic) {
    if (logic == false) {
        // HIDE ROW (first evaluate if a checkbox)
        var is_chkbx = 0;
        var fldLen = 0;
        eval("var domfld = document.forms['form']."+this_field+";");
        if (isIE) {
            try {
                var fldexists = (domfld.value != null);
                if (fldexists) fldLen = domfld.value.length;
            } catch(e) {
                var fldexists = false;
                var fldLen = 0;
            }
        } else {
            var fldexists = (domfld != null);
            if (fldexists) fldLen = domfld.value.length;
        }
        if (!fldexists) {
            // Checkbox fields (might also be a "descriptive" field)
            var fldLen = anyChecked("form","__chkn__"+this_field);
            is_chkbx = 1;
        }
        var msg = (fldLen > 0) ? 'show' : '';
        var hasMissingDataCode = false;
        // Now hide the row
        if (msg=='show') {
            if (showEraseValuePrompt) {
                // If using randomization, make sure we're not going to erase the randomization field or stata field values
                if (randomizationCriteriaFieldList != null && in_array(this_field, randomizationCriteriaFieldList)) {
                    // Randomization fields CANNOT be hidden after randomization has happened, so stop here.
                    return false;
                }
                // Determine if we should prompt the user and erase the value
                hasMissingDataCode = (missing_data_codes_check && domfld.value != '' && in_array(domfld.value, missing_data_codes));
                var eraseIt = (page == 'surveys/index.php') ? true : (byPassEraseFieldPrompt ? false : (hasMissingDataCode ? true : confirm(brErase(this_field))));
            } else {
                var eraseIt = false;
            }
            if (eraseIt) {
                if (!hasMissingDataCode) {
                    if (is_chkbx) {
                        // Checkbox fields
                        checkAll(0, "form", "__chkn__" + this_field);
                    } else {
                        // Regular field
                        domfld.value = '';
                        // If a radio field, additionally make sure the radio buttons are all unchecked
                        if (document.forms['form'].elements[this_field + '___radio'] != null) {
                            uncheckRadioGroup(document.forms['form'].elements[this_field + '___radio']);
                        }
                        // If a select field with auto-complete enabled, then
                        if (document.getElementById('rc-ac-input_' + this_field) != null) {
                            document.getElementById('rc-ac-input_' + this_field).value = '';
                        }
                    }
                }
                document.getElementById(this_field+'-tr').style.display='none';
                // Remove "hasval" attribute from row
                if (document.getElementById(this_field+'-tr').getAttribute('hasval') != null) {
                    document.getElementById(this_field+'-tr').removeAttribute('hasval');
                }
            }
        } else {
            document.getElementById(this_field+'-tr').style.display='none';
        }
    } else {
        // SHOW ROW
        var showit = true;
        if (page == 'surveys/index.php') {
            // Survey page: Treat differently since it contains fields on the form that might need to remain hidden (because of multi-paging)
            if (document.getElementById(this_field+'-tr').getAttribute('class') != null) {
                if (document.getElementById(this_field+'-tr').getAttribute('class').indexOf('hidden') > -1) {
                    // If row has class 'hidden', then keep hidden
                    showit = false;
                }
            }
        }
        // Do not show it if it has any @HIDDEN action tag
        if (showit && document.getElementById(this_field+'-tr').getAttribute('class') != null) {
            var rowClasses = document.getElementById(this_field+'-tr').getAttribute('class').split(" ");
            if (in_array('@HIDDEN', rowClasses)
                || (page != 'surveys/index.php' && in_array('@HIDDEN-FORM', rowClasses))
                || (page == 'surveys/index.php' && in_array('@HIDDEN-SURVEY', rowClasses))
            ) {
                showit = false;
            }
        }
        // Now show the row, if applicable
        if (showit) document.getElementById(this_field+'-tr').style.display = (isIE && IEv<10 ? 'block' : 'table-row');
    }
}

// Action Tags: Function that is run on forms and surveys to perform actions based on tags in the Field Annotation text
function triggerActionTags() {
    // Is this a survey page?
    var isSurvey = (page == 'surveys/index.php');

    // Note: @HIDDEN tags are handled via CSS and also inside doBranching()
    // on forms/surveys, so we don't need to force them to be hidden here.

    // DISABLES ANY FIELD THAT CONTAINS @READONLY
    // Disable survey and form
    $("#questiontable tr.\\@READONLY").disableRowActionTag();
    // Disable surveyonly
    if (isSurvey) $("#questiontable tr.\\@READONLY-SURVEY").disableRowActionTag();
    // Disable formonly
    else $("#questiontable tr.\\@READONLY-FORM").disableRowActionTag();
}

// Hide row via @HIDDEN action tag
function triggerActionTagsHidden(isSurvey) {
    // Note: This is already done by CSS, but this is in case branching logic tries to reveal it.
    // Hide survey and form
    $("#questiontable tr.\\@HIDDEN").hide();
    // Hide surveyonly
    if (isSurvey) $("#questiontable tr.\\@HIDDEN-SURVEY").hide();
    // Hide formonly
    else $("#questiontable tr.\\@HIDDEN-FORM").hide();
}

// Disable row via @READONLY action tag
(function ( $ ) {
    $.fn.disableRowActionTag = function() {
        var tr = this;
        if (tr.length < 1) return;
        // Disable all inputs row, trigger blur (to update any piping), and gray out whole row
        $('input, select, textarea', tr).prop("disabled", true);
        // Disable buttons and all text links (ignore images surrounded by links, we just want text links)
        $('a:not(a:has(img))', tr).each(function(){
            $(this).attr('onfocus', '');
            if ($(this).hasClass('fileuploadlink')) {
                $(this).attr('href', 'javascript:;').attr('onclick', 'return false;');
            }
        });
        $('button, .ui-datepicker-trigger', tr).hide();
        // Disable sliders
        $("[id^=sldrmsg-]", tr).css('visibility','hidden');
        $("[id^=slider-]", tr).attr('onmousedown', '').slider("disable");
        setTimeout(function(){ $("[id^=slider-]", tr).slider("disable"); },100);
        setTimeout(function(){ $("[id^=slider-]", tr).slider("disable"); },1000);
    };
}( jQuery ));