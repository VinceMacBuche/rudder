/*
*************************************************************************************
* Copyright 2011 Normation SAS
*************************************************************************************
*
* This file is part of Rudder.
*
* Rudder is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* In accordance with the terms of section 7 (7. Additional Terms.) of
* the GNU General Public License version 3, the copyright holders add
* the following Additional permissions:
* Notwithstanding to the terms of section 5 (5. Conveying Modified Source
* Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU General
* Public License version 3, when you create a Related Module, this
* Related Module is not considered as a part of the work and may be
* distributed under the license agreement of your choice.
* A "Related Module" means a set of sources files including their
* documentation that, without modification of the Source Code, enables
* supplementary functions or services in addition to those offered by
* the Software.
*
* Rudder is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Rudder.  If not, see <http://www.gnu.org/licenses/>.

*
*************************************************************************************
*/

/* Event handler function */


var bootstrapButton = $.fn.button.noConflict();
var bootstrapAlert = $.fn.alert.noConflict();
var bootstrapCarousel = $.fn.carousel.noConflict();
var bootstrapCollapse = $.fn.collapse.noConflict();
var bootstrapTooltip = $.fn.tooltip.noConflict();
var bootstrapPopover = $.fn.popover.noConflict();
var bootstrapScrollspy = $.fn.scrollspy.noConflict();
var bootstrapTab = $.fn.tab.noConflict();
var bootstrapAffix = $.fn.affix.noConflict();
var bootstrapModal = $.fn.modal.noConflict();
$.fn.bsModal = bootstrapModal;


/**
 * Instanciate the tooltip
 * For each element having the "tooltipable" class, when hovering it will look for it's
 * tooltipid attribute, and display in the tooltip the content of the div with the id
 * tooltipid
 */
function createTooltiptr() {
    $(".tooltipabletr").tooltip({
        show: {
            effect: "none",
            delay: 100
        },
        hide: {
            effect: "none",
            delay: 0
        },
        content: function () {
            return $("#" + $(this).attr("tooltipid")).html();
        },
        track : true
    });
}

function createTooltip() {
  $(".tooltipable").tooltip({
    show: {
      effect: "none",
            delay: 0
        },
        hide: {
      effect: "none",
            delay: 0
        },
      content: function() {
          return $("#"+$(this).attr("tooltipid")).html();
        },
        position: {
          my: "left top+15",
          at: "right top",
          collision: "flipfit"
        }
  });
  createTooltiptr();
}

function createTooltiptr() {
    $(".tooltipabletr").tooltip({
        show: {
          effect: "none",
                  delay: 100
              },
              hide: {
                    effect: "none",
                  delay: 0
              },
        content: function() {
            return $("#"+$(this).attr("tooltipid")).html();
          },
          track : true
    });
  }


function callPopupWithTimeout(timeout, popupName){
  setTimeout("createPopup('"+popupName+"')", timeout);
}

function createPopup(popupName){
    $('#'+popupName).bsModal('show');
    if($('#'+popupName+' #titleWorkflow').length){
        $('#'+popupName+' .modal-dialog').addClass('modal-lg');
    }
}

function reverseErrorDetails(){
    $('#showTechnicalErrorDetails .panel-title span').toggleClass('up');
}
/* ignore event propagation (IE compliant) */

function noBubble(event){
    if(event.stopPropagation){
      event.stopPropagation();
    };
    event.cancelBubble=true;
}

/* ignore enter in a field */

function refuseEnter(event)
{
    // IE / Firefox
    if(!event && window.event) {
        event = window.event;
    }
    // IE
    if(event.keyCode == 13) {
        event.returnValue = false;
        event.cancelBubble = true;
    }
    // DOM
    if(event.which == 13) {
        event.preventDefault();
        event.stopPropagation();
    }
}

/* portlet */

$(function() {

    $(".portlet").addClass("ui-widget ui-widget-content ui-helper-clearfix arrondis")
      .find(".portlet-header")
        .addClass("ui-widget-header arrondishaut")
        .end()
      .find(".portlet-content");

    $(".portlet-header .ui-icon").click(function() {
      $(this).toggleClass("ui-icon-minusthick").toggleClass("ui-icon-plusthick");
      $(this).parents(".portlet:first").find(".portlet-content").toggle();
    });

  });


/**
 * Check all checkbox named name according to the status of the checkbox with id id
 * @param id
 * @param name
 * @return
 */
function jqCheckAll( id, name )
{
   $("input[name=" + name + "][type='checkbox']").prop('checked', $('#' + id).is(':checked'));
}

/* popin */

// increase the default animation speed to exaggerate the effect
  $.fx.speeds._default = 1000;
  $(function() {
    $('#dialog').dialog({
      autoOpen: false,
      position: [250,100],
      width: 535,
      show: '',
      hide: ''
    });
    $('#openerAccount').click(function() {
      $('#dialog').dialog('open');
      return false;
    });
  });


// Details

  $.fx.speeds._default = 1000;
  $(function() {
    $('#dialogDetail').dialog({
    autoOpen: false,
    position: 'center',
    width: 300,
    show: '',
    hide: ''		
  });
  $('.openDetail').click(function() {
    $('#dialogDetail').dialog('open');
	  return false;
    });
  });

  // Logout
  $('#logout').click(function() {
    $('#ModalLogOut').bsModal('show');
    return false;
  });

/* button */

function correctButtons() {
  $("button, input:submit", "form").not(".tw-bs .btn").button();

  $(":button").not(".tw-bs .btn").button();

  $("button, input:submit, a", ".whoUser").not(".tw-bs .btn").button();

  $("a", ".whoUser").not(".tw-bs .btn").click(function() { return false; });

  $("#logout").click(function() { return false; });
}

function processKey(e , buttonId){
    if (null == e)
        e = window.event ;
    if (e.keyCode == 13)  {
        e.preventDefault();
        document.getElementById(buttonId).click();
        return false;
    }
}

function roundTabs() {
  $(".tabs").tabs();
  $(".tabsv").tabs();
  $(".tabsv").removeClass('arrondishaut').addClass('ui-tabs-vertical ui-helper-clearfix arrondis');
  $(".tabsv > ul").removeClass('arrondishaut').addClass('arrondisleft');
  $(".tabsv > ul > li").removeClass('arrondishaut').addClass('arrondisleft');

  // test auto-ready logic - call corner before DOM is ready
    $('.arrondis').corner("5px");
  $('.arrondishaut').corner("5px top");
  $('.arrondisbas').corner("5px bottom");
  $('.arrondisleft').corner("5px left");
}

/**
 * Move the filter and paginate zones in the location described by tableId_paginate_area and tableId_filter_area
 * @param tableId
 * @return
 */
function moveFilterAndPaginateArea(tableId) {
  $(tableId+"_paginate_area").append($(tableId+"_next")).append($(tableId+"_info")).append($(tableId+"_previous"));

  if ($(tableId+"_filter_area")) {
    $(tableId+"_filter_area").append($(tableId+"_filter"));
  }
}



/**
 * Move the filter and paginate zones in the location described by tableId_paginate_area and tableId_filter_area
 * move the info (1 to 10) to the info area
 * @param tableId
 * @return
 */
function moveFilterAndFullPaginateArea(tableId) {
  $(tableId+"_paginate_area").append($(tableId+"_paginate"));
  $(tableId+"_info_area").append($(tableId+"_info"));
  if ($(tableId+"_filter_area")) {
    $(tableId+"_filter_area").append($(tableId+"_filter"));
  }
  if ($(tableId+"_length")) {
    $(tableId+"_info_area").append($(tableId+"_length"));
  }

}


function dropFilterArea(tableId) {
  $(tableId+"_info").remove();
  $(tableId+"_filter").remove();
  $(tableId+"_length");
}

function activateButtonOnFormChange(containerDivId, buttonId, status) {
  $("#"+buttonId).button();

  if ("false"==status) {
    $('#'+buttonId).button( "option", "disabled", true );
  } else {
    $('#'+buttonId).button( "option", "disabled", false );
  }

  // all change on the form
  $('#'+containerDivId+' > form').change(function() { $('#'+buttonId).button( "option", "disabled", false );});
  // This one is for all input (text, textarea, password... and yes, button)
  $('#'+containerDivId+' :input').change(function() { $('#'+buttonId).button( "option", "disabled", false );});
  // this one is for the checkbox when using IE
  //if ($.browser.msie)
  //  $('#'+containerDivId+' > form :checkbox').bind('propertychange', function(e) {if (e.type == "change" || (e.type == "propertychange" && window.event.propertyName == "checked")) {  $('#'+buttonId).prop("disabled", false);}});

  // all change on not the form
  $('#'+containerDivId+' :radio').change(function() { $('#'+buttonId).button( "option", "disabled", false );});
  // This one is for all input (text, textarea, password... and yes, button)
  $('#'+containerDivId+' :input').keyup(function() { $('#'+buttonId).button( "option", "disabled", false );});

  $('#'+containerDivId+' :checkbox').bind('propertychange', function(e) {if (e.type == "change" || (e.type == "propertychange" && window.event.propertyName == "checked")) {  $('#'+buttonId).button( "option", "disabled", false );}});

}

/**
 *
 */
function activateButtonDeactivateGridOnFormChange(containerDivId, buttonId, gridId, saveButton) {
  $("#"+buttonId).button();

  // all change on the form
  $('#'+containerDivId+' > form').change(function() { disableButton(saveButton);});
  // This one is for all input (text, textarea, password... and yes, button)
  $('#'+containerDivId+' :input').change(function() { disableButton(saveButton);});

  // all change on not the form
  $('#'+containerDivId+' :radio').change(function() { disableButton(saveButton);});
  // This one is for all input (text, textarea, password... and yes, button)
  $('#'+containerDivId+' :input').keyup(function() { disableButton(saveButton);});

  $('#'+containerDivId+' :checkbox').bind('propertychange', function(e) {
    if (e.type == "change" || (e.type == "propertychange" && window.event.propertyName == "checked")) {
      disableButton(saveButton);
    }
  });
}

/**
 * Disable button with id buttonId
 * @param buttonId
*/
function disableButton(buttonId) {
  $('#'+buttonId).button( "option", "disabled", true );
}

function scrollToElement(elementId, containerSelector) {
  var container = $(containerSelector);
  // We need to remove the container offset from the elem offset so we scroll the correct amount in scroll function
  var offset = $("#"+ elementId).offset().top - container.offset().top;
  container.animate({ scrollTop: offset }, 500);
}

function scrollToElementPopup(elementSelector, popupId){
    //get the top offset of the target anchor
    var target_offset = $("#"+ popupId +" .modal-body "+elementSelector).offset();
    var container = $("#"+popupId+" .modal-body");
    var target_top = target_offset.top-container.offset().top;
    //goto that anchor by setting the body scroll top to anchor top
    container.animate({scrollTop:target_top}, 500, 'easeInSine');
};
/*

Correctly handle PNG transparency in Win IE 5.5 & 6.
http://homepage.ntlworld.com/bobosola. Updated 18-Jan-2006.

Use in <HEAD> with DEFER keyword wrapped in conditional comments:
<!--[if lt IE 7]>
<script defer type="text/javascript" src="rudder.js"></script>
<![endif]-->

*/

var arVersion = navigator.appVersion.split("MSIE")
var version = parseFloat(arVersion[1])
/*
 * Sometimes body is not initiated on IE when javascript is launched
 * default value should be true/false ?
 */
var filters= true ;
if (document.body != null)
  {
    filters = document.body.filters;
  }

if ((version >= 5.5) && (filters))
{
   for(var i=0; i<document.images.length; i++)
   {
      var img = document.images[i]
      var imgName = img.src.toUpperCase()
      if (imgName.substring(imgName.length-3, imgName.length) == "PNG")
      {
         var imgID = (img.id) ? "id='" + img.id + "' " : ""
         var imgClass = (img.className) ? "class='" + img.className + "' " : ""
         var imgTitle = (img.title) ? "title='" + img.title + "' " : "title='" + img.alt + "' "
         var imgStyle = "display:inline-block;" + img.style.cssText
         if (img.align == "left") imgStyle = "float:left;" + imgStyle
         if (img.align == "right") imgStyle = "float:right;" + imgStyle
         if (img.parentElement.href) imgStyle = "cursor:hand;" + imgStyle
         var strNewHTML = "<span " + imgID + imgClass + imgTitle
         + " style=\"" + "width:" + img.width + "px; height:" + img.height + "px;" + imgStyle + ";"
         + "filter:progid:DXImageTransform.Microsoft.AlphaImageLoader"
         + "(src=\'" + img.src + "\', sizingMethod='scale');\"></span>"
         img.outerHTML = strNewHTML
         i = i-1
      }
   }
}

function showParameters(s){
  if(document.getElementById("showParametersInfo" + s).style.display == "none")
    document.getElementById("showParametersInfo" + s).style.display = "block";
  else
    document.getElementById("showParametersInfo" + s).style.display = "none";
}

function redirectTo(url,event) {
  // If using middle button, open the link in a new tab
  if( event.which == 2 ) {
    window.open(url, '_blank');
  } else {
    // On left button button, open the link the same tab
    if ( event.which == 1 ) {
      location.href=url;
    }
  }
}

/*
 * This function takes the content of 2 elements (represented by their ids)
 * , produce a diff beetween them and add the result in third element
 */
function makeDiff(beforeId,afterId,resultId) {
  function appendLines(c, s) {
    var res = s.replace(/\n/g, "\n" + c);
    res = c+res;
    if(res.charAt(res.length -1) == c)
      res = res.substring(0, res.length - 1);
    if(res.charAt(res.length -1) == "\n")
      return res;
    else
      return res+"\n"
  }
  var before = $('#'+beforeId);
  var after  = $('#'+afterId);
  var result = $('#'+resultId);
  var diff = JsDiff.diffLines(before.text(), after.text());
  var fragment = document.createDocumentFragment();
  for (var i=0; i < diff.length; i++) {
    if (diff[i].added && diff[i + 1] && diff[i + 1].removed) {
      var swap = diff[i];
      diff[i] = diff[i + 1];
      diff[i + 1] = swap;
    }

    var node;
    if (diff[i].removed) {
      node = document.createElement('del');
      node.appendChild(document.createTextNode(appendLines('-', diff[i].value)));
    }
    else
      if (diff[i].added) {
        node = document.createElement('ins');
        node.appendChild(document.createTextNode(appendLines('+', diff[i].value)));
      }
      else
        node = document.createTextNode(appendLines(" ", diff[i].value));

    fragment.appendChild(node);
  }

  result.text('');
  result.append(fragment);
}

function filterTableInclude(tableId, filter, include) {
  var finalFilter = "^"+filter+"$";
  var includeFilter;
  // No filter defined or table is not initialized
  if (filter === undefined || ! $.fn.dataTable.isDataTable( tableId )) {
    return;
  }

  
  if (filter === "") {
    includeFilter = filter;
  } else {
    includeFilter = finalFilter +"|^"+filter+" »";
  }
   
  var table = $(tableId).DataTable({"retrieve": true});
  if (include === undefined || include) {
    table.column(column).search(includeFilter,true,false,true ).draw();
  } else {
    table.column(column).search(finalFilter,true,false,true ).draw();
  }
}

var openAllNodes = function(treeId)  { $(treeId).jstree('open_all' ); return false; }
function toggleTree(treeId, toggleButton) {
  var tree = $(treeId).jstree()
  var isOpen = $(treeId).find(".jstree-open").length > 0
  if (isOpen) {
    tree.close_all()
  } else {
    tree.open_all()
  }
  $(toggleButton).children().toggleClass('fa-folder-open');
}
var searchTree = function(inputId, treeId) {

  if($(inputId).val() && $(inputId).val().length >= 3) {
      $(treeId).jstree('search', $(inputId).val());
  } else {
      $(treeId).jstree('clear_search');
  }
  enableSubtree($(".jstree-search"));
  return false;
}
var clearSearchFieldTree = function(inputId, treeId) {
  $(inputId).val('');
  $(treeId).jstree('clear_search');
  return false;
}

var twoDigitsFormator = d3.format("02d");

/* Facility to format a number on two digits */
function formatOn2Digits(number) {
  return twoDigitsFormator(number);
}

$(document).ready(function() {
  $.extend( $.fn.dataTable.ext.oSort, {
    "percent-pre": function ( a ) {
      var x = (a == "-") ? 0 : a.replace( /%/, "" );
      return parseFloat( x );
    }
  , "percent-asc": function ( a, b ) {
      return ((a < b) ? -1 : ((a > b) ? 1 : 0));
    }
  , "percent-desc": function ( a, b ) {
      return ((a < b) ? 1 : ((a > b) ? -1 : 0));
    }
  , "num-html-pre": function ( a ) {
      var x = String(a).replace( /<[\s\S]*?>/g, "" );
      return parseFloat( x );
    }
  , "num-html-asc": function ( a, b ) {
      return ((a < b) ? -1 : ((a > b) ? 1 : 0));
    }
  , "num-html-desc": function ( a, b ) {
      return ((a < b) ? 1 : ((a > b) ? -1 : 0));
    }
  } );
});

function checkMigrationButton(currentVersion,selectId) {
  var selectedVersion = $("#"+selectId+" option:selected" ).text()
  if (currentVersion === selectedVersion) {
    $('#migrationButton').button( "option", "disabled", true );
  } else {
    $('#migrationButton').button( "option", "disabled", false );
  }
}


/*
 * a function that allows to set the height of a div to roughtly
 * the height of the content of a Rudder page (i.e: without
 * the header/footer and the place for a title).
 */
function correctContentHeight(selector) {
  $(selector).height(Math.max(400, $(document).height() - 200));
}



/**
 * A pair of function that allows to parse and set the # part of
 * the url to some value for node/query
 */
function parseURLHash() {
  try {
    return JSON.parse(decodeURI(window.location.hash.substring(1)));
  } catch(e) {
    return {};
  }
}

function parseSearchHash(nodeIdCallback, queryCallback) {
  var hash = parseURLHash();
  if( hash.nodeId != null && hash.nodeId.length > 0) {
    nodeIdCallback(JSON.stringify(hash));
    $('#query-search-content').hide();
    $('#querySearchSection').removeClass('unfoldedSectionQuery');
  } else {
    $('#query-search-content').toggle();
    $('#querySearchSection').toggleClass('unfoldedSectionQuery');
  }
  if( hash.query != null && JSON.stringify(hash.query).length > 0) {
    queryCallback(JSON.stringify(hash.query));
  }
}

function updateHashString(key, value) {
  var hash = parseURLHash();
  hash[key] = value;
  window.location.hash = "#" + JSON.stringify(hash);
}

$(document).ready(function() {
  correctButtons();
  $("a", "form").click(function() { return false; });
  createTooltip();
  roundTabs();
});


