

/*
 * That javascript function gather all the data needed to display the details
 * They are stocked in the details row of the line (which is not displayed)
 */
function fnFormatDetails( oTable, nTr ) {
  var fnData = oTable.fnGetData( nTr );
  var oTable2 = fnData[fnData.length-1];
  var sOut ='<div class="innerDetails">'+oTable2+'</div>';
  return sOut;
}

var anOpen = [];
// Create Rule table
function createRuleTable (gridId, data, needCheckbox, isPopup, allCheckboxCallback, refresh) {

  // Define which columns should be sorted by default
  var sortingDefault;
  if (needCheckbox) {
    sortingDefault = 1;
  } else {
    sortingDefault = 0;
  }

  // Define all columns of the table

  // Checkbox used in check if a Directive is applied by the Rule
  var checkbox =
    { "mDataProp": "applying"
    , "sTitle" : "<input id='checkAll' type='checkbox'></input>"
    , "sWidth": "30px"
    , "bSortable": false
    , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
        var data = oData;
        var elem = $("<input type='checkbox'></input>");
        elem.prop("checked", data.applying);
        elem.click( function () {
          data.checkboxCallback(elem.prop("checked"));
        } );
        elem.attr("id",data.id+"Checkbox");
        $(nTd).empty();
        $(nTd).prepend(elem);
      }
    };

  // Name of the rule
  // First mandatory row, so do general thing on the row ( line css, description tooltip ...)
  var name = 
    { "mDataProp": "name"
    , "sWidth": "90px"
    , "sTitle": "Name"
    , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
        var data = oData;
        // Define the elem and its callback
        var elem = $("<a></a>");
        if(isPopup) {
          elem.attr("href","/secure/configurationManager/ruleManagement#{'ruleId':"+data.id+"}");
        } else {
          elem.click(function() {data.callback("showForm");});
          elem.attr("href","javascript://");
        }
        elem.text(data.name);

        // Row parameters
        var parent = $(nTd).parent()
        // Add Class on the row, and id
        parent.addClass(data.trClass);
        parent.attr("id",data.id);

        // Description tooltip over the row
        if ( data.description.length > 0) {
          var tooltipId = data.id+"-description";
          parent.attr("tooltipid",tooltipId);
          parent.attr("title","");
          parent.addClass("tooltip tooltipabletr");
          var tooltip= $("<div></div>");
          var toolTipContainer = $("<div><h3>"+data.name+"</h3></div>");
          toolTipContainer.addClass("tooltipContent");
          toolTipContainer.attr("id",tooltipId);
          tooltip.text(data.description);
          toolTipContainer.append(tooltip);
          elem.append(toolTipContainer);
        }

        // Append the content to the row
        $(nTd).empty();
        $(nTd).prepend(elem);
      }
    };

  // Rule Category
  var category =
    { "mDataProp": "category"
    , "sWidth": "120px"
    , "sTitle": "Category"
    };

  // Status of the rule (disabled) add reson tooltip if needed
  var status=
    { "mDataProp": "status"
    , "sWidth": "60px"
    , "sTitle": "Status"
    , "sClass" : "statusCell"
    , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
        var data = oData;
        $(nTd).empty();
        var elem = $("<span></span>");
        elem.text(data.status);
        // If there a reasons field, add the tooltip
        if ("reasons" in data) {
          var tooltipId = data.id+"-status";
          elem.attr("tooltipid",tooltipId);
          elem.attr("title","");
          elem.addClass("tooltip tooltipable");
          var tooltip= $("<div></div>");
          var toolTipContainer = $("<div><h3>Reason(s)</h3></div>");
          toolTipContainer.addClass("tooltipContent");
          toolTipContainer.attr("id",tooltipId);
          tooltip.text(data.reasons);
          toolTipContainer.append(tooltip);
          $(nTd).prepend(toolTipContainer);
        }
        $(nTd).prepend(elem);
      }
    };

  // Compliance, with link to the edit form
  var compliance =
    { "mDataProp": "compliance"
    , "sWidth": "40px"
    , "sTitle": "Compliance"
    , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
        var data = oData;
        var elem = $("<a></a>");
        if(isPopup) {
          elem.attr("href","/secure/configurationManager/ruleManagement#{'ruleId':"+data.id+"}");
        } else {
          elem.click( function() {
            data.callback("showForm");
          } );
          elem.attr("href","javascript://");
        }
        elem.text(sData);
        $(nTd).empty();
        $(nTd).addClass(data.complianceClass+ " compliance");
        $(nTd).prepend(elem);
      }
    };

  // Action buttons
  var actions =
    { "mDataProp": "actions"
    , "sWidth": "20px"
    , "bSortable" : false
    , "sClass" : "parametersTd"
    , "fnCreatedCell" :    function (nTd, sData, oData, iRow, iCol) {
        var data = oData;
        var elem = $("<buton></button>");
        elem.button();
        elem.addClass("smallButton");
        elem.click( function() { 
          data.callback("showEditForm");
        } );
        elem.text("Edit");
        $(nTd).empty();
        $(nTd).prepend(elem);
      }
    };

  // Choose which columns should be included
  var columns = [];
  if (needCheckbox) {
    columns.push(checkbox);
  }
  columns.push(name);
  columns.push(category);
  columns.push(status);
  columns.push(compliance);
  if (!isPopup) {
    columns.push(actions);
  }

  var params = {
      "bFilter" : true
	, "bPaginate" : true
	, "bLengthChange": true
	, "sPaginationType": "full_numbers"
	, "bStateSave": true
	, "sCookiePrefix": "Rudder_DataTables_"
	, "oLanguage": {
	      "sZeroRecords": "No matching rules!"
	    , "sSearch": ""
      }
	, "aaSorting": [[ sortingDefault, "asc" ]]
    , "sDom": '<"dataTables_wrapper_top"f<"dataTables_refresh">>rt<"dataTables_wrapper_bottom"lip>'
  }
  
  createTable(gridId,data,columns, params, refresh);
 
  // Add callback to checkbox column
  $("#checkAll").prop("checked", false);
  $("#checkAll").click( function () {
      var checked = $("#checkAll").prop("checked");
      allCheckboxCallback(checked);
  } );
  
}


function createComponentValueTable( gridId, data, level) {
	
  var columns = [
     {   "sWidth": "72.5%" 
       , "mDataProp": "value"
       , "sTitle": "Value"
     }
   , {   "sWidth": "16.4%" 
       , "mDataProp": "status"
       , "sTitle": "Status"
       , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
           $(nTd).addClass("center "+oData.statusClass);
         }
     }
   , {   "sWidth": "11.1%" 
       , "mDataProp": "compliance"
       , "sTitle": "Compliance"
       , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
            var elem = $("<a></a>");
            elem.attr("href","javascript://");
            elem.addClass("right noexpand");
            elem.text(sData);
            elem.click(function() {oData.callback()});
            $(nTd).empty();
            $(nTd).append(elem);
          }
     }
   ];
	
  var params = {
      "bFilter" : false
    , "bPaginate" : false
    , "bLengthChange": false
    , "bInfo" : false
    , "aaSorting": [[ 0, "asc" ]]
  }

  createTable(gridId,data,columns, params);

}

function createComponentTable (gridId,data,level) {
  
	var columns = [
	               {   "sWidth": "73.7%" 
	                   , "mDataProp": "component"
	                   , "sTitle": "Component"
	                   , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
	                	    if (oData.noExpand) {
			                  $(nTd).addClass("noexpand");
	                	    } else {
		                      $(nTd).addClass("listopen");
	                	    }
	                      }
	                 }
	               , {   "sWidth": "15.8%" 
	                   , "mDataProp": "status"
	                   , "sTitle": "Status"
	                   , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
	                       $(nTd).addClass("center "+oData.statusClass);
	                      }
	                 }
	               , {   "sWidth": "10.5%" 
	                   , "mDataProp": "compliance"
	                   , "sTitle": "Compliance"
	                   , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
	                       var elem = $("<a></a>");
	                       elem.addClass("right noexpand");
	                       elem.attr("href","javascript://");
	                       elem.text(sData);
	                       elem.click(function() {oData.callback()});
	                       $(nTd).empty();
	                       $(nTd).append(elem);
	                     }
	                 }
	               ];
	
  var params = {
      "bFilter" : false
    , "bPaginate" : false
    , "bLengthChange": false
    , "bInfo" : false
    , "aaSorting": [[ 0, "asc" ]]
    , "fnDrawCallback" : function( oSettings ) {
  	    createInnerTable(this,level,createComponentValueTable);
      }
  }

  createTable(gridId,data,columns, params);
}

function createDirectiveTable (gridId,data, level) {
	
	var columns = [
	               {   "sWidth": "75%" 
	                   , "mDataProp": "directive"
	                   , "sTitle": "Directive"
	                   , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
	                        $(nTd).addClass("listopen");
	                        
	                        var tooltipIcon = $("<img />");
	                        tooltipIcon.attr("src","/images/ic_question.png");
	                        tooltipIcon.addClass("reportIcon");
	                        var tooltipId = oData.id+"-tooltip";
	                        tooltipIcon.attr("tooltipid",tooltipId);
	                        tooltipIcon.attr("title","");
	                        tooltipIcon.addClass("tooltip tooltipable");
	                        var toolTipContainer= $("<div>Directive '<b>"+sData+"</b>' is based on technique '<b>"+oData.techniqueName+"</b>' (version "+oData.techniqueVersion+")</div>");
	                        toolTipContainer.addClass("tooltipContent");
	                        toolTipContainer.attr("id",tooltipId);

	                        var editLink = $("<a />");
	                        editLink.attr("href",'/secure/configurationManager/directiveManagement#{"directiveId":"'+oData.id+'"}')
	                        var editIcon = $("<img />");
	                        editIcon.attr("src","/images/icPen.png");
	                        editLink.click(function(e) {e.stopPropagation();})
	                        editLink.append(editIcon);
	                        editLink.addClass("reportIcon");
	                        
	                        $(nTd).append(tooltipIcon);
	                        $(nTd).append(toolTipContainer);
	                        $(nTd).append(editLink);
	                      }
	                 }
	               , {   "sWidth": "15%" 
	                   , "mDataProp": "status"
	                   , "sTitle": "Status"
	                   , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
	                     $(nTd).addClass("center "+oData.statusClass);
	                   }
	                 }
	               , {   "sWidth": "10%" 
	                   , "mDataProp": "compliance"
	                   , "sTitle": "Compliance"
	                   , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
	                       var elem = $("<a></a>");
	                       elem.addClass("right noexpand");
	                       elem.attr("href","javascript://");
	                       elem.text(sData);
	                       elem.click(function() {oData.callback()});
	                       $(nTd).empty();
	                       $(nTd).append(elem);
	                     }
	                 }
	               ];

  var params = {
	      "bFilter" : true
	    , "bPaginate" : true
	    , "bLengthChange": true
	    , "sPaginationType": "full_numbers"
	    , "bStateSave": true
	    , "sCookiePrefix": "Rudder_DataTables_"
	    , "oLanguage": {
	        "sSearch": ""
	      }
	    , "aaSorting": [[ 0, "asc" ]]
	      , "fnDrawCallback" : function( oSettings ) {
	    	  createInnerTable(this,level,createComponentTable);
	      }
	    , "sDom": '<"dataTables_wrapper_top"f<"dataTables_refresh">>rt<"dataTables_wrapper_bottom"lip>'
	  };	

  createTable(gridId,data,columns, params);

  createTooltip();
}

function refreshTable (gridId, data, level) {
  var table = $('#'+gridId).dataTable();
  table.fnClearTable();
  table.fnAddData(data);
}

function createInnerTable(myTable, level, createFunction) {
  var plusTd = $(myTable.fnGetNodes());
  plusTd.each( function () {
    $(this).unbind();
    $(this).click( function (e) {
      if ($(e.target).hasClass('noexpand')) {
        return false;
      } else {
        var fnData = myTable.fnGetData( this );
        var i = $.inArray( this, anOpen );
        var detailsId = fnData.id + "-details";
        if ( i === -1 ) {
          $(this).find("td.listopen").removeClass("listopen").addClass("listclose");
          var table = $("<table></table>");
          var tableId = fnData.id + "-compliance";
          table.attr("id",tableId);
          table.attr("cellspacing",0);
          table.addClass("noMarginGrid");
          var div = $("<div></div>");
          div.addClass("innerDetails level"+level);
          div.attr("id",detailsId);
          div.append(table);
          var nDetailsRow = myTable.fnOpen( this, div, 'details' );
          var res = createFunction(tableId, fnData.details, level);
          $('div.dataTables_wrapper:has(table.noMarginGrid)').addClass('noMarginGrid');
          $('#'+detailsId).slideDown(300);
          anOpen.push( this );
        } else {
          $(this).find("td.listclose").removeClass("listclose").addClass("listopen");
          $('#'+detailsId).slideUp(300, function () {
          	myTable.fnClose( this );
            anOpen.splice( i, 1 );
          } );
        }
      }
    } );
  } );
}


// Create a table from its id, data, columns, maybe the last one need to be all specific attributes, but for now only sorting
function createTable(gridId,data,columns, customParams, refresh) {
	
  var defaultParams = {
      "asStripeClasses": [ 'color1', 'color2' ]
    , "bAutoWidth": false
    , "aoColumns": columns
    , "aaData": data
    , "bJQueryUI": true
    }
  
  var params = $.extend({},defaultParams,customParams)
  $('#'+gridId).dataTable( params );
  $('#'+gridId+' thead tr').addClass("head");
  
  var refreshButton = $("<button><img src='/images/icRefresh.png'/></button>");
  refreshButton.button();
  refreshButton.attr("title","Refresh");
  refreshButton.click( function() { refresh(); } );
  refreshButton.addClass("refreshButton");
  $("#"+gridId+"_wrapper .dataTables_refresh").append(refreshButton);
  $("#"+gridId+"_wrapper .dataTables_refresh button").tooltip({
	  show: { effect: "none", delay: 0 }
    , hide: { effect: "none",  delay: 0 }
    , position: { my: "left+40 bottom-10", collision: "flipfit" } 
  } );
  
  $('.dataTables_filter input').attr("placeholder", "Search");
}
