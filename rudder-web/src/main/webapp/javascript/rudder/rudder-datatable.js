

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
function createRuleTable (gridId, data, needCheckbox, isPopup, allCheckboxCallback) {

  // Define which columns should be sorted by default
  var sortingDefault;
  if (needCheckbox) {
    sortingDefault = 1;
  } else {
    sortingDefault = 0;
  }

  // Define all columns of the table
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

  var name = 
    { "mDataProp": "name"
    , "sWidth": "90px"
    , "sTitle": "Name"
    , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
        var data = oData;
        var elem = $("<a></a>");
        if(isPopup) {
          elem.attr("href","/secure/configurationManager/ruleManagement#{'ruleId':"+data.id+"}");
        } else {
          elem.click(function() {data.callback("showForm");});
          elem.attr("href","javascript://");
        }
        elem.text(data.name);
        var parent = $(nTd).parent()
        parent.addClass(data.trClass);
        parent.attr("id",data.id);
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
        $(nTd).empty();
        $(nTd).prepend(elem);
      }
    };

  var category =
    { "mDataProp": "category"
    , "sWidth": "120px"
    , "sTitle": "Category"
    };
  
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

  var compliance =
    { "mDataProp": "compliance"
    , "sWidth": "40px"
    , "sClass": "compliance"
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
        $(nTd).addClass(data.complianceClass);
        $(nTd).prepend(elem);
      }
    };

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
	, "sDom": '<"dataTables_wrapper_top"fl>rt<"dataTables_wrapper_bottom"ip>'
  }
  
  createTable(gridId,data,columns, params);
 
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
  
  /*
            if (component.componentValues.forall( x => x.componentValue =="None")) {
              // only None, we won't show the details, we don't need the plus and that td should not be clickable
              ("* [class+]" #> "noexpand").apply(componentDetails)
            } else {
              // standard  display that can be expanded
              val tooltipid = Helpers.nextFuncName
              val value = showComponentValueReport(component.componentValues,worstseverity)
              ( "#details *" #>  value &
                "tr [class+]" #> "cursor" &
                "#component [class+]" #>  "listopen"
              ) (componentDetails )
            }
   */
	
	var columns = [
	               {   "sWidth": "73.7%" 
	                   , "mDataProp": "component"
	                   , "sTitle": "Component"
	                   , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
	                       $(nTd).addClass("listopen");
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
	        "sSearch": "Search"
	      }
	    , "sDom": '<"dataTables_wrapper_top"fl>rt<"dataTables_wrapper_bottom"ip>'
	    , "aaSorting": [[ 0, "asc" ]]
	      , "fnDrawCallback" : function( oSettings ) {
	    	  createInnerTable(this,level,createComponentTable);
	      }
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
function createTable(gridId,data,columns, customParams) {
	
  var defaultParams = {
      "asStripeClasses": [ 'color1', 'color2' ]
    , "bAutoWidth": false
    , "aoColumns": columns
    , "aaData": data
    , "bJQueryUI": true
  }
  
  var params = $.extend({},defaultParams,customParams)
  $('#'+gridId).dataTable(params );
  $("#"+gridId+" thead tr").addClass("head");
  $('.dataTables_filter input').attr("placeholder", "Search");
}
