

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

  createTable(gridId,data,columns, sortingDefault);
 
  // Add callback to checkbox column
  $("#checkAll").prop("checked", false);
  $("#checkAll").click( function () {
      var checked = $("#checkAll").prop("checked");
      allCheckboxCallback(checked);
  } );
  
}


function refreshTable (gridId, data) {
  var table = $('#'+gridId).dataTable();
  table.fnClearTable();
  table.fnAddData(data);
}

// Create a table from its id, data, columns, maybe the last one need to be all specific attributes, but for now only sorting
function createTable(gridId,data,columns, sortingDefault) {
  $('#'+gridId).dataTable(
    { "asStripeClasses": [ 'color1', 'color2' ]
    , "bAutoWidth": false
    , "bFilter" : true
    , "bPaginate" : true
    , "bLengthChange": true
    , "sPaginationType": "full_numbers"
    , "bJQueryUI": true
    , "bStateSave": true
    , "sCookiePrefix": "Rudder_DataTables_"
    , "oLanguage": {
          "sZeroRecords": "No matching rules!"
        , "sSearch": ""
        }
    , "aaData": data
    , "aaSorting": [[ sortingDefault, "asc" ]]
    , "aoColumns": columns
    , "sDom": '<"dataTables_wrapper_top"fl>rt<"dataTables_wrapper_bottom"ip>'
    }
  );
  $('.dataTables_filter input').attr("placeholder", "Search");
}
