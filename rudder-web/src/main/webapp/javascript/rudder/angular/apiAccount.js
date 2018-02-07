/*
*************************************************************************************
* Copyright 2017 Normation SAS
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

var accountManagement = angular.module('accountManagement', ['datatables']);

var popUpCreated = false;
accountManagement.directive('validEmpty', function() {
  return {
      restrict: 'A'
    , require: 'ngModel'
    , link: function(scope, elm, attrs, ctrl) {
              var validator = function (viewValue) {
                var valid = (scope.newAccount == undefined || viewValue != "");

                ctrl.$setValidity('valid_empty', valid);
                return viewValue;
              }
              ctrl.$parsers.unshift(validator);
              ctrl.$formatters.unshift(validator);
            }
  };
} );
    
accountManagement.controller('AccountCtrl', function ($scope, $http, DTOptionsBuilder) {
  console.log(DTOptionsBuilder.newOptions());
  $scope.getAccounts = function() {
    $http.get(apiPath).
    then(function (response) {
      console.log(response.data.data.accounts);
      $scope.accounts = response.data.data.accounts;
      return $scope.accounts
    }, function(response) {
      $scope.errorTable = response.data;
      return $scope.errorTable
    });
  }


  $scope.deleteAccount = function(account,index) {
    $http.delete(apiPath + '/'+account.token).
        success(function(data, status, headers, config) {
          $scope.accounts.splice(index,1);
          $scope.myNewAccount = undefined;
        }).
        error(function(data, status, headers, config) {
         $scope.errorTable = data;
        });
        $('#oldAccountPopup').bsModal('hide');
  }

  $scope.regenerateAccount = function(account) {
	  console.log(account);
     $http.post(apiPath + '/'+account.token+"/regenerate").
       success(function(data, status, headers, config) {
         var newAccount = data.data.accounts[0];
         account.token = newAccount.token;
       }).
       error(function(data, status, headers, config) {
         $scope.errorTable = data;
       });
      $('#oldAccountPopup').bsModal('hide');
  }


$scope.addAccount = function() {
  var newAccount = { id : "", name : "", token: "not yet obtained", enabled : true, description : ""}
  $scope.myNewAccount = newAccount;
  $('#newAccountPopup').bsModal('show');  
}

//define what each column of the grid
//get from the JSON people
/*$scope.columnDefs = [
    { "aTargets":[0], "mDataProp": "name", "sWidth": "20%", "sTitle" : "Account Name" }
  , {   "aTargets":[1]
      , "mDataProp": "token"
      , 'bSortable': false
      , "sWidth": "40%", "sTitle" : "Token"
      , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
          var content  = $("<span class='tw-bs'><button class='btn btn-default reload-token'><span class='glyphicon glyphicon-repeat'></span></button></span>")
          content.find('.btn').click( function() {
            $scope.popupDeletion(oData,iRow,$scope.regenerateAccount,"Regenerate token of");
          });
          var stringContent = $("<span>"+sData+"</span>");
          $(nTd).empty();
          $(nTd).prepend(stringContent);
          $(nTd).prepend(content);
      }
    }
  , {   "aTargets"      : [2]
      , "mDataProp"     : "enabled"
      , 'bSortable': false
      , "sWidth": "10%"
        , "sTitle" : "Status"
    , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
        var color = sData ? "#5cb85c" : "#c9302c";
        var content  = $(" <b>"+$scope.accountDisabled(sData)+"</b>")
        $(nTd).empty();
        $(nTd).attr("style","text-align:center;  background-color:"+color);
        $(nTd).prepend(content);
      }
    }
  , {   "aTargets"      : [3]
      , "mDataProp"     : "enabled"
      , 'bSortable': false
      , "sWidth": "10%"
      
    , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
      $(nTd).addClass("tw-bs action-left");
      var data = oData
      var editButton  = $("<button class='btn btn-default'>Edit</button>");
      editButton.click( function() {
        data.oldName = data.name;
        $scope.popupCreation(data,iRow);
      });
      $(nTd).empty();
      $(nTd).prepend(editButton);
    }
  }
  , {   "aTargets"      : [4]
  , "mDataProp"     : "enabled"
  , 'bSortable': false
  , "sWidth": "10%"
  , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
      $(nTd).addClass("tw-bs action-center");
      var data = oData
      var statusButton = $scope.enableButton(data,iRow);
      $(nTd).empty();
      $(nTd).prepend(statusButton);
    }
}
  , {   "aTargets"      : [5]
  , "mDataProp"     : "enabled"
  , 'bSortable': false
  , "sWidth": "10%"
  , "fnCreatedCell" : function (nTd, sData, oData, iRow, iCol) {
      $(nTd).addClass("tw-bs action-right");
      var data = oData
      var deleteButton  = $("<button class='btn btn-danger'>Delete</button>");
      deleteButton.click( function() {
        $scope.popupDeletion(data,iRow,$scope.deleteAccount,"Delete");
      });
      $(nTd).empty();
      $(nTd).prepend(deleteButton);
    }
} ,  { "aTargets":[6], "mDataProp": "acls", "sWidth": "20%", "sTitle" : "ACLs" }
]
*/

$scope.options =
	DTOptionsBuilder.newOptions().
	  withPaginationType('full_numbers').
	  withDOM('<"dataTables_wrapper_top newFilter"f<"dataTables_refresh">>rt<"dataTables_wrapper_bottom"lip>').
	  withLanguage({
		    "sSearch": ""
	  }).
	  withOption("bLengthChange", true)
/*{
      "bFilter" : true
    , "bPaginate" : true
    , "bLengthChange": true
    , "sPaginationType": "full_numbers"
    , "oLanguage": 
    , "aaSorting": [[ 0, "asc" ]]
    , "sDom": 
  };*/

$scope.popupCreation = function(account,index) {
     $scope.myNewAccount = account;
     // Maybe should use indexOf
     $scope.myNewAccount.index = index;
     $("#newAccountName").focus();
  $('#newAccountPopup').bsModal('show');

  return account;
};

$scope.popupDeletion = function(account, action, actionName) {
    $scope.myOldAccount = account;
    $scope.myOldAccount.action = function(a) { return action(account); };
    $scope.myOldAccount.actionName = actionName;
    $('#oldAccountPopup').bsModal('show');
  return account;
};

 $scope.closePopup = function () {
   $scope.myNewAccount = undefined;
   $scope.errorPopup = undefined;
 }

 $scope.checkAndSaveAccount = function (account,index,form) {
   if (form.$valid) {
     $scope.saveAccount(account,index,true);
   }
 }
 $scope.saveAccount = function(account,isPopup) {
   if (isPopup)  {
     $scope.errorPopup = undefined;
   } else {
     $scope.errorTable = undefined;
   }
   if(account.token == "not yet obtained" ) {
     $http.put(apiPath,account).
     success( function(data, status, headers, config) {
       var newAccount = data.data.accounts[0];
       var newLength = $scope.accounts.push(newAccount);
       newAccount.index = newLength - 1 ;
       $scope.myNewAccount = undefined;
       $('#newAccountPopup').bsModal('hide');
     }).
     error(function(data, status, headers, config) {
       if (isPopup)  {
         $scope.errorPopup = data;
       } else {
         $scope.errorTable = data;
       }
     });
   } else {
     $http.post(apiPath + '/'+account.token,account).
     success(function(data, status, headers, config) {
       var newAccount = data.data.accounts[0];
       //$scope.accounts[index] = newAccount;
       $.extend($scope.myNewAccount, newAccount);
       $scope.myNewAccount = undefined;
       $('#newAccountPopup').bsModal('hide');

     }).
     error(function(data, status, headers, config) {
       if (isPopup)  {
         $scope.errorPopup = data;
       } else {
         $scope.errorTable = data;
       }
     });
   }
 }

  $scope.formTitle = function(account) {
    if (account!= undefined && account.index!=undefined) {
        return  "Update account '"+account.oldName+"'";
    } else {
      return "Create a new Account";
    }
  }
  $scope.accountDisabled = function(isEnabled) {
    if(isEnabled) return "Enabled";
    else return "Disabled";
  }
  
  $scope.getAccounts();
    
  $scope.defineActionName = function(formName){
      if(formName !== undefined) {
        return formName.split(' ')[0];
      }
  }  
} );

$( document ).ready(function() {
  angular.bootstrap(document.getElementById('accountManagement'), ['accountManagement']);
});
