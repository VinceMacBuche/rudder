'use strict';

// Helpers functions

var converter = new showdown.Converter();
// Swap two two items in an array based on their index
function swapTwoArrayItems(array, index1, index2) {
    var item = array[index1];
    array[index1] = array[index2];
    array[index2] = item;
    return array;
};

// check if the parameter used contains $(somecontent), where somecontent is neither
// a valid variable name (key.value), nor does contain /
// If the content is invalid, returns true, else false
var re = new RegExp(/\$\([^./]*\)/, 'm');

function detectIfVariableIsInvalid(parameter) {
  return re.test(parameter);
}

// define ncf app, using ui-bootstrap and its default templates
var app = angular.module('ncf', ['ui.bootstrap', 'ui.bootstrap.tpls', 'monospaced.elastic', 'dndLists', 'ngMessages', 'FileManagerApp'])
// A directive to add a filter on the technique name controller
// It should prevent having techniques with same name (case insensitive)
// It should not check with the original name of the technique so we can change its case
app.directive('techniquename', function($filter, $http, $q) {
  return {
    require: 'ngModel',
    link: function(scope, elm, attrs, ctrl) {
      ctrl.$asyncValidators.techniqueName = function(modelValue, viewValue) {
        if(viewValue===undefined) return false;

        // Get all techniqueNames in lowercase
        var request = $http.get(contextPath + '/secure/api/techniques').then(
        function successCallback(techniques) {
          // Technique provided by RUDDER
          var activeTechniqueNames = techniques.data.data.techniques.map(function(a) {return a.name.toLowerCase();});
          // Custom technique defined by user, display in editor
          var customTechniqueNames = scope.techniques.map(function (technique,index) { return technique.name.toLowerCase()});
          var allTechniques = activeTechniqueNames.concat(customTechniqueNames);
           // Remove the original name from the technique names array
          if (scope.originalTechnique !== undefined && scope.originalTechnique.name !== undefined) {
            allTechniques = $filter("filter")(allTechniques, scope.originalTechnique.name.toLowerCase(), function(actual,expected) { return ! angular.equals(expected,actual)})
          }
          // technique name is ok if the current value is not in the array
          var res = $.inArray(viewValue.toLowerCase(), allTechniques) === -1
          if (!res) {
            return $q.reject('Technique name must be unique');
          }
          return true;
        },
        function errorCallback(error) {
          errorNotification("Try to get all Techniques name, failed", error)
          return $q.reject ("Try to get all Techniques name, failed");
        });
        return request;
      };
    }
  };
});

app.directive('focusOn', function() {
   return function(scope, elem, attr) {
      scope.$on('focusOn', function(e, name) {
        if(name === attr.focusOn) {
          elem[0].focus();
        }
      });
   };
});

app.factory('focus', function ($rootScope, $timeout) {
  return function(name) {
    $timeout(function (){
      $rootScope.$broadcast('focusOn', name);
    });
  }
});

app.directive('bundlename', function($filter) {
  return {
    require: 'ngModel',
    link: function(scope, elm, attrs, ctrl) {
      ctrl.$validators.bundleName = function(modelValue, viewValue) {
         // Get all bundleNames
         var bundleNames = scope.techniques.map(function (technique,index) { return technique.bundle_name})
         // Remove the original bundle name from the bundle names array
         if (scope.originalTechnique !== undefined && scope.originalTechnique.bundle_name !== undefined) {
           bundleNames = $filter("filter")(bundleNames, scope.originalTechnique.bundle_name, function(actual,expected) { return ! angular.equals(expected,actual)})
         }
         // bundle name is ok if the current value is not in the array
         return $.inArray(viewValue, bundleNames) === -1
      };
    }
  };
});

app.directive('constraint', function($http, $q, $timeout) {
  return {
    scope: {
      parameter: '='
    , parameterinfo: "="
    },
    require: 'ngModel',
    link: function(scope, elm, attrs, ctrl) {
    if((scope.parameterinfo.constraints.allow_empty_string)&&(scope.parameter.value === undefined)){
      scope.parameter.value = "";
    }
    ctrl.$asyncValidators.constraint = function(modelValue, viewValue) {
      scope.parameter.$errors= [];
      if (modelValue === undefined) {
        return $q.reject("Value is empty");
      }
      var data = {"value" : modelValue, "constraints" : scope.parameterinfo.constraints}

      var timeoutStatus = false;
      var timeout = $q.defer();

      var request = $http.post(contextPath + "/secure/api/internal/techniques/parameter/check",data, { 'timeout' : timeout.promise }).then(
          function(successResult) {
            scope.parameter.$errors= [];
            if (! successResult.data.data.parameterCheck.result) {
              scope.parameter.$errors = successResult.data.data.parameterCheck.errors;
              return $q.reject('Constraint is not valid');
            }
            return $q.when(modelValue);
          }
        , function(errorResult) {
          // If there was an error with the request, accept the value, it will be checked when saving
          // Maybe we should display a warning but I'm not sure at all ...
            if (timeoutStatus) {
              return $q.when('request timeout');
            }
            return $q.when('Error during check request')
          }
      )

      $timeout( function() {
        timeoutStatus = true;
        timeout.resolve();
      }, 500);
      return request;
    };
  }
};
});

app.directive('showErrors', function() {
return {
  restrict: 'A',
  require:  '^form',
  link: function (scope, el, attrs, formCtrl) {
    // find the text box element, which has the 'name' attribute
    var inputEl   = el[0].querySelector("[name]");
    // convert the native text box element to an angular element
    var inputNgEl = angular.element(inputEl);
    // get the name on the text box so we know the property to check
    // on the form controller
    var inputName = inputNgEl.attr('name');
    // only apply the has-error class after the user leaves the text box
    inputNgEl.bind('blur', function() {
      el.toggleClass('has-error', formCtrl[inputName].$invalid);
    })
  }
}
});

app.filter('parameterName', function () {
  return function (text) {
      if (!text) {
          return text;
      }

      return text.charAt(0).toUpperCase() + text.substring(1).replace(/_/g, ' '); // Replaces all occurences
  };
})

// Declare controller ncf-builder
app.controller('ncf-builder', function ($scope, $uibModal, $http, $q, $location, $anchorScroll, $timeout, focus, $sce, fileManagerConfig, apiMiddleware, apiHandler, $window) {
  // Variable we use in the whole application
  $scope.test = "haha"
  //UI state
  $scope.ui = {
    loaded                 : false
  , showTechniques         : true
  , activeTab              : 'general'
  , showMethodsFilter      : true
  , methodTabs             : {}
  , selectedMethods        : []
  , editForm               : {}
  , openedParamDescription : []
  }
  // Path of ncf files, defined as a url parameter
  $scope.path;
  // generic methods container
  $scope.generic_methods;
  // Generic methods order by category, used when we want to add new methods
  $scope.methodsByCategory;
  // ncf technique container
  $scope.techniques;
  $scope.techniqueCategories = [];

  $scope.fileManagerState = {
    open : false,
    updating : false,
  };

  $scope.closeWindow = function(event, enforce){
    if((enforce)||(event.currentTarget == event.target)){
      $scope.fileManagerState.open = false;
    }
  }

  // Selected technique, undefined when there is no selected technique
  $scope.parameters;
  $scope.selectedTechnique;
  $scope.originalTechnique;

  //Generic methods filters
  $scope.filter = {
    compatibility  : "all"
  , showDeprecated : false
  , text           : ""
  }

  //Array containing the divergences of a technique
  $scope.diverges = [];

  var usingRudder = false;

  $scope.CForm = {};

  $scope.setPath = function() {
    var path = $location.search().path;
    if (path === undefined) {
      $scope.path = "";
    } else if ( path === true) {
      $scope.path = "";
    } else {
      $scope.path = path;
    }
  };
  // Callback when an element is dropped on the list of method calls
  // return the element that will be added, if false do not add anything
  $scope.dropCallback = function(elem, nextIndex, type){
    // Add element
    // if type is a bundle, then transform it to a method call and add it
    if (type === "bundle") {
      return toMethodCall(elem);
    }
    var selectedMethodIndex = $scope.ui.selectedMethods.findIndex(function(m){return angular.equals(elem, m)});
    // Keep the method opened if it was
    if (selectedMethodIndex>=0) {
      $scope.ui.selectedMethods[selectedMethodIndex] = elem;
    }
    return elem;
  }

// Define path by getting url params now
$scope.setPath();

// Define hash location url, this will make the page scroll to the good element since we use $anchorScroll
$scope.scroll = function(id) {
  $location.hash(id);
  $anchorScroll();
};

// Capitalize first letter of a string
$scope.capitaliseFirstLetter = function (string) {
  if (string.length === 0) {
    return string;
  } else {
    return string.charAt(0).toUpperCase() + string.slice(1);
  }
};

function errorNotification (message,details) {
  var errorMessage = message
  if (details !== undefined) {
    errorMessage += "\n\n Details: " + details
  }
  createErrorNotification(errorMessage);
}

function handle_error ( actionName ) {
  return function(data, status, headers, config) {
    if (status === 401) {
      $scope.authenticated = false;
      errorNotification('Could not authenticate '+ actionName, data.error.details);
    } else {
      if (data.error !== undefined) {
          $.each(data.error, function(index,error) {
            var details = error.details === undefined ? error.errorDetails : error.details
            errorNotification(error.message,details);
          })
      } else {
        errorNotification('Error '+ actionName, data.errorDetails)
      }
    }
  }
};

function defineMethodClassContext (method_call) {
  // First split from .
  var myclasses =  method_call.class_context.split(".");
  // find os class from the first class of class_context
  var osClass = find_os_class(myclasses[0], cfengine_OS_classes);
  if ( $.isEmptyObject(osClass)) {
    // first class is not an os class, class_context is only advanced class
    method_call.advanced_class = method_call.class_context;
  } else {
    // We have an os class !
    method_call.OS_class = osClass;
    if (myclasses.length > 1) {
      // We have more than one class, rest of the context is an advanced class
      myclasses.splice(0,1);
      var advanced_class = myclasses.join(".");
      if (advanced_class.startsWith("(")) {
        advanced_class = advanced_class.slice(1,-1);
      }
      method_call.advanced_class = advanced_class;
    }
  }
}
function updateResources() {
  var urlParam= ($scope.originalTechnique.bundle_name !== undefined) ?  $scope.selectedTechnique.bundle_name : "draft/" + $scope.selectedTechnique.internalId
  var resourceUrl = contextPath + '/secure/api/internal/techniques/' + urlParam +"/" + $scope.selectedTechnique.version +"/resources"
  $http.get(resourceUrl).then(
    function(response) {
      $scope.selectedTechnique.resources = response.data.data.resources;
      $scope.originalTechnique.resources = $scope.originalTechnique.resources === undefined ? response.data.data.resources : $scope.originalTechnique.resources;
      $scope.fileManagerState.updating = false;
    }
  , function(response) {
      // manage error
      $scope.fileManagerState.updating = false;
    }
  )
}


// Transform a ncf technique into a valid UI technique
// Add original_index to the method call, so we can track their modification on index
// Handle classes so we split them into OS classes (the first one only) and advanced classes
function toTechUI (technique, version) {
  if ("method_calls" in technique) {
    var calls = technique.method_calls.map( function (method_call, method_index) {
      method_call["original_index"] = method_index;

      // Handle class_context
      defineMethodClassContext(method_call)

      // support of version 1.0, method parameters are formatted like that : 'args':[param1,param2 ...]
      // be careful position of the parameters in 'args' define in which method's parameters  it will be paired
      if(version == 1.0){
        method_call.parameters=$scope.getMethodParameters(method_call)
      }

      return method_call;
    } );
    technique.method_calls = calls;
  }
  return technique;
};

// to support version 1.0 of technique
$scope.getMethodParameters = function(method_call) {
  var params = [];
  // parameter information are stored into generic methods (maybe a better solution would be to merge data when we fetch them ...)
  if (method_call.method_name in $scope.generic_methods ) {
    var method = angular.copy($scope.generic_methods[method_call.method_name]);
    for (var i = 0; i < method.parameter.length; i++) {
       var parameter = method.parameter[i];
       var param_value = method_call.args[i];
       // Maybe parameter does not exists in current method_call, replace with empty value
       param_value = param_value !== undefined ? param_value : '';
       parameter["value"] = param_value;
       parameter["$errors"] = [];
       params.push(parameter);
    }
  } else {
    // We have no informations about this generic method, just call it 'parameter'
    params = method_call.args.map( function(value) {
               return {
                   "name" : "Paramter"
                 , "value" : value
                 , "description" : "Unknown parameter"
                 , "type" : "string"
                 , "$errors" : []
               };
             });
  }
  return params;
};



// Transform a ui technique into a valid ncf technique by removint original_index param
function toTechNcf (baseTechnique) {
  var technique = angular.copy(baseTechnique);
  var calls = technique.method_calls.map( function (method_call, method_index) {
    delete method_call.original_index;
    return method_call
  });
  technique.method_calls = calls;
  return technique;
};


// Check if a technique is selected
$scope.isSelected = function(technique) {
  if($scope.originalTechnique==undefined || technique==undefined) return false;
  return $scope.originalTechnique.bundle_name == technique.bundle_name;
};

$scope.getSelectedMethodIndex = function(method) {
  var result = $scope.ui.selectedMethods.findIndex(function(m) {
    return method["$$hashKey"] === m["$$hashKey"];
  });
  return result;
};
// Check if a method is selected
$scope.methodIsSelected = function(method) {
  return $scope.getSelectedMethodIndex(method) >= 0;
};

$scope.getSessionStorage = function(){
  $scope.resetFlags();

  //Used to know if the technique has been deleted while the user was away
  var deleted = false;

  var storedSelectedTechnique,storedOriginalTechnique;
  storedSelectedTechnique = JSON.parse(sessionStorage.getItem('storedSelectedTechnique'));
  storedOriginalTechnique = JSON.parse(sessionStorage.getItem('storedOriginalTechnique'));

  if(storedOriginalTechnique !== null && storedOriginalTechnique.bundle_name === "") storedOriginalTechnique.bundle_name=undefined;
  if(storedSelectedTechnique !== null && storedSelectedTechnique.bundle_name === "") storedSelectedTechnique.bundle_name=undefined;

  $scope.originalTechnique = angular.copy(storedOriginalTechnique);

  if(storedSelectedTechnique !== null){
    //Restore selected technique and inform user
    createInfoNotification("Technique restored from current session.")
    $scope.selectedTechnique = angular.copy(storedSelectedTechnique);
    updateFileManagerConf()

    //Check if it's not a new technique
    if(storedOriginalTechnique.bundle_name !== undefined){

      var existingTechnique = $scope.techniques.find(function(technique){return technique.bundle_name === storedOriginalTechnique.bundle_name })
      if (existingTechnique !== undefined) {

        //Check if the origignal technique stored is different from the one actually saved
        if($scope.checkDiff(existingTechnique, storedOriginalTechnique)){
          $scope.conflictFlag = true;
          var modalInstance = $uibModal.open({
            templateUrl: 'RestoreWarningModal.html',
            controller: RestoreWarningModalCtrl,
            size: 'lg',
            backdrop : 'static',
            resolve: {
              technique  : function () {
                return $scope.selectedTechnique;
              }
              , editForm : function() {
                return  $scope.ui.editForm
              }
              , diverges : function() {
                return $scope.diverges;
              }
            }
          });
          modalInstance.result.then(function (doSave) {
            $scope.originalTechnique = existingTechnique;
            if (doSave) {
              $scope.selectedTechnique = angular.copy($scope.originalTechnique);
              updateFileManagerConf();
              $scope.resetFlags();
            }
          });
        }
      }else{
        //Technique has been deleted while user was away
        deleted = true;
      }
      $scope.suppressFlag = (deleted && !$scope.conflictFlag && $scope.originalTechnique.bundle_name !== undefined);
      if(!$scope.conflictFlag && !deleted){
        $scope.originalTechnique = existingTechnique;
      }
    }else{// else : New technique
      var saved = $scope.techniques.find(function(technique){return technique.bundle_name === $scope.selectedTechnique.bundle_name })
      if(saved !== undefined){
        $scope.originalTechnique = angular.copy(saved);
        $scope.selectedTechnique = angular.copy(saved);
      }
    }
  }// else : Empty session storage
  (function() {
    new ClipboardJS('.clipboard');
  })();
  //Correctly resize elastic textareas
  $timeout(function() {
    $scope.$broadcast("elastic:adjust");
  }, 0);
}
$scope.$watch('selectedTechnique', function(newValue, oldValue) {
  $scope.updateItemSessionStorage('storedSelectedTechnique' , oldValue, newValue);
},true);
$scope.$watch('originalTechnique', function(newValue, oldValue) {
  $scope.updateItemSessionStorage('storedOriginalTechnique' , oldValue, newValue);

},true);

$scope.clearSessionStorage = function(){
  //<Cleaning old cache>
  sessionStorage.removeItem('selectedTechnique');
  sessionStorage.removeItem('originalTechnique');
  //</>
  sessionStorage.removeItem('storedSelectedTechnique');
  sessionStorage.removeItem('storedOriginalTechnique');
  $scope.resetFlags();
}

$scope.resetFlags = function(){
  $scope.suppressFlag = false;
  $scope.conflictFlag = false;
}


$scope.updateItemSessionStorage = function(item, oldTechnique, newTechnique){
  //Checking oldTechnique allows us to not clear the session storage when page is loading so $scope.selectedTechnique and $scope.originalTechnique are still undefined.
  if(oldTechnique && !newTechnique){
    $scope.clearSessionStorage();
  } else if(newTechnique){
    var checkList      = $scope.getChecksList();
    var savedTechnique = {};
    var check, propertyChecked;
    for(check in checkList){
      propertyChecked       = newTechnique[check];
      //We can't store 'undefined' value, so we convert it into an empty string
      savedTechnique[check] = propertyChecked === undefined ? "" : angular.copy(propertyChecked);
    }
    sessionStorage.setItem(item, JSON.stringify(savedTechnique));
  }
}

// Call ncf api to get techniques
$scope.getTechniques = function () {
  $scope.techniques = [];
  $scope.generic_methods = {};

  $http.get(contextPath + '/secure/api/internal/methods').success(function(response, status, headers, config) {
      if (response.data !== undefined && response.data.methods !== undefined) {

        $.each( response.data.methods, function(methodName, method) {
          method.documentation = converter.makeHtml(method.documentation)
          $scope.generic_methods[methodName] = method;
        });
        $scope.methodsByCategory = $scope.groupMethodsByCategory();
      } else {
        errorNotification( "Error while fetching methods", "Data received via api are invalid")
      }

      $http.get(contextPath + '/secure/api/internal/techniques').success(function(response, status, headers, config) {
        if (response.data !== undefined && response.data.techniques !== undefined) {

          $.each( response.data.techniques, function(techniqueName, technique_raw) {
            var technique = toTechUI(technique_raw);
            $scope.techniques.push(technique);
          });

          $scope.getSessionStorage();
        } else {
          errorNotification( "Error while fetching techniques", "Data received via api are invalid")
        }

        // Display single errors
        $.each( response.errors, function(index, error) {
          errorNotification(error.message,error.details)
        })
      } ).error(handle_error(" while fetching techniques"));

    }).error(handle_error(" while fetching methods")).then(function(){$scope.ui.loaded = true;})

  $http.get(contextPath + '/secure/api/internal/techniques/categories').
  success(function(response, status, headers, config) {

    if (response.data !== undefined && response.data.techniqueCategories !== undefined) {

      for (var key in response.data.techniqueCategories) {
        $scope.techniqueCategories.push({"key" :key, "value": response.data.techniqueCategories[key]  })
      }

    } else {
      errorNotification( "Error while fetching technique categories", "Data received via api are invalid")
    }

    // Display single errors
    $.each( response.errors, function(index, error) {
      errorNotification(error.message,error.details)
    })
  } ).
  error(handle_error(" while fetching technique categories"));

};


// Group methods by category, a category of a method is the first word in its name
$scope.groupMethodsByCategory = function () {
  var groupedMethods = {};
  for (var methodKey in $scope.generic_methods) {
    var method = $scope.generic_methods[methodKey];
    var name = methodKey.split('_')[0];
    var grouped = groupedMethods[name];
    if (grouped === undefined) {
      groupedMethods[name] = [method];
    } else {
      groupedMethods[name].push(method);
    }
  };
  return groupedMethods;
};

// method to export technique content into a json file
$scope.exportTechnique = function(){
  // selectedTechnique exists otherwise the buttons couldn't call us
  var filename = $scope.selectedTechnique.name+'.json';
  // build the exported technique object from the current selected technique
  var calls = [];
  for (var i = 0; i < $scope.selectedTechnique['method_calls'].length; i++) {
    var call = $scope.selectedTechnique['method_calls'][i]
    calls[i] = {
      parameters:    call["parameters"],
      class_context: call["class_context"],
      method_name:   call["method_name"],
      component:     call["component"]
    }
  }
  var exportedTechnique = {
    type: 'ncf_technique', version: 2.0,
    data: {
      bundle_name : $scope.selectedTechnique["bundle_name"],
      description : $scope.selectedTechnique["description"],
      name        : $scope.selectedTechnique["name"],
      version     : $scope.selectedTechnique["version"],
      parameter   : $scope.selectedTechnique["parameter"],
      category    : $scope.selectedTechnique["category"],
      method_calls: calls
    }
  };

  var blob = new Blob([angular.toJson(exportedTechnique, true)], {type: 'text/plain'});
  if (window.navigator && window.navigator.msSaveOrOpenBlob) {
    window.navigator.msSaveOrOpenBlob(blob, filename);
  } else{
    var e = document.createEvent('MouseEvents'),
    a = document.createElement('a');
    a.download = filename;
    a.href = window.URL.createObjectURL(blob);
    a.dataset.downloadurl = ['text/json', a.download, a.href].join(':');
    e.initEvent('click', true, false, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
    a.dispatchEvent(e);
    // window.URL.revokeObjectURL(url); // clean the url.createObjectURL resource
  }
}


// method to import technique content from a json file
$scope.onImportFileChange = function (fileEl) {
  var files = fileEl.files;
  var file = files[0];
  var reader = new FileReader();

  reader.onloadend = function(evt) {
    if (evt.target.readyState === FileReader.DONE) {
      $scope.$apply(function () {
        var importedTechnique = JSON.parse(evt.target.result);
        if(importedTechnique['type'] == 'ncf_technique') {
          var version = importedTechnique['version']
          if(version != 1.0 && version != 2.0){
           alert("Unsupported technique version ! This version of Rudder only support import of ncf techniques files in format 1.0 and 2.0")
          } else {
            var technique = toTechUI(importedTechnique['data'], version);
            $scope.checkSelect(technique, $scope.selectTechnique);
            $scope.originalTechnique.bundle_name = undefined;
            $scope.originalTechnique.name = "";
            $scope.ui.editForm.$setDirty();
            $scope.suppressFlag = true;
          }
        } else {
          alert("Unsupported file type " + "'" + importedTechnique['type'] + "'");
        }
      });
    }
  };
  reader.readAsText(file);
}

  $scope.getCategory = function(selectedTechniqueCategory) {
    var category = $scope.techniqueCategories.find(function(value) { return value.key === selectedTechniqueCategory});

    category = category === undefined ? selectedTechniqueCategory : category
    return category
  }

  // Method used to check if we can select a technique without losing changes
  $scope.checkSelect = function(technique, select) {
    // No selected technique, select technique
    if ($scope.selectedTechnique === undefined) {
      select(technique);
    } else {
      if  ($scope.checkSelectedTechnique()) {
        // Selected technique is the same than actual selected technique, unselect it
        select(technique);
      } else {
        // Display popup that changes will be lost, and possible discard them
        $scope.selectPopup(technique, select);
      }
    }

    focus('focusTechniqueName');
  };

  // Click on a Technique
  // Select it if it was not selected, unselect it otherwise
  $scope.selectTechnique = function(technique) {
    $scope.fileManagerState.open   = false;
    $scope.resetFlags();
    // Always clean Selected methods and display methods list
    $scope.ui.selectedMethods = [];
    $scope.ui.openedParamDescription = [];
    // Check if that technique is the same as the original selected one
    var test1 = angular.copy($scope.originalTechnique)
    var test2 = angular.copy(technique)
    if(test1 && test1.resources !== undefined) delete test1.resources;
    if(test2 && test2.resources !== undefined) delete test2.resources;
    if(angular.equals(test1,test2) ) {
      // It's the same, unselect the technique
      $scope.selectedTechnique = undefined;
      $scope.originalTechnique = undefined;
      $scope.ui.activeTab      = 'general';
    } else {
      // Select the technique, by using angular.copy to have different objects
      $scope.selectedTechnique=angular.copy(technique);
      $scope.originalTechnique=angular.copy($scope.selectedTechnique);
      if (technique.isClone) {
        $scope.originalTechnique.bundle_name = undefined;
      }
      $scope.$broadcast('endSaving');
      updateFileManagerConf()
    }
  };

  ////////// OS Class ////////

  // Structures we will use to select our class, we can't use the big structure os_classes, we have to use simple list with angular

  // List of all OS types
  $scope.type_classes = $.map(cfengine_OS_classes,function(val,i){return val.name;});

  // Build Map of all OS  by type
  $scope.os_classes_by_type = {};
  for (var index in cfengine_OS_classes) {
      // for each type ...
      var current_type = cfengine_OS_classes[index];
      // Get all oses
      var oses = $.map(current_type.childs, function(os,i) {
         return os.name;
      });
      $scope.os_classes_by_type[current_type.name] = oses;
  }

  // Regexp used for input version fields
  $scope.versionRegex = /^\d+$/;

  // List of oses using major or minor version
  $scope.major_OS = $.map(cfengine_OS_classes, function(v,i) { return $.map($.grep(v.childs,function(os,i2) { return os.major}), function(os,i2) {return os.name});});
  $scope.minor_OS = $.map(cfengine_OS_classes, function(v,i) { return $.map($.grep(v.childs,function(os,i2) { return os.minor}), function(os,i2) {return os.name});});

  // Functiopn to check if the os selected need major/minor versionning
  function checkVersion (os_list, method) {
    if (method.OS_class === undefined ) {
      return false;
    }
    return $.inArray(method.OS_class.name,os_list) >= 0;
  }

  $scope.checkMajorVersion= function(method) {
    return checkVersion($scope.major_OS, method);
  }

  $scope.checkMinorVersion= function(method) {
    return checkVersion($scope.minor_OS, method);
  }
  $scope.checkDeprecatedFilter = function(methods){
    return methods.some(function(method){return method.deprecated === undefined });
  }

  $scope.checkFilterCategory = function(methods){
    //If this function returns true, the category is displayed. Else, it is hidden by filters.
    var deprecatedFilter = $scope.filter.showDeprecated || $scope.checkDeprecatedFilter(methods);
    var agentTypeFilter  = false;
    var nameFilter = methods.some(function(m) {return m.name.toLowerCase().includes($scope.filter.text.toLowerCase())});
    var i = 0;
    switch($scope.filter.compatibility){
      case "dsc":
        while(!agentTypeFilter && i<methods.length){
          agentTypeFilter = methods[i].agent_support.some(function(agent){return agent === "dsc"});
          i++;
        }
        break;
      case "classic":
        while(!agentTypeFilter && i<methods.length){
          agentTypeFilter = methods[i].agent_support.some(function(agent){return agent === "cfengine-community"});
          i++;
        }
        break;
      case "all":
        agentTypeFilter  = true;
        break;
    }
    return agentTypeFilter && deprecatedFilter && nameFilter;
  }
  $scope.checkFilterAllCategories = function(){
    for (var x in $scope.methodsByCategory){
      if($scope.checkFilterCategory($scope.methodsByCategory[x])){
        return true;
      }
    }
    return false;
  }

  $scope.checkMethodCallAgentSupport = function(methodName, agent){
    var gKey = Object.keys($scope.generic_methods).find(function(method){return $scope.generic_methods[method].bundle_name === methodName});
    var method = $scope.generic_methods[gKey];
    return $scope.checkAgentSupport(method,agent);
  }
  $scope.checkAgentSupport = function(method, agentSupport){
    return method.agent_support.some(function(agent){return agent === agentSupport});
  }
  $scope.checkFilterMethod = function(method){
    //If this function returns true, the method  is displayed. Else, it is hidden by filters.
    var deprecatedFilter = $scope.filter.showDeprecated || !method.deprecated;
    var agentTypeFilter  = false;
    switch($scope.filter.compatibility){
      case "dsc":
        agentTypeFilter = method.agent_support.some(function(agent){return agent === "dsc"});
        break;
      case "classic":
        agentTypeFilter = method.agent_support.some(function(agent){return agent === "cfengine-community"});
        break;
      case "all":
        agentTypeFilter = true;
        break;
    }
    return agentTypeFilter && deprecatedFilter;
  }

  // Function used when changing os type
  $scope.updateOSType = function(method) {
    // Reset selected OS
    method.OS_class.name = "Any";
    // Do other update cleaning
    $scope.updateOSName(method);
  }
  // Function used when changing selected os
  $scope.updateOSName = function(method) {
    // Reset versions inputs
    method.OS_class.majorVersion = undefined;
    method.OS_class.minorVersion = undefined;

    // Update class context
    $scope.updateClassContext(method);
  }

  // Update class context, after a change was made on classes
  $scope.updateClassContext = function(method) {

    // Define os class from selected inputs
    var os = undefined;

    // do not define os if nothing was selected
    if ( !(method.OS_class === undefined) ) {
      // Get class from os type and selected os
      os = getClass(method.OS_class);
    }

    if (os === undefined) {
      // No OS selected, only use advanced OS
      method.class_context = method.advanced_class;
    } else {
      if (method.advanced_class === undefined || method.advanced_class === "") {
        // No adanced class, use only OS
        method.class_context = os;
      } else {
        // Both OS and advanced. Use class_context os.advanced
        method.class_context = os+".("+method.advanced_class+")";
      }
    }
  }

  // Select a method in a technique
  $scope.selectMethod = function(method_call) {
    if($scope.methodIsSelected(method_call)){
      var methodIndex = $scope.getSelectedMethodIndex(method_call);
      $scope.ui.selectedMethods.splice(methodIndex, 1);
      // Scroll to the previously selected method category
      // We need a timeout so model change can be taken into account and element to scroll is displayed
      $timeout( function() {$anchorScroll();}, 0 , false)
    } else {
      $scope.updateClassContext(method_call);
      $scope.ui.selectedMethods.push(method_call);
    }
  };

  // toggle description of a technique parameter
  $scope.toggleParameterDescription = function(parameter) {
    if($scope.parameterDescriptionOpened(parameter)){
      $scope.ui.openedParamDescription = $scope.ui.openedParamDescription.filter(function(elem) { return elem !== parameter.id})
      $timeout( function() {$anchorScroll();}, 0 , false)
    } else {
      $scope.ui.openedParamDescription.push(parameter.id);
    }
  };

  $scope.parameterDescriptionOpened = function(parameter) {
    return $scope.ui.openedParamDescription.includes(parameter.id)
  }

  function toMethodCall(bundle) {
    var original_index = undefined;
    var call = {
        "method_name"    : bundle.bundle_name
      , "original_index" : original_index
      , "class_context"  : "any"
      , "agent_support"  : bundle.agent_support
      , "parameters"     : bundle.parameter.map(function(v,i) {
        return  {"name" : v['name'], "value" : undefined};
      })
      , 'deprecated'     : bundle.deprecated
      , "component"      : bundle.name
    }
    defineMethodClassContext(call)
    return angular.copy(call)
  }

  // Add a method to the technique
  $scope.addMethod = function(bundle) {
    if ($scope.selectedTechnique) {
      var call = toMethodCall(bundle);
      $scope.selectedTechnique.method_calls.push(call);
      $(function() {
        var gmList = $('form.editForm');
        var height = gmList[0].scrollHeight;
        gmList.stop().animate( {scrollTop:height} , 400);
      });
    }
  };

  // Check if a technique has not been changed, and if we can use reset function
  $scope.isUnchanged = function(technique) {
    if(technique === undefined) return true;
    //Check if there is unsaved resource
    var checkUntouchedResources = true;
    if( technique.resources !== undefined && technique.resources !== null ){
      checkUntouchedResources = !technique.resources.some(function(r){
        return r.state != "untouched";
      })
    }
    return (!$scope.checkDiff($scope.originalTechnique, technique) && checkUntouchedResources);
  };

  // Check if a technique has been saved,
  $scope.isNotSaved = function() {
    return ($scope.originalTechnique !== undefined && $scope.originalTechnique !== null) && $scope.originalTechnique.bundle_name === undefined;
  };

  // Check if a method has not been changed, and if we can use reset function
  $scope.canResetMethod = function(method) {
    var canReset = method ? true : false;
    if (!canReset) return false;
    if (method.original_index === undefined) {
      var current_method_name = method.method_name;
      var oldValue = toMethodCall($scope.generic_methods[current_method_name]);
      canReset = !(angular.equals(method.class_context, oldValue.class_context) && (angular.equals(method.parameters, oldValue.parameters)));
    } else {
      var oldValue = $scope.originalTechnique.method_calls[method.original_index];
      if ( oldValue === undefined) {
        canReset = false;
      }  else {
        canReset = ! angular.equals(method, oldValue);
      }
    }
    return canReset;
  };

  // Reset a method to the current value in the technique
  $scope.resetMethod = function(method) {
    var oldValue = undefined;
    if (method.original_index === undefined) {
      var current_method_name = method.method_name;
      oldValue = toMethodCall($scope.generic_methods[current_method_name]);
    }else{
      oldValue = $scope.originalTechnique.method_calls[method.original_index];
    }
    method.class_context = oldValue.class_context;
    method.parameters = oldValue.parameters;
  };

  // Create a new technique stub
  function newTech() {
    return { "method_calls" : []
    , "name"         : ""
    , "description"  : ""
    , "version"      : "1.0"
    , "bundle_name"  : undefined
    , "parameter"    : []
    , "resources"    : []
    , "internalId"   : uuidv4()
    , "category"     : "ncf_techniques"
    };
  }

  $scope.newTechnique = function() {
    if($scope.selectedTechnique !== undefined && $scope.selectedTechnique.bundle_name===undefined){
      $scope.resetTechnique();
    }else if($scope.selectedTechnique === undefined || $scope.selectedTechnique.bundle_name){
      $scope.checkSelect(newTech(), $scope.selectTechnique);
    }else{
      $scope.selectedTechnique = newTech
    }
    $scope.toggleDisplay(false)
  };


  // Utilitary methods on Method call


  $scope.getMethodBundleName = function(method_call) {
    if (method_call && method_call.method_name in $scope.generic_methods ) {
      return $scope.generic_methods[method_call.method_name].bundle_name;
    }
    return method_call.method_name;
  };

  // Get the desciption of a method call in definition of the generic method
  $scope.getMethodDescription = function(method_call) {
    if (method_call && method_call.method_name in $scope.generic_methods ) {
      return $scope.generic_methods[method_call.method_name].description;
    }
    return "";
  };

  $scope.getMethodDocumentation = function(method_call) {
    if (method_call && method_call.method_name in $scope.generic_methods ) {
      return $scope.generic_methods[method_call.method_name].documentation;
    }
    return "";
  };
  $scope.getMethodDocumentationHtml = function(method){
    return method.documentation ? $sce.trustAsHtml(method.documentation) : "";
  }

  $scope.showMethodDocumentation = function(event, method_name){
    event.stopPropagation();
    $scope.ui.showTechniques = false;
    $scope.generic_methods[method_name].showDoc = true;
    var method_element = document.getElementById(method_name);
    var topPos = method_element.offsetTop;
    $('#methods-list-container').stop().animate({scrollTop:topPos}, 200, 'swing', undefined);
  }

  $scope.methodUrl = function(method) {
    var name = method.bundle_name !== undefined ? method.bundle_name : $scope.getMethodBundleName(method);
    if (usingRudder) {
      return "/rudder-doc/reference/current/reference/generic_methods.html#"+name
    }
    return "https://docs.rudder.io/reference/current/reference/generic_methods.html#_"+name;
  }

  // Get the value of the parameter used in generated class
  $scope.getClassParameter= function(method_call) {
    if (method_call.method_name in $scope.generic_methods ) {
      var method = $scope.generic_methods[method_call.method_name];
      var class_parameter = method.class_parameter;
      var param = method_call.parameters.find(element => element.name === class_parameter);
      if (param === undefined)
        return method_call.parameters[0];
      else
        return param;
    } else {
      return method_call.parameters[0];
    }
  }

  // Get the class prefix value
  $scope.getClassPrefix= function(method_call) {
    if (method_call.method_name in $scope.generic_methods ) {
      var method = $scope.generic_methods[method_call.method_name];
      return method.class_prefix;
    } else {
      // Not defined ... use method name
      return method_call.method_name;
    }
  }

  // Get the class value generated from a class prefix and a class kind (kept,repaired,error, ...)
  $scope.getClassKind= function(method_call,kind) {
    var param = $scope.getClassParameter(method_call).value
    if (param === undefined) {
      param=""
    }
    // do not canonify what is between ${ }
    // regex description: replace every valid sequence followed by and invalid char with the sequence followed by _
    //                    a valid sequence is either a word or a ${} expression
    //                    a ${} expression can contain a word, dots, []'s, or a variable replacement (recursion is not managed, only first level)
    //                    ! do not replace by _ if we match the end of line
    param = param.replace(/\\'/g, "'")
                 .replace(/\\"/g, '"')
                 .replace(/((?:\w+|\$\{(?:[\w\.\[\]]|\$\{[\w\.\[\]]+?\})+?\})?)([^\$\w]|$)/g,
                          function(all,g1,g2) {
                            if (g2 == "" || g2 == "\n") { return g1; }
                            return g1+"_";
                          });
    return  $scope.getClassPrefix(method_call)+"_"+param +"_"+kind
  }


  $scope.checkMethodCall = function(method_call) {}
  // Check if the selected technique is correct
  // selected technique is correct if:
  // * There is at least one method call
  $scope.checkSelectedTechnique = function() {
    if($scope.selectedTechnique===undefined || $scope.selectedTechnique===null) return false;
    if (typeof($scope.selectedTechnique.method_calls) !== 'undefined') {
      var res = $scope.selectedTechnique.method_calls.length === 0;
      if ($scope.selectedTechnique.isClone) {
        return res
      }
      return res || $scope.isUnchanged($scope.selectedTechnique);
    }
    return false;
  }

  // Technique actions
  // clone method of specified index and add it right after index
  $scope.cloneMethod= function(index) {
    var newMethod = angular.copy($scope.selectedTechnique.method_calls[index]);
    delete newMethod.original_index;
    $scope.selectedTechnique.method_calls.splice(index+1, 0, newMethod);
    $scope.ui.selectedMethods.push(newMethod);
  }

  // Remove method on specified index
  $scope.removeMethod= function(index) {
    var methodIndex = $scope.getSelectedMethodIndex($scope.selectedTechnique.method_calls[index]);
    $scope.ui.selectedMethods.splice(methodIndex, 1);
    $scope.selectedTechnique.method_calls.splice(index, 1);
  }

  // Check if a method is deprecated
  $scope.isDeprecated = function(methodName) {
    return $scope.generic_methods[methodName].deprecated;
  };

  $scope.hasDeprecatedMethod = function(t){
    for(var i=0; i<t.method_calls.length; i++){
      if($scope.isDeprecated(t.method_calls[i].method_name))return true
    }
    return false;
  }

  // Move a method from an index to another index, and switch those
  $scope.move = function(from,to) {
    $scope.selectedTechnique.method_calls = swapTwoArrayItems($scope.selectedTechnique.method_calls,from,to);
  }

  // Move up the method in the hierachy
  $scope.moveUp = function(event, index) {
    event.stopPropagation();
    if(!$(event.currentTarget).hasClass('disabled')){
      $scope.move(index,index+1);
    }
  }

  // Move down the method in the hierachy
  $scope.moveDown = function(event, index) {
    event.stopPropagation();
    if(!$(event.currentTarget).hasClass('disabled')){
      $scope.move(index,index-1);
    }
  }

  // Resets a Technique to its original state
  $scope.resetTechnique = function() {
    $scope.ui.editForm.$setPristine();
    $scope.selectedTechnique=angular.copy($scope.originalTechnique);
    $scope.resetFlags();
    $scope.$broadcast('endSaving');
  };

  // Delete a technique
  $scope.deleteTechnique = function() {
    $http.delete(contextPath + "/secure/api/internal/techniques/"+$scope.selectedTechnique.bundle_name+"/"+$scope.selectedTechnique.version, {params : {force : false}}).
      success(function(data, status, headers, config) {
        createSuccessNotification("Technique '" + $scope.originalTechnique.name + "' deleted!")
        var index = $scope.techniques.findIndex(function(t){return t.bundle_name === $scope.originalTechnique.bundle_name});
        $scope.techniques.splice(index,1);
        $scope.ui.selectedMethods = [];
        $scope.selectedTechnique  = undefined;
        $scope.originalTechnique  = undefined;
      } ).
      error(handle_error("while deleting Technique '"+$scope.selectedTechnique.name+"'"));
  };

  $scope.setBundleName = function (technique) {
    if (technique.bundle_name === undefined) {
      technique.bundle_name =  $scope.getBundleName(technique.name);
    }
    return technique;
  }

  $scope.getBundleName = function (techniqueName) {
    // Replace all non alpha numeric character (\W is [^a-zA-Z_0-9]) by _
    return techniqueName ? techniqueName.replace(/\W/g,"_") : "";
  }

  $scope.updateBundleName = function () {
    if($scope.originalTechnique.bundle_name===undefined){
      $scope.selectedTechnique.bundle_name = $scope.getBundleName($scope.selectedTechnique.name);
    }
  };

  $scope.trimParameter = function(parameter) {
    return ! (parameter.constraints.allow_whitespace_string);
  }

  $scope.$on("saving",function(){
    $scope.saving = true;
  });
  $scope.$on("endSaving",function(){
    $scope.saving = false;
  });

  $scope.newParam = {}

  $scope.addParameter = function() {
    $scope.newParam.id = uuidv4();
    $scope.selectedTechnique.parameter.push(angular.copy($scope.newParam));
    $scope.newParam.name = "";
  }
  // Save a technique
  $scope.saveTechnique = function() {
    $scope.$broadcast('saving');
    // Set technique bundle name
    $scope.setBundleName($scope.selectedTechnique);
    // make a copy of data so we don't lose the selected technique
    var technique = angular.copy($scope.selectedTechnique);
    var origin_technique = angular.copy($scope.originalTechnique);
    // transform technique so it is valid to send to API:
    var ncfTechnique = toTechNcf(technique);

    // Get methods used for our technique so we can send only those methods to Rudder api instead of sending all methods like we used to do...
    var usedMethodsSet = new Set();
    ncfTechnique.method_calls.forEach(
      function(m) {
        usedMethodsSet.add($scope.generic_methods[m.method_name]);
      }
    );

    var usedMethods = Array.from(usedMethodsSet);

    var reason = "Updating Technique " + technique.name + " using the Technique editor";

    var data = { "technique": ncfTechnique, "methods":usedMethods, "reason":reason }

    // Function to use after save is done
    // Update selected technique if it's still the same technique
    // update technique from the tree
    var saveSuccess = function(data, status, headers, config) {

      // Technique may have been modified by ncf API
      // Not good anymore, but maybe
      ncfTechnique = data.data.techniques.technique;

      // Transform back ncfTechnique to UITechnique, that will make it ok
      var savedTechnique = toTechUI(ncfTechnique, 2.0);

      var invalidParametersArray = [];
      // Iterate over each parameters to ensure their validity
      ncfTechnique.method_calls.forEach(
        function(m) {
          m.parameters.forEach(
            function(parameter) {
              var value = parameter.value;
              if (detectIfVariableIsInvalid(value)) {
                invalidContent.push("<div>In generic method: <b>" +m.component + "</b>,  parameter: " + parameter.name + " has incorrect value " + value+"</div>");
              }
            }
          )
        }
      );

      if (invalidParametersArray.length > 0) {
        createWarningNotification("Some variables might be invalid (containing $() without . nor /):\n\n" + invalidParametersArray.join("\n"));
      }
      createSuccessNotification("Technique '" + technique.name + "' saved!");

      // Find index of the technique in the actual tree of technique (look for original technique)
      var index = $scope.techniques.findIndex(function(t){return t.bundle_name == origin_technique.bundle_name});
      if ( index === -1) {
       // Add a new techniuqe
       $scope.techniques.push(savedTechnique);
      } else {
       // modify techique in array
       $scope.techniques[index] = savedTechnique;
      }

      // Update technique if still selected
      if (angular.equals($scope.originalTechnique, origin_technique)) {
        // If we were cloning a technique, remove its 'clone' state
        savedTechnique.isClone    = false;
        $scope.originalTechnique  = angular.copy(savedTechnique);
        // Resources will be reset by resource manager if original technique resources are undefined
        $scope.selectedTechnique  = angular.copy(savedTechnique);
        // We will lose the link between the selected method and the technique, to prevent unintended behavior, close the edit method panel
        $scope.ui.selectedMethods = [];
      }

      updateFileManagerConf()
      $scope.resetFlags();
    }

    var saveError = function(action, data) {
      return handle_error("while "+action+" Technique '"+ data.technique.name+"'")

    }

    // Actually save the technique through API
    if ($scope.originalTechnique.bundle_name === undefined) {
      $http.put(contextPath + "/secure/api/internal/techniques", data).success(saveSuccess).error(saveError("creating", data)).finally(function(){$scope.$broadcast('endSaving');});
    } else {
      $http.post(contextPath + "/secure/api/internal/techniques", data).success(saveSuccess).error(saveError("updating", data)).finally(function(){$scope.$broadcast('endSaving');});
    }
  };

  // Popup definitions
  // Popup to know if there is some changes to save before switching of selected technique
  // parameters:
  // - Next technique you want to switch too
  // - Action to perform once the technique you validate the popup
  $scope.selectPopup = function( nextTechnique, select ) {
    var modalInstance = $uibModal.open({
      templateUrl: 'SaveChangesModal.html',
      controller: SaveChangesModalCtrl,
      size: 'lg',
      resolve: {
          technique : function () {
            return $scope.originalTechnique;
          }
        , editForm  : function() {
            return  $scope.ui.editForm
          }
        , diverges  : function(){
            return $scope.diverges;
          }
      }
    });
    modalInstance.result.then(function (doSave) {
      if (doSave) {
        $scope.saveTechnique();
      }
      // run on success function
      select(nextTechnique)
    });
  };

  $scope.clonePopup = function() {
    var modalInstance = $uibModal.open({
      templateUrl: 'template/cloneModal.html',
      controller: cloneModalCtrl,
      resolve: {
        technique: function () {
          return angular.copy($scope.originalTechnique);
        }
        , techniques : function() { return $scope.techniques}
      }
    });
    modalInstance.result.then(function (technique) {
      technique.isClone = true
      $scope.selectTechnique(technique);
    });
  };

  $scope.confirmPopup = function(actionName,kind,action,elem, name) {
    var modalInstance = $uibModal.open({
      templateUrl: 'template/confirmModal.html',
      controller: confirmModalCtrl,
      resolve: {
        actionName: function() { return actionName; }
      , kind : function() { return kind; }
      , name : function() { return name; }
      }
    });
    modalInstance.result.then(function () {
        action(elem)
    });
  };

  $scope.reloadData = function() {
    $scope.getTechniques();
  }

  $scope.reloadPage = function() {
    window.top.location.reload();
  }

  $scope.checkMissingParameters = function(parameters, parameterInfo){
    var result = [];
    for(var i=0; i<parameters.length; i++) {
      if(parameterInfo[i].constraints.allow_empty_string === false && !parameters[i].value && (parameters[i].$errors && parameters[i].$errors.length <= 0)){
        result.push(parameters[i].name);
      }
    }
    return result;
  }
  $scope.checkErrorParameters = function(parameters){
    var result = [];
    for(var i=0; i<parameters.length; i++) {
      if(parameters[i].$errors && parameters[i].$errors.length > 0){
        result.push(parameters[i].name);
      }
    }
    return result;
  }

  $scope.isUsed = function(method){
    var i,j = 0;
    if(method.deprecated){
      for(i=0; i<$scope.techniques.length; i++){
        while(j<$scope.techniques[i].method_calls.length){
          if($scope.techniques[i].method_calls[j].method_name == method.bundle_name){
            return true;
          }
          j++;
        }
      }
    }
  return false;
  };

  $scope.getTooltipContent = function(method){
    var description = "";
    var deprecatedMessage = "";
    description = $scope.getMethodDescription(method)!= "" ? "<div class='description'>"+$scope.getMethodDescription(method)+"</div>" : "";
    if(method.deprecated || $scope.isDeprecated(method.method_name)){
      deprecatedMessage = "<div class='deprecated-info'><div>This generic method is <b>deprecated</b>.</div> <div class='deprecated-message'><b>↳</b>"+method.deprecated+"</div></div>";
    }
    var tooltipContent = "<div>" + description + deprecatedMessage + "</div>";
    return $sce.trustAsHtml(tooltipContent);
  }

  $scope.getStatusTooltipMessage = function(method){
    var msg;
    var missingParameters = $scope.checkMissingParameters(method.parameters,$scope.generic_methods[method.method_name].parameter).length;
    var errorParameters   = $scope.checkErrorParameters(method.parameters).length;
    if(errorParameters>0){
      msg = (errorParameters + " invalid parameter" + (errorParameters > 1 ? 's' : ''))
    }else if (missingParameters>0){
      msg = (missingParameters + " required parameter"+ (missingParameters > 1 ? 's' : '') +" missing")
    }else if ($scope.canResetMethod(method)){
      msg = "This generic method has been edited"
    }else{
      msg = ""
    }
    return msg;
  }

  $scope.toggleDisplay = function(showTechniques){
    $scope.ui.showTechniques = showTechniques;
  }

  $scope.getChecksList = function(){
    // List of technique properties that we want to compare
    // If we add another properties here, we have to make sure that is is correctly compared in the checkDiff() function
    var checks =
      { "name"         : "Name"
      , "bundle_name"  : "Technique ID"
      , "description"  : "Description"
      , "version"      : "Version"
      , "method_calls" : "Generic Methods have been modified"
      , "parameter"    : "Parameters"
      , "resources"    : "Resources"
      , "category"     : "Category"
      }
    return checks;
  }

  $scope.checkDiff = function(storedTech, currentTech){
    // Avoid errors if scope is not completly intialized
    if(storedTech===undefined || currentTech===undefined) return false;

    // get the list of technique's properties that we want to compare
    var checks   = $scope.getChecksList();
    // Used to store all divergency problems
    $scope.diverges = [];
    var check, diff, diverge;
    for (check in checks) {
      diverge = false;
      //Compare properties in the right way according to their "type"
      switch(check){
        case "method_calls" :
          var st = angular.copy(storedTech.method_calls );
          var ct = angular.copy(currentTech.method_calls);
          var stList = [];
          var ctList = [];
          var nb, field;
          for(var i in ct) {
            ctList.push(i + "-\xA0" + ct[i].component)
          }
          for(var i in st) {
            stList.push(i + "-\xA0" + st[i].component)
          }
          if(ct.length == st.length) {
            var diffParams = false;
            var st_params, ct_params;
            for(var i=0; i<st.length; i++){
              if (st[i].method_name === ct[i].method_name) {
                //Remove some properties that we don't want to compare
                delete st[i].agent_support;
                delete ct[i].agent_support;
                delete st[i].promiser;
                delete ct[i].promiser;
                if(st[i].OS_class.minorVersion === undefined) delete st[i].OS_class.minorVersion;
                if(st[i].OS_class.majorVersion === undefined) delete st[i].OS_class.majorVersion;
                if(ct[i].OS_class.minorVersion === undefined) delete ct[i].OS_class.minorVersion;
                if(ct[i].OS_class.majorVersion === undefined) delete ct[i].OS_class.majorVersion;
                //Store parameters to compare them separetly
                st_params = st[i].parameters;
                delete st[i].parameters;
                ct_params = ct[i].parameters;
                delete ct[i].parameters;
                for(var p in st_params){
                  if(st_params[p].value != ct_params[p].value){
                    diffParams = true;
                  }
                }
              } else {
                diffParams = true
              }
            }
            diff  = diffParams || !angular.equals(st , ct);
            field = diff ? checks[check] : undefined;
          } else {
            diff = true;
            if(ct.length < st.length) {
              nb    = st.length - ct.length;
              field = (nb + " Method" + (nb>1 ? 's' : '' ) + " ha" + (nb>1 ? 've' : 's' ) + " been added.");
            } else {
              nb    = ct.length - st.length;
              field = (nb + " Method" + (nb>1 ? 's' : '' ) + " ha" + (nb>1 ? 've' : 's' ) + " been removed.");
            }
          }
          if(diff){
            diverge =
            { "field": field
            , "storage_value": stList.join('\n')
            , "current_value": ctList.join('\n')
            };
          }
          break;

        case "resources" :
          var st = angular.copy(storedTech.resources );
          var ct = angular.copy(currentTech.resources);
          diff   = !$scope.fileManagerState.updating && !angular.equals(st , ct);
          break;
        case "category" :
          diff = false;
          break;
        default    :
          diff = !angular.equals(storedTech[check] , currentTech[check]);
          break;
      }
      // If there is a difference, the old value and the current one are stored
      if(diff){
        if(!diverge){
          diverge =
          { "field": checks[check]
          , "storage_value": storedTech[check]
          , "current_value": currentTech[check]
          };
        }
        $scope.diverges.push(diverge);
      }
    }
    // If there is no difference then return false, else return the array
    return $scope.diverges.length>0 ? $scope.diverges : false;
  }

  $scope.getWarningTooltipMessage = function(params){
    var paramString = params.join(', ');
    return ("Parameter" + (params.length > 1 ? "s " : " ") + paramString + (params.length > 1 ? " are" : " is") + " missing.")
  }
  $scope.getErrorTooltipMessage = function(params){
    var paramString = params.join(', ');
    return ("Invalid constraint for parameter" + (params.length > 1 ? "s: " : " ") + paramString + ".")
  }

  $scope.getResourcesByState = function(resources, state){
    return resources === undefined ? [] : resources.filter(resource => resource.state == state);
  }

  $scope.getResourcesInfo = function(){
    var msg, nbResources, nbUnsaved;
    nbResources = ($scope.selectedTechnique !== undefined && $scope.selectedTechnique.resources !== undefined) ? $scope.selectedTechnique.resources.length : undefined;
    if(nbResources === undefined || nbResources <= 0){
      msg = "There is no resource files."
    }else{
      nbUnsaved = $scope.selectedTechnique.resources.filter(resource => resource.state != 'untouched').length;
      if(nbUnsaved <= 0){
        msg = "There " + (nbResources   > 1 ? "are " : "is ") + nbResources   + " resource file" + (nbResources > 1 ? "s." : ".");
      }else{
        msg = "There " + (nbUnsaved > 1 ? "are " : "is ") + nbUnsaved + " unsaved file" + (nbUnsaved > 1 ? "s" : "") + ", save your changes to complete upload.";
      }
    }
    return msg;
  }
  $scope.reloadData();
  $scope.setPath();
});

var confirmModalCtrl = function ($scope, $uibModalInstance, actionName, kind, name) {
  $scope.actionName = actionName;
  $scope.kind = kind;
  $scope.name = name;

  $scope.displayName = function() {
    if (name === undefined) {
      return "this "+ kind;
    } else {
      return kind + " '" + name + "'"
    }
  };

  $scope.confirm = function() {
    $uibModalInstance.close();
  };

  $scope.cancel = function () {
    $uibModalInstance.dismiss('cancel');
  };
};

var cloneModalCtrl = function ($scope, $uibModalInstance, technique, techniques) {
  technique.bundle_name = undefined;
  $scope.techniques = techniques;
  $scope.technique = technique;
  $scope.oldTechniqueName = technique.name;

  $scope.updateBundleName = function () {
      $scope.technique.bundle_name = technique.name ? technique.name.replace(/\W/g,"_") : "";
  };
  $scope.clone = function() {
    $uibModalInstance.close(technique);
  }
  $scope.cancel = function () {
    $uibModalInstance.dismiss('cancel');
  };
};


var SaveChangesModalCtrl = function ($scope, $uibModalInstance, technique, editForm, diverges) {
  $scope.technique = technique;
  $scope.diverges  = diverges;
  $scope.showDiff  = false;
  $scope.save = function() {
    $uibModalInstance.close(true);
  }
  $scope.discard = function () {
    $uibModalInstance.close(false);
  };
  $scope.cancel = function () {
    $uibModalInstance.dismiss('cancel');
  };
  $scope.toggleShowDiff = function () {
    $scope.showDiff = !$scope.showDiff;
  }
};

var RestoreWarningModalCtrl = function ($scope, $uibModalInstance, technique, editForm, diverges) {
  $scope.diverges  = diverges;
  $scope.showDiff  = false;
  $scope.save = function() {
    $uibModalInstance.close(true);
  }
  $scope.discard = function () {
    $uibModalInstance.close(false);
  };
  $scope.toggleShowDiff = function () {
    $scope.showDiff = !$scope.showDiff;
  }
};

app.config(function($httpProvider,$locationProvider) {
    $locationProvider.html5Mode(false).hashPrefix('!');
    //On some browsers, HTML get headers are bypassed during Angular GET requests, so these headers have to be reinitialized in the Angular httpProvider defaults headers GET.
    if (!$httpProvider.defaults.headers.get) {
        $httpProvider.defaults.headers.get = {};
    }
    // Allows the browser to not use the GET request response to satisfy subsequent responses without first checking with the originating server
    $httpProvider.defaults.headers.get['Cache-Control'] = 'no-cache';
    //This previous header is ignored by some caches and browsers, for that it may be simulated by setting the Expires HTTP version 1.0 header field value to a time earlier than the response time
    $httpProvider.defaults.headers.get['Expires'] = 'Thu, 01 Jan 1970 12:00:00 GMT';
    //Allows the browser to indicate to the cache to retrieve the GET request content from the original server rather than sending one he must keep.
    $httpProvider.defaults.headers.get['Pragma'] = 'no-cache';
});

app.config(['fileManagerConfigProvider', function (config) {
  var baseUrl = contextPath ? contextPath : "/rudder";
  var apiPath = baseUrl + '/secure/api/techniques/';
  var defaults = config.$get();

  	config.set({
    appName : 'resources',
    listUrl             : apiPath,
    uploadUrl           : apiPath,
    renameUrl           : apiPath,
    copyUrl             : apiPath,
    moveUrl             : apiPath,
    removeUrl           : apiPath,
    editUrl             : apiPath,
    getContentUrl       : apiPath,
    createFolderUrl     : apiPath,
    downloadFileUrl     : apiPath,
    downloadMultipleUrl : apiPath,
    compressUrl         : apiPath,
    extractUrl          : apiPath,
    permissionsUrl      : apiPath,
    isEditableFilePattern : /.*/,
    tplPath             : baseUrl + '/templates/angular/technique-editor-filemanager',
    allowedActions: angular.extend(defaults.allowedActions, {
      compress: false,
      compressChooseName: false,
      preview : true,
      edit: true,
      extract: false
    })
  });
}]);

// From https://stackoverflow.com/questions/105034/create-guid-uuid-in-javascript
function uuidv4() {
  return ([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g, c =>
    (c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)
  );
}
