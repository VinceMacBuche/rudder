function updateAgentRun (run) {
    var scope = angular.element($("#complianceModeController")).scope();
    scope.$apply(function(){
          scope.updateAgentRun(run);
        } );
  }

var complianceModeModule = angular.module("complianceMode", ['ngAnimate'])
complianceModeModule.controller("complianceModeController", function($scope) {

  $scope.complianceMode = { name : "full-compliance", heartbeatFrequency : 1, overrides : true};
  $scope.globalValue
  $scope.agentRun = 5;
  $scope.isNodePage = false;
  $scope.callback;
  $scope.savedValue;
  $scope.contextPath;

 $scope.init = function(complianceMode, heartbeatFreq, globalValue, callback, contextPath) {
   $scope.complianceMode.name=complianceMode;
   $scope.complianceMode.heartbeatFrequency=heartbeatFreq;
   $scope.globalValue = globalValue;
   $scope.isNodePage = globalValue !== undefined
   $scope.callback=callback;
   $scope.contextPath=contextPath;
   $scope.savedValue = angular.copy($scope.complianceMode)
   
  }

  $scope.onChange = function() {
    $("#complianceModeMessage").empty();
  }
  
  $scope.updateAgentRun = function(run) {
    $scope.agentRun = run;
  }


  $scope.save = function() {
    var run = JSON.stringify($scope.complianceMode);
    $scope.callback(run);
    $scope.savedValue = angular.copy($scope.complianceMode);
  }

  $scope.isUnchanged = function(agentRun) {
    return angular.equals($scope.complianceMode, $scope.savedValue);
  };

  $scope.displayGlobal = function() {
    return $scope.complianceMode.overrides && $scope.globalRun !== undefined
  }

});
