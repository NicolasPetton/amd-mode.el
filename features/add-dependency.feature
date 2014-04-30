Feature: Add module dependency
  Background: 
    Given the buffer is empty
    when I turn on js2-mode
    And I turn on amd-mode
    And I press "C-c C-d a"

  Scenario: Add a file
    When I press "C-c C-d f test-files/foo.js"
    Then I should see:
    """
    define([
        'test-files/foo'
    ], function(foo){
    
    });
    """

  Scenario: Add a module name
    When I press "C-c C-d m module/name"
    Then I should see:
    """
    define([
        'module/name'
    ], function(name){
    
    });
    """
