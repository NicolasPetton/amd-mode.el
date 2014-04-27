Feature: Add module dependency
  Background: 
    Given the buffer is empty
    when I turn on js2-mode
    And I turn on amd-mode
    And I press "C-c C-d a"

  Scenario:
    When I press "C-c C-d i test-files/foo.js"
    Then I should see:
    """
    define([
        'test-files/foo'
    ], function(foo){
    
    });
    """