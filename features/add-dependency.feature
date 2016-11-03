Feature: Add module dependency
  Background: 
    Given I open temp file "bar.js"
    When I turn on js2-mode
    And I turn on amd-mode
    And I press "C-c C-a a"
    And I wait for parsing to finish

  Scenario: Add a file
    When I press "C-c C-a f test-files/foo.js"
    Then I should see:
    """
    define(['test-files/foo'], function(foo) {
        
    });
    """

  Scenario: Add a module name
    When I press "C-c C-a m module/name"
    Then I should see:
    """
    define(['module/name'], function(name) {
        
    });
    """
