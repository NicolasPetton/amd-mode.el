Feature: Auto insert
  Background:
    Given I open temp file "bar.js"
    When I turn on js2-mode
    And I turn on amd-mode

  Scenario: Auto insert in empty file
    And I press "C-c C-a a"
    Then I should see:
    """
    define([], function() {
        
    });
    """
