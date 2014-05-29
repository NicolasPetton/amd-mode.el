Feature: Auto insert
  Background:
    When I turn on js2-mode
    And I turn on amd-mode

  Scenario: Auto insert in empty file
    And I press "C-c C-d a"
    Then I should see:
    """
    define(function(){
    
    });
    """
