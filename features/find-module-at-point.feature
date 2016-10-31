Feature: Find module at point
  Background: 
    Given I open temp file "bar.js"
    And I turn on js2-mode
    And I turn on amd-mode
    And I type "var foo;"
    And I go to word "foo"

  Scenario:
    When I press "C-c C-a ."
    Then I should be in buffer "foo.js"
