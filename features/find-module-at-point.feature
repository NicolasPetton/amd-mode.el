Feature: Find module at point
  Background: 
    Given the buffer is empty
    when I turn on js2-mode
    And I turn on amd-mode
    And I type "var bar;"
    And I go to word "bar"

  Scenario:
    When I press "C-c C-d o bar"
    Then I should be in buffer "bar.js"
