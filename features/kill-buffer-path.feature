Feature: Kill buffer path
  Background: 
    Given I am in file "foo.js"
    when I turn on js2-mode
    And I turn on amd-mode

  Scenario: Kill and yank buffer path
    When I press "C-c C-d k"
    Then I press "C-y"
    Then I should see:
    """
    'test-files/foo'
    """