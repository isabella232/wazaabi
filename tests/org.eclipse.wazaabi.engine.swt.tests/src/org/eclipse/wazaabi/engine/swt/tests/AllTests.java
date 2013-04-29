/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.tests;


import org.eclipse.wazaabi.engine.swt.tests.layouts.TestBarLayoutRule;
import org.eclipse.wazaabi.engine.swt.tests.layouts.TestExpandLayout;
import org.eclipse.wazaabi.engine.swt.tests.layouts.TestExpandLayoutStyleRule;
import org.eclipse.wazaabi.engine.swt.tests.layouts.TestFillLayout;
import org.eclipse.wazaabi.engine.swt.tests.layouts.TestGridLayout;
import org.eclipse.wazaabi.engine.swt.tests.layouts.TestLayoutInALayout;
import org.eclipse.wazaabi.engine.swt.tests.layouts.TestRowLayout;
import org.eclipse.wazaabi.engine.swt.tests.layouts.TestTabbedLayout;
import org.eclipse.wazaabi.engine.swt.tests.layouts.TestTabbedLayoutStyleRule;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestBooleanRuleMultiLineTextComponent;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestColorRuleBackgroundColorLabel;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestColorRuleForegroundColorLabel;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestColorRuleSwitchBackgroundAndForegroundColorLabel;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestColorRuleSwitchForegroundAndBackgroundColorLabel;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestDirectionRuleDirectionLabel;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestFontRuleFontLabel;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestFontRuleFontTextComponent;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestHyperlinkRuleLookAndFeelLabel;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestImageRuleImageCheckBox;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestImageRuleImageLabel;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestImageRuleImagePushButton;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestImageRuleImageRadioButton;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestIntRuleIncrementScale;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestIntRuleIncrementSlider;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestIntRuleIncrementSpinner;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestIntRuleMaximumProgressBar;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestIntRuleMaximumScale;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestIntRuleMaximumSlider;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestIntRuleMaximumSpinner;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestIntRuleMinimumProgressBar;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestIntRuleMinimumScale;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestIntRuleMinimumSlider;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestIntRuleMinimumSpinner;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestIntRulePageIncrementScale;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestIntRulePageIncrementSlider;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestLabel;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestMenu;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestOrientationRuleOrientationProgressBar;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestOrientationRuleOrientationScale;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestOrientationRuleOrientationSlider;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestPushButton;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestRuleImageInLabel;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestSashForm;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestSelectedCheckBox;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestSelectedRadioButton;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestStringRuleTextCheckBox;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestStringRuleTextLabel;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestStringRuleTextPushButton;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestStringRuleTextRadioButton;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestStringRuleTooltipTextLabel;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestTextTextComponent;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestValueProgressBar;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestValueScale;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestValueSlider;
import org.eclipse.wazaabi.engine.swt.tests.widgets.TestValueSpinner;
import org.eclipse.wazaabi.engine.swt.tests.widgets.nonosgi.events.TestEventHandler;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses(value = {
		TestPushButton.class,
		TestLabel.class,
		TestRuleImageInLabel.class,
		TestRowLayout.class,
		TestFillLayout.class,
		TestGridLayout.class,
		TestBooleanRuleMultiLineTextComponent.class,
		TestColorRuleBackgroundColorLabel.class,
		TestColorRuleForegroundColorLabel.class,
		TestColorRuleSwitchBackgroundAndForegroundColorLabel.class,
		TestColorRuleSwitchForegroundAndBackgroundColorLabel.class,
		TestDirectionRuleDirectionLabel.class,
		TestFontRuleFontLabel.class,
		TestFontRuleFontTextComponent.class,
		TestHyperlinkRuleLookAndFeelLabel.class,
		TestImageRuleImageCheckBox.class,
		TestImageRuleImageLabel.class,
		TestImageRuleImagePushButton.class,
		TestImageRuleImageRadioButton.class,
		TestIntRuleIncrementScale.class,
		TestIntRuleIncrementSlider.class,
		TestIntRuleIncrementSpinner.class,
		TestIntRuleMaximumProgressBar.class,
		TestIntRuleMaximumScale.class,
		TestIntRuleMaximumSlider.class,
		TestIntRuleMaximumSpinner.class,
		TestIntRuleMinimumProgressBar.class,
		TestIntRuleMinimumScale.class,
		TestIntRuleMinimumSlider.class,
		TestIntRuleMinimumSpinner.class,
		TestIntRulePageIncrementScale.class,
		TestIntRulePageIncrementSlider.class,
		TestOrientationRuleOrientationProgressBar.class,
		TestOrientationRuleOrientationScale.class,
		TestOrientationRuleOrientationSlider.class,
		TestSelectedCheckBox.class,
		TestSelectedRadioButton.class,
		TestStringRuleTextCheckBox.class,
		TestStringRuleTextLabel.class,
		TestStringRuleTextPushButton.class,
		TestStringRuleTextRadioButton.class,
		TestStringRuleTooltipTextLabel.class,
		TestTextTextComponent.class,
		TestValueProgressBar.class,
		TestValueScale.class,
		TestValueSlider.class,
		TestValueSpinner.class,
		TestLayoutInALayout.class,
		TestBarLayoutRule.class,
		TestSashForm.class,
		TestExpandLayout.class,
		TestExpandLayoutStyleRule.class,
		TestTabbedLayout.class,
		TestTabbedLayoutStyleRule.class,
		TestMenu.class,
		TestEventHandler.class
})

public class AllTests {

}
