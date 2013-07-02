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

package org.eclipse.wazaabi.engine.swt.snippets.forms;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.forms.nonosgi.SWTFormsHelper;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.FontRule;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class FormSnippet1 {

	public static void main(String[] args) {

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(500, 500);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);

		// init SWT Engine in standalone mode
		SWTFormsHelper.init(viewer);
		SWTHelper.init(viewer);

		// init the 'urn:java' resolver
		URNJavaLocatorHelper.init(viewer);

		// create a container and set its layout
		Container rootContainer = CoreWidgetsFactory.eINSTANCE
				.createContainer();
		// inject the container into the viewer
		viewer.setContents(rootContainer);

		FillLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createFillLayoutRule();
		layoutRule.setPropertyName("layout");
		rootContainer.getStyleRules().add(layoutRule);
		layoutRule.setType(Orientation.HORIZONTAL);

		Container section1 = createSection(
				rootContainer,
				"General Information",
				"This section describes general information about this plug-in.",
				"twistie", true, true, true);

		addLabelAndText(section1, "ID:");
		addLabelAndText(section1, "Version:");
		addLabelAndText(section1, "Name:");
		addLabelAndText(section1, "Vendor:");
		addLabelAndText(section1, "Platform Filter:");

		Container section2 = createSection(rootContainer, "Plug-In Content",
				null, "twistie", true, true, true);

		StringBuffer buf = new StringBuffer();
		buf.append("<form>");
		buf.append("<p>");
		buf.append("Here is some plain text for the text to render; ");
		buf.append("this text is at <a href=\"http://www.eclipse.org\" "
				+ "nowrap=\"true\">http://www.eclipse.org</a> web site.");
		buf.append("</p>");
		buf.append("<p>");
		buf.append("<span color=\"header\" font=\"header\">"
				+ "This text is in header font and color.</span>");
		buf.append("</p>");
		buf.append("<p>This line will contain some <b>bold</b> and "
				+ "some <span font=\"code\">source</span> text. ");
		buf.append("We can also add <img href=\"image\"/> an image. ");
		buf.append("</p>");
		buf.append("<li>A default (bulleted) list item.</li>");
		buf.append("<li>Another bullet list item.</li>");
		buf.append("<li style=\"text\" value=\"1.\">A list item with text.</li>");
		buf.append("<li style=\"text\" value=\"2.\">Another list item with text</li>");
		buf.append("<li style=\"image\" value=\"image\">List item with "
				+ "an image bullet</li>");
		buf.append("<li style=\"text\" bindent=\"20\" indent=\"40\" value=\"3.\">"
				+ "A list item with text.</li>");
		buf.append("<li style=\"text\" bindent=\"20\" indent=\"40\" value=\"4.\">"
				+ "A list item with text.</li>");
		buf.append("<p>     leading blanks;      more white \n\n new "
				+ "lines   <br/>  \n   more <b>   bb   </b>  white  . </p>");
		buf.append("</form>");

		Label formText = CoreWidgetsFactory.eINSTANCE.createLabel();
		formText.setText(buf.toString());
		section2.getChildren().add(formText);

		BooleanRule booleanRule = CoreStylesFactory.eINSTANCE
				.createBooleanRule();
		booleanRule.setPropertyName("render-xml");
		booleanRule.setValue(true);
		formText.getStyleRules().add(booleanRule);
		section2.getChildren().add(formText);

		FontRule fr1 = CoreStylesFactory.eINSTANCE.createFontRule();
		fr1.setPropertyName("key:header");
		formText.getStyleRules().add(fr1);
		fr1.setHeight(15);
		FontRule fr2 = CoreStylesFactory.eINSTANCE.createFontRule();
		fr2.setPropertyName("key:code");
		formText.getStyleRules().add(fr2);
		fr2.setItalic(true);
		fr2.setHeight(6);

		ColorRule cr1 = CoreStylesFactory.eINSTANCE.createColorRule();
		cr1.setPropertyName("key:header");
		formText.getStyleRules().add(cr1);
		cr1.setBlue(100);
		cr1.setRed(50);

		// addLabelAndText(section2, "Test1:");
		// addLabelAndText(section2, "Test2:");

		rootContainer.getChildren().add(section1);
		rootContainer.getChildren().add(section2);

		
		cr1.setBlue(255);
		
		StringRule laf = CoreStylesFactory.eINSTANCE.createStringRule();
		laf.setPropertyName("look-and-feel");
		laf.setValue("form");
		rootContainer.getStyleRules().add(laf);

		StringRule formTitle = CoreStylesFactory.eINSTANCE.createStringRule();
		formTitle.setPropertyName("title"); //$NON-NLS-1$
		formTitle.setValue("Overview");
		rootContainer.getStyleRules().add(formTitle);

		ImageRule imageRule = CoreStylesFactory.eINSTANCE.createImageRule();
		imageRule.setPropertyName("header-image"); //$NON-NLS-1$
		imageRule.setValue("urn:java:plugin_mf_obj.gif");
		rootContainer.getStyleRules().add(imageRule);

		BooleanRule bRule = CoreStylesFactory.eINSTANCE.createBooleanRule();
		bRule.setPropertyName("decorate-form-heading"); //$NON-NLS-1$
		bRule.setValue(true);
		rootContainer.getStyleRules().add(bRule);
		// rootContainer.getStyleRules().remove(bRule);
		// rootContainer.getStyleRules().add(bRule);
		// rootContainer.getStyleRules().remove(laf);
		// rootContainer.getStyleRules().add(laf);
		// rootContainer.getStyleRules().remove(laf);
		// rootContainer.getStyleRules().add(laf);

		// StringRule expansionStyle = CoreStylesFactory.eINSTANCE
		// .createStringRule();
		//		expansionStyle.setPropertyName("expansion-toggle"); //$NON-NLS-1$
		// expansionStyle.setValue("twistie");
		// section1.getStyleRules().add(expansionStyle);

		// StringRule expansionStyle1 = (StringRule) section1.getFirstStyleRule(
		// "expansion-toggle", CoreStylesPackage.Literals.STRING_RULE);
		// if (expansionStyle1 != null)
		// section1.getStyleRules().remove(expansionStyle1);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}

	protected static void addLabelAndText(Container container, String labelText) {

		Label label1 = CoreWidgetsFactory.eINSTANCE.createLabel();
		label1.setText(labelText);
		container.getChildren().add(label1);
		TextComponent text1 = CoreWidgetsFactory.eINSTANCE
				.createTextComponent();

		BooleanRule booleanRule = CoreStylesFactory.eINSTANCE
				.createBooleanRule();
		booleanRule.setPropertyName("border");
		booleanRule.setValue(true);
		text1.getStyleRules().add(booleanRule);
		container.getChildren().add(text1);

	}

	protected static Container createSection(Container container, String title,
			String description, String expansionToggle, Boolean expanded,
			boolean titleBar, boolean clientIndent) {
		Container result = CoreWidgetsFactory.eINSTANCE.createContainer();

		StringRule laf = CoreStylesFactory.eINSTANCE.createStringRule();
		laf.setPropertyName("look-and-feel");
		laf.setValue("section");
		// laf.setValue("form");
		result.getStyleRules().add(laf);

		if (title != null) {
			StringRule formTitle = CoreStylesFactory.eINSTANCE
					.createStringRule();
			formTitle.setPropertyName("title"); //$NON-NLS-1$
			formTitle.setValue(title);
			result.getStyleRules().add(formTitle);
		}
		StringRule expansionStyle = CoreStylesFactory.eINSTANCE
				.createStringRule();
		expansionStyle.setPropertyName("expansion-toggle"); //$NON-NLS-1$
		expansionStyle.setValue(expansionToggle);
		result.getStyleRules().add(expansionStyle);

		if (description != null) {
			StringRule descriptionStyle = CoreStylesFactory.eINSTANCE
					.createStringRule();
			descriptionStyle.setPropertyName("description"); //$NON-NLS-1$
			descriptionStyle.setValue(description);
			result.getStyleRules().add(descriptionStyle);
		}
		BooleanRule expandedStyle = CoreStylesFactory.eINSTANCE
				.createBooleanRule();
		expandedStyle.setPropertyName("expanded"); //$NON-NLS-1$
		expandedStyle.setValue(expanded);
		result.getStyleRules().add(expandedStyle);

		BooleanRule titleBarStyle = CoreStylesFactory.eINSTANCE
				.createBooleanRule();
		titleBarStyle.setPropertyName("title-bar"); //$NON-NLS-1$
		titleBarStyle.setValue(titleBar);
		result.getStyleRules().add(titleBarStyle);

		BooleanRule clientIndentStyle = CoreStylesFactory.eINSTANCE
				.createBooleanRule();
		clientIndentStyle.setPropertyName("client-indent"); //$NON-NLS-1$
		clientIndentStyle.setValue(clientIndent);
		result.getStyleRules().add(clientIndentStyle);

		GridLayoutRule gridLayout = SWTStylesFactory.eINSTANCE
				.createGridLayoutRule();
		gridLayout.setPropertyName("layout");
		result.getStyleRules().add(gridLayout);
		gridLayout.setNumColumns(2);
		return result;
	}

}
