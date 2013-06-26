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
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.ExpandLayoutRule;
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

		Container section1[] = createSection(rootContainer);

		addLabelAndText(section1[1], "ID:");
		addLabelAndText(section1[1], "Version:");
		addLabelAndText(section1[1], "Name:");
		addLabelAndText(section1[1], "Vendor:");
		addLabelAndText(section1[1], "Platform Filter:");

		Container section2[] = createSection(rootContainer);
		addLabelAndText(section2[1], "Test1:");
		addLabelAndText(section2[1], "Test2:");

		rootContainer.getChildren().add(section1[0]);
		rootContainer.getChildren().add(section2[0]);

		StringRule r = CoreStylesFactory.eINSTANCE.createStringRule();
		r.setPropertyName("form-header-title"); //$NON-NLS-1$
		r.setValue("Overview");
		rootContainer.getStyleRules().add(r);

		ImageRule imageRule = CoreStylesFactory.eINSTANCE.createImageRule();
		imageRule.setPropertyName("form-header-image"); //$NON-NLS-1$
		imageRule.setValue("urn:java:plugin_mf_obj.gif");
		rootContainer.getStyleRules().add(imageRule);

		BooleanRule bRule = CoreStylesFactory.eINSTANCE.createBooleanRule();
		bRule.setPropertyName("form-decorate-form-heading"); //$NON-NLS-1$
		bRule.setValue(true);
		rootContainer.getStyleRules().add(bRule);
//		rootContainer.getStyleRules().remove(bRule);
//		rootContainer.getStyleRules().add(bRule);

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

	protected static Container[] createSection(Container container) {
		Container topContainer = CoreWidgetsFactory.eINSTANCE.createContainer();

		ExpandLayoutRule expandLayout = CoreStylesFactory.eINSTANCE
				.createExpandLayoutRule();
		expandLayout.setPropertyName("layout");
		topContainer.getStyleRules().add(expandLayout);

		Container childContainer = CoreWidgetsFactory.eINSTANCE
				.createContainer();
		topContainer.getChildren().add(childContainer);

		GridLayoutRule gridLayout = SWTStylesFactory.eINSTANCE
				.createGridLayoutRule();
		gridLayout.setPropertyName("layout");
		childContainer.getStyleRules().add(gridLayout);
		gridLayout.setNumColumns(2);
		return new Container[] { topContainer, childContainer };
	}
}
