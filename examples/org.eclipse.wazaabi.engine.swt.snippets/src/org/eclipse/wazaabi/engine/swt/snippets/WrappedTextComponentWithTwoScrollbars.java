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

package org.eclipse.wazaabi.engine.swt.snippets;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.ScrollBarRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class WrappedTextComponentWithTwoScrollbars {

	public static void main(String[] args) {


		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);

		// init SWT Engine in standalone mode
		SWTHelper.init(viewer);

		// init the 'urn:java' resolver
		URNJavaLocatorHelper.init(viewer);
		LocationPathsHelper.init(viewer);
		
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();

		FillLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createFillLayoutRule();
		layoutRule.setPropertyName("layout");
		container.getStyleRules().add(layoutRule);

		// create a TextComponent
		TextComponent text0 = CoreWidgetsFactory.eINSTANCE
				.createTextComponent();
		String content = "";

		for (int i = 0; i < 50; i++)
			content += "hello world ";//$NON-NLS-1$

		text0.setText(content);
		container.getChildren().add(text0);

		BooleanRule wrapRule = CoreStylesFactory.eINSTANCE.createBooleanRule();
		wrapRule.setPropertyName("wrap");
		wrapRule.setValue(true);
		text0.getStyleRules().add(wrapRule);
		
		ScrollBarRule verticalScrollBarRule = CoreStylesFactory.eINSTANCE
				.createScrollBarRule();
		verticalScrollBarRule.setPropertyName("vertical-scrollbar");
		text0.getStyleRules().add(verticalScrollBarRule);

		ScrollBarRule horizontalScrollBarRule = CoreStylesFactory.eINSTANCE
				.createScrollBarRule();
		horizontalScrollBarRule.setPropertyName("horizontal-scrollbar");
		text0.getStyleRules().add(horizontalScrollBarRule);

		viewer.setContents(container);

		mainShell.open();
		text0.getStyleRules().remove(horizontalScrollBarRule);
		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
