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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.core.editparts.CollectionEditPart;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule;
import org.eclipse.wazaabi.mm.core.styles.collections.PathSelector;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class CollectionOfListOfObjects {

	public static void main(String[] args) {

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(400, 400);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);
		// init SWT Engine in standalone mode
		SWTHelper.init(viewer);
		// initialize the locationPaths processor
		LocationPathsHelper.init(viewer);
		
		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
		FillLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createFillLayoutRule();
		layoutRule.setPropertyName("layout");
		container.getStyleRules().add(layoutRule);

		// create a collection

		final Collection collection = CoreWidgetsFactory.eINSTANCE
				.createCollection();

		BooleanRule booleanRule = CoreStylesFactory.eINSTANCE
				.createBooleanRule();
		booleanRule.setValue(true);

		booleanRule.setPropertyName("header-visible");
		collection.getStyleRules().add(booleanRule);

		BooleanRule multiselect = CoreStylesFactory.eINSTANCE
				.createBooleanRule();
		multiselect
				.setPropertyName(CollectionEditPart.MULTIPLE_SELECTION_PROPERTY_NAME);
		multiselect.setValue(true);
		collection.getStyleRules().add(multiselect);

		LookAndFeelRule lookAndFeelRule = CoreCollectionsStylesFactory.eINSTANCE
				.createLookAndFeelRule();
		lookAndFeelRule.setPropertyName("lookandfeel"); //$NON-NLS-1$

		lookAndFeelRule.setValue(LookAndFeel.COMBOBOX);
		collection.getStyleRules().add(lookAndFeelRule);

		PathSelector pathSelector = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector.setPropertyName("content-provider");
		pathSelector.setEClassifierName("[]");
		pathSelector.getPaths().add("*");

		collection.getStyleRules().add(pathSelector);

		collection.setInput(createDomainObject());

		// append the collection to the container's children list.
		container.getChildren().add(collection);

		// inject the container into the viewer
		viewer.setContents(container);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}

	public static List<String> createDomainObject() {
		List<String> result = new ArrayList<String>();
		result.add("hello");
		result.add("Liuxihua");
		return result;
	}

}
