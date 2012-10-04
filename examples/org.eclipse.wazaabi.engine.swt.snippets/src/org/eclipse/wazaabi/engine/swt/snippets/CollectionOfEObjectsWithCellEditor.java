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

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.coderesolution.reflection.java.codelocators.nonosgi.ReflectionJavaHelper;
import org.eclipse.wazaabi.engine.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.extras.CoreExtrasFactory;
import org.eclipse.wazaabi.mm.core.extras.TextCellEditor;
import org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule;
import org.eclipse.wazaabi.mm.core.styles.collections.PathSelector;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.swt.styles.RowDataRule;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class CollectionOfEObjectsWithCellEditor {

	public static void main(String[] args) {

		// init SWT Engine in standalone mode
		SWTHelper.init();
		// initialize the locationPaths processor
		LocationPathsHelper.init();
		// init the 'urn:java' resolver
		ReflectionJavaHelper.init();

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(400, 400);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);

		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
		RowLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createRowLayoutRule();
		layoutRule.setPropertyName("layout");
		container.getStyleRules().add(layoutRule);

		// create a collection

		final Collection collection = CoreWidgetsFactory.eINSTANCE
				.createCollection();

		LookAndFeelRule lookAndFeelRule = CoreCollectionsStylesFactory.eINSTANCE
				.createLookAndFeelRule();
		lookAndFeelRule.setPropertyName("lookandfeel"); //$NON-NLS-1$
		lookAndFeelRule.setValue(LookAndFeel.TREE);
		collection.getStyleRules().add(lookAndFeelRule);

		RowDataRule rowDataRule = SWTStylesFactory.eINSTANCE
				.createRowDataRule();
		rowDataRule.setPropertyName("layout-data");
		rowDataRule.setHeight(250);
		rowDataRule.setWidth(400);

		PathSelector pathSelector1 = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector1.setPropertyName("content-provider");
		pathSelector1.setEClassifierName("EPackage");
		pathSelector1.getPaths().add("&eSubpackages");
		PathSelector pathSelector2 = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector2.setPropertyName("content-provider");
		pathSelector2.setEClassifierName("EPackage");
		pathSelector2.getPaths().add("&eClassifiers");
		PathSelector pathSelector3 = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector3.setPropertyName("label-renderer");
		pathSelector3.setEClassifierName("EPackage");
		pathSelector3.getPaths().add("@name");
		pathSelector3.getPaths().add("@nsPrefix");
		pathSelector3.getPaths().add("@nsURI");
		PathSelector pathSelector4 = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector4.setPropertyName("label-renderer");
		pathSelector4.setEClassifierName("EClass");
		pathSelector4.getPaths().add("@name");
		pathSelector4.getPaths().add(null);
		pathSelector4.getPaths().add("@name");

		collection.getStyleRules().add(pathSelector1);
		collection.getStyleRules().add(pathSelector2);
		collection.getStyleRules().add(pathSelector3);
		collection.getStyleRules().add(pathSelector4);

		collection.getStyleRules().add(rowDataRule);

		ColumnDescriptor columnDescriptor1 = CoreCollectionsStylesFactory.eINSTANCE
				.createColumnDescriptor();
		columnDescriptor1.setLabel("test1");
		columnDescriptor1.setPropertyName("column-descriptor");
		columnDescriptor1.setWidth(100);
		TextCellEditor textCellEditor = CoreExtrasFactory.eINSTANCE.createTextCellEditor();
		columnDescriptor1.setCellEditor(textCellEditor);
		columnDescriptor1.setEditingSupport("urn:java:org.eclipse.wazaabi.engine.swt.snippets.providers.editingsupports.EditingSupport2");
		
		
		ColumnDescriptor columnDescriptor2 = CoreCollectionsStylesFactory.eINSTANCE
				.createColumnDescriptor();
		columnDescriptor2.setLabel("test2");
		columnDescriptor2.setPropertyName("column-descriptor");
		columnDescriptor2.setWidth(100);

		ColumnDescriptor columnDescriptor3 = CoreCollectionsStylesFactory.eINSTANCE
				.createColumnDescriptor();
		columnDescriptor3.setLabel("test3");
		columnDescriptor3.setPropertyName("column-descriptor");
		columnDescriptor3.setWidth(100);

		collection.getStyleRules().add(columnDescriptor1);
		collection.getStyleRules().add(columnDescriptor2);
		collection.getStyleRules().add(columnDescriptor3);

		EPackage rootPackage = createDomainObject();
		collection.setInput(rootPackage);
		// append the collection to the container's children list.
		container.getChildren().add(collection);

		collection.eAdapters().add(new AdapterImpl() {

			@Override
			public void notifyChanged(Notification msg) {
				switch (msg.getFeatureID(Collection.class)) {
				case CoreWidgetsPackage.COLLECTION__SELECTION:
					if (msg.getEventType() == Notification.ADD)
						System.out.println("add:" + msg.getNewValue());
					if (msg.getEventType() == Notification.REMOVE)
						System.out.println("remove:" + msg.getOldValue());
				}
			}

		});

		collection.getSelection().add(rootPackage.getESubpackages().get(1));

		// inject the container into the viewer
		viewer.setContents(container);
		collection.getSelection().set(0, rootPackage.getESubpackages().get(0));
		collection.getSelection().clear();

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}

	public static EPackage createDomainObject() {
		EPackage ePackage1 = EcoreFactory.eINSTANCE.createEPackage();
		ePackage1.setName("package1");
		ePackage1.setNsPrefix("p1");
		ePackage1.setNsURI("urn:p1");
		EPackage ePackage2 = EcoreFactory.eINSTANCE.createEPackage();
		ePackage2.setName("package2");
		ePackage2.setNsPrefix("p2");
		ePackage2.setNsURI("urn:p2");
		ePackage1.getESubpackages().add(ePackage2);
		EPackage ePackage3 = EcoreFactory.eINSTANCE.createEPackage();
		ePackage3.setName("package3");
		ePackage3.setNsPrefix("p3");
		ePackage3.setNsURI("urn:p3");
		ePackage1.getESubpackages().add(ePackage3);

		EClass eClass1 = EcoreFactory.eINSTANCE.createEClass();
		eClass1.setName("class1");
		ePackage1.getEClassifiers().add(eClass1);

		return ePackage1;
	}
}
