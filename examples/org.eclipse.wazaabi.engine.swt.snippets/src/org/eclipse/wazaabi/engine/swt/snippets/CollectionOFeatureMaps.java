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

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.util.BasicFeatureMap;
import org.eclipse.emf.ecore.util.FeatureMap;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.core.editparts.CollectionEditPart;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.mm.core.Alignment;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule;
import org.eclipse.wazaabi.mm.core.styles.collections.PathSelector;
import org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class CollectionOFeatureMaps {

	public static void main(String[] args) {

		// init SWT Engine in standalone mode
		SWTHelper.init();
		// initialize the locationPaths processor
		LocationPathsHelper.init();

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(400, 400);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);

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
		// booleanRule.setPropertyName("allow-row-selection");
		// booleanRule.setPropertyName("show-horizontal-lines");
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
		lookAndFeelRule.setValue(LookAndFeel.TREE);
		collection.getStyleRules().add(lookAndFeelRule);

		PathSelector pathSelector1 = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector1.setPropertyName("content-provider");
		pathSelector1.setEClassifierName("EPackage");
		pathSelector1.getPaths().add("&eSubpackages");
		PathSelector pathSelector2 = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector2.setPropertyName("content-provider");
		pathSelector2.setEClassifierName("[]");
		pathSelector2.getPaths().add("*/*");
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

		collection.getStyleRules().add(pathSelector1);
		collection.getStyleRules().add(pathSelector2);
		collection.getStyleRules().add(pathSelector3);
		collection.getStyleRules().add(pathSelector4);

		ColumnDescriptor columnDescriptor1 = CoreCollectionsStylesFactory.eINSTANCE
				.createColumnDescriptor();
		columnDescriptor1.setLabel("test1");
		columnDescriptor1.setPropertyName("column-descriptor");
		columnDescriptor1.setWidth(100);
		columnDescriptor1.setHeaderAlignment(Alignment.CENTER);
		WeightedColumnDescriptor columnDescriptor2 = CoreCollectionsStylesFactory.eINSTANCE
				.createWeightedColumnDescriptor();
		columnDescriptor2.setLabel("test2");
		columnDescriptor2.setPropertyName("column-descriptor");
		columnDescriptor2.setWeight(50);
		columnDescriptor2.setHeaderAlignment(Alignment.LEAD);

		WeightedColumnDescriptor columnDescriptor3 = CoreCollectionsStylesFactory.eINSTANCE
				.createWeightedColumnDescriptor();
		columnDescriptor3.setLabel("test3");
		columnDescriptor3.setPropertyName("column-descriptor");
		columnDescriptor3.setWeight(200);
		columnDescriptor3.setHeaderAlignment(Alignment.TRAIL);

		collection.getStyleRules().add(columnDescriptor1);
		collection.getStyleRules().add(columnDescriptor2);
		collection.getStyleRules().add(columnDescriptor3);

		collection.setInput(createDomainObject());
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

		// collection.getSelection().add(rootPackage.getESubpackages().get(1));

		// inject the container into the viewer
		viewer.setContents(container);
		// collection.getSelection().set(0,
		// rootPackage.getESubpackages().get(0));
		// collection.getSelection().set(0,
		// rootPackage.getESubpackages().get(1));
		// collection.getSelection().clear();

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}

	public static List<FeatureMap> createDomainObject() {
		List<FeatureMap> featureMaps = new ArrayList<FeatureMap>();
		for (int i = 0; i < 5; i++)
			featureMaps.add(createFeatureMap("p" + i));
		return featureMaps;
	}

	public static FeatureMap createFeatureMap(String id) {
		EPackage p = createEPackage(id);
		FeatureMap fm = new BasicFeatureMap((InternalEObject) p, -1);
		for (int i = 0; i < 5; i++) {
			fm.add(EcorePackage.Literals.EPACKAGE__ESUBPACKAGES,
					createEPackage(id + "-" + i));
			fm.add(EcorePackage.Literals.EPACKAGE__ECLASSIFIERS,
					createEClass("class (" + id + "-" + i + ")"));
		}
		return fm;
	}

	public static EPackage createEPackage(String packageName) {
		EPackage ePackage = EcoreFactory.eINSTANCE.createEPackage();
		ePackage.setName(packageName);
		ePackage.setNsPrefix(packageName);
		ePackage.setNsURI("urn:" + packageName);
		return ePackage;
	}

	public static EClass createEClass(String eClassName) {
		EClass eClass = EcoreFactory.eINSTANCE.createEClass();
		eClass.setName(eClassName);
		return eClass;
	}
}
