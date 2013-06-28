package org.eclipse.wazaabi.engine.swt.snippets.forms;

import java.io.File;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.forms.nonosgi.SWTFormsHelper;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.collections.DynamicProvider;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.swt.styles.RowDataRule;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class CollectionOfNonEObjectsInAForm {

	public static void main(String[] args) {

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(400, 400);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);

		// init SWT Engine in standalone mode
		SWTFormsHelper.init(viewer);
		SWTHelper.init(viewer);

		// initialize the locationPaths processor
		LocationPathsHelper.init(viewer);
		// initialize the urn:java code locator
		URNJavaLocatorHelper.init(viewer);

		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
		StringRule laf = CoreStylesFactory.eINSTANCE.createStringRule();
		laf.setPropertyName("look-and-feel");
		laf.setValue("form");
		container.getStyleRules().add(laf);

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
		// lookAndFeelRule.setValue(LookAndFeel.TREE);
		// lookAndFeelRule.setValue(LookAndFeel.COMBOBOX);
		lookAndFeelRule.setValue(LookAndFeel.TABLE);
		collection.getStyleRules().add(lookAndFeelRule);

		RowDataRule rowDataRule = SWTStylesFactory.eINSTANCE
				.createRowDataRule();
		rowDataRule.setPropertyName("layout-data");
		rowDataRule.setHeight(250);
		rowDataRule.setWidth(400);
		collection.getStyleRules().add(rowDataRule);

		DynamicProvider dynamicProvider1 = CoreCollectionsStylesFactory.eINSTANCE
				.createDynamicProvider();
		dynamicProvider1.setPropertyName("dynamic-provider");
		dynamicProvider1
				.setUri("org.eclipse.wazaabi.engine.swt.snippets.providers.FileListProvider");

		collection.getStyleRules().add(dynamicProvider1);

		ColumnDescriptor columnDescriptor1 = CoreCollectionsStylesFactory.eINSTANCE
				.createColumnDescriptor();
		columnDescriptor1.setLabel("test1");
		columnDescriptor1.setPropertyName("column-descriptor");
		columnDescriptor1.setWidth(100);
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

		collection.setInput(getInput());
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

		viewer.setCodeLocatorBaseUri("urn:java:");
		// inject the container into the viewer
		viewer.setContents(container);

		DynamicProvider comparator = CoreCollectionsStylesFactory.eINSTANCE
				.createDynamicProvider();
		comparator.setPropertyName("comparator");
		comparator
				.setUri("urn:java:org.eclipse.wazaabi.engine.swt.snippets.providers.FilesReverseComparator");
		collection.getStyleRules().add(comparator);
		// collection.getStyleRules().remove(sorter);

		DynamicProvider filter1 = CoreCollectionsStylesFactory.eINSTANCE
				.createDynamicProvider();
		filter1.setPropertyName("filter");
		filter1.setUri("urn:java:org.eclipse.wazaabi.engine.swt.snippets.providers.FilenamesMoreThan3CharFilter");
		collection.getStyleRules().add(filter1);
		collection.getStyleRules().remove(filter1);

		// collection.getSelection().set(0,
		// rootPackage.getESubpackages().get(0));
		collection.getSelection().clear();
		StringRule r = CoreStylesFactory.eINSTANCE.createStringRule();
		r.setPropertyName("form-header-title");
		container.getStyleRules().add(r);
		r.setValue("Hello World");

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}

	public static Object getInput() {

		File root = new File(".");
		if (root.exists())
			return root;

		return null;
	}
}
