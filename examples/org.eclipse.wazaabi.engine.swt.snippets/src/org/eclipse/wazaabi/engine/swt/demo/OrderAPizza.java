package org.eclipse.wazaabi.engine.swt.demo;


import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.core.widgets.Spinner;
import org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment;
import org.eclipse.wazaabi.mm.swt.styles.GridDataRule;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class OrderAPizza {

	public static void main(String[] args) {

		// init SWT Engine in standalone mode
		SWTHelper.init();

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(200, 110);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);

		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
		GridLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createGridLayoutRule();
		layoutRule.setNumColumns(2);
		layoutRule.setMarginLeft(20);
		layoutRule.setPropertyName("layout");
		container.getStyleRules().add(layoutRule);

		// create a first Slider
		Spinner spinner1 = CoreWidgetsFactory.eINSTANCE.createSpinner();
		spinner1.setValue(1);
		spinner1.setMaximum(100);
		spinner1.setMinimum(0);
		spinner1.setTextLimit(5);
		
		spinner1.eAdapters().add(new AdapterImpl() {

			@Override
			public void notifyChanged(Notification msg) {
				switch (msg.getFeatureID(Spinner.class)) {
				case CoreWidgetsPackage.SPINNER__VALUE:
					System.out.println("---> " + msg.getNewIntValue());
					break;
				}
			}

		});
		container.getChildren().add(spinner1);
		
		
		Label label = CoreWidgetsFactory.eINSTANCE.createLabel();
		label.setText("Pizza");
		container.getChildren().add(label);

		PushButton push = CoreWidgetsFactory.eINSTANCE.createPushButton();
		push.setText("Order");
		GridDataRule layoutData1 = SWTStylesFactory.eINSTANCE.createGridDataRule();
		layoutData1.setPropertyName("layout-data");
		layoutData1.setHorizontalSpan(2);
		layoutData1.setHorizontalAlignement(GridDataAlignment.CENTER);
		push.getStyleRules().add(layoutData1);
		container.getChildren().add(push);
		
		Label label2 = CoreWidgetsFactory.eINSTANCE.createLabel();
		label2.setText("You are ready to order 1 pizza");
		GridDataRule layoutData2 = SWTStylesFactory.eINSTANCE.createGridDataRule();
		layoutData2.setPropertyName("layout-data");
		layoutData2.setHorizontalSpan(2);
		label2.getStyleRules().add(layoutData2);
		
		container.getChildren().add(label2);


		viewer.setContents(container);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
