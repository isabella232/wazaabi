package org.eclipse.wazaabi.engine.swt.snippets.forms;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.forms.nonosgi.SWTFormsHelper;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class PushButtonInAGroupInAForm {

	public static void main(String[] args) {

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);
		// init SWT Engine in standalone mode
		SWTFormsHelper.init(viewer);
		SWTHelper.init(viewer);

		// init the 'urn:java' resolver
		URNJavaLocatorHelper.init(viewer);

		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();

		// inject the container into the viewer
		viewer.setContents(container);

		RowLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createRowLayoutRule();
		ColorRule color = CoreStylesFactory.eINSTANCE.createColorRule();
		color.setBlue(50);
		layoutRule.setPropertyName("layout");
		StringRule group = CoreStylesFactory.eINSTANCE.createStringRule();
		group.setPropertyName("title-value");
		group.setValue("my group");
		BooleanRule border = CoreStylesFactory.eINSTANCE.createBooleanRule();
		border.setPropertyName("title-border");
		border.setValue(true);
		container.getStyleRules().add(border);
		container.getStyleRules().add(group);
		container.getStyleRules().add(layoutRule);

		// create a label
		Label label = CoreWidgetsFactory.eINSTANCE.createLabel();
		label.setText("my label");

		// create a pushButton
		PushButton pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
		pushButton.setText("Hello World"); //$NON-NLS-1$
		pushButton.getStyleRules().add(color);

		// append the button to the container's children list.
		container.getChildren().add(pushButton);
		container.getChildren().add(label);

		StringRule r = CoreStylesFactory.eINSTANCE.createStringRule();
		r.setPropertyName("form-header-title"); //$NON-NLS-1$
		r.setValue("Hello World");
//		container.getStyleRules().add(r);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
