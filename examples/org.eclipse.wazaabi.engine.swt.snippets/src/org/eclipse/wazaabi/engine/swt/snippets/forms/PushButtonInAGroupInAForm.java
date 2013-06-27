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
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
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

		Container topContainer = CoreWidgetsFactory.eINSTANCE.createContainer();
		FillLayoutRule fillLayout = SWTStylesFactory.eINSTANCE
				.createFillLayoutRule();
		fillLayout.setPropertyName("layout");
		topContainer.getStyleRules().add(fillLayout);
		viewer.setContents(topContainer);

		Container subContainer = CoreWidgetsFactory.eINSTANCE.createContainer();
		topContainer.getChildren().add(subContainer);
		FillLayoutRule fillLayout2 = SWTStylesFactory.eINSTANCE
				.createFillLayoutRule();
		fillLayout2.setPropertyName("layout");
		subContainer.getStyleRules().add(fillLayout);

		Container groupContainer = CoreWidgetsFactory.eINSTANCE
				.createContainer();
		subContainer.getChildren().add(groupContainer);

		RowLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createRowLayoutRule();
		ColorRule color = CoreStylesFactory.eINSTANCE.createColorRule();
		color.setBlue(0);
		color.setRed(50);
		color.setGreen(250);
		layoutRule.setPropertyName("layout");
		StringRule group = CoreStylesFactory.eINSTANCE.createStringRule();
		group.setPropertyName("title-value");
		group.setValue("my group");
		BooleanRule border = CoreStylesFactory.eINSTANCE.createBooleanRule();
		border.setPropertyName("title-border");
		border.setValue(true);
		groupContainer.getStyleRules().add(border);
		groupContainer.getStyleRules().add(group);
		groupContainer.getStyleRules().add(layoutRule);

		// create a label
		Label label = CoreWidgetsFactory.eINSTANCE.createLabel();
		label.setText("my label");

		// create a pushButton
		PushButton pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
		pushButton.setText("Hello World"); //$NON-NLS-1$
		pushButton.getStyleRules().add(color);

		// append the button to the container's children list.
		groupContainer.getChildren().add(pushButton);
		groupContainer.getChildren().add(label);

		StringRule r = CoreStylesFactory.eINSTANCE.createStringRule();
		r.setPropertyName("form-header-title"); //$NON-NLS-1$
		r.setValue("Hello World");
		topContainer.getStyleRules().add(r);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
