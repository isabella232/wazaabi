package org.eclipse.wazaabi.demo.ecna2014.swt;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.demo.ecna2014.core.ui.DemoUI;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class SWTDemo {

    private static final Logger log = LoggerFactory.getLogger(SWTDemo.class);


    public static void main(String[] args) {
        log.info("Starting SWT version");

        Display display = new Display();
        Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
        mainShell.setLayout(new FillLayout());
        mainShell.setSize(600, 400);

        SWTControlViewer viewer = new SWTControlViewer(mainShell);
        SWTHelper.init(viewer);
        URNJavaLocatorHelper.init(viewer);
        LocationPathsHelper.init(viewer);

        Container container = DemoUI.create(viewer, "org.eclipse.wazaabi.demo.ecna2014.swt.handlers.");

        GridLayoutRule layout = SWTStylesFactory.eINSTANCE.createGridLayoutRule();
        layout.setNumColumns(1);
        layout.setPropertyName("layout");
        container.getStyleRules().add(layout);

        viewer.setContents(container);

        mainShell.open();
        while (!mainShell.isDisposed()) {
            if (!display.readAndDispatch())
                display.sleep();
        }
        display.dispose();
    }
}
