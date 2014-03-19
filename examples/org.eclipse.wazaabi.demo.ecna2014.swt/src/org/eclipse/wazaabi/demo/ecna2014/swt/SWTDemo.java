package org.eclipse.wazaabi.demo.ecna2014.swt;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.demo.ecna2014.core.model.ModelFactory;
import org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie;
import org.eclipse.wazaabi.swt.starterkit.Wazaabi;
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
        mainShell.setText("SWT");

        final Winnie w = ModelFactory.eINSTANCE.createWinnie();
        w.eAdapters().add(new AdapterImpl() {
            @Override
            public void notifyChanged(Notification notification) {
                System.out.println(w);
            }
        });

        Wazaabi.createUI(mainShell, "urn:java:demo.ui", w);

        mainShell.open();
        while (!mainShell.isDisposed()) {
            if (!display.readAndDispatch())
                display.sleep();
        }
        display.dispose();
    }
}
