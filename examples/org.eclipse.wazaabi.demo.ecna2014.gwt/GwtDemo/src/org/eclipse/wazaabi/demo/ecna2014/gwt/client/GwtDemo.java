package org.eclipse.wazaabi.demo.ecna2014.gwt.client;

import org.eclipse.wazaabi.demo.ecna2014.core.ui.DemoUI;
import org.eclipse.wazaabi.engine.gwt.nonosgi.GWTHelper;
import org.eclipse.wazaabi.engine.gwt.viewers.GWTViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.widgets.Container;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.RootPanel;


public class GwtDemo implements EntryPoint {

    public void onModuleLoad() {
        Panel panel = new FlowPanel();
        RootPanel.get("wazaabi").add(panel);

        GWTViewer viewer = new GWTViewer(panel);
        GWTHelper.init(viewer);
//        URNJavaLocatorHelper.init(viewer);
//        LocationPathsHelper.init(viewer);

        Container container = DemoUI.create(viewer, "org.eclipse.wazaabi.demo.ecna2014.gwt.handlers.");

        // set engine-specific layout

        viewer.setContents(container);
    }
}