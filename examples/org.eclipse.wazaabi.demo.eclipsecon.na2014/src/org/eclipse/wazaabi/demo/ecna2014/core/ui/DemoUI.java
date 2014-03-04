package org.eclipse.wazaabi.demo.ecna2014.core.ui;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.wazaabi.demo.ecna2014.core.model.ModelFactory;
import org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie;
import org.eclipse.wazaabi.engine.core.gef.EditPartViewer;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;


public class DemoUI {


    public static Container create(EditPartViewer viewer, String layoutActionPrefix) {
        Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
        container.set("input", createBusinessModel());

        container.getChildren().add(Utils.createButton("Change Layout", "ChangeLayoutAction", layoutActionPrefix));
        container.getChildren().add(Utils.createLabel("Given name:"));
        container.getChildren().add(Utils.createText(false, "$input/@name"));
        container.getChildren().add(Utils.createLabel("Family name:"));
        container.getChildren().add(Utils.createText(true, "$input/@name"));

        //container.getChildren().add(Utils.createButton("Say Hello", "SayHelloAction"));
        container.getChildren().add(Utils.createButton("Submit", "ReplaceTextWithLabelAction"));

        viewer.setCodeLocatorBaseUri("urn:java:");

        return container;
    }

    private static Winnie createBusinessModel() {
        final Winnie w = ModelFactory.eINSTANCE.createWinnie();
        w.setName("Pooh");

        w.eAdapters().add(new AdapterImpl() {
            @Override
            public void notifyChanged(Notification notification) {
                System.out.println(" > Winnie model changed: {}" + w);
            }
        });
        return w;
    }
}
