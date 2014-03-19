package org.eclipse.wazaabi.demo.ecna2014.core.ui;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.wazaabi.demo.ecna2014.core.model.ModelFactory;
import org.eclipse.wazaabi.demo.ecna2014.core.model.Winnie;
import org.eclipse.wazaabi.engine.core.gef.EditPartViewer;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.BoxLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;


public class DemoUI {

    public static void create(EditPartViewer viewer) {
        create(viewer, true);
    }

    public static void create(EditPartViewer viewer, boolean eventHanlding) {
        Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
        container.set("input", createBusinessModel());

        BoxLayoutRule layout = CoreStylesFactory.eINSTANCE.createBoxLayoutRule();
        layout.setPropertyName("layout");
        layout.setOrientation(Orientation.VERTICAL);
        layout.setMargin(15);
        layout.setSpacing(5);
        container.getStyleRules().add(layout);

        container.getChildren().add(Utils.createButton("Change Layout", "ChangeLayoutAction", eventHanlding));
        container.getChildren().add(Utils.createLabel("Given name:"));
        container.getChildren().add(Utils.createText(false, "$input/@name", eventHanlding));
        container.getChildren().add(Utils.createLabel("Family name:"));
        container.getChildren().add(Utils.createText(true, "$input/@name", eventHanlding));

        //container.getChildren().add(Utils.createButton("Say Hello", "SayHelloAction", eventHandling));
        container.getChildren().add(Utils.createButton("Submit", "ReplaceTextWithLabelAction", eventHanlding));

        viewer.setCodeLocatorBaseUri("urn:java:");
        viewer.setContents(container);
    }

    private static Winnie createBusinessModel() {
        final Winnie w = ModelFactory.eINSTANCE.createWinnie();
        w.setLname("Pooh");

        w.eAdapters().add(new AdapterImpl() {
            @Override
            public void notifyChanged(Notification notification) {
                System.out.println(" > Winnie model changed: {}" + w);
            }
        });
        return w;
    }
}
