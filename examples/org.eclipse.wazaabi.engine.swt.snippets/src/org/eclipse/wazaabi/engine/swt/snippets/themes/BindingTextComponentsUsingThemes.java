package org.eclipse.wazaabi.engine.swt.snippets.themes;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.coderesolution.reflection.java.codelocators.nonosgi.ReflectionJavaHelper;
import org.eclipse.wazaabi.engine.core.themes.nonosgi.CoreThemesHelper;
import org.eclipse.wazaabi.engine.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.annotations.Annotation;
import org.eclipse.wazaabi.mm.core.annotations.AnnotationContent;
import org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsFactory;
import org.eclipse.wazaabi.mm.core.themes.Themes.CoreThemesFactory;
import org.eclipse.wazaabi.mm.core.themes.Themes.Theme;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Spinner;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;
import org.eclipse.wazaabi.mm.edp.handlers.Validator;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class BindingTextComponentsUsingThemes {

	public static void main(String[] args) {

		// init SWT Engine in standalone mode
		SWTHelper.init();

		// init the 'urn:java' resolver
		ReflectionJavaHelper.init();
		LocationPathsHelper.init();
		CoreThemesHelper.init();
		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);

		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();

		GridLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createGridLayoutRule();
		layoutRule.setPropertyName("layout");
		container.getStyleRules().add(layoutRule);



		// create a TextComponent
		TextComponent text0 = CoreWidgetsFactory.eINSTANCE
				.createTextComponent();
		
		Annotation classAnnotation = CoreAnnotationsFactory.eINSTANCE
				.createAnnotation();
		classAnnotation.setSource("http://www.wazaabi.org/core/themes/class");
		AnnotationContent content1 = CoreAnnotationsFactory.eINSTANCE
				.createAnnotationContent();
		content1.setKey("class");
		content1.setValue("class1");
		classAnnotation.getContents().add(content1);

		text0.getAnnotations()
				.add((Annotation) EcoreUtil.copy(classAnnotation));
		text0.setText("Hello World"); //$NON-NLS-1$

		Annotation param = CoreAnnotationsFactory.eINSTANCE.createAnnotation();
		AnnotationContent content2 = CoreAnnotationsFactory.eINSTANCE
				.createAnnotationContent();
		param.getContents().add(content2);
		content2.setKey("value");
		content2.setValue("../TextComponent[1]/@text");
		text0.getAnnotations().add(param);
		param.setSource("http://www.wazaabi.org/core/themes/parameter");

		TextComponent text1 = CoreWidgetsFactory.eINSTANCE
				.createTextComponent();

		Spinner spinner = CoreWidgetsFactory.eINSTANCE.createSpinner();

		TextComponent text2 = CoreWidgetsFactory.eINSTANCE
				.createTextComponent();

		container.getChildren().add(text0);
		container.getChildren().add(text1);
		container.getChildren().add(spinner);
		container.getChildren().add(text2);

		Binding spinnerToText = EDPHandlersFactory.eINSTANCE.createBinding();
		StringParameter source2 = EDPHandlersFactory.eINSTANCE
				.createStringParameter();
		StringParameter target2 = EDPHandlersFactory.eINSTANCE
				.createStringParameter();
		source2.setName("source");
		source2.setValue("@value");
		target2.setName("target");
		target2.setValue("../TextComponent[2]/@text");
		spinnerToText.getParameters().add(source2);
		spinnerToText.getParameters().add(target2);

		spinner.getHandlers().add(spinnerToText);

		Validator validator = EDPHandlersFactory.eINSTANCE.createValidator();
		validator.setId("bundledIsIntValidator");

		Event event2 = EDPEventsFactory.eINSTANCE.createEvent();
		spinnerToText.getEvents().add(event2);
		spinnerToText.getExecutables().add(validator);
		event2.setId("core:ui:focus:out");

		// Condition condition = EDPHandlersFactory.eINSTANCE.createCondition();
		// condition.setUri("urn:java:org.eclipse.wazaabi.engine.swt.snippets.conditions.BadCondition");
		// eventHandler.getConditions().add(condition);

		
		container.getAnnotations().add(createThemeDeclaration());

		viewer.setContents(container);
		// condition.setUri(null);

		// action.setUri(null);

		Resource res = new XMIResourceImpl();
		res.getContents().add(container);
		try {
			res.save(System.out, null);
		} catch (IOException e) {
			e.printStackTrace();
		}
		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
	
	protected static Annotation createThemeDeclaration () {
		TextComponent themedTextComponent = CoreWidgetsFactory.eINSTANCE
				.createTextComponent();
		Binding binding = EDPHandlersFactory.eINSTANCE.createBinding();

		StringParameter source = EDPHandlersFactory.eINSTANCE
				.createStringParameter();
		StringParameter target = EDPHandlersFactory.eINSTANCE
				.createStringParameter();
		source.setName("source");
		source.setValue("@text");
		target.setName("target");
		target.setValue("${value}");
		binding.getParameters().add(source);
		binding.getParameters().add(target);

		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		event.setId("core:ui:focus:out");
		binding.getEvents().add(event);

		themedTextComponent.getHandlers().add(binding);

		Theme theme = CoreThemesFactory.eINSTANCE.createTheme();
		theme.getChildren().add(themedTextComponent);

		Annotation containerAnnotation = CoreAnnotationsFactory.eINSTANCE
				.createAnnotation();
		containerAnnotation
				.setSource("http://www.wazaabi.org/core/themes/declaration");
		AnnotationContent content0 = CoreAnnotationsFactory.eINSTANCE
				.createAnnotationContent();
		containerAnnotation.getContents().add(content0);

		content0.setKey("insert-inline");

		Annotation classAnnotation = CoreAnnotationsFactory.eINSTANCE
				.createAnnotation();
		classAnnotation.setSource("http://www.wazaabi.org/core/themes/class");
		AnnotationContent content1 = CoreAnnotationsFactory.eINSTANCE
				.createAnnotationContent();
		content1.setKey("class");
		content1.setValue("class1");
		classAnnotation.getContents().add(content1);
		themedTextComponent.getAnnotations().add(classAnnotation);

		Resource r0 = new XMIResourceImpl();
		r0.getContents().add(theme);
		ByteArrayOutputStream bout = new ByteArrayOutputStream();
		try {
			r0.save(bout, null);
		} catch (IOException e) {
			e.printStackTrace();
		}

		try {
			content0.setValue(new String(bout.toByteArray(), "UTF-8"));

		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

//		 System.out.println(content0.getValue());
		return containerAnnotation;
	}
}
