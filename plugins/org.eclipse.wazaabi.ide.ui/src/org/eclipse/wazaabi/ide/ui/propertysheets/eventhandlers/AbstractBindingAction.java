/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.propertysheets.eventhandlers;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.wazaabi.engine.edp.adapters.BindingAdapter;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;

public abstract class AbstractBindingAction extends AbstractAction {

	public static final String TARGET_EPACKAGE_PARAMETER_NAME = "targetEPackage"; //$NON-NLS-1$
	public static final String TARGET_ECLASS_PARAMETER_NAME = "targetEClass"; //$NON-NLS-1$

	public static enum BindingDirection {
		WIDGET_TO_DOMAIN, DOMAIN_TO_WIDGET
	};

	protected EClass getTargetWidgetEClass(EventHandler eventHandler) {
		String targetEpackage = getStringParameterValue(eventHandler,
				TARGET_EPACKAGE_PARAMETER_NAME);
		String targetEClass = getStringParameterValue(eventHandler,
				TARGET_ECLASS_PARAMETER_NAME);

		EPackage ePackage = EPackage.Registry.INSTANCE
				.getEPackage(targetEpackage);
		if (ePackage != null) {
			EClassifier eClassifier = ePackage.getEClassifier(targetEClass);
			if (eClassifier instanceof EClass)
				return (EClass) eClassifier;
		}

		return null;
	}

	protected String getEventDispatcherDefaultPropertyBoundFor(EClass eClass,
			BindingDirection bindingDirection) {
		if (eClass == CoreWidgetsPackage.Literals.TEXT_COMPONENT) {
			return "@text";
		} else if (eClass == CoreWidgetsPackage.Literals.CHECK_BOX) {
			return "@selected";
		}
		return null;
	}

	/**
	 * Returns the list of events a eventDispatcher's binding is supposed to be
	 * triggered by. If the bound property is not given but required, the method
	 * returns null.
	 * 
	 * @param eClass
	 *            The EventDispatcher eClass
	 * @param boundProperty
	 *            the bound property when required (could be null).
	 * @param bindingDirection
	 * @return A list which could be empty. Null if the boundProperty was
	 *         required but not given.
	 */
	protected List<Event> getDefaultTriggeringEventsFor(EClass eClass,
			String boundProperty, BindingDirection bindingDirection) {
		List<Event> events = Collections.emptyList();

		if (eClass == CoreWidgetsPackage.Literals.TEXT_COMPONENT) {
			if ("@text".equals(getEventDispatcherDefaultPropertyBoundFor(
					eClass, bindingDirection))) {
				events = new ArrayList<Event>();
				if (bindingDirection == BindingDirection.WIDGET_TO_DOMAIN) {
					Event event1 = EDPEventsFactory.eINSTANCE.createEvent();
					event1.setId("core:ui:focus:out");
					Event event2 = EDPEventsFactory.eINSTANCE.createEvent();
					event2.setId("core:ui:default:selection");
					events.add(event1);
					events.add(event2);
				} else {
					if (boundProperty == null)
						return null;
					PropertyChangedEvent event1 = EDPEventsFactory.eINSTANCE
							.createPropertyChangedEvent();
					event1.setPath(boundProperty);
					Event event2 = EDPEventsFactory.eINSTANCE.createEvent();
					event2.setId("core:ui:refresh");
					events.add(event1);
					events.add(event2);
				}
			}
		} else if (eClass == CoreWidgetsPackage.Literals.CHECK_BOX) {
			if ("@selected".equals(getEventDispatcherDefaultPropertyBoundFor(
					eClass, bindingDirection))) {
				events = new ArrayList<Event>();
				if (bindingDirection == BindingDirection.WIDGET_TO_DOMAIN) {
					if (boundProperty == null)
						return null;
					PropertyChangedEvent event = EDPEventsFactory.eINSTANCE
							.createPropertyChangedEvent();
					event.setPath(boundProperty);
					events.add(event);
				} else {
					if (boundProperty == null)
						return null;
					PropertyChangedEvent event1 = EDPEventsFactory.eINSTANCE
							.createPropertyChangedEvent();
					event1.setPath(boundProperty);
					Event event2 = EDPEventsFactory.eINSTANCE.createEvent();
					event2.setId("core:ui:refresh");
					events.add(event1);
					events.add(event2);
				}
			}
		}

		return events;
	}

	protected String getCurrentContextKey() {
		return getDefaultContextKey();
	}

	protected String getDefaultContextKey() {
		return "input";
	}

	protected boolean areEquals(List<Event> events1, List<Event> events2) {
		// are both null or both not null ??
		if (events1 == null) {
			if (events2 == null)
				return true;
			else
				return false;
		} else if (events2 == null)
			return false;
		// are both empty or both not empty ??
		if (events1.isEmpty()) {
			if (events2.isEmpty())
				return true;
			else
				return false;
		} else if (events2.isEmpty())
			return false;

		if (events1.size() != events2.size())
			return false;

		// Both lists have the same size, the comparison is easy
		for (Event event : events1)
			if (!contains(events2, event))
				return true;

		return true;
	}

	/**
	 * Returns true if events contains event.
	 * 
	 * @param events
	 * @param event
	 * @return
	 */
	protected boolean contains(List<Event> events, Event event) {
		for (Event e : events)
			if (areEquals(e, event))
				return true;
		return false;
	}

	/**
	 * Returns true if the two events are equals.
	 * 
	 * @param event1
	 * @param event2
	 * @return
	 */
	protected boolean areEquals(Event event1, Event event2) {
		if (event1 == null) {
			if (event2 == null)
				return true;
			else
				return false;
		} else if (event2 == null)
			return false;
		if (event1.eClass() == EDPEventsPackage.Literals.EVENT
				&& event2.eClass() == EDPEventsPackage.Literals.EVENT)
			if (event1.getId() == null)
				return event2.getId() == null;
			else
				return event1.getId().equals(event2.getId());

		if (event1.eClass() == EDPEventsPackage.Literals.PROPERTY_CHANGED_EVENT
				&& event2.eClass() == EDPEventsPackage.Literals.PROPERTY_CHANGED_EVENT)
			if (((PropertyChangedEvent) event1).getPath() == null)
				return ((PropertyChangedEvent) event2).getPath() == null;
			else
				return ((PropertyChangedEvent) event1).getPath().equals(
						((PropertyChangedEvent) event2).getPath());
		return false;
	}

	protected Binding createDefaultBindingFor(EClass eClass,
			String domainBoundProperty, BindingDirection bindingDirection) {
		Binding binding = null;
		String defaultDispatcherBoundProperty = getEventDispatcherDefaultPropertyBoundFor(
				eClass, bindingDirection);
		if (defaultDispatcherBoundProperty == null
				|| "".equals(defaultDispatcherBoundProperty))
			return null;
		List<Event> defaultEvents = getDefaultTriggeringEventsFor(eClass,
				domainBoundProperty, bindingDirection);
		if (defaultEvents == null)
			return null;
		if (CoreWidgetsPackage.Literals.TEXT_COMPONENT == eClass
				|| CoreWidgetsPackage.Literals.CHECK_BOX == eClass) {
			binding = EDPHandlersFactory.eINSTANCE.createBinding();
			if (bindingDirection == BindingDirection.DOMAIN_TO_WIDGET) {
				addStringParameter(binding,
						BindingAdapter.SOURCE_PARAMETER_NAME,
						domainBoundProperty);
				addStringParameter(binding,
						BindingAdapter.TARGET_PARAMETER_NAME,
						defaultDispatcherBoundProperty);
			} else {
				addStringParameter(binding,
						BindingAdapter.SOURCE_PARAMETER_NAME,
						defaultDispatcherBoundProperty);
				addStringParameter(binding,
						BindingAdapter.TARGET_PARAMETER_NAME,
						domainBoundProperty);
			}

		}
		if (binding != null)
			binding.getEvents().addAll(defaultEvents);
		return binding;
	}

	protected void updateDomainBoundPropertyInBindingFor(Binding binding,
			EClass eClass, String newDomainBoundProperty,
			BindingDirection bindingDirection) {
		if (CoreWidgetsPackage.Literals.TEXT_COMPONENT == eClass
				|| CoreWidgetsPackage.Literals.CHECK_BOX == eClass) {
			if (bindingDirection == BindingDirection.DOMAIN_TO_WIDGET) {
				String existingDomainBoundProperty = getStringParameterValue(
						binding, BindingAdapter.SOURCE_PARAMETER_NAME);
				updateStringParameterValue(binding,
						BindingAdapter.SOURCE_PARAMETER_NAME,
						newDomainBoundProperty);
				if (existingDomainBoundProperty != null
						&& !"".equals(existingDomainBoundProperty))
					for (Event event : binding.getEvents())
						if (event instanceof PropertyChangedEvent
								&& existingDomainBoundProperty
										.equals(((PropertyChangedEvent) event)
												.getPath())) {
							((PropertyChangedEvent) event)
									.setPath(newDomainBoundProperty);
							return;
						}
			} else {
				updateStringParameterValue(binding,
						BindingAdapter.TARGET_PARAMETER_NAME,
						newDomainBoundProperty);
			}

		}
	}

	protected Parameter addStringParameter(Binding binding, String name,
			String value) {
		StringParameter parameter = EDPHandlersFactory.eINSTANCE
				.createStringParameter();
		parameter.setName(name);
		parameter.setValue(value);
		binding.getParameters().add(parameter);
		return parameter;
	}

	protected String getBindingDomainBoundProperty(Binding binding,
			BindingDirection bindingDirection) {
		if (bindingDirection == BindingDirection.DOMAIN_TO_WIDGET)
			return getStringParameterValue(binding,
					BindingAdapter.SOURCE_PARAMETER_NAME);
		else
			return getStringParameterValue(binding,
					BindingAdapter.TARGET_PARAMETER_NAME);

	}

	protected String getBindingEventDispatcherBoundProperty(Binding binding,
			BindingDirection bindingDirection) {
		if (bindingDirection == BindingDirection.WIDGET_TO_DOMAIN)
			return getStringParameterValue(binding,
					BindingAdapter.SOURCE_PARAMETER_NAME);
		else
			return getStringParameterValue(binding,
					BindingAdapter.TARGET_PARAMETER_NAME);
	}

	/**
	 * Finds an existing default binding in the given widget for this direction.
	 * Returns null if no default binding has been found.
	 * 
	 * @param widget
	 * @param bindingDirection
	 * @return
	 */
	protected Binding getExistingDefaultBinding(Widget widget,
			BindingDirection bindingDirection) {
		String widgetDefaultPropertyBound = getEventDispatcherDefaultPropertyBoundFor(
				widget.eClass(), bindingDirection);
		if (widgetDefaultPropertyBound == null
				|| "".equals(widgetDefaultPropertyBound)) //$NON-NLS-1$
			return null;
		for (EventHandler eventHandler : widget.getHandlers()) {
			if (eventHandler instanceof Binding) {
				Binding binding = (Binding) eventHandler;
				List<Event> expectedBindingTriggeringEvents = getDefaultTriggeringEventsFor(
						widget.eClass(),
						getBindingDomainBoundProperty(binding, bindingDirection),
						bindingDirection);
				if (expectedBindingTriggeringEvents == null)
					return null;
				if (!widgetDefaultPropertyBound
						.equals(getBindingEventDispatcherBoundProperty(binding,
								bindingDirection)))
					continue;
				if (areEquals(expectedBindingTriggeringEvents,
						binding.getEvents()))
					return binding;
			}
		}

		return null;
	}

	protected boolean isSameBinding(EClass eClass, String domainBoundProperty,
			String eventDispatcherBoundProperty, List<Object> converter,
			Binding binding, BindingDirection bindingDirection) {

		String bindingDomainBoundProperty = getBindingDomainBoundProperty(
				binding, bindingDirection);
		if (domainBoundProperty == null) {
			if (bindingDomainBoundProperty != null)
				return false;
		} else if (!domainBoundProperty.equals(bindingDomainBoundProperty))
			return false;

		String bindingEventdispatcherBoundProperty = getBindingEventDispatcherBoundProperty(
				binding, bindingDirection);
		if (eventDispatcherBoundProperty == null) {
			if (bindingEventdispatcherBoundProperty != null)
				return false;
		} else if (!eventDispatcherBoundProperty
				.equals(bindingEventdispatcherBoundProperty))
			return false;

		List<Event> expectedEvents = getDefaultTriggeringEventsFor(eClass,
				domainBoundProperty, bindingDirection);
		if (expectedEvents == null)
			return false; // We were not able to compute the list because
							// boundProperty is required but is null
		if (!areEquals(expectedEvents, binding.getEvents()))
			return false;

		// TODO : converters!!

		return true;
	}

}
