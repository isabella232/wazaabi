package org.eclipse.wazaabi.ide.ui.editors.viewer;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;

public class FFactory {

	class Descriptor {

		private final Method method;
		private final Object containingInstance;

		public Descriptor(Object containingInstance, Method method) {
			this.method = method;
			this.containingInstance = containingInstance;
		}

		protected Method getMethod() {
			return method;
		}

		protected Object getContainingInstance() {
			return containingInstance;
		}

	}

	protected void registerMethod() {

	}

	protected Descriptor getDescriptor(EObject targetUI, int index,
			EObject source, EClass topic, Object context) {
		return null;
	}

	public List<?> get(EObject targetUI, int index, EObject source,
			EClass topic, Object context) {
		if (targetUI == null || source == null || topic == null)
			return Collections.emptyList();
		Descriptor descriptor = getDescriptor(targetUI, index, source, topic,
				context);
		if (descriptor != null)
			try {
				List<?> result = (List<?>) descriptor.getMethod().invoke(
						descriptor.getContainingInstance(), new Object[] {});
				return result != null ? result : Collections.emptyList();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				e.printStackTrace();
			}
		return Collections.emptyList();

	}
}
