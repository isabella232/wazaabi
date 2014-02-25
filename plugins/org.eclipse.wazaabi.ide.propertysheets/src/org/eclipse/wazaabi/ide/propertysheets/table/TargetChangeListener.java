package org.eclipse.wazaabi.ide.propertysheets.table;

import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;

public interface TargetChangeListener {

	public void targetAdded(EObject target, int position);

	public void targetModified(EObject target, EStructuralFeature feature,
			int position, Object oldValue, Object newValue);

	public void targetMultipleModified(EObject target,
			List<EStructuralFeature> features, List<Integer> positions,
			List<Object> oldValues, List<Object> newValues);

	public void targetRemoved(EObject target);

}
