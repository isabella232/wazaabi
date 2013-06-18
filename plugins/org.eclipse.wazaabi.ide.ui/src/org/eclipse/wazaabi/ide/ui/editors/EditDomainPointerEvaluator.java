package org.eclipse.wazaabi.ide.ui.editors;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.gef.EditDomain;
import org.eclipse.wazaabi.ide.ui.editparts.commands.SetFeatureCommand;
import org.eclipse.wazaabi.ide.ui.propertysheets.eventhandlers.AbstractStyleRuleAction;
import org.eclipse.wazaabi.locationpaths.PointersEvaluatorImpl;
import org.eclipse.wazaabi.mm.edp.Context;

public class EditDomainPointerEvaluator extends PointersEvaluatorImpl {

	public static final String FACTORY_ID = EditDomainPointerEvaluator.class
			.getName();
	private final EditDomain editDomain;

	public EditDomainPointerEvaluator(EditDomain editDomain) {
		this.editDomain = editDomain;
	}

	protected void setFeature(EObject target, EStructuralFeature feature,
			Object value) {
		// we check if target belongs to the property sheet or to the domain
		if (target instanceof Context
				&& ((Context) target)
						.get(AbstractStyleRuleAction.EDIT_DOMAIN_KEY) == getEditDomain())
			// the target belongs to the property sheet, no command needed
			super.setFeature(target, feature, value);
		else {
			SetFeatureCommand command = new SetFeatureCommand();
			command.setFeature(feature);
			command.setNewValue(value);
			command.setTarget(target);
			getEditDomain().getCommandStack().execute(command);
		}
	}

	protected EditDomain getEditDomain() {
		return editDomain;
	}

	@Override
	public String getFactoryID() {
		return FACTORY_ID;
	}

}
