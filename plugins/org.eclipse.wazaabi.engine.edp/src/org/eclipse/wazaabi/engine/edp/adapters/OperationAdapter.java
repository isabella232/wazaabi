package org.eclipse.wazaabi.engine.edp.adapters;

import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.coderesolution.DeferredAdapter;
import org.eclipse.wazaabi.engine.edp.coderesolution.ExecutableAdapter;

public interface OperationAdapter extends DeferredAdapter, ExecutableAdapter {

	public class MethodSignature {
		final private String methodName;
		final private String[] parameterNames;
		final private Class<?>[] parameterTypes;
		final private Class<?> returnType;

		public MethodSignature(String methodName, String[] parameterNames,
				Class<?>[] parameterTypes, Class<?> returnType) {
			this.methodName = methodName;
			this.parameterNames = parameterNames;
			this.parameterTypes = parameterTypes;
			this.returnType = returnType;
		}

		public String getMethodName() {
			return methodName;
		}

		public String[] getParameterNames() {
			return parameterNames;
		}

		public Class<?>[] getParameterTypes() {
			return parameterTypes;
		}

		public Class<?> getReturnType() {
			return returnType;
		}
	};

	public MethodSignature[] getMethodSignatures();

	public void registerMethods(AbstractCodeDescriptor codeDescriptor);
}
