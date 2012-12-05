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

package org.eclipse.wazaabi.ide.ui.editors.viewer;

import java.io.*;

import org.eclipse.swt.dnd.ByteArrayTransfer;
import org.eclipse.swt.dnd.TransferData;

public class ModelDescriptorTransfert extends ByteArrayTransfer {

    private static ModelDescriptorTransfert instance = new ModelDescriptorTransfert();
    private static final String TYPE_NAME = "wazaabi-model-descriptor-format";
    private static final int TYPEID = registerType(TYPE_NAME);

    public static ModelDescriptorTransfert getInstance() {
        return instance;
    }

    private ModelDescriptorTransfert() {
    }

    public ModelDescriptor[] fromByteArray(byte[] bytes) {
        try {
            ObjectInputStream in = new ObjectInputStream(
                    new ByteArrayInputStream(bytes));
            // read number of modelDescriptors
            int n = in.readInt();
            // read modelDescriptors
            ModelDescriptor[] modelDescriptors = new ModelDescriptor[n];
            for (int i = 0; i < n; i++) {
                ModelDescriptor modelDescriptor = (ModelDescriptor) in
                        .readObject();
                if (modelDescriptor == null)
                    return null;
                modelDescriptors[i] = modelDescriptor;
            }
            return modelDescriptors;
        } catch (IOException e) {
            return null;
        } catch (ClassNotFoundException e) {
            return null;
        }
    }

    protected int[] getTypeIds() {
        return new int[] { TYPEID };
    }

    protected String[] getTypeNames() {
        return new String[] { TYPE_NAME };
    }

    protected void javaToNative(Object object, TransferData transferData) {
        byte[] bytes = toByteArray((ModelDescriptor[]) object);
        if (bytes != null)
            super.javaToNative(bytes, transferData);
    }

    protected Object nativeToJava(TransferData transferData) {
        byte[] bytes = (byte[]) super.nativeToJava(transferData);
        return fromByteArray(bytes);
    }

    public byte[] toByteArray(ModelDescriptor[] modelDescriptors) {
        ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
        byte[] bytes = null;
        try {
            ObjectOutputStream out = new ObjectOutputStream(byteOut);

            out.writeInt(modelDescriptors.length);

            for (ModelDescriptor modelDescriptor : modelDescriptors)
                out.writeObject(modelDescriptor);

            out.close();
            bytes = byteOut.toByteArray();
        } catch (IOException e) {
            // when in doubt send nothing
        }
        return bytes;
    }
}