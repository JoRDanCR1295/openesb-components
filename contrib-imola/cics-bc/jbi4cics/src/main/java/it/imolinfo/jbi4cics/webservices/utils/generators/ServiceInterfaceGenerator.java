/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.webservices.utils.generators;

import static org.objectweb.asm.Opcodes.ACC_ABSTRACT;
import static org.objectweb.asm.Opcodes.ACC_INTERFACE;
import static org.objectweb.asm.Opcodes.ACC_PUBLIC;
import static org.objectweb.asm.Opcodes.V1_1;
import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.exception.ClassGenerationException;
import it.imolinfo.jbi4cics.jbi.BCELClassLoader;
import it.imolinfo.jbi4cics.webservices.descriptor.ServiceDescriptor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Type;

public final class ServiceInterfaceGenerator {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(ServiceInterfaceGenerator.class);

    /**
     * The name of the java class that will be generated. This field contains
     * values like <i>foo.bar.MyClass</i> and so on.
     */
    private final String completeClassName;

    /**
     * The service descriptor.
     */
    private final ServiceDescriptor serviceDescriptor;

    public ServiceInterfaceGenerator(
            final ServiceDescriptor serviceDescriptor) {
        completeClassName = serviceDescriptor.getServiceInterfacePackageName()
                            + "." + serviceDescriptor.getServiceInterfaceName();
        this.serviceDescriptor = serviceDescriptor;

        if (LOG.isDebugEnabled()) {
            LOG.debug("Complete interface name: " + completeClassName);
        }
    }

    public Class generateServiceInterface(final BCELClassLoader loader)
            throws ClassGenerationException {

        // 'true' so ClassWriter calculate computeMaxs fields in the bytecode
        ClassWriter cw = new ClassWriter(true);
        String internalClassName = completeClassName.replace('.', '/');
        String returnType
                = Type.getDescriptor(serviceDescriptor.getOutputBean());
        String paramType = Type.getDescriptor(serviceDescriptor.getInputBean());
        MethodVisitor mv;
        Class clazz;

        cw.visit(V1_1, ACC_PUBLIC + ACC_ABSTRACT + ACC_INTERFACE,
                 internalClassName, null, "java/lang/Object", null);
        mv = cw.visitMethod(ACC_PUBLIC + ACC_ABSTRACT,
                            serviceDescriptor.getOperationName(),
                            "(" + paramType + ")" + returnType, null, null);
        mv.visitEnd();
        loader.addClass(completeClassName, cw.toByteArray());
        try {
            clazz = loader.loadClass(completeClassName);
        } catch (ClassNotFoundException e) {
            Object[] args = new Object[] { completeClassName };

            LOG.error("CIC002400_Class_generation_error", args, e);
            throw new ClassGenerationException(
                    "CIC002400_Class_generation_error", args, e);
        }
        serviceDescriptor.setServiceInterface(clazz);
        return clazz;
    }
}
