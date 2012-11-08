/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.webservices.utils.generators;

import static org.objectweb.asm.Opcodes.ACC_PRIVATE;
import static org.objectweb.asm.Opcodes.ACC_PUBLIC;
import static org.objectweb.asm.Opcodes.ACC_SUPER;
import static org.objectweb.asm.Opcodes.ALOAD;
import static org.objectweb.asm.Opcodes.ARETURN;
import static org.objectweb.asm.Opcodes.GETFIELD;
import static org.objectweb.asm.Opcodes.ILOAD;
import static org.objectweb.asm.Opcodes.INVOKESPECIAL;
import static org.objectweb.asm.Opcodes.INVOKESTATIC;
import static org.objectweb.asm.Opcodes.IRETURN;
import static org.objectweb.asm.Opcodes.PUTFIELD;
import static org.objectweb.asm.Opcodes.RETURN;
import static org.objectweb.asm.Opcodes.V1_1;
import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.exception.ClassGenerationException;
import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.jbi.BCELClassLoader;
import it.imolinfo.jbi4cics.messageformat.FieldDescriptor;
import it.imolinfo.jbi4cics.messageformat.MappingDescriptor;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolType;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolTypeDescriptor;
import it.imolinfo.jbi4cics.webservices.descriptor.ServiceDescriptor;
import java.util.Map;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Type;

public final class ServiceBeanGenerator {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(ServiceBeanGenerator.class);

    /**
     * The name of the class that will be generated. This field contains values
     * like <i>foo.bar.MyClass</i>.
     */
    private final String completeClassName;

    /**
     * The name of the class that will be generated converted in an internal
     * format, used by ASM library. It is calculated from
     * {@link #completeClassName}.
     */
    private final String internalClassName;

    /**
     * The mapping descriptor.
     */
    private final MappingDescriptor mappingDescriptor;

    /**
     * The object able to generate a Java class "from scratch", without writing
     * data on disk.
     */
    // 'true' to leave ClassWriter calculate computeMaxs fields in the bytecode
    private final ClassWriter classWriter = new ClassWriter(true);

    public ServiceBeanGenerator(final ServiceDescriptor desc,
                                final boolean input) {
        this(desc.getServiceInterfacePackageName() + "."
             + (input ? desc.getInputBeanClassName()
                      : desc.getOutputBeanClassName()),
             input ? desc.getInputMappingDescriptor()
                   : desc.getOutputMappingDescriptor());
    }

    private ServiceBeanGenerator(final String completeClassName,
                                 final MappingDescriptor mappingDescriptor) {
        this.completeClassName = completeClassName;
        this.mappingDescriptor = mappingDescriptor;

        internalClassName = completeClassName.replace('.', '/');
        classWriter.visit(V1_1, ACC_PUBLIC + ACC_SUPER, internalClassName, null,
                          "java/lang/Object", null);
    }

    /**
     * Per gestire il nesting devo potere genereare e usare al volo le classi
     * visitando la gerarchia di classi in nesting con il metodo depth first. Le
     * classi generate sono salvate nel bcel classloader che garantisce che
     * possano essere caricate in seguito.
     */
    public Class generateBeanClass(final BCELClassLoader loader)
            throws ClassGenerationException {
        boolean debug = LOG.isDebugEnabled();
        Class clazz;

        addDefaultConstructor();
        addToStringMethod();
        addEqualsMethod();
        addHashCodeMethod();
        try {
            Map<String, FieldDescriptor> fields
                    = mappingDescriptor.getFieldMap();

            for (Map.Entry<String, FieldDescriptor> entry : fields.entrySet()) {
                String propertyName = entry.getKey();
                FieldDescriptor fieldDescriptor = entry.getValue();
                Type type;

                if (fieldDescriptor instanceof CobolTypeDescriptor) {
                    CobolTypeDescriptor cobolTypeDescriptor
                            = (CobolTypeDescriptor) fieldDescriptor;
                    CobolType cobolType = cobolTypeDescriptor.getType();

                    if ((cobolType == CobolType.NESTED_COMMAREA)
                            || (cobolType == CobolType.OCCURS)) {

                        // We need to generate the nested class
                        String innerClassName =
                                completeClassName.substring(0, completeClassName.lastIndexOf('.') + 1)
                                + propertyName;
                        ServiceBeanGenerator inner = new ServiceBeanGenerator(
                                innerClassName,
                                cobolTypeDescriptor.getNestedCommarea());

                        inner.generateBeanClass(loader);
                    }
                }

                // Create field, setter and getter
                if (debug) {
                    LOG.debug("handling property name: " + propertyName);
                }
                type = Type.getType(fieldDescriptor.getPreferredJavaType());
                addPrivateField(propertyName, type);
                addSetMethod(propertyName, type);
                addGetMethod(propertyName, type);
            }
        } catch (FormatException e) {
            Object[] args = new Object[] { completeClassName };

            LOG.error("CIC002400_Class_generation_error", args, e);
            throw new ClassGenerationException(
                    "CIC002400_Class_generation_error", args, e);
        }

        loader.addClass(completeClassName, classWriter.toByteArray());
        try {
            clazz = loader.loadClass(completeClassName);
        } catch (ClassNotFoundException e) {
            Object[] args = new Object[] { completeClassName };

            LOG.error("CIC002400_Class_generation_error", args, e);
            throw new ClassGenerationException(
                    "CIC002400_Class_generation_error", args, e);
        }
        mappingDescriptor.setBeanClass(clazz);
        return clazz;
    }

    /**
     * Adds the default constructor to the java class we're creating. The
     * generated constructor is defined as <code>public</code>.
     */
    private void addDefaultConstructor() {
        MethodVisitor mv = classWriter.visitMethod(
                ACC_PUBLIC, "<init>", "()V", null, null);

        // Pushes the 'this' variable
        mv.visitVarInsn(ALOAD, 0);

        // Invokes the super class constructor
        mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
        mv.visitInsn(RETURN);

        // This code uses a maximum of one stack element and one local variable
        mv.visitMaxs(1, 1);
        mv.visitEnd();
    }

    /**
     * Creates the <code>toString()</code> method. The generated method is
     * equivalent to:
     * <pre><code>
     * public String toString() {
     *     return ReflectionToStringBuilder.toString(this);
     * }
     * </code></pre>
     */
    private void addToStringMethod() {
        MethodVisitor mv = classWriter.visitMethod(ACC_PUBLIC,
                "toString", "()Ljava/lang/String;", null, null);

        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESTATIC,
                "org/apache/commons/lang/builder/ReflectionToStringBuilder",
                "toString", "(Ljava/lang/Object;)Ljava/lang/String;");
        mv.visitInsn(ARETURN);
        mv.visitMaxs(0, 0);
        mv.visitEnd();
    }

    /**
     * Create the <code>equals(Object o)</code> method. The generated method is
     * equivalent to:
     * <pre><code>
     * public boolean equals(Object obj) {
     *     return EqualsBuilder.reflectionEquals(this, obj);
     * }
     * </code></pre>
     */
    private void addEqualsMethod() {
        MethodVisitor mv = classWriter.visitMethod(
                ACC_PUBLIC, "equals", "(Ljava/lang/Object;)Z", null, null);

        mv.visitVarInsn(ALOAD, 0);
        mv.visitVarInsn(ALOAD, 1);
        mv.visitMethodInsn(INVOKESTATIC,
                "org/apache/commons/lang/builder/EqualsBuilder",
                "reflectionEquals", "(Ljava/lang/Object;Ljava/lang/Object;)Z");
        mv.visitInsn(IRETURN);
        mv.visitMaxs(0, 0);
        mv.visitEnd();
    }

    /**
     * Create the <code>hashCode()</code> method. The generated method is
     * equivalent to:
     * <pre><code>
     * public boolean equals(Object obj) {
     *     return HashCodeBuilder.reflectionHashCode(this);
     * }
     * </code></pre>
     */
    private void addHashCodeMethod() {
        MethodVisitor mv = classWriter.visitMethod(ACC_PUBLIC, "hashCode",
                                                   "()I", null, null);

        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESTATIC,
                           "org/apache/commons/lang/builder/HashCodeBuilder",
                           "reflectionHashCode", "(Ljava/lang/Object;)I");
        mv.visitInsn(IRETURN);
        mv.visitMaxs(0, 0);
        mv.visitEnd();
    }

    /**
     * Adds a <code>private</code> field to the java class in construction.
     *
     * @param  fieldName  the field name. Must be not <code>null</code>.
     * @param  fieldType  the field type. Must be not <code>null</code>.
     */
    private void addPrivateField(final String fieldName, final Type fieldType) {
        FieldVisitor fv = classWriter.visitField(
                ACC_PRIVATE, fieldName, fieldType.getDescriptor(), null, null);

        fv.visitEnd();
    }

    /**
     * Adds a getter method.
     *
     * @param  propertyName  the <i>java bean</i> property name. Must be not
     *                       <code>null</code>.
     * @param  propertyType  the <i>java bean</i> property type. Must be not
     *                       <code>null</code>.
     */
    private void addGetMethod(final String propertyName,
                              final Type propertyType) {
        StringBuilder methodName = new StringBuilder(propertyName);
        String returnType = propertyType.getDescriptor();
        MethodVisitor mv;

        methodName.setCharAt(0, Character.toUpperCase(methodName.charAt(0)));
        methodName.insert(0, "get");

        mv = classWriter.visitMethod(ACC_PUBLIC, methodName.toString(),
                                     "()".concat(returnType), null, null);
        mv.visitVarInsn(ALOAD, 0);
        mv.visitFieldInsn(GETFIELD, internalClassName, propertyName,
                          returnType);
        mv.visitInsn(propertyType.getOpcode(IRETURN));
        mv.visitMaxs(0, 0);
    }

    /**
     * Adds a setter method.
     *
     * @param  propertyName  the <i>java bean</i> property name. Must be not
     *                       <code>null</code>.
     * @param  propertyType  the <i>java bean</i> property type. Must be not
     *                       <code>null</code>.
     */
    private void addSetMethod(final String propertyName,
                              final Type propertyType) {
        StringBuilder methodName = new StringBuilder(propertyName);
        String desc = propertyType.getDescriptor();
        MethodVisitor mv;

        methodName.setCharAt(0, Character.toUpperCase(methodName.charAt(0)));
        methodName.insert(0, "set");

        mv = classWriter.visitMethod(ACC_PUBLIC, methodName.toString(),
                                     "(" + desc + ")V", null, null);
        mv.visitVarInsn(ALOAD, 0);
        mv.visitVarInsn(propertyType.getOpcode(ILOAD), 1);
        mv.visitFieldInsn(PUTFIELD, internalClassName, propertyName, desc);
        mv.visitInsn(RETURN);
        mv.visitMaxs(0, 0);
    }
}
