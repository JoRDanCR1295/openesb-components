 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

package it.imolinfo.jbi4corba.webservice.generator.bcm;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.Messages;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;



/**
 * This class is used to supply all the utility methods for the bytecode
 * manipulation.
 */
public class ByteCodeManipulationUtil {

    /**
     * Logger.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(ByteCodeManipulationUtil.class);

    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES
            = Messages.getMessages(ByteCodeManipulationUtil.class);
    
    /**
     * XmlAttribute annotation.
     */
    public static final String JAXB_XML_ATTRIBUTE = "Ljavax/xml/bind/annotation/XmlAttribute;";    

    /**
     * Default constructor.
     */
    public ByteCodeManipulationUtil (){
    }
    
    /**
    * This method remove the '.class' suffix.
    *
    * @param    cn    The class name to modify.
    *
    * @return    The class name modified.
    */
    String bytecodeClassName(String cn) {
        final String suffix = ".class";

        if (cn == null) {
            LOG.error("CRB000601_Class_name_is_null");
            throw new NullPointerException(
                    MESSAGES.getString("CRB000601_Class_name_is_null"));
        }

        // If .class is not present, do not remove it.
        int suffixLengthToRemove = 0;
        if (cn.indexOf(suffix) != -1) {
        	suffixLengthToRemove = suffix.length();
        }
        String bcn = cn.substring(0, cn.length() - suffixLengthToRemove);

        bcn = bcn.replace(".", "/");

        LOG.debug("bytecodeClassName:" + bcn);
        return bcn;
    }

    /**
     * This method add a setter method.
     *
     * @param    classWriter        The object used to create the setter.
     * @param    className          The name of the class.
     * @param    propertyName       The name of the property of the setter.
     * @param    type               The type of the property of the setter.
     *
     */
    public void createSetter(ClassWriter classWriter,
                      String className,
                      String propertyName,
                      String type) {

    LOG.debug(">>>>> createSetter - begin");

    LOG.debug("createSetter.propertyName=" + propertyName + "; type=" + type);

    String methodName = "set"
                        + propertyName.substring(0, 1).toUpperCase()
                        + propertyName.substring(1);

    MethodVisitor mv = classWriter.visitMethod(Opcodes.ACC_PUBLIC,
                                                methodName,
                                                "(" + type + ")V",
                                                null,
                                                null);
    mv.visitVarInsn(Opcodes.ALOAD, 0);
    mv.visitVarInsn(Type.getType(type).getOpcode(Opcodes.ILOAD), 1);

    mv.visitFieldInsn(Opcodes.PUTFIELD,
                      bytecodeClassName(className),
                      propertyName,
                      type);

    mv.visitInsn(Opcodes.RETURN);
    mv.visitMaxs(0, 0);
    mv.visitEnd();
    LOG.debug("<<<<< createSetter - end");
    }

    /**
     * This method add a getter method.
     *
     * @param    classWriter     The object used to create the setter.
     * @param    className       The name of the class.
     * @param    propertyName    The name of the property of the getter.
     * @param    returnType      The type of the property of the getter.
     * @param    isXmlAttribute  if True, adds the XmlAttribute annotation to the method (useful for exception mapping)
     *
     */
    public void createGetter(ClassWriter classWriter,
                      String className,
                      String propertyName,
                      String returnType, 
                      boolean isXmlAttribute) {

        LOG.debug(">>>>> createGetter - begin");

        LOG.debug("createGetter.propertyName=" + propertyName
                  + "; returnType=" + returnType);

        String methodName = "get"
            + propertyName.substring(0, 1).toUpperCase()
            + propertyName.substring(1);

        MethodVisitor mv = classWriter.visitMethod(Opcodes.ACC_PUBLIC,
                                                   methodName,
                                                   "()" + returnType,
                                                   null,
                                                   null);
        
        // If isXmlAttribute, adds the XmlAttribute to getter methods
        if (isXmlAttribute) {
            LOG.debug("Adding XMLAttribute to getter for the property: " + propertyName);
            AnnotationVisitor av = null;
            // We annotate the classes with property access type
            av = mv.visitAnnotation(JAXB_XML_ATTRIBUTE, true);
            av.visitEnd();
        }
        
        mv.visitVarInsn(Opcodes.ALOAD, 0);

        mv.visitFieldInsn(Opcodes.GETFIELD,
                          bytecodeClassName(className),
                          propertyName,
                          returnType);

        mv.visitInsn(Type.getType(returnType).getOpcode(Opcodes.IRETURN));
        mv.visitMaxs(0, 0);
                
        mv.visitEnd();
        
        
        LOG.debug("<<<<< createGetter - end");
    }

    /**
    * This method create a method toString().
    *
    * <code>
    *     public String toString() {
    *         return ReflectionToStringBuilder.toString(this);
    *     }
    * </code>
    *
    * @param    classWriter        The object used to create the method.
    */
    void createToString(ClassWriter classWriter) {
        MethodVisitor mv = classWriter.visitMethod(Opcodes.ACC_PUBLIC,
                                                "toString",
                                                "()Ljava/lang/String;",
                                                null,
                                                null);
        mv.visitCode();
        Label start = new Label();
        mv.visitLabel(start);
        mv.visitLineNumber(53, start);
        mv.visitVarInsn(Opcodes.ALOAD, 0);

        mv.visitMethodInsn(
                Opcodes.INVOKESTATIC,
                "org/apache/commons/lang/builder/ReflectionToStringBuilder",
                "toString",
                "(Ljava/lang/Object;)Ljava/lang/String;");

        mv.visitInsn(Opcodes.ARETURN);
        Label stop = new Label();
        mv.visitLabel(stop);
        mv.visitMaxs(1, 1);
        mv.visitEnd();
    }

    /**
    * This method create a method equals(Object).
    *
    * <code>
    * public boolean equals(Object obj) {
    *     return EqualsBuilder.reflectionEquals(this, obj);
    * }
    * </code>
    *
    * @param    classWriter        The object used to create the method.
    */
    void createEquals(ClassWriter classWriter) {
        MethodVisitor mv = classWriter.visitMethod(Opcodes.ACC_PUBLIC,
                                                "equals",
                                                "(Ljava/lang/Object;)Z",
                                                null,
                                                null);
        mv.visitCode();
        Label start = new Label();
        mv.visitLabel(start);
        mv.visitLineNumber(53, start);
        mv.visitVarInsn(Opcodes.ALOAD, 0);
        mv.visitVarInsn(Opcodes.ALOAD, 1);

        mv.visitMethodInsn(
                Opcodes.INVOKESTATIC,
                "org/apache/commons/lang/builder/EqualsBuilder",
                "reflectionEquals",
                "(Ljava/lang/Object;Ljava/lang/Object;)Z");

        mv.visitInsn(Opcodes.IRETURN);
        Label stop = new Label();
        mv.visitLabel(stop);
        mv.visitMaxs(2, 2);
        mv.visitEnd();
    }


}
