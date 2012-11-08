/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.webservice.generator.bcm;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.MethodVisitor;

/**
 * Adds the Exception superclass and removes the default constructor.
 */
public class AddExceptionSuperclass extends ClassAdapter {


    /** Logger. */
    private static final Logger LOG
    = LoggerFactory.getLogger(AddExceptionSuperclass.class);   


    /**
     * The adapter used to manipulate the code.
     * 
     * @param cv
     *            The <code>ClassVisitor</code>
     */
    public AddExceptionSuperclass(ClassVisitor cv) {
        super(cv);
    }


    /**
     * Adds the java.lang.Exception superclass.
     * 
     * @param version the version
     * @param access the class access modifier
     * @param name the class name
     * @param signature the class signature
     * @param superName the superclass name
     * @param interfaces the interfaces impelemented
     */
    public void visit(int version,
            int access,
            String name,
            String signature,
            String superName,
            String [] interfaces) {

        LOG.debug("Adding the java.lang.Exception superclass to class: " + name);                

        // Adding the java.lan.Excpetion superclass
        String javaLangExceptionClassName = "java/lang/Exception"; 

        super.visit(version, access, name, signature, javaLangExceptionClassName, interfaces);
    }

    /**
     * Removes the default constructor (at bytecode level call the Object
     * constructor.
     * 
     * @param access
     *          The access modifier
     * @param name
     *          The method name
     * @param desc
     *          The description
     * @param signature
     *          The method signature
     * @param exceptions
     *          The throwed exceptions
     * 
     * @return
     *          The MethodVisitor
     */
    public MethodVisitor visitMethod(int access,
            String name,
            String desc,
            String signature,
            String[] exceptions) {

        LOG.debug(">>>>> visitMethod - begin");

        // if we found the default constructor then ...
        if ("<init>".equals(name)) {

            LOG.debug("Default constructor modifications of the class:" + name);
            // removes the default constructor
            return null;        

        }
        return super.visitMethod(access, name, desc, signature, exceptions);
    }           


}
