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

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * This class visit the bytecode of another class and executes the following
 * three main tasks:
 * 1) add java.io.Serializable interface
 * 2) remove getter and setter
 * 3) modify the scope of the member from private to public.
 */
public class ValueTypeAdapter extends ClassAdapter {

  /**
   * Logger.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(ValueTypeAdapter.class);

  /**
   * The Class Writer.
   */
  protected ClassWriter classWriter = null;

  /**
   * The class name.
   */
  protected String className = null;

  /**
   * Constructor.
   *
   * @param    cv        The class visitor.
   * @param    cw        The class writer.
   * @param    cn        The class name.
   */
  public ValueTypeAdapter(ClassVisitor cv, ClassWriter cw, String cn) {
    super(cv);
    classWriter = cw;
    className = cn;
  }

  /**
   * Override.
   * @param version     The version
   * @param access      The access
   * @param name        The name
   * @param signature   The signature
   * @param superName   The super name
   * @param interfaces  The interfaces
   */
    @Override
    public void visit(int version,
                    int access,
                    String name,
                    String signature,
                    String superName,
                    String [] interfaces) {

        LOG.debug(">>>>> visit - begin");

        LOG.debug("VISIT"
                + ".\n version="    + version
                + ";\n access="     + access
                + ";\n name="       + name
                + ";\n signature="  + signature
                + ";\n superName="  + superName
                + ";\n interfaces=" + interfaces);

        // extends java.io.Serializable
        //         java/io/Serializable

        if (interfaces == null || interfaces.length == 0) {

            // no interfaces found ... create the array and add my own one.
            interfaces = new String [] {"java/io/Serializable"};

        } else {

            // there are one or more interfaces ...
            // ... adjust the size of the array and add my interface.
            String [] temp = new String [interfaces.length + 1];

            System.arraycopy(interfaces, 0, temp, 0, interfaces.length);
            temp[interfaces.length] = "java/io/Serializable";

            interfaces = temp;
        }

        LOG.debug("<<<<< visit - end");
        super.visit(version, access, name, signature, superName, interfaces);
    }

    /**
     * Override.
     * @param access      The access
     * @param name        The name
     * @param desc        The description
     * @param signature   The signature
     * @param value       The value
     * @return            The return
     */
    @Override
    public FieldVisitor visitField(int access,
                                   String name,
                                   String desc,
                                   String signature,
                                   Object value) {
        LOG.debug(">>>>> visitField - begin");

        if ((Opcodes.ACC_STATIC & access) != Opcodes.ACC_STATIC) {
            access = Opcodes.ACC_PUBLIC;
        }
        LOG.debug("The field " + name + " will be 'public'.");

        LOG.debug(">>>>> visitField - end");
        return super.visitField(access, name, desc, signature, value);
    }

    /**
     * Override.
     * @param access      The access
     * @param name        The name
     * @param desc        The description
     * @param signature   The signature
     * @param exceptions  The exceptions
     * @return            The return
     */
    @Override
    public MethodVisitor visitMethod(int access,
            String name,
            String desc,
            String signature,
            String[] exceptions) {

        LOG.debug(">>>>> visitMethod - begin");

        LOG.debug("visitMethod. access="     + access
                        + "; name="       + name
                        + "; desc="       + desc
                        + "; signature="  + signature
                        + "; exceptions=" + exceptions);

        // Excludes getFaultInfo...
        if (!name.equals("getFaultInfo")) {
            // if we found the default constructor then ...
            if (isGetter(name, desc) || isSetter(name, desc)) {

                LOG.debug("<<<<< visitMethod - remove method:" + name);
                return null;
            }
        }

        // else
        LOG.debug("<<<<< visitMethod - untouched:" + name);
        return super.visitMethod(access, name, desc, signature, exceptions);
    }

  
    /**
     * @param methodName  The method name
     * @param params      The params
     * @return            The return
     */
    private boolean isGetter(String methodName, String params) {
        if (methodName == null) {
        	return false;
        }
        // else
        if (methodName.startsWith("get") && params.startsWith("()")
                && methodName.length() > 3) {
            return true;
        }
        // else
        if (methodName.startsWith("is") && params.startsWith("()")
                && methodName.length() > 2) {
            return true;
        }
        // else
        return false;
    }

    /**
     * @param methodName  The method name
     * @param params      The params
     * @return            The return
     */
    private boolean isSetter(String methodName, String params) {
        if (methodName == null) { 
        	return false;
        }
        // else
        if (methodName.length() <= 3) { 
        	return false;
        }
        // else
        if (methodName.startsWith("set") && (! params.startsWith("()"))) {
            return true;
        }
        // else
        return false;
    }

}
