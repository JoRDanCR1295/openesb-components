/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.webservice.generator.bcm;

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.MethodVisitor;


/**
 * "Remotizes" the interface. A copy from the jbi4corba project, but no "CorbaInterface" is added.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class RemoteEnancherAdapter extends ClassAdapter {


  /** The name of the class manipulated. */
  private String completeName;

  /** The name of the class that represents the PortType. */
  private String portTypeClassName;

  /**
     * Constructor.
     * 
     * @param cv
     *          The <code>ClassVisitor</code>
     * @param portTypeClassName
     *          The port type class name
     */
  public RemoteEnancherAdapter(ClassVisitor cv, String portTypeClassName) {
    super(cv);
    this.portTypeClassName = portTypeClassName;
  }

  
  /**
   * Adds the <code>java.rmi.Remote</code>.
   * 
   * @param version 
   *            The version
   * @param access 
   *            The access modifier
   * @param name 
   *            The method name
   * @param signature 
   *            The signature
   * @param superName 
   *            The superclass name
   * @param interfaces
   *            The interfaces implemented
   */
  public void visit(int version,
                    int access,
                    String name,
                    String signature,
                    String superName,
                    String [] interfaces) {
    

    // MARCO: Modified from jbi4corba
    String newName = portTypeClassName;

    String [] newInterfaces;

    if (interfaces == null){

      newInterfaces = new String [] {"java/rmi/Remote"};

    } else {

      newInterfaces = new String[interfaces.length + 1];

      for (int i = 0; i < interfaces.length; i++) {
        newInterfaces[i] = interfaces[i];
      }

      newInterfaces[newInterfaces.length - 1] = "java/rmi/Remote";
    }

    super.visit(version, access, newName, signature, superName, newInterfaces);

    completeName = newName;
    
  }

 
  /**
     * Adds the <code>java.rmi.Exception</code> to the exceptions throws.
     * 
     * @param access
     *          The access modifier
     * @param name
     *          The method name
     * @param desc
     *          The method desc
     * @param signature
     *          The signature
     * @param exceptions
     *          The exception raised
     * 
     * @return
     *          The method visitor
     * 
     * @see org.objectweb.asm.ClassAdapter#visitMethod(int, java.lang.String,
     *      java.lang.String, java.lang.String, java.lang.String[])
     */
  public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
    String[] newExceptions;
    if (exceptions==null) {
      newExceptions=new String[]{"java/rmi/RemoteException"};
    }
    else {
        newExceptions = new String[exceptions.length + 1];
        if (exceptions!=null) {
            for (int i = 0; i < exceptions.length; i++) {
                newExceptions[i] = exceptions[i];
            }
        }
      newExceptions[newExceptions.length - 1] = "java/rmi/RemoteException";
    }
    return super.visitMethod(access, name, desc, signature, newExceptions);
  }

  /**
     * Gets the complete name.
     * 
     * @return the complete name
     */
  public String getCompleteName() {
    return completeName;
  }

}
    

