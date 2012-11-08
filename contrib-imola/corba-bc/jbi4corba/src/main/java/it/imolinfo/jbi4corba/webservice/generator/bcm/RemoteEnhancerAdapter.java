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
import org.objectweb.asm.MethodVisitor;

/**
 * Bytecode manipulation.
 *
 * 1) add the java.rmi.Remote interface to the class
 * 2) add the java.rmi.RemoteException to the 'throws' clause of each method.
 */
public class RemoteEnhancerAdapter extends ClassAdapter {

    /**
     * Logger.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(RemoteEnhancerAdapter.class);
    
    /**
     * The name of the class that represents the PortType.
     */
    protected String portTypeClassName;

  /**
   * The name of the class manipulated.
   */
  String completeName;


  /**
   * Constructor.
   *
   * @param    arg0    The ClassVisitor
   * @param    portTypeClassName    The port type class name
   */
  public RemoteEnhancerAdapter(ClassVisitor arg0, String portTypeClassName) {
    super(arg0);
    this.portTypeClassName = portTypeClassName;
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

    String newName = portTypeClassName + "CorbaInterface";

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

    LOG.debug("<<<<< visit - end");
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
  public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
    String[] newExceptions;
    if (exceptions==null){
      newExceptions=new String[]{"java/rmi/RemoteException"};
    } else {
      newExceptions = new String[exceptions.length + 1];
      if (exceptions!=null){
      for (int i = 0; i < exceptions.length; i++) {
        newExceptions[i] = exceptions[i];
      }
      }
      newExceptions[newExceptions.length - 1] = "java/rmi/RemoteException";
    }
    return super.visitMethod(access, name, desc, signature, newExceptions);
  }

  /**
   * @return  the complete name
   */
  public String getCompleteName() {
    return completeName;
  }

}
