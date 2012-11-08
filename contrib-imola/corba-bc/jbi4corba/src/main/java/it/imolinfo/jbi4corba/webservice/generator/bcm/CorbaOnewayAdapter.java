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
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;


import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.MethodVisitor;

/**
 * This class is used to discover if the class visited is a corba stub with
 * oneway operations.
 */
public class CorbaOnewayAdapter extends ClassAdapter {

  /** Logger. */
  private static final Logger LOG
    = LoggerFactory.getLogger(CorbaOnewayAdapter.class);

  /** The internal class name of the class. */
  private String internalClassName = null;

  /** The associated IDL interface. */
  private String associatedInterface = null;

  /** The list of the 'oneway' operations. */
  private List<String> onewayOperationList = new ArrayList<String>();

  /** The class writer that contains the bytecode. */
  protected StringWriter stringWriter = null;

  /**
   * The adapater used to manipulate the code.
   *
   * @param   cv    The ClassVisitor used in this object.
   * @param   cw    The string writer.
   *
   */
  public CorbaOnewayAdapter(ClassVisitor cv, StringWriter  cw) {
    super(cv);
    stringWriter = cw;
  }
  
  /**
   * Ovveride.
   * @param version     The version
   * @param access      The access
   * @param name        The name
   * @param signature   The signature
   * @param superName   The superName
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

    setInternalClassName(name);


    /* WARNING:
     *
     * The class inspected is a stub but the original class where the operation
     * was defined is not the stub.
     * There is a name convention between stub and original interface:
     *
     *          OrigInterface => _OrigInterfaceStub
     *                        => OrigInterfaceOperations
     */

    int underscoreIndex = name.lastIndexOf('/') + 1;

    // _InterfaceNameStub - underscore - Stub
    String tempSimpleName
      = name.substring(underscoreIndex + 1, name.length() - 4);
    String tempPackage
      = name.substring(0, underscoreIndex);
    String temp = tempPackage.replace('/', '.') + tempSimpleName;

    setAssociatedInterface(temp + "Operations");
    

    LOG.debug("<<<<< visit - end");
    super.visit(version, access, name, signature, superName, interfaces);
  }

  /**
   * Override.
   * @param access      The access
   * @param name        The name
   * @param desc        The desc
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

    CorbaOnewayMethodVisitor corbaOnewayMethodVisitor
      = new CorbaOnewayMethodVisitor(getInternalClassName(),
                                     name,
                                     getOnewayOperationList());

    MethodVisitor mv = corbaOnewayMethodVisitor.visitMethod(access,
                                                            name,
                                                            desc,
                                                            signature,
                                                            exceptions);

    LOG.debug("<<<<< visitMethod - end. "
            + "methodName=" + name + "; methodDescription=" + desc);

    return mv;
  }


  // getter and setter
/**
 * @return  the return
 */
  public String getInternalClassName() {
    return internalClassName;
  }

  /**
   * @param internalClassName  The internal class name
   */
  public void setInternalClassName(String internalClassName) {
    this.internalClassName = internalClassName;
  }

  /**
   * @return  The return
   */
  public List<String> getOnewayOperationList() {
      return onewayOperationList;
  }

  /**
   * @param onewayOperationList  The oneway operation list
   */
  public void setOnewayOperationList(List<String> onewayOperationList) {
      this.onewayOperationList = onewayOperationList;
  }

  /**
   * @return  The return
   */
  public String getAssociatedInterface() {
      return associatedInterface;
  }

  /**
   * @param associatedInterface  The associated interface.
   */
  public void setAssociatedInterface(String associatedInterface) {
      this.associatedInterface = associatedInterface;
  }

}
