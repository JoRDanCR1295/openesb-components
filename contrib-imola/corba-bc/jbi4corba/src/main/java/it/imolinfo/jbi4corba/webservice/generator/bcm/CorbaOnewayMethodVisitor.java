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
import java.util.ArrayList;
import java.util.List;
import org.objectweb.asm.Opcodes;

import org.objectweb.asm.commons.EmptyVisitor;

/**
 * This class is used to discover if a method is a oneway corba request.
 */
public class CorbaOnewayMethodVisitor extends EmptyVisitor {

  /**
   * Logger.
   */
  private static final Logger LOG
    = LoggerFactory.getLogger(CorbaOnewayMethodVisitor.class);

  /** The internal name of the stub. */
  protected String internalStubName = null;

  /** The method name. */
  protected String methodName = null;

  /** The list of the 'oneway' operations. */
  protected List<String> onewayOperationList = new ArrayList<String>();

  /** The boolean information to create the oneway corba request. */
  protected boolean zero = false;

  /**
   * Constructor.
   *
   * @param   stub                  The internal class name of the stub.
   * @param   aMethodName           The name of the method visited.
   * @param   aOnewayOperationList  The oneway request collected.
   */
  public CorbaOnewayMethodVisitor(String stub, String aMethodName,
    List<String> aOnewayOperationList) {
    super();
    internalStubName = stub;
    methodName = aMethodName;
    onewayOperationList = aOnewayOperationList;
  }

  /**
   * Override.
   * @param opcode  The op code
   */
  @Override
  public void visitInsn(int opcode) {
    super.visitInsn(opcode);

    if (Opcodes.ICONST_0 == opcode) {
      zero = true;
    } else {
      zero = false;
    }
    //LOG.debug("___ ZERO=" + zero);
  }

  /**
   * Override.
   * @param opcode  The op code
   * @param owner   The owner
   * @param name    The name
   * @param desc    The desc
   */
  @Override
  public void visitMethodInsn(int opcode,String owner,String name,String desc) {

    LOG.debug("CorbaOnewayMethodVisitor.visitMethodInsn. opcode=" + opcode
      + "; owner=" + owner + "; name=" + name + "; desc=" + desc);

    final String corbaOutputStream = "(Ljava/lang/String;Z)"
                                   + "Lorg/omg/CORBA/portable/OutputStream;";
    /*
    mv.visitMethodInsn(
        INVOKEVIRTUAL,
        "it/imolinfo/jbi4corba/test/async/_EchoAsyncStub",
        "_request",
        "(Ljava/lang/String;Z)Lorg/omg/CORBA/portable/OutputStream;"
     );
     */
    if (Opcodes.INVOKEVIRTUAL == opcode
      && owner.equals(internalStubName)
      && "_request".equals(name)
      && corbaOutputStream.equals(desc)
      && zero) {

      LOG.debug("The method " + methodName + " is a oneway corba request.");
      onewayOperationList.add(methodName);
    } else {
      LOG.debug("The method " + methodName + " is NOT a oneway corba request.");
    }

    super.visitMethodInsn(opcode, owner, name, desc);
  }

}
