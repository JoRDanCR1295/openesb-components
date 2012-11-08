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
import it.imolinfo.jbi4corba.webservice.generator.InterfaceType;
import it.imolinfo.jbi4corba.webservice.generator.TypeUtils;
import it.imolinfo.jbi4corba.webservice.generator.Util;

import java.util.HashMap;
import java.util.Map;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.util.TraceMethodVisitor;

/**
 * This class is to modify the constructor 
 * change the field Type : Interfacetype with type W3CEndpointReference 
 * the code to initialize some fields.
 */
public class ConstructorModMethodVisitor extends TraceMethodVisitor {

  /**
   * Logger.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(ConstructorModMethodVisitor.class);


 /**
  * The name of the class.
  */
  private String className = null;

  // Utility Object
  private ByteCodeManipulationUtil bcmUtil = new ByteCodeManipulationUtil();
  
  private Map<String, InterfaceType> allIntTypes;

 /**
  * Constructor.
  *
  * @param    mv                The method visitor.
  * @param    mapOfFields        The fields to initialize.
  * @param    className        The name of the class.
 * @param allInterfaceTypes 
  */
  public ConstructorModMethodVisitor(MethodVisitor mv,
                              String className, Map<String, InterfaceType> allInterfaceTypes) {
    super(mv);

    
    this.className = className;
    allIntTypes=allInterfaceTypes;
  }

  
  /**
   * This method visit the code of the constructor and change InterfaceType with W3CEndpointRef 
   * 
   * */
  
  public void visitFieldInsn(int opcode,String className,String fieldName,String fieldType){
	  
	  LOG.debug("<<<<< AppenderMethodVisitor.visitInsn - end");
	  if(opcode==Opcodes.PUTFIELD){
		  boolean isArray=TypeUtils.isArray(fieldType);
	  		  if(TypeUtils.isSearchedType(fieldType, isArray, allIntTypes, TypeUtils.INTERFACE)!=null){
	  			  if (isArray){
	  				  fieldType= Type.getDescriptor(javax.xml.ws.wsaddressing.W3CEndpointReference[].class);
		  
	  			  }else{
	  				  
	  				  fieldType= Type.getDescriptor(javax.xml.ws.wsaddressing.W3CEndpointReference.class);
	  				
	  			  }
	  }
	  super.visitFieldInsn(opcode,className,fieldName,fieldType);
	  
	  }
	  
  }
  
  
 
	
  
  

}
