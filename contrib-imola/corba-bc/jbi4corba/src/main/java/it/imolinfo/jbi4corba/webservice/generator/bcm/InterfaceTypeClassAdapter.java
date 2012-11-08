 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator.bcm;

import it.imolinfo.jbi4corba.webservice.generator.InterfaceType;
import it.imolinfo.jbi4corba.webservice.generator.InterfaceTypeUtils;
import it.imolinfo.jbi4corba.webservice.generator.TypeUtils;
import it.imolinfo.jbi4corba.webservice.generator.Util;
import java.util.Map;

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Type;

/**
 * 
 * This ASM Adapter class changes the change the return type of a method that
 * return an interface for all methods that return's an interface it returns an
 * W3CEndpointReference
 * 
 * @author Luca Acquaviva
 */
public class InterfaceTypeClassAdapter extends ClassAdapter {

	private Map<String, InterfaceType> opInterfaceTypes = null;
	private static final String W3CEPR = Type
			.getDescriptor(javax.xml.ws.wsaddressing.W3CEndpointReference.class);
	private static final String W3CEPRA = Type
			.getDescriptor(javax.xml.ws.wsaddressing.W3CEndpointReference[].class);
       

	/**
	 * 
	 * The Format of descriptor is
	 * (Ljava/lang/String;)Lit/imolinfo/jbi4corba/test;
	 * /testprovidersimplemult/Echo2; input return type
	 */
	@Override
	public MethodVisitor visitMethod(int access, String name, String desc,
			String signature, String[] exceptions) {

		if (!"<init>".equals(name)) {
			
             // change all interface type with W3CENDPOINTREFERENCE for 
			if (desc != null) {
				desc=InterfaceTypeUtils.changeInterfaceTypeDesc(desc, opInterfaceTypes);
	
			}
		}
		return super.visitMethod(access, name, desc, signature, exceptions);
	}

        
       
	
	

        
	public InterfaceTypeClassAdapter(ClassVisitor arg0,
			Map<String, InterfaceType> opIntTypes) {
		super(arg0);
		opInterfaceTypes = opIntTypes;
	}
}
